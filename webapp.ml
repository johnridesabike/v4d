(**************************************************************************)
(*                                                                        *)
(*                      Copyright (c) John Jackson.                       *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Gg
open Vg
open Brr
open Common

type shape = Cube | Polycube | Cross_double

let shape_get = function
  | Cube -> Data.cube
  | Polycube -> Data.polycube
  | Cross_double -> Data.cross_double

let shape_eq (a : shape) (b : shape) = a = b

type projection = Perspective | Slice | Oblique

let projection_eq (a : projection) (b : projection) = a = b

let projection_show = function
  | Perspective -> "Perspective"
  | Slice -> "Slice"
  | Oblique -> "Oblique"

type scene = { shape : shape; view : Box2.t }

let scene_eq a { shape; view = _ } = shape_eq a.shape shape

let scene shape height depth =
  let size = (height +. (sqrt 2.0 *. depth)) *. 1.125 in
  let o = size /. -2.0 in
  { shape; view = Box2.v (V2.v o o) (V2.v size size) }

let scene_cube = scene Cube 2.0 2.0
let scene_polycube = scene Polycube 3.0 1.5
let scene_cross = scene Cross_double 2.0 0.4

module State : sig
  type 'a t

  val make : ('a -> 'a -> bool) -> 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
  val listen : 'a t -> ('a -> unit) -> unit
  val is_dirty : unit -> bool
  val set_dirty : unit -> unit
  val set_clean : unit -> unit
end = struct
  type 'a t = {
    eq : 'a -> 'a -> bool;
    mutable v : 'a;
    mutable fs : ('a -> unit) list;
  }

  let dirty = ref true
  let make eq v = { eq; v; fs = [] }
  let get t = t.v

  let set t v =
    if not (t.eq t.v v) then (
      t.v <- v;
      dirty := true;
      List.iter (( |> ) v) t.fs)

  let listen t f =
    f t.v;
    t.fs <- f :: t.fs

  let is_dirty () = !dirty
  let set_dirty () = dirty := true
  let set_clean () = dirty := false
end

let state_make_float = State.make Float.equal
let state_make_bool = State.make Bool.equal
let state_scene = State.make scene_eq scene_cube
let state_projection = State.make projection_eq Slice
let state_slice = state_make_float 0.0
let state_xy = state_make_float 0.0
let state_yz = state_make_float (-20.0)
let state_xz = state_make_float 30.0
let state_xw = state_make_float 1.0
let state_yw = state_make_float 1.0
let state_zw = state_make_float 1.0
let state_auto_xy = state_make_bool false
let state_auto_yz = state_make_bool false
let state_auto_xz = state_make_bool false
let state_auto_xw = state_make_bool false
let state_auto_yw = state_make_bool false
let state_auto_zw = state_make_bool false
let state_auto_slice = state_make_bool false

type click_rotate =
  | Not_clicked
  | Clicked of {
      start_x : float;
      start_y : float;
      curr_x : float;
      curr_y : float;
      xz : float;
      yz : float;
    }

let click_rotate_equal a b =
  match (a, b) with
  | Not_clicked, Not_clicked -> true
  | Clicked a, Clicked { start_x; start_y; curr_x; curr_y; xz; yz } ->
      a.start_x = start_x && a.start_y = start_y && a.curr_x = curr_x
      && a.curr_y = curr_y && a.xz = xz && a.yz = yz
  | Not_clicked, _ | Clicked _, _ -> false

let state_clicked = State.make click_rotate_equal Not_clicked

(** The next options aren't configurable... yet. *)

let distance = 6.0
let bg = I.const Data.color_darkblack
let color = I.const Data.color_brightwhite
let size = Size2.v 200.0 200.0 (* mm *)

let draw (canvas : El.t) =
  let scene = State.get state_scene in
  let path =
    match State.get state_projection with
    | Perspective ->
        shape_get scene.shape
        |> Polygon.rotate
             ~xy:(Float.rad_of_deg (State.get state_xy))
             ~yz:(Float.rad_of_deg (State.get state_yz))
             ~xz:(Float.rad_of_deg (State.get state_xz))
             ~xw:(Float.rad_of_deg (State.get state_xw))
             ~yw:(Float.rad_of_deg (State.get state_yw))
             ~zw:(Float.rad_of_deg (State.get state_zw))
        |> Polygon.wireframe
        |> Path4d.project_perspective distance
    | Slice ->
        shape_get scene.shape
        |> Polygon.rotate
             ~xy:(Float.rad_of_deg (State.get state_xy))
             ~yz:(Float.rad_of_deg (State.get state_yz))
             ~xz:(Float.rad_of_deg (State.get state_xz))
             ~xw:(Float.rad_of_deg (State.get state_xw))
             ~yw:(Float.rad_of_deg (State.get state_yw))
             ~zw:(Float.rad_of_deg (State.get state_zw))
        |> Polygon.slice (State.get state_slice)
        |> Path4d.project_perspective distance
    | Oblique ->
        shape_get scene.shape |> Polygon.wireframe |> Path4d.project_oblique
  in
  let width = Box2.w scene.view *. 0.0025 in
  let area = `O { P.o with cap = `Round; join = `Round; width } in
  let outline = I.cut ~area path color in
  let image = I.void |> I.blend bg |> I.blend outline in
  let r = Vgr.create (Vgr_htmlc.target ~resize:false canvas) `Other in
  let rec loop v =
    match Vgr.render r v with
    | `Partial -> loop `Await
    | `Ok -> ignore (Vgr.render r `End)
  in
  loop (`Image (size, scene.view, image))

let el_input label el =
  El.label
    ~at:[ At.class' (Jstr.v "input-row") ]
    [ El.span ~at:[ At.class' (Jstr.v "label-text") ] [ El.txt' label ]; el ]

let el_range ~auto_allowed ~min ~max ~step ~frac state_auto state_angle label =
  let el_range =
    El.input
      ~at:
        [
          At.type' (Jstr.v "range");
          At.v (Jstr.v "min") (Jstr.v min);
          At.v (Jstr.v "max") (Jstr.v max);
          At.v (Jstr.v "step") (Jstr.v step);
        ]
      ()
  in
  (* Display the range value visually, but hide it for screen readers since they
     can read the range directly. *)
  let el_value =
    El.span
      ~at:
        [
          At.class' (Jstr.v "label-value");
          At.v (Jstr.v "aria-hidden") (Jstr.v "true");
        ]
      [ El.txt' "" ]
  in
  let el_checkbox = El.input ~at:[ At.type' (Jstr.v "checkbox") ] () in
  ignore
  @@ Ev.listen Ev.input
       (fun _e ->
         State.set state_angle (El.prop El.Prop.value el_range |> Jstr.to_float))
       (El.as_target el_range);
  ignore
  @@ Ev.listen Ev.input
       (fun _e -> State.set state_auto (El.prop El.Prop.checked el_checkbox))
       (El.as_target el_checkbox);
  State.listen state_auto (fun v -> El.set_prop El.Prop.checked v el_checkbox);
  State.listen state_projection (fun v ->
      if auto_allowed v then El.set_at At.Name.disabled None el_checkbox
      else (
        El.set_at At.Name.disabled (Some Jstr.empty) el_checkbox;
        State.set state_auto false));
  State.listen state_angle (fun v ->
      El.set_prop El.Prop.value (Jstr.of_float v) el_range;
      El.set_children el_value [ El.txt (Jstr.of_float ~frac v) ]);
  El.div
    ~at:[ At.class' (Jstr.v "input-row input-range-container") ]
    [
      El.label
        ~at:[ At.class' (Jstr.v "input-range-label") ]
        [
          El.span ~at:[ At.class' (Jstr.v "label-text") ] [ El.txt' label ];
          el_range;
          el_value;
        ];
      El.label
        ~at:[ At.class' (Jstr.v "input-range-label") ]
        [ El.txt' "Auto"; el_checkbox ];
    ]

let el_input_shape scene label =
  let el =
    El.input
      ~at:
        [
          At.type' (Jstr.v "radio");
          At.name (Jstr.v "shape");
          At.value (Jstr.v "cube");
        ]
      ()
  in
  ignore
  @@ Ev.listen Ev.input
       (fun _e -> State.set state_scene scene)
       (El.as_target el);
  State.listen state_scene (fun v ->
      El.set_prop El.Prop.checked (scene_eq v scene) el);
  el_input label el

let el_controls_specialized = El.div []

let el_input_projection projection children =
  let el =
    El.input ~at:[ At.type' (Jstr.v "radio"); At.name (Jstr.v "projection") ] ()
  in
  ignore
  @@ Ev.listen Ev.input
       (fun _e -> State.set state_projection projection)
       (El.as_target el);
  State.listen state_projection (fun v ->
      let b = projection_eq v projection in
      El.set_prop El.Prop.checked b el;
      if b then El.set_children el_controls_specialized children);
  el_input (projection_show projection) el

let canvas, canvas_container =
  let el =
    El.canvas
      ~at:
        [
          At.v (Jstr.v "aria-label")
            (Jstr.v "An interactive image of a geometric shape.");
        ]
      []
  in
  let container = El.div ~at:[ At.class' (Jstr.v "canvas-container") ] [ el ] in
  ignore
  @@ Ev.listen Ev.pointerdown
       (fun e ->
         Ev.prevent_default e;
         let e = Ev.as_type e |> Ev.Pointer.as_mouse in
         match State.get state_projection with
         | Oblique -> ()
         | Perspective | Slice ->
             State.set state_clicked
               (Clicked
                  {
                    start_x = Ev.Mouse.screen_x e;
                    start_y = Ev.Mouse.screen_y e;
                    curr_x = Ev.Mouse.screen_x e;
                    curr_y = Ev.Mouse.screen_y e;
                    xz = State.get state_xz;
                    yz = State.get state_yz;
                  }))
       (El.as_target container);
  ignore
  @@ Ev.listen Ev.pointermove
       (fun e ->
         match State.get state_clicked with
         | Not_clicked -> ()
         | Clicked s ->
             Ev.prevent_default e;
             let e = Ev.as_type e |> Ev.Pointer.as_mouse in
             State.set state_clicked
               (Clicked
                  {
                    s with
                    curr_x = Ev.Mouse.screen_x e;
                    curr_y = Ev.Mouse.screen_y e;
                  }))
       G.target;
  let unclick _e = State.set state_clicked Not_clicked in
  ignore @@ Ev.listen Ev.pointerup unclick G.target;
  ignore @@ Ev.listen Ev.pointerleave unclick G.target;
  (el, container)

let el_angle =
  el_range
    ~auto_allowed:(function Perspective | Slice -> true | Oblique -> false)
    ~min:"-180" ~max:"180" ~step:"0.1" ~frac:0

let el_angle_controls =
  [
    El.fieldset
      ~at:[ At.class' (Jstr.v "fieldset-range") ]
      [
        El.legend [ El.txt' "3D rotate" ];
        el_angle state_auto_xy state_xy "XY";
        el_angle state_auto_xz state_xz "XZ";
        el_angle state_auto_yz state_yz "YZ";
      ];
    El.fieldset
      ~at:[ At.class' (Jstr.v "fieldset-range") ]
      [
        El.legend [ El.txt' "4D rotate" ];
        el_angle state_auto_xw state_xw "XW";
        el_angle state_auto_yw state_yw "YW";
        el_angle state_auto_zw state_zw "ZW";
      ];
  ]

let () =
  El.append_children (Document.body G.document)
    [
      El.v (Jstr.v "main")
        [
          canvas_container;
          El.div
            ~at:[ At.class' (Jstr.v "controls-container") ]
            [
              El.div
                ~at:[ At.class' (Jstr.v "controls") ]
                [
                  El.h1 [ El.txt' "V4D: Visualize 4D Shapes" ];
                  El.fieldset
                    [
                      El.legend [ El.txt' "Shape" ];
                      el_input_shape scene_cube "Cube";
                      el_input_shape scene_polycube "Polycube";
                      el_input_shape scene_cross "Cross";
                    ];
                  El.fieldset
                    [
                      El.legend [ El.txt' "Projection" ];
                      el_input_projection Slice
                        (el_angle_controls
                        @ [
                            El.fieldset
                              ~at:[ At.class' (Jstr.v "fieldset-range") ]
                              [
                                El.legend [ El.txt' "Slice" ];
                                el_range ~auto_allowed:(projection_eq Slice)
                                  ~min:"-2" ~max:"2" ~step:"0.01" ~frac:2
                                  state_auto_slice state_slice "Depth";
                              ];
                          ]);
                      el_input_projection Perspective el_angle_controls;
                      el_input_projection Oblique [];
                    ];
                  el_controls_specialized;
                  El.details
                    [
                      El.summary [ El.txt' "What is this?" ];
                      El.p
                        [
                          El.txt'
                            "This application visualizes how a \
                             four-dimensional object would look to a \
                             three-dimensional being like you or me.";
                        ];
                      El.h2 [ El.txt' "Regarding the shapes" ];
                      El.p
                        [
                          El.txt' "The ";
                          El.strong [ El.txt' "cube" ];
                          El.txt'
                            " (i.e. tesseract) demonstrates how 4D rotations \
                             work most simply.";
                        ];
                      El.p
                        [
                          El.txt' " The ";
                          El.strong [ El.txt' "polycube" ];
                          El.txt'
                            " cannot be mirrored by 3D rotations, but it can \
                             in 4D.";
                        ];
                      El.p
                        [
                          El.txt' "The ";
                          El.strong [ El.txt' "double crossʼs" ];
                          El.txt'
                            " numerous arms can manifest as \
                             seemingly-disconnected slices.";
                        ];
                      El.h2 [ El.txt' "Regarding the projections" ];
                      El.p
                        [
                          El.txt' "The ";
                          El.strong [ El.txt' "slice" ];
                          El.txt'
                            " projection is how a 4D object would appear if it \
                             passed through our space. This is analogous to if \
                             a 3D object intersected a plane that cut out a 2D \
                             cross-section. Similarly here, we can see a 3D \
                             “slice” of a 4D shape.";
                        ];
                      El.p
                        [
                          El.txt' "The ";
                          El.strong [ El.txt' "perspective" ];
                          El.txt'
                            " projection shows the entire 4D shape at once. \
                             This is analogous to how 3D objects cast 2D \
                             shadows, except this “shadow” is 3D.";
                        ];
                      El.p
                        [
                          El.txt' "The ";
                          El.strong [ El.txt' "oblique" ];
                          El.txt'
                            " projection also shows the entire 4D shape at \
                             once but distorts it so parallel lines remain \
                             parallel. This is not how a shape would ever \
                             really appear, but it makes it easy for us to see \
                             how the shape is composed.";
                        ];
                      El.h2 [ El.txt' "Credits" ];
                      El.p
                        [
                          El.a
                            ~at:
                              [
                                At.href
                                  (Jstr.v
                                     "https://github.com/johnridesabike/v4d");
                              ]
                            [ El.txt' "You can get the source code here" ];
                          El.txt' ". This was inspired by, among other things, ";
                          El.a
                            ~at:[ At.href (Jstr.v "https://4dtoys.com/") ]
                            [ El.txt' "the 4D Toys app" ];
                          El.txt' ".";
                        ];
                      El.p
                        [
                          El.txt' "—";
                          El.a
                            ~at:[ At.href (Jstr.v "https://johnridesa.bike/") ]
                            [ El.txt' "John" ];
                        ];
                    ];
                ];
            ];
        ];
    ]

let () = ignore @@ Ev.listen Ev.resize (fun _e -> State.set_dirty ()) G.target
let start_time = ref 0.0

let auto_rotate ~low ~high ~step state_auto state =
  if State.get state_auto then
    let x = State.get state +. step in
    let x = if x >= high then x -. high +. low else x in
    State.set state x

let rec constrain_drag x =
  if x < -180.0 then constrain_drag (x +. 360.0)
  else if x > 180.0 then constrain_drag (x -. 360.0)
  else x

let rec animation_loop time =
  let elapsed = time -. !start_time in
  start_time := time;
  let step = elapsed *. 0.02 in
  let slice_step = 4.0 /. 360.0 *. elapsed *. 0.02 in
  auto_rotate ~low:(-180.0) ~high:180.0 ~step state_auto_xy state_xy;
  auto_rotate ~low:(-180.0) ~high:180.0 ~step state_auto_xz state_xz;
  auto_rotate ~low:(-180.0) ~high:180.0 ~step state_auto_yz state_yz;
  auto_rotate ~low:(-180.0) ~high:180.0 ~step state_auto_xw state_xw;
  auto_rotate ~low:(-180.0) ~high:180.0 ~step state_auto_yw state_yw;
  auto_rotate ~low:(-180.0) ~high:180.0 ~step state_auto_zw state_zw;
  auto_rotate ~low:(-2.0) ~high:2.0 ~step:slice_step state_auto_slice
    state_slice;
  (match State.get state_clicked with
  | Not_clicked -> ()
  | Clicked { start_x; start_y; curr_x; curr_y; xz; yz } ->
      State.set state_xz (constrain_drag (xz +. ((curr_x -. start_x) /. 2.0)));
      State.set state_yz (constrain_drag (yz -. ((curr_y -. start_y) /. 2.0))));
  if State.is_dirty () then (
    State.set_clean ();
    draw canvas);
  ignore @@ G.request_animation_frame animation_loop

let () =
  ignore
  @@ G.request_animation_frame (fun time ->
         start_time := time;
         animation_loop time)

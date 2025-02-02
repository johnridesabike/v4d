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
open Common

let shape = ref Data.cube

let projection =
  ref (fun () -> Polygon.wireframe !shape |> Path4d.project_oblique)

let view_size = ref 0.0
let distance = 6.0
let slice_at = ref 0.0
let axy = ref 0.0
let ayz = ref (-20.0)
let axz = ref 30.0
let axw = ref 0.0
let ayw = ref 0.0
let azw = ref 0.0

let () =
  Arg.parse
    [
      ( "--shape",
        Symbol
          ( [ "cube"; "polycube"; "latin-cross"; "double-cross" ],
            function
            | "cube" -> shape := Data.cube
            | "polycube" -> shape := Data.polycube
            | "latin-cross" -> shape := Data.cross_latin
            | _ -> shape := Data.cross_double ),
        " Set the shape." );
      ( "--project",
        Symbol
          ( [ "isometric"; "dimetric"; "oblique"; "perspective"; "slice" ],
            function
            | "isometric" ->
                projection :=
                  fun () -> Polygon.wireframe !shape |> Path4d.project_isometric
            | "dimetric" ->
                projection :=
                  fun () -> Polygon.wireframe !shape |> Path4d.project_dimetric
            | "perspective" ->
                projection :=
                  fun () ->
                    Polygon.rotate ~xy:(Float.rad_of_deg !axy)
                      ~yz:(Float.rad_of_deg !ayz) ~xz:(Float.rad_of_deg !axz)
                      ~xw:(Float.rad_of_deg !axw) ~yw:(Float.rad_of_deg !ayw)
                      ~zw:(Float.rad_of_deg !azw) !shape
                    |> Polygon.wireframe
                    |> Path4d.project_perspective distance
            | "slice" ->
                projection :=
                  fun () ->
                    Polygon.rotate ~xy:(Float.rad_of_deg !axy)
                      ~yz:(Float.rad_of_deg !ayz) ~xz:(Float.rad_of_deg !axz)
                      ~xw:(Float.rad_of_deg !axw) ~yw:(Float.rad_of_deg !ayw)
                      ~zw:(Float.rad_of_deg !azw) !shape
                    |> Polygon.slice !slice_at
                    |> Path4d.project_perspective distance
            | _ ->
                projection :=
                  fun () -> Polygon.wireframe !shape |> Path4d.project_oblique
          ),
        " Set the projection." );
      ("--xy", Set_float axy, " Set the XY angle.");
      ("--xz", Set_float axz, " Set the XZ angle.");
      ("--yz", Set_float ayz, " Set the YZ angle.");
      ("--xw", Set_float axw, " Set the XW angle.");
      ("--yw", Set_float ayw, " Set the YW angle.");
      ("--zw", Set_float azw, " Set the ZW angle.");
      ("--slice", Set_float slice_at, " Set the slice distance.");
      ("--view", Set_float view_size, " Set the view size.");
    ]
    invalid_arg "Project a 4D shape."

let shape = !projection ()

let view =
  match !view_size with
  | 0.0 ->
      let bounds =
        P.fold
          (fun box -> function
            | `Sub p | `Line p -> Box2.add_pt box p | _ -> box)
          Box2.empty shape
      in
      if Box2.is_empty bounds then Box2.v (V2.v (-1.0) (-1.0)) (V2.v 2.0 2.0)
      else
        let origin = Box2.o bounds in
        let size = Box2.size bounds in
        let max_dim = max (Box2.w bounds) (Box2.h bounds) in
        let padded = max_dim *. 1.0625 in
        let size_square = V2.v padded padded in
        let origin = V2.sub origin (V2.sub size_square size |> V2.half) in
        Box2.v origin size_square
  | view_size ->
      let size = V2.v view_size view_size in
      let origin = V2.half size |> V2.neg in
      Box2.v origin size

let bg = I.const Data.color_darkblack
let color = I.const Data.color_brightwhite
let width = Box2.w view *. 0.0025
let area = `O { P.o with cap = `Round; join = `Round; width }
let outline = I.cut ~area shape color
let image = I.void |> I.blend bg |> I.blend outline
let r = Vgr.create (Vgr_svg.target ()) (`Channel stdout)
let size = Size2.v 200. 200. (* mm *)

let rec loop v =
  match Vgr.render r v with
  | `Partial -> loop `Await
  | `Ok -> ignore (Vgr.render r `End)

let () = loop (`Image (size, view, image))

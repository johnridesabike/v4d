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

let[@ocamlformat "disable"] mirror_x =
  M4.v  (-1.)     0.        0.        0.
        0.        1.        0.        0.
        0.        0.        1.        0.
        0.        0.        0.        1.

let[@ocamlformat "disable"] mirror_y =
  M4.v  1.        0.        0.        0.
        0.        (-1.)     0.        0.
        0.        0.        1.        0.
        0.        0.        0.        1.

let[@ocamlformat "disable"] mirror_z =
  M4.v  1.        0.        0.        0.
        0.        1.        0.        0.
        0.        0.        (-1.)     0.
        0.        0.        0.        1.

let[@ocamlformat "disable"] mirror_w =
  M4.v  1.        0.        0.        0.
        0.        1.        0.        0.
        0.        0.        1.        0.
        0.        0.        0.        (-1.)

let[@ocamlformat "disable"] rotate_yz a =
  M4.v  1.        0.        0.        0.
        0.        (cos a)   (sin a)   0.
        0.        (-.sin a) (cos a)   0.
        0.        0.        0.        1.

let[@ocamlformat "disable"] rotate_xz a =
  M4.v  (cos a)   0.        (sin a)   0.
        0.        1.        0.        0.
        (-.sin a) 0.        (cos a)   0.
        0.        0.        0.        1.

let[@ocamlformat "disable"] rotate_xy a =
  M4.v  (cos a)   (sin a)   0.        0.
        (-.sin a) (cos a)   0.        0.
        0.        0.        1.        0.
        0.        0.        0.        1.

let[@ocamlformat "disable"] rotate_zw a =
  M4.v  1.        0.        0.        0.
        0.        1.        0.        0.
        0.        0.        (cos a)   (sin a)
        0.        0.        (-.sin a) (cos a)

let[@ocamlformat "disable"] rotate_yw a =
  M4.v  1.        0.        0.        0.
        0.        (cos a)   0.        (sin a)
        0.        0.        1.        0.
        0.        (-.sin a) 0.        (cos a)

let[@ocamlformat "disable"] rotate_xw a =
  M4.v  (cos a)   0.        0.        (sin a)
        0.        1.        0.        0.
        0.        0.        1.        0.
        (-.sin a) 0.        0.        (cos a)

module Path4d = struct
  type t = (v4 -> v2) -> path
  (** Lazy 4D paths that need to be projected into 2D. *)

  let empty _ = P.empty
  let sub point path f = P.sub (f point) (path f)
  let line point path f = P.line (f point) (path f)
  let v4_to_v2 p = V2.v (V4.x p) (V4.y p)

  let project_perspective distance path =
    path (fun p ->
        (* Project from a distance without shrinking the image. *)
        let denom = distance -. V4.z p -. V4.w p in
        let d = if denom = 0.0 then 1.0 else distance /. denom in
        V2.v (V4.x p *. d) (V4.y p *. d))

  let project_aux rotate path = path (fun p -> V4.ltr rotate p |> v4_to_v2)

  let project_isometric =
    let a = asin (tan (Float.rad_of_deg 30.)) in
    let b = Float.rad_of_deg 45. in
    let[@ocamlformat "disable"] rotate_x =
      M4.v  1.      0.        0.        0.
            0.      (cos a)   (sin a)   (sin a)
            0.      (-.sin a) (cos a)   0.
            0.      0.        0.        (cos a)
    in
    let[@ocamlformat "disable"] rotate_y =
      M4.v  (cos b) 0.        (-.sin b) (sin b)
            0.      1.        0.        0.
            (sin b) 0.        (cos b)   0.
            0.      0.        0.        (cos b)
    in
    let rotate = M4.mul rotate_x rotate_y in
    project_aux rotate

  let project_dimetric =
    let a = Float.rad_of_deg 7. in
    let b = Float.rad_of_deg 42. in
    let[@ocamlformat "disable"] rotate =
      M4.v  (cos a)   0.  (cos b /. 2.) (cos b /. 2.)
            (-.sin a) 1.  (sin b /. 2.) (-.sin b /. 2.)
            0.        0.  1.            0.
            0.        0.  0.            1.
    in
    project_aux rotate

  let project_oblique =
    let a = Float.rad_of_deg 45. in
    let[@ocamlformat "disable"] rotate =
      M4.v  1.  0.  (cos a) (-.cos a)
            0.  1.  (sin a) (sin a)
            0.  0.  1.      0.
            0.  0.  0.      1.
    in
    project_aux rotate
end

module Polygon = struct
  module Fan = struct
    type close = Close_on_center | Close_on_first_point

    type t = {
      center : v4;
      last : v4;
      next : v4;
      rest : v4 list;
      close : close;
    }
    (** last, next, and rest are stored and processed in reverse order. *)

    let center_inside ~center next last =
      { center; last; next; rest = []; close = Close_on_first_point }

    let center_on_edge ~center next last =
      { center; last; next; rest = []; close = Close_on_center }

    let add point t =
      { t with last = point; next = t.last; rest = t.next :: t.rest }

    (** Fold the lines in reverse order. *)
    let to_wireframe { center; last; next; close; rest } path =
      let rec aux acc final = function
        | point :: tl -> aux (Path4d.line point acc) final tl
        | [] -> Path4d.line final acc
      in
      match close with
      | Close_on_center ->
          aux
            (Path4d.sub center path |> Path4d.line last |> Path4d.line next)
            center rest
      | Close_on_first_point ->
          aux (Path4d.sub last path |> Path4d.line next) last rest

    (** Fold the triangles in reverse order. *)
    let fold_triangles f { center; last; next; close; rest } init =
      let rec aux acc last_point = function
        | point :: tl -> aux (f center last_point point acc) point tl
        | [] -> (
            match close with
            | Close_on_center -> acc
            | Close_on_first_point -> f center last_point last acc)
      in
      aux (f center last next init) next rest

    let map_points f { center; last; next; close; rest } =
      {
        center = f center;
        last = f last;
        next = f next;
        rest = List.map f rest;
        close;
      }
  end

  type t = Fan.t Seq.t

  let empty = Seq.empty
  let add = Seq.cons
  let append = Seq.append
  let to_path4d f t = Seq.fold_left f Path4d.empty t
  let map f s = Seq.map (Fan.map_points f) s
  let ltr m s = map (V4.ltr m) s
  let move v poly = map (V4.add v) poly

  let rotate ~xy ~xz ~yz ~xw ~yw ~zw poly =
    ltr
      (M4.mul
         (M4.mul
            (M4.mul
               (M4.mul (M4.mul (rotate_yz yz) (rotate_xz xz)) (rotate_xy xy))
               (rotate_xw xw))
            (rotate_yw yw))
         (rotate_zw zw))
      poly

  let wireframe poly =
    to_path4d (fun path tri -> Fan.to_wireframe tri path) poly

  (* a, aw
      |\
      | * result
      | .\
      | . \
      --.-- b, bw
        w *)
  let intersection =
    let aux ratio a b = a -. (ratio *. (a -. b)) in
    fun w a b ->
      let r = (w -. V4.w a) /. (V4.w b -. V4.w a) in
      V4.v
        (aux r (V4.x a) (V4.x b))
        (aux r (V4.y a) (V4.y b))
        (aux r (V4.z a) (V4.z b))
        0.0

  let between w a b = (V4.w a -. w) *. (V4.w b -. w) < 0.

  let slice w poly =
    to_path4d
      (fun acc tri ->
        Fan.fold_triangles
          (fun a b c path ->
            match (between w a b, between w b c, between w c a) with
            | false, false, false -> path
            | true, true, _ ->
                Path4d.sub (intersection w a b) path
                |> Path4d.line (intersection w b c)
            | true, false, true ->
                Path4d.sub (intersection w a b) path
                |> Path4d.line (intersection w c a)
            | false, true, true ->
                Path4d.sub (intersection w b c) path
                |> Path4d.line (intersection w c a)
            (* The slice must intersect the triangle at exactly zero or two
                points. If it only intersected one edge, then assume the other
                intersection is exactly on the opposite point. *)
            | true, false, false ->
                Path4d.sub (intersection w a b) path |> Path4d.line c
            | false, true, false ->
                Path4d.sub (intersection w b c) path |> Path4d.line a
            | false, false, true ->
                Path4d.sub (intersection w c a) path |> Path4d.line b)
          tri acc)
      poly
end

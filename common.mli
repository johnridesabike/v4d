(**************************************************************************)
(*                                                                        *)
(*                      Copyright (c) John Jackson.                       *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

val mirror_x : Gg.m4
val mirror_y : Gg.m4
val mirror_z : Gg.m4
val mirror_w : Gg.m4

module Path4d : sig
  type t

  val project_perspective : float -> t -> Vg.path
  val project_isometric : t -> Vg.path
  val project_dimetric : t -> Vg.path
  val project_oblique : t -> Vg.path
end

module Polygon : sig
  module Fan : sig
    type t
    (** A triangle fan. *)

    val center_inside : center:Gg.v4 -> Gg.v4 -> Gg.v4 -> t
    (** Implicitly connect the last vertex to the first non-center vertex. E.g.,
        to create a fan fully expanded into a circle. *)

    val center_on_edge : center:Gg.v4 -> Gg.v4 -> Gg.v4 -> t
    (** Implicitly connect the first and last vertices to the center. E.g., to
        create a fan partially expanded into a wedge. *)

    val add : Gg.v4 -> t -> t
  end

  type t

  val empty : t
  val add : Fan.t -> t -> t
  val append : t -> t -> t
  val move : Gg.v4 -> t -> t

  val rotate :
    xy:float ->
    xz:float ->
    yz:float ->
    xw:float ->
    yw:float ->
    zw:float ->
    t ->
    t

  val wireframe : t -> Path4d.t
  val slice : float -> t -> Path4d.t
end

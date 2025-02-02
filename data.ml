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
open Common
module P = Polygon

let color_darkblack = Color.v_srgbi 0x0D 0x1A 0x16
let color_brightwhite = Color.v_srgbi 0xE7 0xFF 0xFF

let box x y z w =
  (*
        a b
        c d
    e f     i j
    g h     k l
        m n
        o p      *)
  let a = V4.v (-.x) y z w in
  let b = V4.ltr mirror_x a in
  let c = V4.ltr mirror_y a in
  let d = V4.ltr mirror_x c in
  let e = V4.ltr mirror_z a in
  let f = V4.ltr mirror_z b in
  let g = V4.ltr mirror_z c in
  let h = V4.ltr mirror_z d in
  let i = V4.ltr mirror_w a in
  let j = V4.ltr mirror_w b in
  let k = V4.ltr mirror_w c in
  let l = V4.ltr mirror_w d in
  let m = V4.ltr mirror_w e in
  let n = V4.ltr mirror_w f in
  let o = V4.ltr mirror_w g in
  let p = V4.ltr mirror_w h in
  P.empty
  (* cube abcdefg *)
  |> P.add (P.Fan.center_on_edge ~center:a b d |> P.Fan.add c)
  |> P.add (P.Fan.center_on_edge ~center:a e g |> P.Fan.add c)
  |> P.add (P.Fan.center_on_edge ~center:a e f |> P.Fan.add b)
  |> P.add (P.Fan.center_on_edge ~center:h g e |> P.Fan.add f)
  |> P.add (P.Fan.center_on_edge ~center:h d b |> P.Fan.add f)
  |> P.add (P.Fan.center_on_edge ~center:h d c |> P.Fan.add g)
     (* cube ijklmnop *)
  |> P.add (P.Fan.center_on_edge ~center:i j l |> P.Fan.add k)
  |> P.add (P.Fan.center_on_edge ~center:i m o |> P.Fan.add k)
  |> P.add (P.Fan.center_on_edge ~center:i m n |> P.Fan.add j)
  |> P.add (P.Fan.center_on_edge ~center:p o m |> P.Fan.add n)
  |> P.add (P.Fan.center_on_edge ~center:p l j |> P.Fan.add n)
  |> P.add (P.Fan.center_on_edge ~center:p l k |> P.Fan.add o)
     (* cube eaimbfjn *)
  |> P.add (P.Fan.center_on_edge ~center:e a i |> P.Fan.add m)
  |> P.add (P.Fan.center_on_edge ~center:e f n |> P.Fan.add m)
  |> P.add (P.Fan.center_on_edge ~center:j b f |> P.Fan.add n)
  |> P.add (P.Fan.center_on_edge ~center:j b a |> P.Fan.add i)
     (* cube gckohdpl *)
  |> P.add (P.Fan.center_on_edge ~center:g c k |> P.Fan.add o)
  |> P.add (P.Fan.center_on_edge ~center:g h p |> P.Fan.add o)
  |> P.add (P.Fan.center_on_edge ~center:l d c |> P.Fan.add k)
  |> P.add (P.Fan.center_on_edge ~center:l d h |> P.Fan.add p)
     (* connect the last cubes *)
  |> P.add (P.Fan.center_on_edge ~center:a c k |> P.Fan.add i)
  |> P.add (P.Fan.center_on_edge ~center:b d l |> P.Fan.add j)
  |> P.add (P.Fan.center_on_edge ~center:e g o |> P.Fan.add m)
  |> P.add (P.Fan.center_on_edge ~center:f h p |> P.Fan.add n)

let cube = box 1.0 1.0 1.0 1.0

let polycube =
  let c = box 0.5 0.5 0.5 0.5 in
  P.move (V4.v 0.0 0.5 (-0.5) 0.0) c
  |> P.append (P.move (V4.v (-1.0) 0.5 (-0.5) 0.0) c)
  |> P.append (P.move (V4.v 0.0 (-0.5) (-0.5) 0.0) c)
  |> P.append (P.move (V4.v 1.0 (-0.5) (-0.5) 0.0) c)
  |> P.append (P.move (V4.v 0.0 (-0.5) 0.5 0.0) c)

let cross_latin =
  (* The center of the "chest" is 0, 0, 0, 0.
                a1 b1
             c1 d1 e1 f1
             g1 h1 i1 j1
                k1 l1
       m1 n1            a2 b2
    o1 p1 q1 r1      c2 d2 e2 f2
    s1 t1 u1 v1      g2 h2 i2 j2
       w1 x1            k2 l2
               m2 n2
            o2 p2 q2 r2
            s2 t2 u2 v2
               w2 x2             *)
  let z = 0.125 in
  let w = 0.125 in
  let centera = V4.v 0.0 0.0 z w in
  let a1 = V4.v (-0.125) 0.625 z w in
  let b1 = V4.ltr mirror_x a1 in
  let c1 = V4.v (-0.875) 0.125 z w in
  let d1 = V4.v (-0.125) 0.125 z w in
  let e1 = V4.ltr mirror_x d1 in
  let f1 = V4.ltr mirror_x c1 in
  let g1 = V4.ltr mirror_y c1 in
  let h1 = V4.ltr mirror_y d1 in
  let i1 = V4.ltr mirror_x h1 in
  let j1 = V4.ltr mirror_x g1 in
  let k1 = V4.v (-0.125) (-1.375) z w in
  let l1 = V4.ltr mirror_x k1 in
  let m1 = V4.ltr mirror_z a1 in
  let n1 = V4.ltr mirror_z b1 in
  let o1 = V4.ltr mirror_z c1 in
  let p1 = V4.ltr mirror_z d1 in
  let q1 = V4.ltr mirror_z e1 in
  let r1 = V4.ltr mirror_z f1 in
  let s1 = V4.ltr mirror_z g1 in
  let t1 = V4.ltr mirror_z h1 in
  let u1 = V4.ltr mirror_z i1 in
  let v1 = V4.ltr mirror_z j1 in
  let w1 = V4.ltr mirror_z k1 in
  let x1 = V4.ltr mirror_z l1 in
  let a2 = V4.ltr mirror_w a1 in
  let b2 = V4.ltr mirror_w b1 in
  let c2 = V4.ltr mirror_w c1 in
  let d2 = V4.ltr mirror_w d1 in
  let e2 = V4.ltr mirror_w e1 in
  let f2 = V4.ltr mirror_w f1 in
  let g2 = V4.ltr mirror_w g1 in
  let h2 = V4.ltr mirror_w h1 in
  let i2 = V4.ltr mirror_w i1 in
  let j2 = V4.ltr mirror_w j1 in
  let k2 = V4.ltr mirror_w k1 in
  let l2 = V4.ltr mirror_w l1 in
  let m2 = V4.ltr mirror_w m1 in
  let n2 = V4.ltr mirror_w n1 in
  let o2 = V4.ltr mirror_w o1 in
  let p2 = V4.ltr mirror_w p1 in
  let q2 = V4.ltr mirror_w q1 in
  let r2 = V4.ltr mirror_w r1 in
  let s2 = V4.ltr mirror_w s1 in
  let t2 = V4.ltr mirror_w t1 in
  let u2 = V4.ltr mirror_w u1 in
  let v2 = V4.ltr mirror_w v1 in
  let w2 = V4.ltr mirror_w w1 in
  let x2 = V4.ltr mirror_w x1 in
  P.empty
  (* face a1-l1 *)
  |> P.add
       (P.Fan.center_inside ~center:centera a1 b1
       |> P.Fan.add e1 |> P.Fan.add f1 |> P.Fan.add j1 |> P.Fan.add i1
       |> P.Fan.add l1 |> P.Fan.add k1 |> P.Fan.add h1 |> P.Fan.add g1
       |> P.Fan.add c1 |> P.Fan.add d1)
  (* face m1-x1 *)
  |> P.add
       (P.Fan.center_inside ~center:(V4.ltr mirror_z centera) m1 n1
       |> P.Fan.add q1 |> P.Fan.add r1 |> P.Fan.add v1 |> P.Fan.add u1
       |> P.Fan.add x1 |> P.Fan.add w1 |> P.Fan.add t1 |> P.Fan.add s1
       |> P.Fan.add o1 |> P.Fan.add p1)
  (* face a2-l2 *)
  |> P.add
       (P.Fan.center_inside ~center:(V4.ltr mirror_w centera) a2 b2
       |> P.Fan.add e2 |> P.Fan.add f2 |> P.Fan.add j2 |> P.Fan.add i2
       |> P.Fan.add l2 |> P.Fan.add k2 |> P.Fan.add h2 |> P.Fan.add g2
       |> P.Fan.add c2 |> P.Fan.add d2)
  (* face m2-x2 *)
  |> P.add
       (P.Fan.center_inside
          ~center:(V4.ltr mirror_w (V4.ltr mirror_z centera))
          m2 n2
       |> P.Fan.add q2 |> P.Fan.add r2 |> P.Fan.add v2 |> P.Fan.add u2
       |> P.Fan.add x2 |> P.Fan.add w2 |> P.Fan.add t2 |> P.Fan.add s2
       |> P.Fan.add o2 |> P.Fan.add p2)
  (* Connect connect a1-l1 to m1-x1 *)
  |> P.add (P.Fan.center_on_edge ~center:a1 d1 p1 |> P.Fan.add m1)
  |> P.add (P.Fan.center_on_edge ~center:d1 c1 o1 |> P.Fan.add p1)
  |> P.add (P.Fan.center_on_edge ~center:c1 g1 s1 |> P.Fan.add o1)
  |> P.add (P.Fan.center_on_edge ~center:g1 h1 t1 |> P.Fan.add s1)
  |> P.add (P.Fan.center_on_edge ~center:h1 k1 w1 |> P.Fan.add t1)
  |> P.add (P.Fan.center_on_edge ~center:k1 l1 x1 |> P.Fan.add w1)
  |> P.add (P.Fan.center_on_edge ~center:l1 i1 u1 |> P.Fan.add x1)
  |> P.add (P.Fan.center_on_edge ~center:i1 j1 v1 |> P.Fan.add u1)
  |> P.add (P.Fan.center_on_edge ~center:j1 f1 r1 |> P.Fan.add v1)
  |> P.add (P.Fan.center_on_edge ~center:f1 e1 q1 |> P.Fan.add r1)
  |> P.add (P.Fan.center_on_edge ~center:e1 b1 n1 |> P.Fan.add q1)
  |> P.add (P.Fan.center_on_edge ~center:b1 a1 m1 |> P.Fan.add n1)
  (* Connect connect a2-l2 to m2-x2 *)
  |> P.add (P.Fan.center_on_edge ~center:a2 d2 p2 |> P.Fan.add m2)
  |> P.add (P.Fan.center_on_edge ~center:d2 c2 o2 |> P.Fan.add p2)
  |> P.add (P.Fan.center_on_edge ~center:c2 g2 s2 |> P.Fan.add o2)
  |> P.add (P.Fan.center_on_edge ~center:g2 h2 t2 |> P.Fan.add s2)
  |> P.add (P.Fan.center_on_edge ~center:h2 k2 w2 |> P.Fan.add t2)
  |> P.add (P.Fan.center_on_edge ~center:k2 l2 x2 |> P.Fan.add w2)
  |> P.add (P.Fan.center_on_edge ~center:l2 i2 u2 |> P.Fan.add x2)
  |> P.add (P.Fan.center_on_edge ~center:i2 j2 v2 |> P.Fan.add u2)
  |> P.add (P.Fan.center_on_edge ~center:j2 f2 r2 |> P.Fan.add v2)
  |> P.add (P.Fan.center_on_edge ~center:f2 e2 q2 |> P.Fan.add r2)
  |> P.add (P.Fan.center_on_edge ~center:e2 b2 n2 |> P.Fan.add q2)
  |> P.add (P.Fan.center_on_edge ~center:b2 a2 m2 |> P.Fan.add n2)
  (* Connect a1-l1 to a2-l2 *)
  |> P.add (P.Fan.center_on_edge ~center:a1 b1 b2 |> P.Fan.add a2)
  |> P.add (P.Fan.center_on_edge ~center:b1 e1 e2 |> P.Fan.add b2)
  |> P.add (P.Fan.center_on_edge ~center:e1 f1 f2 |> P.Fan.add e2)
  |> P.add (P.Fan.center_on_edge ~center:f1 j1 j2 |> P.Fan.add f2)
  |> P.add (P.Fan.center_on_edge ~center:j1 i1 i2 |> P.Fan.add j2)
  |> P.add (P.Fan.center_on_edge ~center:i1 l1 l2 |> P.Fan.add i2)
  |> P.add (P.Fan.center_on_edge ~center:l1 k1 k2 |> P.Fan.add l2)
  |> P.add (P.Fan.center_on_edge ~center:k1 h1 h2 |> P.Fan.add k2)
  |> P.add (P.Fan.center_on_edge ~center:h1 g1 g2 |> P.Fan.add h2)
  |> P.add (P.Fan.center_on_edge ~center:g1 c1 c2 |> P.Fan.add g2)
  |> P.add (P.Fan.center_on_edge ~center:c1 d1 d2 |> P.Fan.add c2)
  |> P.add (P.Fan.center_on_edge ~center:d1 a1 a2 |> P.Fan.add d2)
  (* Connect m1-x1 to m2-x2 *)
  |> P.add (P.Fan.center_on_edge ~center:m1 n1 n2 |> P.Fan.add m2)
  |> P.add (P.Fan.center_on_edge ~center:n1 q1 q2 |> P.Fan.add n2)
  |> P.add (P.Fan.center_on_edge ~center:q1 r1 r2 |> P.Fan.add q2)
  |> P.add (P.Fan.center_on_edge ~center:r1 v1 v2 |> P.Fan.add r2)
  |> P.add (P.Fan.center_on_edge ~center:v1 u1 u2 |> P.Fan.add v2)
  |> P.add (P.Fan.center_on_edge ~center:u1 x1 x2 |> P.Fan.add u2)
  |> P.add (P.Fan.center_on_edge ~center:x1 w1 w2 |> P.Fan.add x2)
  |> P.add (P.Fan.center_on_edge ~center:w1 t1 t2 |> P.Fan.add w2)
  |> P.add (P.Fan.center_on_edge ~center:t1 s1 s2 |> P.Fan.add t2)
  |> P.add (P.Fan.center_on_edge ~center:s1 o1 o2 |> P.Fan.add s2)
  |> P.add (P.Fan.center_on_edge ~center:o1 p1 p2 |> P.Fan.add o2)
  |> P.add (P.Fan.center_on_edge ~center:p1 m1 m2 |> P.Fan.add p2)
  (* The rest of the connections *)
  |> P.add (P.Fan.center_on_edge ~center:b1 b2 n2 |> P.Fan.add n1)
  |> P.add (P.Fan.center_on_edge ~center:e1 e2 q2 |> P.Fan.add q1)
  |> P.add (P.Fan.center_on_edge ~center:f1 f2 r2 |> P.Fan.add r1)
  |> P.add (P.Fan.center_on_edge ~center:j1 j2 v2 |> P.Fan.add v1)
  |> P.add (P.Fan.center_on_edge ~center:i1 i2 u2 |> P.Fan.add u1)
  |> P.add (P.Fan.center_on_edge ~center:l1 l2 x2 |> P.Fan.add x1)
  |> P.add (P.Fan.center_on_edge ~center:k1 k2 w2 |> P.Fan.add w1)
  |> P.add (P.Fan.center_on_edge ~center:h1 h2 t2 |> P.Fan.add t1)
  |> P.add (P.Fan.center_on_edge ~center:g1 g2 s2 |> P.Fan.add s1)
  |> P.add (P.Fan.center_on_edge ~center:c1 c2 o2 |> P.Fan.add o1)
  |> P.add (P.Fan.center_on_edge ~center:d1 d2 p2 |> P.Fan.add p1)
  |> P.add (P.Fan.center_on_edge ~center:a1 a2 m2 |> P.Fan.add m1)

let cross_double =
  let len = 1.0 in
  let depth = 0.2 in
  box len depth depth depth
  |> P.append (box depth len depth depth)
  |> P.append (box depth depth len depth)
  |> P.append (box depth depth depth len)

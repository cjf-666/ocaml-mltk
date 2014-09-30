(* module for representing vector *)

open Core.Std

type vec_t = Int_vec of Int.t Array.t | Float_vec of Float.t Array.t

type t = {dim : Int.t; vec : vec_t}

exception Distinct_dim of Int.t * Int.t
exception Wrong_type_vec

let vec_int_to_float {dim = d; vec = iv} =
  let fv = Array.create d 0.0 in
  match iv with
  | Int_vec iv ->
     for i = 0 to d - 1 do
       fv.(i) <- Float.of_int iv.(i)
     done;
     {dim = d; vec = Float_vec fv}
  | Float_vec iv ->
     for i = 0 to d - 1 do
       fv.(i) <- Float.of_float iv.(i)
     done;
     {dim = d; vec = Float_vec fv}

let inner_product x y =
  let inp x_vec y_vec dim =
    match x_vec, y_vec with
    | Float_vec x_vec, Float_vec y_vec ->
       let sum = ref 0.0 in
       for i = 0 to dim - 1 do
	 sum := !sum +. x_vec.(i) *. y_vec.(i)
       done;
       !sum
    | Int_vec x_vec, Int_vec y_vec ->
       let sum = ref 0 in
       for i = 0 to dim - 1 do
	 sum := !sum + x_vec.(i) * y_vec.(i)
       done;
       Float.of_int !sum
    | _ -> raise Wrong_type_vec
  in
  let type_dispatch x y =
    match x, y with
    | {dim = d; vec = Int_vec x_vec}, {dim = _; vec = Int_vec y_vec} ->
       inp x.vec y.vec x.dim

    | {dim = _; vec = Int_vec _}, {dim = _; vec = Float_vec _} ->
       let x = vec_int_to_float x in
       inp x.vec y.vec x.dim

    | {dim = _; vec = Float_vec _}, {dim = _; vec = Int_vec _} ->
       let y = vec_int_to_float y in
       inp x.vec y.vec y.dim

    | {dim = _; vec = Float_vec _}, {dim = _; vec = Float_vec _} ->
       inp x.vec y.vec x.dim
  in
  match (x.dim = y.dim) with
  | false -> raise (Distinct_dim (x.dim, y.dim))
  | true -> type_dispatch x y


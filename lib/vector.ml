(* module for representing vector *)

open Core.Std

type vec_t = Int_vec of Int.t Array.t | Float_vec of Float.t Array.t

type t = {dim : Int.t; vec : vec_t}

exception Distinct_dim of Int.t * Int.t
exception Distinct_type_vec

let vec_int_to_float {dim = d; vec = iv} =
  let fv = Array.create ~len:d 0.0 in
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

let type_align x y =
  match x , y with
  | {dim = _; vec = Int_vec _}, {dim = _; vec = Int_vec _}
  | {dim = _; vec = Float_vec _}, {dim = _; vec = Float_vec _} ->
    x , y

  | {dim = _; vec = Int_vec _}, {dim = _; vec = Float_vec _} ->
    let x = vec_int_to_float x in
    x , y

  | {dim = _; vec = Float_vec _}, {dim = _; vec = Int_vec _} ->
    let y = vec_int_to_float y in
    x , y

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
    | _ -> raise Distinct_type_vec
  in
  match (x.dim = y.dim) with
  | false -> raise (Distinct_dim (x.dim, y.dim))
  | true -> let x,y = type_align x y in
    inp x.vec y.vec x.dim

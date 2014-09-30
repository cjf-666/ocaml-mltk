(** use this type to determine the parameters *)
module type KMeans_param = sig val k : int end

(** use this type to provide interface accessing all the data*)
module type KMeans_adapter =
sig
  type t
  val entity_n : int

  (** get the i-th entity, within [0..entity_n) *)
  val get_i : int -> t

  (** iterate all the entities, return a accumulative value*)
  val fold_all : init:'accum -> f:('accum -> t -> 'accum) -> 'accum

  (** return the new mean of (n+1) entities given the origin
      mean of n entities and a new entity *)
  val update_mean : int * t -> t -> int * t
  val nearly_same : t -> t -> bool
  val distance : t -> t -> float
end

module KMeans :
  functor (Kp : KMeans_param) ->
  functor (Ka : KMeans_adapter) ->
  sig
    val k : int
    val get_i : int -> Ka.t
    val fold_all : init:'a -> f:('a -> Ka.t -> 'a) -> 'a
    val dis : Ka.t -> Ka.t -> float
    val update_mean : int * Ka.t -> Ka.t -> int * Ka.t

    (** return the initialized k centroids *)
    val init : unit -> Ka.t list

    (** the E-M procedure of KMeans,
        returns k centroids of k clusters *)
    val e_m : unit -> Ka.t list
  end

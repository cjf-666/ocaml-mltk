open Core.Std

(** use this type to determine the parameters *)
module type KMeans_param = sig
  val k : int
end

(** use this type to provide interface accessing all the data*)
module type KMeans_adapter = sig
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

  val distance : (t -> t -> Float.t)
end

module KMeans (Kp : KMeans_param) (Ka : KMeans_adapter) = struct
  let k = Kp.k
  let get_i = Ka.get_i
  let fold_all = Ka.fold_all
  let dis = Ka.distance
  let update_mean = Ka.update_mean

  (** return the initialized k centroids *)
  let init () =
    let group = Ka.entity_n / k in
    let rec pick n centers =
      if n = k then
        centers
      else
        pick (n+1) ((get_i (n*group))::centers)
    in
    pick 0 []

  (** the E-M procedure of KMeans,
      returns k centroids of k clusters *)
  let e_m () =
    let centroids = init () in
      
    let rec iter centroids =
      let n_m = uw (List.zip (List.init k ~f:(fun x -> 0)) centroids) in
      (** clusters is a list with items with form (old_centroid, (n, mean)) where
          n is the number of entities assigned to the old_centroid so far and
          mean is the mean of these n entities, it is then convenient to
          compute the new mean once a new entity is assigned to this cluster*)
      let clusters = uw (List.zip centroids n_m) in
      let new_clusters = fold_all ~init:clusters
                                  ~f:(fun a e ->
                                    let e_c,_ = List.fold centroids
                                                  ~init:(uw (List.hd centroids), (dis (uw (List.hd centroids)) e))
                                                  ~f:(fun a c ->
                                                      let _, d = a in
                                                      if dis c e < d then
                                                        c, (dis c e)
                                                      else
                                                        a) in
                                    List.map a ~f:(fun c ->
                                                     let ct, m = c in
                                                     if ct = e_c then
                                                       ct,(update_mean m e)
                                                     else
                                                       c)) in

      let old, ne = List.unzip new_clusters in
      let _, ne = List.unzip ne in
      let o_n = uw (List.zip old ne) in
      if (List.fold o_n ~init:true ~f:(fun a x ->
                                         let o,n = x in
                                         if not a then false
                                         else if Ka.nearly_same o n then
                                           true
                                         else
                                           false)) then
        ne
      else
        iter ne
    in
    iter centroids

end

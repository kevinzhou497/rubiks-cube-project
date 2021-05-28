(* A* with some very minor modications taken from
   https://gist.github.com/akabe/ad9a6deaaa639124944f *)
module Astar : sig
  type 'a t = {
    (* [cost state goal] determines the cost from [state] to [goal] *)
    cost : 'a -> 'a -> float;
    goal : 'a;
    (* [get_next_states state prev_states] gets the available states
       from [state] that aren't in the list [prev_states] *)
    get_next_states : 'a -> 'a list -> 'a list;
    (* [eq st1 st2] determines if the states [st1] and [st2] are equal *)
    eq : 'a -> 'a -> bool;
  }

  (** [search problem start timeout] returns a path (a list of states)
      from [start] to [problem.goal]. The path minimizes [problem.cost].
      Raises [Not_found] if it cannot find a solution or if it takes
      longer than [timeout] to do so *)
  val search : 'a t -> 'a -> float option -> 'a list
end = struct
  type 'a t = {
    cost : 'a -> 'a -> float;
    goal : 'a;
    get_next_states : 'a -> 'a list -> 'a list;
    eq : 'a -> 'a -> bool;
  }

  type 'a path = {
    cost_from_start : float;  (** the cost from the start to [head]. *)
    total_cost : float;
        (** the total cost from the start to the goal. *)
    head : 'a;
    tail : 'a list;
    eq : 'a -> 'a -> bool;
  }

  let create_path ?from problem state =
    let cost_from_start, tail =
      match from with
      | None -> (0., [])
      | Some p ->
          ( float_of_int (List.length p.tail) +. 1.
            (* +. problem.cost p.head state *),
            p.head :: p.tail )
    in
    let total_cost =
      cost_from_start +. problem.cost state problem.goal
    in
    { cost_from_start; total_cost; tail; head = state; eq = problem.eq }

  (** [better p q] returns [true] if path [p] is better than path [q]. *)
  let better p q = p.total_cost < q.total_cost

  (** [pickup_eq_path p l] returns [Some (q, l')] where [q] is the path
      that indicates the same position as [p] and [l] is a list
      excluding [q]. *)
  let pickup_eq_path p l =
    match List.partition (fun q -> p.eq p.head q.head) l with
    | [], _ -> None
    | [ q ], l' -> Some (q, l')
    | _ -> failwith "duplicated paths in open/close list"

  (** [trace_next_states problem open_list close_list path] traces the
      next states of [path.head].

      @return [(open_list', close_list')] where [open_list'] and
      [close_list'] are respectively an open list and a close list after
      all of the next states are traced. *)
  let trace_next_states problem ol0 cl0 path0 =
    let trace_state (ol, cl) state =
      let path = create_path ~from:path0 problem state in
      match pickup_eq_path path ol with
      | Some (q, ol') ->
          if better path q then (path :: ol', cl) else (ol, cl)
      | None -> (
          match pickup_eq_path path cl with
          | Some (q, cl') ->
              if better path q then (path :: ol, cl') else (ol, cl)
          | None -> (path :: ol, cl))
    in
    List.fold_left trace_state (ol0, cl0)
      (problem.get_next_states path0.head path0.tail)

  (** [pickup_best_path l] returns [Some (p, l')] where [p] is the path
      that has the least cost in [l] and [l'] is an open list without
      [p]. *)
  let pickup_best_path = function
    | [] -> None
    | h :: t ->
        let aux (y, l) x =
          if better y x then (y, x :: l) else (x, y :: l)
        in
        Some (List.fold_left aux (h, []) t)

  let search problem start time_limit =
    let time_start = Unix.time () in
    let rec aux (ol, cl) =
      match pickup_best_path ol with
      | None -> None (* No path reaches to [problem.goal] *)
      | Some (p, ol') -> (
          if p.eq p.head problem.goal then Some p
            (* reached to the goal *)
          else
            let f () =
              aux (trace_next_states problem ol' (p :: cl) p)
            in
            match time_limit with
            | Some x ->
                if Unix.time () -. time_start <= x then f () else None
            | None -> f ())
    in
    match aux ([ create_path problem start ], []) with
    | None -> raise Not_found
    | Some p -> p.head :: p.tail
end

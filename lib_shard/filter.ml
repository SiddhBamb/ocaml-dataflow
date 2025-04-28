open Shards

type 'a t =
  { input : 'a list
  ; pred : 'a -> bool
  }

let sequential n = filter n.pred (of_list ~n:1 n.input)
let parallel n_domains n = filter n.pred (of_list ~n:n_domains n.input)

let run ?(num_domains = 4) node =
  if num_domains = 0 then sequential node else parallel num_domains node
;;

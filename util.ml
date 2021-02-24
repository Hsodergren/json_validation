let rec unzip3 l =
  match l with
  | (x,y,z)::tl ->
    let xs,ys,zs = unzip3 tl in
    x::xs,y::ys,z::zs
  | [] -> [],[],[]

module ModuleList : sig
  type t [@@deriving yojson]

  val to_list: t -> string list
  val of_list: string list -> t
end = struct
  type t = string list [@@deriving yojson]

  let to_list t = t
  let of_list t = t
end

module Module = struct
  type t = {
    schema : Yojson.Safe.t;
    jsons : Yojson.Safe.t list
  } [@@deriving yojson]
end
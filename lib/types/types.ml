module Module = struct
  type t = {
    schema : Yojson.Safe.t;
    jsons : Yojson.Safe.t list
  } [@@deriving yojson]
end

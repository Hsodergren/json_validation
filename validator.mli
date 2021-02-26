open Jsch
open React

type t [@@deriving show]
type v = private
  | Empty
  | Simple of string
  | Array of v list
  | Object of (string * v) list [@@deriving show, eq]
type action = [`Update of Jsonpath.t * string | `AddField of Jsonpath.t]

module Validation : sig
  type t = [
    | `Valid
    | `Invalid of (Jsonpath.t * string) list
    | `Parsing
    | `Empty
  ] [@@deriving eq]

  val merge: t -> t -> t
end

val schema: t -> Schema.t
val v: t -> v

val equal: t -> t -> bool

val get: Jsonpath.t -> t -> t option
val get_arr: Jsonpath.t -> t -> v list
val get_obj: Jsonpath.t -> t -> (string * v) list
val to_yojson: t -> Yojson.Safe.t
val make: Schema.t -> Yojson.Safe.t -> t
val update: t -> Jsonpath.t -> string -> t
val add_field: Jsonpath.t -> t -> t

val view:
  ?disabled:bool ->
  ?handle_required:bool ->
  ?id:string ->
  t signal ->
  Validation.t signal * action event * Brr.El.t list

open Jsch
open React

type t [@@deriving show]
type v = private
  | Simple of string
  | Array of v list
  | Object of (string * v) list [@@deriving show, eq]
type action = [ `Update of Jsonpath.t * string
              | `AddItem of Jsonpath.t
              | `AddField of Jsonpath.t * string
              | `RemField of Jsonpath.t ]

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

val equal: t -> t -> bool

val to_yojson: t -> Yojson.Safe.t option
val make: Schema.t -> Yojson.Safe.t -> t
val update: t -> Jsonpath.t -> string -> t
val add_field: Jsonpath.t -> string -> t -> t
val add_item: Jsonpath.t -> t -> t
val rem_field: Jsonpath.t -> t -> t

val view:
  ?disabled:bool ->
  ?handle_required:bool ->
  ?id:string ->
  ?search:string signal ->
  t signal ->
  Validation.t signal * action event * Brr.El.t list

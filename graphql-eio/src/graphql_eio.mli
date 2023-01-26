(** GraphQL schema with Eio support *)
module Schema : sig
  include
    Graphql_intf.Schema
      with type 'a Io.t = 'a
       and type 'a Io.Stream.t = 'a List.t * (unit -> unit)
       and type field_error = string
end

module Schema =
  Graphql_schema.Make
    (struct
      type 'a t = 'a

      let return = Fun.id
      let bind v f = f v

      module Stream = struct
        type 'a t = 'a List.t * (unit -> unit)

        let map (t, close) f = (List.map f t, close)
        let iter (t, _close) f = List.iter f t
        let close (_, close) = close ()
      end
    end)
    (struct
      type t = string

      let message_of_field_error t = t
      let extensions_of_field_error _t = None
    end)

let stream_blocks ctxt =
  let process block =
    Format.printf "Printing block: %a@." Block_hash.pp block in
  Block_services.monitor ctxt >>=? fun (stream, stop) ->
  let exception WrapError of error list in
  let stream = Lwt_stream.map_list List.concat stream in
  Lwt.catch
    (fun () ->
       Lwt_stream.find_s
         (fun bi ->
            process bi.Block_services.hash ;
            Lwt.return false) stream)
    (function
      | WrapError _e -> Pervasives.failwith "error"
      | exn -> Lwt.fail exn) >>= fun _ ->
  return @@ stop ()

let () =
  let open Tezos_rpc in
  let open Tezos_client_base in
  let open Tezos_rpc_http in
  let rpc_config = RPC_client.{
      RPC_client.default_config with
      port = 18739 ;
    } in
  let rpc_ctxt = new RPC_client.http_ctxt rpc_config Media_type.all_media_types in
  match Lwt_main.run @@ stream_blocks rpc_ctxt with
  | Ok () -> ()
  | Error errors ->
    Format.pp_print_list
      ~pp_sep:Format.pp_print_newline
      Error_monad.pp Format.err_formatter
      errors

open Types
open Aws
type input = DescribePrefixListsRequest.t
type output = DescribePrefixListsResult.t
type error = Errors_internal.t
let service = "ec2"
let to_http req =
  let uri =
    Uri.add_query_params (Uri.of_string "https://ec2.amazonaws.com")
      (List.append
         [("Version", ["2015-04-15"]); ("Action", ["DescribePrefixLists"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DescribePrefixListsRequest.to_query req))))) in
  (`POST, uri, [])
let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp = Xml.member "DescribePrefixListsResponse" (snd xml) in
    try
      Util.or_error (Util.option_bind resp DescribePrefixListsResult.parse)
        (let open Error in
           BadResponse
             {
               body;
               message =
                 "Could not find well formed DescribePrefixListsResult."
             })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing DescribePrefixListsResult - missing field in body or children: "
                      ^ msg)
               })
  with
  | Failure msg ->
      `Error
        (let open Error in
           BadResponse { body; message = ("Error parsing xml: " ^ msg) })
let parse_error code err =
  let errors = [] @ Errors_internal.common in
  match Errors_internal.of_string err with
  | Some var ->
      if
        (List.mem var errors) &&
          ((match Errors_internal.to_http_code var with
            | Some var -> var = code
            | None -> true))
      then Some var
      else None
  | None -> None
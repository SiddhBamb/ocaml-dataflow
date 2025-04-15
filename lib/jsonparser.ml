(* parse json file into a Yojson.Basic.t *)
let parse_json_file (filename: string) : Yojson.Basic.t =
    let ch = open_in filename in
    let json_str = really_input_string ch (in_channel_length ch) in
    close_in ch;
    Yojson.Basic.from_string json_str

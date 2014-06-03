(*	Copyright 2014 Eireann Leverett 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.*)

(*ocamlc -g str.cma Classify6.ml -o classify6*)

(*Load necessary modules*)

let expand l = match l with
(*Expand 8 is an empty string so let last case catch it*)
(*Expand 7 cases*)
	| [""; x] -> ["";"";"";"";"";"";"";x]
	| [x; ""] -> [x;"";"";"";"";"";"";""]
(*Expand 6 cases*)
	| [a;b;""] -> [a;b;"";"";"";"";"";""]
	| [h;"";t] -> [h;"";"";"";"";"";"";t]
	| ["";y;z] -> ["";"";"";"";"";"";y;z]
(*Expand 5 cases*)
	| [a;b;c;""] -> [a;b;c;"";"";"";"";""]
	| [a;b;"";z] -> [a;b;"";"";"";"";"";z]
	| [a;"";y;z] -> [a;"";"";"";"";"";y;z]
	| ["";x;y;z] -> ["";"";"";"";"";x;y;z]
(*Expand 4 cases*)
	| [a;b;c;d;""] -> [a;b;c;d;"";"";"";""]
	| [a;b;c;"";z] -> [a;b;c;"";"";"";"";z]
	| [a;b;"";y;z] -> [a;b;"";"";"";"";y;z]
	| [a;"";x;y;z] -> [a;"";"";"";"";x;y;z]
	| ["";w;x;y;z] -> ["";"";"";"";w;x;y;z]
(*expand 3 cases*)
	| [a;b;c;d;e;""] -> [a;b;c;d;e;"";"";""]
	| [a;b;c;d;"";z] -> [a;b;c;d;"";"";"";z]
	| [a;b;c;"";y;z] -> [a;b;c;"";"";"";y;z]
	| [a;b;"";x;y;z] -> [a;b;"";"";"";x;y;z]
	| [a;"";w;x;y;z] -> [a;"";"";"";w;x;y;z]
	| ["";v;w;x;y;z] -> ["";"";"";v;w;x;y;z]
(*expand 2 cases*)
	| [a;b;c;d;e;f;""] -> [a;b;c;d;e;f;"";""]
	| [a;b;c;d;e;"";z] -> [a;b;c;d;e;"";"";z]
	| [a;b;c;d;"";y;z] -> [a;b;c;d;"";"";y;z]
	| [a;b;c;"";x;y;z] -> [a;b;c;"";"";x;y;z]
	| [a;b;"";w;x;y;z] -> [a;b;"";"";w;x;y;z]
	| [a;"";v;w;x;y;z] -> [a;"";"";v;w;x;y;z]
	| ["";u;v;w;x;y;z] -> ["";"";u;v;w;x;y;z]
(*expand 1 cases*)
	| [a;b;c;d;e;f;g;""] -> [a;b;c;d;e;f;g;""]
	| [a;b;c;d;e;f;"";z] -> [a;b;c;d;e;f;"";z]
	| [a;b;c;d;e;"";y;z] -> [a;b;c;d;e;"";y;z]
	| [a;b;c;d;"";x;y;z] -> [a;b;c;d;"";x;y;z]
	| [a;b;c;"";w;x;y;z] -> [a;b;c;"";w;x;y;z]
	| [a;b;"";v;w;x;y;z] -> [a;b;"";v;w;x;y;z]
	| [a;"";u;v;w;x;y;z] -> [a;"";u;v;w;x;y;z]
	| ["";t;u;v;w;x;y;z] -> ["";t;u;v;w;x;y;z]
(*Already Expanded*)
	| [a;b;c;d;e;f;g;h] -> [a;b;c;d;e;f;g;h]
(*This will catch lists that are too long, or generally seem like garbage*)
	| _ -> ["****"];;

(*Reconstruct the string from the list and insert colons*)
let list_to_string l = String.concat ":" l;;

(*a function which pads a string with leading zeroes up to length 4*)
let rec pad_zero x = if (String.length x) >= 4 then x else pad_zero ("0" ^ x);;

(*Preprocess the address by capitolising strings, adding leading zeros if they're missing, expandin compressed addresses, etc*)
let expand s = 
	let uc = (String.uppercase s) in
	let l = Str.split (Str.regexp ":") uc in
	let res = expand l in 
	let last = List.map pad_zero res in
	list_to_string last;;

(*Return a binary string from an int (MSB)*)
let bin_of_int d =
  if d < 0 then invalid_arg "bin_of_int" else
  if d = 0 then "0" else
  let rec aux acc d =
    if d = 0 then acc else
    aux (string_of_int (d land 1) :: acc) (d lsr 1)
  in
  String.concat "" (aux [] d);;

(*This function bitflips a char, but outputs a string for ease of use in the next function*)
let bit_flip x = if x = '0' then "1" else "0";;

(*This function takes a binary string, and returns a copy with the correct bit flipped (Big Endian)*)
let flip_bit str = (String.sub str 0 1) ^ bit_flip str.[3] ^ (String.sub str 2 6);;

(*Function to make a binary string from a hex string*)
let bin_string x = bin_of_int (int_of_string("0x" ^ x))

(* Convert hex string to binary string via ints, then flip the bit and return the result as a hex string again*)
let flip_n_hex x = Printf.sprintf "%04X" (int_of_string ("0b" ^ (flip_bit (bin_string x))));;

(* a string of length 4 is split into two strings of length 2 and put into a list*)
let string_split str = String.sub str 0 2 ::  String.sub str 2 2 ::[];;

(*Split all the strings in the list and assemble them back into a new list*)
let rec divide_list lst = match lst with
| [] -> []
| h :: t -> (string_split h) @ divide_list t;;

(*turn into a list, and perform the magic extraction (requires some bitflipping) of the MAC on the first group*)
let list_and_bitflip x = let y = Str.split (Str.regexp ":") x in match y with
| h :: t -> (flip_n_hex h) :: t
| _ -> [];;

(*filter out the FE:FF part of the string from a list*)
let filter str = if str = "FE" || str = "FF" then false else true;;

(* Function to extract and print the MAC address from an EUI-64 address*)
let extract_mac eui_64 = if (Str.string_match (Str.regexp("[0-9A-F][0-9A-F][0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]FF:FE[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F][0-9A-F][0-9A-F]")) eui_64 20) 
then
let lst = list_and_bitflip (Str.matched_string eui_64) in
let new_lst = divide_list lst in
let ans = List.filter filter new_lst in
String.concat ":" ans
else
"not discernable from this address.\n";;

(*A 64-bit interface identifier is most commonly derived from its 48-bit MAC address. A MAC address 00:0C:29:0C:47:D5 is turned into a 64-bit EUI-64 by inserting FF:FE in the middle: 
	00:0C:29:FF:FE:0C:47:D5. When this EUI-64 is used to form an IPv6 address it is modified:[1] the meaning of the Universal/Local bit (the 7th most significant bit of the EUI-64, starting 
	from 1) is inverted, so that a 1 now means Universal. To create an IPv6 address with the network prefix 2001:db8:1:2::/64 it yields the address 2001:db8:1:2:020c:29ff:fe0c:47d5 (with the 
	underlined U/L bit inverted to a 1, because the MAC address is universally unique).*)

(* Turn a list of hex strings into their decimal equivalent*)
let rec dec_list lst = match lst with
| [] -> []
| h :: t -> (string_of_int (int_of_string ("0x" ^ h))) :: dec_list t;;

(*extract Server IPv4 Addresses from Teredo*)
let extract_server addy = ignore (Str.string_match (Str.regexp("[0-9A-F][0-9A-F][0-9A-F][0-9A-F]:[0-9A-F][0-9A-F][0-9A-F][0-9A-F]")) addy 10);
let lst = Str.split (Str.regexp ":") (Str.matched_string addy) in
let hex = divide_list lst in
let declst = dec_list hex in
String.concat "." declst;;

let pretty_print addy_str = (string_of_int (int_of_string ("0x" ^String.sub addy_str 0 2))) ^ "." ^ (string_of_int (int_of_string ("0x" ^String.sub addy_str 2 2))) ^ "." ^ (string_of_int (int_of_string ("0x" ^String.sub addy_str 4 2))) ^ "." ^ (string_of_int (int_of_string ("0x" ^String.sub addy_str 6 2)));;

(*Extract Client IPv4 Address*)
let extract_client addy = ignore (Str.string_match (Str.regexp "[0-9A-F][0-9A-F][0-9A-F][0-9A-F]:[0-9A-F][0-9A-F][0-9A-F][0-9A-F]") addy 30);
let lst = Str.split (Str.regexp ":") (Str.matched_string addy) in
let ob_client = String.concat "" lst in
let long = Printf.sprintf "%X" ((int_of_string "0xFFFFFFFF") lxor (int_of_string ("0x" ^ ob_client))) in
pretty_print long;;

(*Extract port from Teredo address*)
let extract_port addy = ignore (Str.string_match (Str.regexp "[0-9A-F][0-9A-F][0-9A-F][0-9A-F]") addy 25);
let ob_port = Str.matched_string addy in
Printf.sprintf "%n" ((int_of_string "0xFFFF") lxor (int_of_string ("0x" ^ ob_port)));;

(*Extract IPv4 host address from 6 to 4 addresses *)
let extract_host addy = ignore (Str.string_match (Str.regexp("[0-9A-F][0-9A-F][0-9A-F][0-9A-F]:[0-9A-F][0-9A-F][0-9A-F][0-9A-F]")) addy 5);
let lst = Str.split (Str.regexp ":") (Str.matched_string addy) in
let hex = divide_list lst in
let declst = dec_list hex in
String.concat "." declst;;

(*Classify the address*)
let classify s =
	let uc = (String.uppercase s) in
	let addy = expand uc in
	match addy with
	| "****" -> "This does not appear to be an IPv6 address."
	| "0000:0000:0000:0000:0000:0000:0000:0000" -> addy ^ " is the unspecified address, used for applications that do not yet know their host address."
	| "0000:0000:0000:0000:0000:0000:0000:0001" -> addy ^ " is the loopback address, used to route packets to the on the same host."
	| addy when Str.string_match (Str.regexp "^0000:0000:0000:0000:0000:0000:[0-9A-F][0-9A-F][0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is an IPv4 Mapped Address used for dual stack transition, you should see the IPv4 Address at the end. \n(RFC 4038)"
	| addy when Str.string_match (Str.regexp "^01[0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is from an IETF Reserved /8 discard only address block. \n(RFC 6666)"
	| addy when Str.string_match (Str.regexp "^0[2-3][0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is from an IETF Deprecated (2004) /7, previously it was OSI NSAP-mapped prefix (RFC 4548) \n(RFC 4048)"
	| addy when Str.string_match (Str.regexp "^0[4-7][0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is from an IETF Reserved /6. \n(RFC 4291)"
	| addy when Str.string_match (Str.regexp "^0[8-F][0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is from an IETF Reserved /5. \n(RFC 4291)"
	| addy when Str.string_match (Str.regexp "^1[0-9A-F][0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is from an IETF Reserved /4. \n(RFC 4291)"
	| addy when Str.string_match (Str.regexp "^2001:0000:") addy 0 -> addy ^ " is a Teredo address, used to map IPv4 Addresses to IPv6. \nThe server address is " ^ (extract_server addy) ^ ". The IPv4 client address is " ^ (extract_client addy) ^ " and the port is " ^ (extract_port addy) ^ "."   
	| addy when Str.string_match (Str.regexp "^2001:0002:") addy 0 -> addy ^ " is a benchmarking address. It should only be used in documentation and shouldn't be routable."
	| addy when Str.string_match (Str.regexp "^2001:001[0-9A-F]:") addy 0 -> addy ^ " is an ORCHID address. These addresses are used for a fixed-term experiment. \nThey should only be visible on an end-to-end basis and routers should not see packets using them as source or destination addresses."
	| addy when Str.string_match (Str.regexp "^2001:0DB8:") addy 0 ->  addy ^ " is from a /32 is used in documentation, and should not be seen on the internet."
	| addy when Str.string_match (Str.regexp "^2002:") addy 0 -> addy ^ " is a 6 to 4 address. \nThe associated IPv4 host address is " ^ (extract_host addy) ^ "."
	| addy when Str.string_match (Str.regexp "^[2-3][0-9A-F][0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is a global unicast address. You should be able to use whois for these. \n(RFC 3587)"
	| addy when Str.string_match (Str.regexp "^FE[8-9A-B][0-9A-F]:") addy 0 -> addy ^ " is a Link Local address, and should not be forwarded by routers. \nThe associated mac address is " ^ (extract_mac addy) ^ "."
	| addy when Str.string_match (Str.regexp "^FC[0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is an Unique local addresses, routable only in cooperating sites. \n(RFC 4193)"
	| addy when Str.string_match (Str.regexp "^FD[0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is a Probabilistically unique local addresses, routable only in cooperating sites. \n(RFC 4193 section 3.2)"
	| addy when Str.string_match (Str.regexp "^FF[0-9A-F][0-9A-F]:") addy 0 -> addy ^ " is a global multicast address."
	| _ -> "This address is not recognised " ^ addy ^ "\nPlease contact blackswanburst@github with these details so the code can be improved.";;

(*functions req'd for batch mode*)
let process_line line =
	print_endline (classify line);
	print_endline "--------------------";;

let process_lines lines =
	Stream.iter process_line lines;;

let line_stream_of_channel channel =
	Stream.from
		(fun _ ->
			try Some (input_line channel) with End_of_file -> None);;

let process_file in_file =
	let in_channel = open_in in_file in
		try
			process_lines (line_stream_of_channel in_channel);
			close_in in_channel
		with e ->
			close_in in_channel;
			raise e;;

let batch_mode = ref false
let in_file = ref "classify6-unit-test.txt"
let out_file = ref "classify6-results.txt"
let address = ref "foo"

let set_infile f = in_file := f
let set_outfile f = out_file := f
let set_address a = address := a

let main =
begin
let speclist = [
("-a", Arg.String (set_address), "The single IPv6 address to check");
("-b", Arg.Set batch_mode, "Sets batch mode, must use -f to provide input filename and -o to use output filename.");
("-f", Arg.String (set_infile), "Sets input file of IPv6 Addresses (one per line)");
("-o", Arg.String (set_outfile), "Sets output file of IPv6 Address classifications")
]
in let usage_msg = "classify6 is a command line tool to tell you more about an IPv6 address or addresses. Options available:"
in Arg.parse speclist print_endline usage_msg;
(*if !batch_mode then let filename = !in_file in
	let oc = (open_out !out_file) in
	Printf.fprintf oc "%s\n" (process_file filename);
	close_out oc;*)
if !batch_mode then let filename = !in_file in
	process_file filename;
else
	let addy = !address in
	print_endline (classify addy); 
end

let () = main

(*Load necessary modules*)
open Printf;;
open Sys;;
open String;;
open List;;

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
let stringify l = String.concat ":" l;;

(*a function which pads a string with leading zeroes up to length 4*)
let rec pad_zero x = if (String.length x) >= 4 then x else pad_zero ("0" ^ x);;

let expand s = 
	let uc = (String.uppercase s) in
	let l = Str.split (Str.regexp ":") uc in
	let res = expand l in 
	let last = List.map pad_zero res in
	stringify last;;

(*TODO: Write mac extraction function*)
(*let x = Str.regexp("[0-9A-E][0-9A-E][0-9A-E][0-9A-E]:[0-9A-E][0-9A-E]FF:FE[0-9A-E][0-9A-E]:[0-9A-E][0-9A-E][0-9A-E][0-9A-E]") in Str.string_match x "2001:0DB8:0001:0002:020C:29FF:FE0C:47D5" 20;;*)

(*A 64-bit interface identifier is most commonly derived from its 48-bit MAC address. A MAC address 00:0C:29:0C:47:D5 is turned into a 64-bit EUI-64 by inserting FF:FE in the middle: 
	00:0C:29:FF:FE:0C:47:D5. When this EUI-64 is used to form an IPv6 address it is modified:[1] the meaning of the Universal/Local bit (the 7th most significant bit of the EUI-64, starting 
	from 1) is inverted, so that a 1 now means Universal. To create an IPv6 address with the network prefix 2001:db8:1:2::/64 it yields the address 2001:db8:1:2:020c:29ff:fe0c:47d5 (with the 
	underlined U/L bit inverted to a 1, because the MAC address is universally unique).*)

(*Classify the address*)
let classify s =
	let uc = (String.uppercase s) in
	let addy = expand uc in
	match addy with
	| "*" -> Printf.printf "This does not appear to be an IPv6 address. \n"; exit 0
	| "0000:0000:0000:0000:0000:0000:0000:0000" -> Printf.printf "This is the unspecified address, used for applications that do not yet know their host address. \n"
	| "0000:0000:0000:0000:0000:0000:0000:0001" -> Printf.printf "This is the loopback address, used to route packets to the on the same host. \n"
	| addy when Str.string_match (Str.regexp "^0000:0000:0000:0000:0000:0000:[0-9A-F][0-9A-F][0-9A-F][0-9A-F]:") addy 0 -> Printf.printf "This is a IPv4 Mapped Address used for dual stack transition, you should see the IPv4 Address at the end. \n(RFC 4038) \n"
	| addy when Str.string_match (Str.regexp "^2001:0000:") addy 0 -> Printf.printf "This is a Teredo address, used to map IPv4 Addresses to IPv6.\nAdd further functions to extract IPv4 address. \n"
	| addy when Str.string_match (Str.regexp "^2001:0002:") addy 0 -> Printf.printf "This is a benchmarking address. It should only be used in documentation and shouldn't be routable. \n"
	| addy when Str.string_match (Str.regexp "^2001:001[0-9A-F]:") addy 0 -> Printf.printf "This is an ORCHID address. These addresses are used for a fixed-term experiment. \nThey should only be visible on an end-to-end basis and routers should not see packets using them as source or destination addresses. \n"
	| addy when Str.string_match (Str.regexp "^2001:0DB8:") addy 0 ->  Printf.printf "This /32 is used in documentation, and should not be seen on the internet. \n"
	| addy when Str.string_match (Str.regexp "^2002:") addy 0 -> Printf.printf "This is a 6 to 4 address. \n Write another function to extract IPv4. \n"
	| addy when Str.string_match (Str.regexp "^[2-3][0-9A-F][0-9A-F][0-9A-F]:") addy 0 -> Printf.printf "This is a global unicast address. You should be able to use whois for these. \n(RFC 3587) \n"
	| addy when Str.string_match (Str.regexp "^FE[8-9A-B][0-9A-F]:") addy 0 -> Printf.printf "Link Local addresses, should not be forwarded by routers. \nFinish Writing MAC extraction function. \n"
	| addy when Str.string_match (Str.regexp "^FC[0-9A-F][0-9A-F]:") addy 0 -> Printf.printf "Unique local addresses, routable only in cooperating sites. \n(RFC 4193) \n"
	| addy when Str.string_match (Str.regexp "^FD[0-9A-F][0-9A-F]:") addy 0 -> Printf.printf "Probabilistically unique local addresses, routable only in cooperating sites. \n(RFC 4193 section 3.2) \n"
	| addy when Str.string_match (Str.regexp "^FF[0-9A-F][0-9A-F]:") addy 0 -> Printf.printf "This is a global multicast address. \n"
	| _ -> Printf.printf "This address is not recognised %s \n please contact the eireann.leverett@cantab.net with these details so he can improve the code. \n" addy;;
print_string "Enter an IPV6 address: ";
let addy = read_line () in
    classify addy;;
    

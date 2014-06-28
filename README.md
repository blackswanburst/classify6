classify6

An OCAML command line tool to classify IPv6 addresses, and provide a bit of further information.

Thanks to Yvan Jannsens for exploring easy deployment options for testers:

apt-get install ocaml git-core											
git clone https://github.com/blackswanburst/classify6.git				
cd classify6															
ocamlopt str.cmxa Classify6.ml -o classify6								

To compile for bytecode for usage with ocamlrun:
ocamlc -g str.cma Classify6.ml -o classify6								
or to compile natively:
ocamlopt str.cmxa Classify6.ml -o classify6								

TODO: Add multicasts of different scopes and specificity.

Build environment to make the ocaml easily portable.

How could you help? 

Review the IPv6 assisgments with your expertise, and test the code.
Report bugs, find better modes of deployment, propose features, and give feedback.


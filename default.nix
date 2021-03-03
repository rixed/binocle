{ stdenv, fetchFromGitHub, ocaml, findlib, batteries, pretty-printers-parsers,
  stdint }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-binocle";
  version = "0.13.0";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "binocle";
    rev = "v${version}";
    sha256 = "1pjckmzskwg8r79k1xvbi9nqcly2nxicpxxpsaa8dnjyav0hwf6z";
  };

  buildInputs = [ ocaml findlib batteries pretty-printers-parsers stdint ];

  createFindlibDestdir = true;

  meta = with stdenv.lib; {
    homepage = https://github.com/rixed/binocle;
    description = "Instrumentation for OCaml programs";
    platforms = ocaml.meta.platforms or [];
    maintainers = [ maintainers.rixed ];
  };
}

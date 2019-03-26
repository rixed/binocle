{ stdenv, fetchFromGitHub, ocaml, findlib, batteries, pretty-printers-parsers,
  stdint }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-binocle";
  version = "0.9.0";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "binocle";
    rev = "v${version}";
    sha256 = "1qfdw2z48f3xy8gd5pn29dliai1k3ac821q05l83cglydhrfhfsl";
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

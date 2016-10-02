#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"netml"
  [ oasis_lib "netml"
  ; file "META" ~section:"lib"
  ]

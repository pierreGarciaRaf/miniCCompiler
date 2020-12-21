#use "Topfind";;
#require "Spelll";;
open Spelll
let dfa = Spelll.of_string ~limit:3 "hello";;
Printf.printf "%b\n" (Spelll.match_with dfa "llo")
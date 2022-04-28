import std/[sequtils]
import vverilog/[parser, lexer]


# for t in extractVTokens readFile "./samples/top.v":
#   echo t


let nodes = parseVerilog readFile "./samples/ex1.v"
# echo nodes
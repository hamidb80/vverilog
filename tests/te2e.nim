import std/[os]
import vverilog/[parser, lexer]

let nodes = parseVerilog readFile "./tests/play.v"
for n in nodes:
  echo "= = = = = = = = = = = = = = = = "
  echo n
  echo "# # # # # # # # # # # # # # # # "
  echo treeRepr n
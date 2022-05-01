import os
import vverilog/[parser]

# for (_, path) in walkDir "./samples/":
#   echo "\n", path, "\n"
#   let nodes = parseVerilog readFile path
#   for n in nodes:
#     echo n


let nodes = parseVerilog readFile "./tests/play.v"
# let nodes = parseVerilog readFile "./tests/eg.v"
for n in nodes:
  echo "= = = = = = = = = = = = = = = = "
  echo n
  echo "# # # # # # # # # # # # # # # # "
  echo treeRepr n
import vverilog/[parser]

let nodes = parseVerilog readFile "./eg.v"
for n in nodes:
  echo "= = = = = = = = = = = = = = = = "
  echo n
  echo "# # # # # # # # # # # # # # # # "
  echo treeRepr n
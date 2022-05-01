import vverilog/[parser]

# for t in extractVTokens readFile "./samples/top.v":
#   echo t


let nodes = parseVerilog readFile "./samples/memory.v"
for n in nodes:
  echo "= = = = = = = = = = = = = = = = "
  echo n
  echo "# # # # # # # # # # # # # # # # "
  echo treeRepr n
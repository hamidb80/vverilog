import vverilog/lexer


for t in extractVTokens readFile "./samples/top.v":
  echo t
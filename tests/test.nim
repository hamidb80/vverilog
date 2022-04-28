import vverilog/lexer


for t in extractVTokens readFile "./samples/ex1.v":
  echo t
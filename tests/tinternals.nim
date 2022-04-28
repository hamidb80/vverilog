import vverilog/parser {.all.}
import vverilog/lexer

let tks = @[
 VToken(kind: vtkScope, scope: '('),
 VToken(kind: vtkKeyword, keyword: "arg1"),
 VToken(kind: vtkSeparator, sign: ','),
 VToken(kind: vtkNumber, digits: "3'b010"),
 VToken(kind: vtkSeparator, sign: ','),
 VToken(kind: vtkKeyword, keyword: "Hello"),
 VToken(kind: vtkSeparator, sign: ','),
 VToken(kind: vtkKeyword, keyword: "Hey"),
 VToken(kind: vtkSeparator, sign: ';'),
 VToken(kind: vtkScope, scope: ')'),
]

echo goTillNextSemiColon(addr tks, 0)

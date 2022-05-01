import std/[sequtils]
import vverilog/parser {.all.}
import vverilog/lexer


let tokens = toseq extractVerilogTokens """
initial begin
	a = 2;
end
"""

var mdl = VNode(kind: vnkModule)
let s = parseVerilogImpl(tokens, @[mdl], @[psTopLevel, psModuleBody])
echo treeRepr s[0]
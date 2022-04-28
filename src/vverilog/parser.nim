import std/[sequtils, options]
import ./lexer, ./conventions

type
  VerilogDeclareKinds* = enum
    vdkInput
    vdkOutput
    vdkInOut
    vdkReg
    vdkWire

  VerilogNodeKinds* = enum
    vnkNormNumber, vnkBinNumber, vnkHexNumber, vnkString
    vnkRange # 3:1
    vnkSymbol, vnkScope, vnkInstantiate
    vnkDeclare, vnkDefine, vnkAsgn

    vnkModule
    vnkCase, vnkOf
    vnkElif
    vnkInfix, vnkPrefix

    vnkPar, vnkCurlyGroup
    vnkBracketExpr

    vnkComment


  ScopeKinds* = enum
    skAlways
    skForever
    skInitial

  VerilogNode = ref object
    body*: seq[VerilogNode]

    case kind*: VerilogNodeKinds
    of vnkNormNumber, vnkBinNumber, vnkHexNumber:
      digits*: string

    of vnkString:
      str*: string

    of vnkSymbol:
      symbol*: string

    of vnkScope:
      scope*: ScopeKinds

    of vnkDeclare, vnkDefine:
      ident*, bitRange*, value*: VerilogNode

    of vnkAsgn:
      container*, newValue*: VerilogNode

    of vnkModule:
      name*: VerilogNode
      params*: seq[VerilogNode]

    of vnkInstantiate:
      module*, instance*: VerilogNode

    of vnkCase:
      select*: VerilogNode

    of vnkOf:
      comparator*: Option[VerilogNode]

    of vnkElif:
      condition*: Option[VerilogNode]

    of vnkInfix, vnkPrefix:
      operator*: VerilogNode

    of vnkPar, vnkCurlyGroup:
      discard

    of vnkRange:
      head*, tail*: VerilogNode

    of vnkBracketExpr:
      lookup*: VerilogNode
      index*: VerilogNode

    of vnkComment:
      content*: string
      inline*: bool

  VNode = VerilogNode

  ParserScopes = enum
    psInitial
    psModuleDef
    psModule
    psAlways
    psCase
    psElIf
    psOther # initial, forever, always


# module <name>(a1, a2[1:2]);

#   initial begin
#     _      
#   end

#   initial expr;

# endmodule

func parseVerilogImpl(tokens: seq[VToken], acc: var seq[VNode]): int =
  var 
    i = 0
    nodeStack: seq[VNode]

  while true:
    let ct = tokens[i] # current token

    if ct.kind == vtkKeyword:
      case ct.keyword:
      of "module":
        discard

      of "endmodule":
        discard

      of "begin":
        discard

      of "end":
        discard

      of "if":
        discard

      of "else":
        discard

      of "case":
        discard


      of "input":
        discard

      of "output":
        discard

      of "inout":
        discard

      of "wire":
        discard

      of "reg":
        discard


      of "`define":
        discard

      else:
        err "what?"


  if nodeStack.len != 0:
    err "stack is not empty"

func parseVerilog*(content: string): seq[VNode] =
  let tokens = toseq extractVerilogTokens content

  discard parseVerilogImpl(tokens, result)
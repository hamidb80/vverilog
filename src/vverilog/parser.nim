import std/[sequtils, options, strutils, strformat]
import ./lexer, ./conventions

type
  VerilogDeclareKinds* = enum
    vdkInput
    vdkOutput
    vdkInOut
    vdkReg
    vdkWire

  VerilogGroupKinds* = enum
    vskBeginEnd
    vskPar
    vskBracket
    vsk

  VerilogNumberKinds* = enum
    vnInt
    vnFloat
    vnBinary
    vnHex

  ScopeKinds* = enum
    skAlways
    skForever
    skInitial

  VerilogNodeKinds* = enum
    vnkNumber, vnkString, vnkRange
    vnkSymbol, vnkGroup

    vnkDeclare, vnkDefine, vnkAsgn, vnkInstantiate

    vnkModule, vnkScope
    vnkCase, vnkOf
    vnkElif

    vnkInfix, vnkPrefix
    vnkBracketExpr

    vnkComment

  VerilogNode* = ref object
    body*: seq[VerilogNode]

    case kind*: VerilogNodeKinds
    of vnkNumber:
      numberKind*: VerilogNumberKinds
      digits*: string

    of vnkString:
      str*: string

    of vnkRange:
      head*, tail*: VerilogNode

    of vnkSymbol:
      symbol*: string

    of vnkDeclare, vnkDefine:
      ident*, bitRange*, value*: VerilogNode

    of vnkAsgn:
      container*, newValue*: VerilogNode

    of vnkModule:
      name*: VerilogNode
      params*: seq[VerilogNode]

    of vnkScope:
      scope: ScopeKinds

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

    of vnkGroup:
      groupKind: VerilogGroupKinds

    of vnkBracketExpr:
      lookup*: VerilogNode
      index*: VerilogNode

    of vnkComment:
      comment*: string
      inline*: bool

  VNode* = VerilogNode

  ParserState = enum
    psTopLevel
    psModuldeIdent, psModuldeParams, psModuleBody
    
    psDefine, psDeclare

    ps

func toVSymbol(name: string): VNode =
  VNode(kind: vnkSymbol, symbol: name)

func toVNumber(digits: string): VNode =
  VNode(kind: vnkNumber, digits: digits)

func toVNode(token: VToken): VNode =
  case token.kind:
  of vtkKeyword: toVSymbol token.keyword
  of vtkNumber: toVNumber token.digits
  of vtkComment:
    VNode(kind: vnkComment, inline: token.inline, comment: token.comment)
  else:
    err "this kind of converting is invalid"

func `$`(node: VNode): string =
  case node.kind:
  of vnkSymbol: node.symbol
  of vnkBracketExpr: fmt"{node.lookup}[{node.index}]"
  of vnkRange: fmt"{node.head}:{node.tail}"
  of vnkNumber: node.digits
  of vnkModule: $node.name & '(' & node.params.join(", ") & ')'
  else:
    err fmt"invalid conversion to string. kind: {node.kind}"


# func goTillNextSemiColon(tokens: ptr seq[VToken], startIndex: int): Natural =
#   ## return a number as progress
#   for i in startIndex .. tokens[].high:
#     let t = tokens[i]
#     if not (t.kind == vtkSeparator and t.sign == ';'):
#       inc result
#     else:
#       break



func parseVerilogImpl(tokens: seq[VToken], acc: var seq[VNode]): int =
  var
    i = 0
    vnodeStack: seq[VNode]

  while i < tokens.len:
    let ct = tokens[i] # current token

    matchVtoken ct:
    of kw "module":
      discard

    of kw "endmodule":
      err "ENDE"

    else:
      inc i

    # of "begin":
    #   discard

    # of "end":
    #   discard

    # of "if":
    #   discard

    # of "else":
    #   discard

    # of "case":
    #   discard


    # of "input":
    #   discard

    # of "output":
    #   discard

    # of "inout":
    #   discard

    # of "wire":
    #   discard

    # of "reg":
    #   discard


    # of "`define":
    #   discard

    # else:
    #   err "what?"
  else:
    inc i


func parseVerilog*(content: string): seq[VNode] =
  let tokens = toseq extractVerilogTokens content

  discard parseVerilogImpl(tokens, result)

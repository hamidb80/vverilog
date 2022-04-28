import std/[sequtils, options]
import ./lexer

type
  VerilogDeclareKinds* = enum
    vdkInput
    vdkOutput
    vdkReg
    vdkWire

  VerilogNodeKinds* = enum
    vnkNormNumber, vnkBinNumber, vnkHexNumber, vnkString
    vnkKeyword, vnkSymbol
    
    vnkDeclare, vnkDefine, vnkAsgn

    vnkModule
    vnkCase, vnkOf
    vnkElif
    vnkInfix, vnkPrefix
    
    vnkPar, vnkCurlyGroup
    vnkBraketRange
    vnkBracketExpr
    
    
  VerilogNode = ref object
    body: seq[VerilogNode]

    case kind: VerilogNodeKinds
    of vnkNormNumber, vnkBinNumber, vnkHexNumber:
      digits: string

    of vnkString:
      str: string
    
    of vnkKeyword:
      keyword: string

    of vnkSymbol:
      symbol: string

    of vnkDeclare:
      ident, bitRange, value: VerilogNode
      
    of vnkDefine:
      alias, replacement: VerilogNode
    
    of vnkAsgn:
      container, newValue: VerilogNode
    
    of vnkModule:
      name: VerilogNode
      params: seq[VerilogNode]

    of vnkCase:
      select: VerilogNode

    of vnkOf:
      comparator: Option[VerilogNode]

    of vnkElif:
      condition: Option[VerilogNode]
      
    of vnkInfix, vnkPrefix:
      operator: VerilogNode

    of  vnkPar, vnkCurlyGroup:
      discard

    of vnkBraketRange:
      head, tail: VerilogNode

    of vnkBracketExpr:
      lookup: VerilogNode
      expr: VerilogNode
    

const VNodeLiteralKinds = {vnkNormNumber, vnkBinNumber, vnkHexNumber, vnkString}

func parseVerilog*(content: string): seq[VerilogNode] =
  let tokens = toseq extractVTokens content

import std/[sequtils, options, strutils]
import ./lexer, ./conventions

type
  VerilogDeclareKinds* = enum
    vdkInput
    vdkOutput
    vdkInOut
    vdkReg
    vdkWire

  VerilogGroupKinds* = enum
    vskPar
    vskBracket
    vskCurly

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

    vnkCall, vnkAction # dumpvars;

    vnkDeclare, vnkDefine, vnkAsgn, vnkInstantiate

    vnkModule, vnkScope
    vnkCase, vnkOf, vnkElif

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

    of vnkCall:
      caller*: VerilogNode

    of vnkAction:
      action*: VerilogNode

    of vnkDeclare, vnkDefine:
      dkind*: VerilogDeclareKinds
      bus*: Option[VerilogNode]
      ident*: VerilogNode

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
      operator*: string

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
    psModuldeIdent, psModuldeParams, psModuleBody, psModuleAddBody

    # psDefine, psAsgn
    psDeclareStart, psDeclareBus, psDeclareApplyBus, psDeclareIdent,
        psDeclareArray, psDeclareEnd

    psParStart, psParBody
    psBracketStart, psBracketBody
    psCurlyStart, psCurlyBody

    psPrefixStart, psPrefixEnd
    psInfix

    psExprStart, psExpr

    psInstanceName, psInstanceArgs
    psScopeBody, psScopeArgs # always @ (...)

    psElIfCond, psElIfBody, psElseBody
    psCaseParam, psCaseOfParam, psCaseOfBody

    psOperator


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

func toDeclareKind(s: string): VerilogDeclareKinds =
  case s:
  of "input": vdkInput
  of "output": vdkOutput
  of "inout": vdkInOut
  of "reg": vdkReg
  of "wire": vdkWire
  else: err "invalid declare type"


template genController(varname): untyped =
  template follow(v): untyped {.dirty.} =
    varname.add v

  template back(): untyped {.dirty.} =
    debugecho ">>> backed ", varname[varname.high]
    del varname, varname.high

  template switch(v): untyped {.dirty.} =
    back()
    follow(v)

func parseVerilogImpl(tokens: seq[VToken]): seq[VNode] =
  var
    i = 0
    nodeStack: seq[VNode]
    stateStack: seq[ParserState] = @[psTopLevel]

  genController stateStack

  while i < tokens.len:
    let ct = tokens[i] # current token
    debugecho " - - - - - - - - - - - - - - - - - "
    debugecho ct
    debugecho "/ ", stateStack.join" / "
    debugecho "> ", nodeStack.mapIt(it.kind).join" > "
    debugecho ": : : : : :"

    if ct.kind == vtkComment:
      debugEcho ">>> SKIPPED"
      inc i
      continue

    case stateStack.last:
      of psTopLevel:
        matchVtoken ct:
        of kw "module":
          nodeStack.add VNode(kind: vnkModule)
          stateStack.add psModuldeIdent
          inc i

        else: err "not implemented: " & $ct


      of psModuldeIdent:
        matchVtoken ct:
        of kw:
          nodeStack.last.name = toVNode ct
          switch psModuldeParams
          inc i

        else:
          err "hee"

      of psModuldeParams:
        matchVtoken ct:
        of g vgcOpenPar:
          follow psParStart

        of w skSemiColon:
          let ln = nodestack.last
          if ln.kind == vnkGroup and ln.groupKind == vskPar:
            let p = nodestack.pop
            nodestack.last.params = p.body

          switch psModuleBody
          inc i

        else:
          err "invalid token"

      of psModuleBody:
        matchVtoken ct:
        of kw"endmodule":
          result.add nodeStack.pop
          inc i

        of kw"input", kw"output", kw"inout", kw"wire", kw"reg":
          follow psModuleAddBody
          follow psDeclareStart

      of psModuleAddBody:
        let p = nodeStack.pop
        nodestack.last.body.add p
        back

        inc i


      of psDeclareStart:
        nodeStack.add VNode(kind: vnkDeclare, dkind: toDeclareKind ct.keyword)
        switch psDeclareBus
        inc i

      of psDeclareBus:
        matchVtoken ct:
        of g vgcOpenBracket:
          switch psDeclareApplyBus
          follow psBracketStart

        of kw:
          switch psDeclareIdent

      of psDeclareApplyBus:
        let p = nodestack.pop
        nodestack.last.bus = some p

        switch psDeclareIdent

      of psDeclareIdent:
        matchVtoken ct:
        of kw:
          nodestack.last.ident = toVNode ct
          switch psDeclareArray

        else: err "what"

        inc i

      of psDeclareArray:
        matchVtoken ct:
        of w skSemiColon:
          switch psDeclareEnd

        of g vgcCloseBracket:
          inc i

        else: err "what"

      of psDeclareEnd:
        back

      # ------------------------------------

      of psParStart:
        matchVtoken ct:
        of g vgcOpenPar:
          nodeStack.add VNode(kind: vnkGroup, groupkind: vskPar)
          switch psParBody
          inc i

        else:
          err "invalid"

      of psParBody:
        matchVtoken ct:
        of w skComma:
          let p = nodeStack.pop
          nodeStack.last.body.add p

          follow psExprStart
          inc i

        of g vgcClosePar:
          let p = nodeStack.pop
          nodeStack.last.body.add p

          back()
          inc i

        else:
          follow psExprStart


      of psBracketStart:
        matchVtoken ct:
        of g vgcOpenBracket:
          nodeStack.add VNode(kind: vnkGroup, groupkind: vskBracket)
          switch psBracketBody
          follow psExprStart
          inc i

        else:
          err "invalid"

      of psBracketBody:
        matchVtoken ct:
        of g vgcCloseBracket:
          # group/number | group/range/number
          let
            ex = nodeStack.pop
            up = nodeStack.pop

          if up.kind == vnkRange:
            up.tail = ex
            discard nodeStack.pop
            nodeStack.add up

          else:
            nodeStack.add up


          back()

        of w skColon:
          let p = nodestack.pop
          nodestack.add VNode(kind: vnkRange, head: p)
          follow psExprStart

        else:
          follow psExprStart

        inc i

      # of psCurlyStart:
      # of psCurlyBody:

      # ------------------------------------


      of psExprStart:
        matchVtoken ct:
        of kw, n:
          nodeStack.add toVNode ct
          inc i

        of g vgcOpenBracket:
          switch psBracketStart

        # of o:
        #   follow psPrefixStart
        #   inc i

        else:
          back()

      of psExpr:
        matchVtoken ct:
        else:
          back



      of psPrefixStart:
        nodeStack.add VNode(kind: vnkPrefix, operator: ct.operator)
        follow psPrefixEnd
        follow psExprStart
        inc i

      of psPrefixEnd:
        let p = nodeStack.pop
        nodeStack.last.body.add p
        back

      else: err "this parser state is not implemented: " & $stateStack.last

func parseVerilog*(content: string): seq[VNode] =
  let tokens = toseq extractVerilogTokens content
  parseVerilogImpl tokens


func `$`*(k: VerilogDeclareKinds): string =
  case k:
  of vdkInput: "input"
  of vdkOutput: "output"
  of vdkInOut: "inout"
  of vdkReg: "reg"
  of vdkWire: "wire"

const indentSize = 4

func toString(vn: VNode, depth: int = 0): string =
  let t =
    case vn.kind:

    of vnkNumber: vn.digits
    of vnkString: '"' & vn.str & '"'
    of vnkSymbol: vn.symbol

    of vnkAction: toString(vn.action, depth+1) & ";\n"
    of vnkRange: toString(vn.head) & ':' & toString(vn.tail)

    of vnkGroup:
      let openClose =
        case vn.groupKind:
        of vskPar: ['(', ')']
        of vskBracket: ['[', ']']
        of vskCurly: ['{', '}']

      openClose[0] & vn.body.mapIt(it.toString).join(", ") & openClose[1]

    of vnkCall:
      toString(vn.caller) & '(' &
      vn.body.mapIt(it.toString).join(", ") & ')'

    of vnkBracketExpr:
      toString(vn.lookup) & '[' & toString(vn.index) & ']'

    of vnkDeclare:
      let b =
        if issome vn.bus: '[' & toString(vn.bus.get) & "] "
        else: ""

      $vn.dkind & ' ' & b & toString(vn.ident) & ';'

    of vnkModule:
      "module " & toString(vn.name) &
      '(' & vn.params.mapIt(it.toString).join(", ") & ");\n" &
      vn.body.mapIt(it.toString depth+1).join("\n") &
      "\nendmodule"

    of vnkPrefix: vn.operator & toString(vn.body[0])

    of vnkInfix:
      toString(vn.body[0]) & ' ' &
      vn.operator & ' ' &
      toString(vn.body[1])

    of vnkInstantiate:
      toString(vn.module) & ' ' & toString(vn.instance) &
      '(' & vn.body.mapIt(it.toString).join(", ") & ");"

    # of vnkElif:

    # of vnkCase:
    # of vnkOf:

    # of vnkDefine:
    # of vnkAsgn:

    of vnkComment:
      if vn.inline:
        "//" & vn.comment
      else:
        "/*" & vn.comment & "*/"

    else: err "wow"

  repeat(" ", depth * indentSize) & t

func `$`*(vn: VNode): string =
  toString vn

import std/[strutils, strformat, macros, sequtils]
import ./conventions

type
  VerilogTokenKinds* = enum
    vtkKeyword   # begin end if `pragma
    vtkSeparator # , : ;
    vtkOperator  # + - && ~^ !== ?:
    vtkGroup     # () [] {}

    vtkString    # "hello"
    vtkNumber    # 12 4.6 3b'101
    vtkComment   # //  /* */

  SeparatorKinds* = enum
    skSemiColon
    skColon
    skComma
    skNewLine

  VerilogGroupChar* = enum
    vgcOpenPar, vgcClosePar
    vgcOpenBracket, vgcCloseBracket
    vgcOpenCurly, vgcCloseCurly
  
  VerilogToken* = object # verilog token
    case kind*: VerilogTokenKinds
    of vtkKeyword:
      keyword*: string

    of vtkString:
      content*: string

    of vtkNumber:
      digits*: string

    of vtkSeparator:
      sepKind*: SeparatorKinds

    of vtkOperator:
      operator*: string

    of vtkGroup:
      group*: VerilogGroupChar

    of vtkComment:
      comment*: string
      inline*: bool

  VToken* = VerilogToken

  LexerState = enum
    lsInit
    lsNumber, lsKeyword, lsString
    lsOperator
    lsInlineComment, lsMultiLineComment


const
  VerilogIdentStartChars = IdentStartChars + {'$'}
  EoC = '\0' # end of content
  Stoppers = Whitespace -  {'\n'} + {EoC}
  Operators = {'+', '-', '*', '/', '#', '@', '~', '?', '^', '|', '&', '%',
      '<', '!', '=', '>', '.'}


func toSep*(c: char): SeparatorKinds =
  case c:
  of ':': skColon
  of ';': skSemiColon
  of ',': skComma
  of '\n': skNewLine
  else:
    err "invalid cahr"

func fromSep*(s: SeparatorKinds): char =
  case s:
  of skColon: ':'
  of skSemiColon: ';'
  of skComma: ','
  of skNewLine: '\n'

func matchOperator*(vt:VToken, op: string): bool=
  vt.kind == vtkOperator and vt.operator == op

func toGroupChar(ch: char): VerilogGroupChar =
  case ch:
  of '(': vgcOpenPar
  of ')': vgcClosePar
  of '[': vgcOpenBracket
  of ']': vgcCloseBracket
  of '{': vgcOpenCurly
  of '}': vgcCloseCurly
  else: err "invalid char"

iterator extractVerilogTokens*(content: string): VerilogToken =
  var
    lxState = lsInit
    i = 0
    start = 0

  alias fetchChar(i):
    if i in 0 ..< content.len: content[i]
    else: EoC
  alias lc: fetchChar(i-1) # last char
  alias fc: fetchChar(i+1) # forward char
  alias reset: lxState = lsInit
  alias push(newToken):
    yield newToken
    reset()


  while true:
    let cc = fetchChar i # current char

    case lxState:
    of lsInit:
      case cc:
      of Digits:
        lxState = lsNumber
        start = i
        inc i

      of VerilogIdentStartChars, '`':
        lxState = lsKeyword
        start = i
        inc i

      of '"':
        lxState = lsString
        inc i
        start = i

      of ',', ':', ';', '\n':
        push VerilogToken(kind: vtkSeparator, sepKind: toSep cc)
        inc i

      of '(', ')', '[', ']', '{', '}':
        push VerilogToken(kind: vtkGroup, group: toGroupChar cc)
        inc i

      of Stoppers:
        inc i

      of Operators:
        if cc == '/':
          case fc:
          of '/':
            lxState = lsInlineComment
            inc i, 2
            start = i

          of '*':
            lxState = lsMultiLineComment
            inc i, 2
            start = i

          else:
            lxState = lsOperator
            start = i
            inc i

        else:
          lxState = lsOperator
          start = i
          inc i

      else:
        err fmt"invalid character at index {i}: '{cc}'"

    of lsKeyword:
      case cc:
      of IdentChars:
        inc i
      else:
        push VerilogToken(kind: vtkKeyword, keyword: content[start ..< i])

    of lsNumber:
      case cc:
      of '.', '_', '\'', 'b', 'h', Digits, 'A' .. 'F', 'X', 'x', 'Z', 'z':
        inc i
      else:
        push VerilogToken(kind: vtkNumber, digits: content[start ..< i])

    of lsString:
      if cc == '"' and lc != '\\':
        push VerilogToken(kind: vtkString, content: content[start ..< i])

      inc i

    of lsOperator:
      if cc in Operators:
        inc i
      else:
        push VerilogToken(kind: vtkOperator, operator: content[start ..< i])

    of lsInlineComment:
      if cc in Newlines:
        push VerilogToken(kind: vtkComment, comment: content[start ..< i], inline: true)

      inc i

    of lsMultiLineComment:
      if cc == '/' and lc == '*':
        push VerilogToken(kind: vtkComment, comment: content[start ..< i-1], inline: false)

      inc i

    if cc == EoC:
      break


func getField(kind: VerilogTokenKinds): string =
  case kind:
  of vtkKeyword: "keyword"
  of vtkSeparator: "sepKind"
  of vtkOperator: "operator"
  of vtkGroup: "group"
  else: err "get field for this type is not implemented: " & $kind

func abbrToTkind(abbr: string): VerilogTokenKinds =
  case abbr:
  of "kw": vtkKeyword
  of "w": vtkSeparator
  of "o": vtkOperator
  of "g": vtkGroup
  of "s": vtkString
  of "n": vtkNumber
  of "c": vtkComment
  else: err "what? " & abbr


func getStrval*(vt: VToken): string=
  case vt.kind:
  of vtkKeyword: vt.keyword
  of vtkOperator: vt.operator
  of vtkString: vt.content
  of vtkNumber: vt.digits
  else: err "not :::"


macro matchVtoken*(comparator: VToken, branches: varargs[untyped]): untyped =
  result = newTree(nnkCaseStmt, newDotExpr(comparator, ident"kind"))

  var
    acc1: array[VerilogTokenKinds, seq[tuple[cond, code: NimNode]]]
    acc2: array[VerilogTokenKinds, NimNode]
    elseBr = newTree(nnkElse, newStmtList newTree(nnkDiscardStmt, newEmptyNode()))

  for br in branches:
    case br.kind:
    of nnkOfBranch:
      let
        conds = br[0..^2]
        body = br[^1]

      for c in conds:
        if c.kind in {nnkCommand, nnkCallStrLit, nnkPrefix} and c.len == 2:
          let index = abbrToTkind c[0].strVal
          acc1[index].add (c[1], body)

        elif c.kind == nnkIdent:
          let index = abbrToTkind c.strval
          acc2[index] = body

        else:
          err "invalid condition"

    of nnkElse:
      elseBr = br

    else: err "invalid entity. kind: " & $br.kind

  for i, brs in acc1:
    if brs.len != 0:
      let node =
        newtree(nnkOfBranch, ident $i).add newStmtList do:
          newTree(nnkCaseStmt, newDotExpr(comparator, ident getField i)).add:
            brs.mapIt newTree(nnkOfBranch, it.cond, it.code)

      node[^1][^1].add elsebr
      result.add node

  for i, code in acc2:
    if code != nil:
      result.add newtree(nnkOfBranch, ident $i, code)

  result.add elseBr

  # echo treeRepr result
  # echo repr result

func matchSep*(t:VToken, sep: char): bool =
  t.kind == vtkSeparator and t.sepKind == toSep sep

# func matchOp*(t:VToken, op: string): bool =
#   t.kind == vtkOperator and t.operator == op

# test -----------------------------------------

# let t = VToken(kind: vtkString, content: "hey")

# let acc = block:
#   matchVtoken t:
#   of kw"1", kw"2": 1
#   of n: 2
#   else: err "what"

# dumptree:
#   case 1:
#   of 2: discard
#   of 3, 4: discard
#   else: discard

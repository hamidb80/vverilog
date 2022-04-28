import std/[strutils, strformat]
import ./conventions

type
  VerilogTokenKinds* = enum
    vtkKeyword   # begin end if `pragma
    vtkString    # "hello"
    vtkNumber    # 12 4.6 3b'101

    vtkScope     # () [] {}
    vtkSeparator # , : ;

    vtkOperator  # + - && ~^ !== ?:
    vtkComment   # //  /* */

  VerilogToken* = object # verilog token
    case kind*: VerilogTokenKinds
    of vtkKeyword:
      keyword*: string

    of vtkString:
      content*: string

    of vtkNumber:
      digits*: string

    of vtkSeparator:
      sign*: char

    of vtkOperator:
      operator*: string

    of vtkScope:
      scope*: char

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
  Stoppers = Whitespace + {EoC}
  Operators = {'+', '-', '*', '/', '#', '@', '~', '?', '^', '|', '&', '%',
      '<', '!', '=', '>', '.'}


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

      of ',', ':', ';':
        push VerilogToken(kind: vtkSeparator, sign: cc)
        inc i

      of '(', ')', '[', ']', '{', '}':
        push VerilogToken(kind: vtkScope, scope: cc)
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

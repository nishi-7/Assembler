package assembler

import assembler.Util._

class Parser(lex: Lexer) {
  private var curToken = lex.nextToken()
  private var nxtToken = lex.nextToken()

  def parseOne(): Asm = {
    curToken.ty match {
      case LParen => this.parseLabel()
      case At     => this.parseACmd()
      case NewLine => {
        this.nextToken()
        this.parseOne()
      }
      case _ => this.parseCCmd()
    }
  }

  def isEnd() = curToken.ty == Eof

  def nextToken() = {
    val tok = curToken
    curToken = nxtToken
    nxtToken = this.lex.nextToken()
    tok
  }

  def curTokenTypeIs(tokTy: TokenType) = {
    (curToken.ty, tokTy) match {
      case (Number(_), Number(_)) | (Symbol(_), Symbol(_)) => true
      case _ => curToken.ty == tokTy
    }
  }

  def nxtTokenTypeIs(tokTy: TokenType) = {
    (nxtToken.ty, tokTy) match {
      case (Number(_), Number(_)) | (Symbol(_), Symbol(_)) => true
      case _ => nxtToken.ty == tokTy
    }
  }

  def expectedAs(tokTy: TokenType): Token = {
    if (!this.curTokenTypeIs(tokTy)) {
      throwSyntaxError(tokTy, curToken)
    }
    this.nextToken()
  }

  def parseLabel(): Asm = {
    val Token(_, loc1) = this.expectedAs(LParen)
    val Token(Symbol(label), loc2) = this.expectedAs(Symbol("_"))
    val Token(_, loc3) = this.expectedAs(RParen)
    this.expectedAs(NewLine)
    Asm(Label(label), loc2)
  }

  def parseACmd(): Asm = {
    val Token(_, loc1) = this.expectedAs(At)
    val asm = curToken match {
      case Token(Symbol(_), loc) => Asm(ACmdLabel(curToken.ty), loc)
      case Token(Number(_), loc) => Asm(ACmd(curToken.ty), loc)
      case Token(Sp, _) | Token(Lcl, _) | Token(Arg, _) | Token(This, _) |
          Token(That, _) | Token(R0, _) | Token(R1, _) | Token(R2, _) |
          Token(R3, _) | Token(R4, _) | Token(R5, _) | Token(R6, _) |
          Token(R7, _) | Token(R8, _) | Token(R9, _) | Token(R10, _) |
          Token(R11, _) | Token(R12, _) | Token(R13, _) | Token(R14, _) |
          Token(R15, _) | Token(Screen, _) | Token(KeyBoard, _) |
          Token(UartRx1, _) | Token(Spi, _) | Token(LED7Seg, _) =>
        Asm(ACmd(curToken.ty), curToken.loc)
      case _ => throwSyntaxError(Symbol("_"), curToken)
    }
    this.nextToken()
    this.expectedAs(NewLine)
    asm
  }

  def parseCCmd() = {
    val Token(dest, _) = this.parseCCmdDest()
    val Token(comp, loc2) = this.parseCCmdComp()
    val Token(jump, _) = this.parseCCmdJump()
    this.expectedAs(NewLine)
    Asm(CCmd(dest, comp, jump), loc2)
  }

  def parseCCmdDest() = {
    curToken.ty match {
      case A | D | M | AD | AM | MD | AMD if nxtTokenTypeIs(Eq) => {
        val tok = this.nextToken()
        this.expectedAs(Eq)
        tok
      }
      case _ => Token(Null, curToken.loc)
    }
  }

  def parseCCmdComp() = {
    curToken.ty match {
      case Number(0) | Number(1) | MinusOne | A | D | M | BangA | BangD |
          BangM | MinusA | MinusD | MinusM | APlusOne | DPlusOne | MPlusOne |
          AMinusOne | DMinusOne | MMinusOne | DPlusA | DPlusM | AMinusD |
          DMinusA | DMinusM | MMinusD | DAndA | DAndM | DOrA | DOrM => {
        this.nextToken()
      }
      case _ => throwTokenError(curToken)
    }
  }

  def parseCCmdJump() = {
    curToken.ty match {
      case SemiColon => {
        this.nextToken()
        curToken.ty match {
          case Jgt | Jeq | Jge | Jlt | Jne | Jle | Jmp => { this.nextToken() }
          case _ => throwTokenError(curToken)
        }
      }
      case _ => Token(Null, curToken.loc)
    }
  }
}

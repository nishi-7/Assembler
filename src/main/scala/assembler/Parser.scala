package assembler

import assembler.Util._

class Parser(lex: Lexer) {
    private var curToken = lex.nextToken()
    private var nxtToken = lex.nextToken()

    def parseOne() = {
        curToken.ty match {
            case LParen => this.parseLabel()
            case At => this.parseACmd()
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
            case A | D | M | AD| AM | MD | AMD if nxtTokenTypeIs(Eq) => {
                val tok = this.nextToken()
                this.expectedAs(Eq)
                tok
            }
            case _ => Token(Null, curToken.loc)
        }
    }

    def parseCCmdComp() = {
        curToken.ty match {
            case Number(0) | Number(1) | MinusOne |
                 A | D | M |
                 BangA | BangD | BangM | MinusA | MinusD | MinusM |
                 APlusOne | DPlusOne | MPlusOne | AMinusOne | DMinusOne | MMinusOne |
                 DPlusA | DPlusM | AMinusD |  DMinusA | DMinusM | MMinusD |
                 DAndA | DAndM | DOrA | DOrM
            => {
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
                    case Jgt | Jeq | Jge | Jlt | Jne | Jle | Jmp
                        => { this.nextToken() }
                    case _ => throwTokenError(curToken)
                }
            }
            case _ => Token(Null, curToken.loc)
        }
    }
}

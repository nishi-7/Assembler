package assembler

import org.scalatest._

class LexerSpec extends FlatSpec with Matchers {
    behavior of "nextToken"
    it should "tokenize all token" in {
        val code =
            """,;()@ foo 321
              | A M D AM AD MD AMD
              | 0 1 -1
              | !A !D !M -A -D -M
              | A+1 D+1 M+1 A-1 D-1 M-1 D+A D+M D-A D-M A-D M-D D&A D&M D|A D|M
              | JGT JEQ JLT JNE JLE JMP
              | SP LCL ARG THIS
              | R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15
              | SCREEN KEYBOARD UARTRX1 UARTRX2 SPI """.stripMargin
        val lex = new Lexer(code)
        lex.nextToken().ty shouldBe(Comma)
        lex.nextToken().ty shouldBe(SemiColon)
        lex.nextToken().ty shouldBe(LParen)
        lex.nextToken().ty shouldBe(RParen)
        lex.nextToken().ty shouldBe(At)
        lex.nextToken().ty shouldBe(Symbol("foo"))
        lex.nextToken().ty shouldBe(Number(321))
        lex.nextToken().ty shouldBe(NewLine)

        lex.nextToken().ty shouldBe(A)
        lex.nextToken().ty shouldBe(M)
        lex.nextToken().ty shouldBe(D)
        lex.nextToken().ty shouldBe(AM)
        lex.nextToken().ty shouldBe(AD)
        lex.nextToken().ty shouldBe(MD)
        lex.nextToken().ty shouldBe(AMD)
        lex.nextToken().ty shouldBe(NewLine)

        lex.nextToken().ty shouldBe(Number(0))
        lex.nextToken().ty shouldBe(Number(1))
        lex.nextToken().ty shouldBe(MinusOne)
        lex.nextToken().ty shouldBe(NewLine)

        lex.nextToken().ty shouldBe(BangA)
        lex.nextToken().ty shouldBe(BangD)
        lex.nextToken().ty shouldBe(BangM)
        lex.nextToken().ty shouldBe(MinusA)
        lex.nextToken().ty shouldBe(MinusD)
        lex.nextToken().ty shouldBe(MinusM)
        lex.nextToken().ty shouldBe(NewLine)

        lex.nextToken().ty shouldBe(APlusOne)
        lex.nextToken().ty shouldBe(DPlusOne)
        lex.nextToken().ty shouldBe(MPlusOne)
        lex.nextToken().ty shouldBe(AMinusOne)
        lex.nextToken().ty shouldBe(DMinusOne)
        lex.nextToken().ty shouldBe(MMinusOne)
        lex.nextToken().ty shouldBe(DPlusA)
        lex.nextToken().ty shouldBe(DPlusM)
        lex.nextToken().ty shouldBe(DMinusA)
        lex.nextToken().ty shouldBe(DMinusM)
        lex.nextToken().ty shouldBe(AMinusD)
        lex.nextToken().ty shouldBe(MMinusD)
        lex.nextToken().ty shouldBe(DAndA)
        lex.nextToken().ty shouldBe(DAndM)
        lex.nextToken().ty shouldBe(DOrA)
        lex.nextToken().ty shouldBe(DOrM)
        lex.nextToken().ty shouldBe(NewLine)

        lex.nextToken().ty shouldBe(Jgt)
        lex.nextToken().ty shouldBe(Jeq)
        lex.nextToken().ty shouldBe(Jlt)
        lex.nextToken().ty shouldBe(Jne)
        lex.nextToken().ty shouldBe(Jle)
        lex.nextToken().ty shouldBe(Jmp)
        lex.nextToken().ty shouldBe(NewLine)

        lex.nextToken().ty shouldBe(Sp)
        lex.nextToken().ty shouldBe(Lcl)
        lex.nextToken().ty shouldBe(Arg)
        lex.nextToken().ty shouldBe(This)
        lex.nextToken().ty shouldBe(NewLine)

        lex.nextToken().ty shouldBe(R0)
        lex.nextToken().ty shouldBe(R1)
        lex.nextToken().ty shouldBe(R2)
        lex.nextToken().ty shouldBe(R3)
        lex.nextToken().ty shouldBe(R4)
        lex.nextToken().ty shouldBe(R5)
        lex.nextToken().ty shouldBe(R6)
        lex.nextToken().ty shouldBe(R7)
        lex.nextToken().ty shouldBe(R8)
        lex.nextToken().ty shouldBe(R9)
        lex.nextToken().ty shouldBe(R10)
        lex.nextToken().ty shouldBe(R11)
        lex.nextToken().ty shouldBe(R12)
        lex.nextToken().ty shouldBe(R13)
        lex.nextToken().ty shouldBe(R14)
        lex.nextToken().ty shouldBe(R15)
        lex.nextToken().ty shouldBe(NewLine)


        lex.nextToken().ty shouldBe(Screen)
        lex.nextToken().ty shouldBe(KeyBoard)
        lex.nextToken().ty shouldBe(UartRx1)
        lex.nextToken().ty shouldBe(UartRx2)
        lex.nextToken().ty shouldBe(Spi)
        lex.nextToken().ty shouldBe(Eof)
    }
}

package assembler

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
    behavior of "parseOne: ACmd"
    it should "parse @1234\\n" in {
        val code =
            """@1234
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(ACmd(Number(1234)))
    }
    it should "parse @Label\\n" in {
        val code =
            """@Label
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(ACmdLabel(Symbol("Label")))
    }

    behavior of "parseOne: CCmd"
    it should "parse M=0" in {
        val code =
            """M=0
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(M, Number(0), Null))
    }
    it should "parse M=1" in {
        val code =
            """M=1
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(M, Number(1), Null))
    }
    it should "parse M=-1" in {
        val code =
            """M=-1
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(M, MinusOne, Null))
    }
    it should "parse M=A" in {
        val code =
            """M=A
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(M, A, Null))
    }
    it should "parse A=M" in {
        val code =
            """A=M
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(A, M, Null))
    }
    it should "parse A=D" in {
        val code =
            """A=D
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(A, D, Null))
    }
    it should "parse A=!D" in {
        val code =
            """A=!D
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(A, BangD, Null))
    }
    it should "parse A=!M" in {
        val code =
            """A=!M
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(A, BangM, Null))
    }
    it should "parse D=!A" in {
        val code =
            """D=!A
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(D, BangA, Null))
    }
    it should "parse D=-D" in {
        val code =
            """D=-D
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(D, MinusD, Null))
    }
    it should "parse D=-A" in {
        val code =
            """D=-A
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(D, MinusA, Null))
    }
    it should "parse D=-M" in {
        val code =
            """D=-M
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(D, MinusM, Null))
    }
    it should "parse MD=D+1" in {
        val code =
            """MD=D+1
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(MD, DPlusOne, Null))
    }
    it should "parse MD=A+1" in {
        val code =
            """MD=A+1
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(MD, APlusOne, Null))
    }
    it should "parse MD=M+1" in {
        val code =
            """MD=M+1
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(MD, MPlusOne, Null))
    }
    it should "parse MD=D-1" in {
        val code =
            """MD=D-1
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(MD, DMinusOne, Null))
    }
    it should "parse AD=A-1" in {
        val code =
            """AD=A-1
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AD, AMinusOne, Null))
    }
    it should "parse AD=M-1" in {
        val code =
            """AD=M-1
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AD,MMinusOne, Null))
    }
    it should "parse AD=D+A" in {
        val code =
            """AD=D+A
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AD,DPlusA, Null))
    }
    it should "parse AD=D+M" in {
        val code =
            """AD=D+M
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AD,DPlusM, Null))
    }
    it should "parse AM=D-A" in {
        val code =
            """AM=D-A
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AM,DMinusA, Null))
    }
    it should "parse AM=D-M" in {
        val code =
            """AM=D-M
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AM,DMinusM, Null))
    }
    it should "parse AM=A-D" in {
        val code =
            """AM=A-D
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AM,AMinusD, Null))
    }
    it should "parse AM=M-D" in {
        val code =
            """AM=M-D
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AM,MMinusD, Null))
    }
    it should "parse AMD=D&M" in {
        val code =
            """AMD=D&M
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AMD,DAndM, Null))
    }
    it should "parse AMD=D&A" in {
        val code =
            """AMD=D&A
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AMD,DAndA, Null))
    }
    it should "parse AMD=D|M" in {
        val code =
            """AMD=D|M
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AMD,DOrM, Null))
    }
    it should "parse AMD=D|A" in {
        val code =
            """AMD=D|A
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(AMD,DOrA, Null))
    }
    it should "parse 0;JGT" in {
        val code =
            """0;JGT
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null,Number(0), Jgt))
    }
    it should "parse 1;JGT" in {
        val code =
            """1;JGT
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null,Number(1), Jgt))
    }
    it should "parse -1;JGT" in {
        val code =
            """-1;JGT
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null,MinusOne, Jgt))
    }
    it should "parse A;JGT" in {
        val code =
            """A;JGT
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, A, Jgt))
    }
    it should "parse M;JEQ" in {
        val code =
            """M;JEQ
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, M, Jeq))
    }
    it should "parse D;JEQ" in {
        val code =
            """D;JEQ
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, D, Jeq))
    }
    it should "parse !D;JEQ" in {
        val code =
            """!D;JEQ
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, BangD, Jeq))
    }
    it should "parse !A;JEQ" in {
        val code =
            """!A;JEQ
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, BangA, Jeq))
    }
    it should "parse !M;JGE" in {
        val code =
            """!M;JGE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, BangM, Jge))
    }
    it should "parse -D;JGE" in {
        val code =
            """-D;JGE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, MinusD, Jge))
    }
    it should "parse -M;JGE" in {
        val code =
            """-M;JGE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, MinusM, Jge))
    }
    it should "parse -A;JGE" in {
        val code =
            """-A;JGE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, MinusA, Jge))
    }
    it should "parse D+1;JLT" in {
        val code =
            """D+1;JLT
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DPlusOne, Jlt))
    }
    it should "parse M+1;JLT" in {
        val code =
            """M+1;JLT
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, MPlusOne, Jlt))
    }
    it should "parse A+1;JLT" in {
        val code =
            """A+1;JLT
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, APlusOne, Jlt))
    }
    it should "parse D-1;JLT" in {
        val code =
            """D-1;JLT
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DMinusOne, Jlt))
    }
    it should "parse M-1;JNE" in {
        val code =
            """M-1;JNE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, MMinusOne, Jne))
    }
    it should "parse A-1;JNE" in {
        val code =
            """A-1;JNE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, AMinusOne, Jne))
    }
    it should "parse D+A;JNE" in {
        val code =
            """D+A;JNE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DPlusA, Jne))
    }
    it should "parse D+M;JNE" in {
        val code =
            """D+M;JNE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DPlusM, Jne))
    }
    it should "parse D-A;JLE" in {
        val code =
            """D-A;JLE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DMinusA, Jle))
    }
    it should "parse D-M;JLE" in {
        val code =
            """D-M;JLE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DMinusM, Jle))
    }
    it should "parse A-D;JLE" in {
        val code =
            """A-D;JLE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, AMinusD, Jle))
    }
    it should "parse M-D;JLE" in {
        val code =
            """M-D;JLE
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, MMinusD, Jle))
    }
    it should "parse D&A;JMP" in {
        val code =
            """D&A;JMP
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DAndA, Jmp))
    }
    it should "parse D&M;JMP" in {
        val code =
            """D&M;JMP
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DAndM, Jmp))
    }
    it should "parse D|A;JMP" in {
        val code =
            """D|A;JMP
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DOrA, Jmp))
    }
    it should "parse D|M;JMP" in {
        val code =
            """D|M;JMP
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)
        psr.parseOne().ty shouldBe(CCmd(Null, DOrM, Jmp))
    }
}

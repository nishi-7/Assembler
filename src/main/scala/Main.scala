
import assembler._
import error.SyntaxError

object Main {
    def main(args: Array[String]): Unit = {

        val code =
            """A;JGT
              |(xxx)
              |@1234
              |@label
              |M=1
              |0;JMP
              |D;JEQ
              |""".stripMargin
        val lex = new Lexer(code)
        val psr = new Parser(lex)

        while(!psr.isEnd()) {
            val cmd = try {
                psr.parseOne()
            } catch {
                case e: SyntaxError => {
                    println(e.errorReport(code))
                    return
                }
            }
            println(cmd)
        }
    }
}
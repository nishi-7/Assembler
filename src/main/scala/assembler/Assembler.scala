package assembler

import information.Info
import assembler.Util._

class Assembler(code: String) {

  def assemble(): (Seq[String], Info) = {
    val asms0 = parse(code)
    val asms1 = translate2DummyACmd(asms0)
    val table = makeSymbolTable(asms1)
    val asms2 = resolveDummyACmd(16, asms1, table)
    val asms3 = removeLabel(asms2)
    codeGen(asms3, table)
  }

  def parse(code: String): Seq[Asm] = {
    val lex = new Lexer(code)
    val psr = new Parser(lex)
    psr.parse()
  }

  def translate2DummyACmd(asms: Seq[Asm]): Seq[Asm] = {
    var ret: Seq[Asm] = Seq.empty

    for (asm <- asms) {
      asm.ty match {
        case ACmdLabel(label) => {
          ret = Asm(DummyACmd(label), asm.loc) +: ret
        }
        case _ => {
          ret = asm +: ret
        }
      }
    }
    ret.reverse
  }

  def makeSymbolTable(asms: Seq[Asm]): Map[String, Int] = {
    val mst = new SymbolTable(asms)
    mst.makeSymbolTable()
  }

  def resolveDummyACmd(
      width: Int,
      asms: Seq[Asm],
      table: Map[String, Int]
  ): Seq[Asm] = {
    // !(2 ** 16 - 1 - X)

    var ret: Seq[Asm] = Seq.empty
    for (asm <- asms) {
      val loc = asm.loc
      asm.ty match {
        case DummyACmd(Symbol(s)) => {
          table.get(s) match {
            case Some(value) => {
              // value > 1**15 - 1
              if (value > (1 << (width - 1)) - 1) {
                checkWidthOfData(16, value, Symbol(s), loc)

                // @(2 ** 16 - 1 - X)
                // A = !A
                val addr = (1 << width) - 1 - value
                ret = Asm(ACmd(Number(addr)), loc) +: ret
                ret = Asm(CCmd(A, BangA, Null), loc) +: ret
              } else {
                ret = Asm(ACmd(Number(value)), loc) +: ret
                ret = Asm(CCmd(A, A, Null), loc) +: ret
              }
            }

            case None => {
              ret = Asm(ACmdLabel(Symbol(s)), loc) +: ret
              ret = Asm(CCmd(A, A, Null), loc) +: ret
            }
          }
        }
        case _ => {
          ret = asm +: ret
        }
      }
    }

    ret.reverse
  }

  def removeLabel(asms: Seq[Asm]): Seq[Asm] = {
    asms.filter(asm =>
      asm.ty match {
        case Label(label) => false
        case _            => true
      }
    )
  }

  def codeGen(asms: Seq[Asm], table: Map[String, Int]): (Seq[String], Info) = {
    val cwr = new CodeWriter(asms, table)
    cwr.codeGen()
  }
}

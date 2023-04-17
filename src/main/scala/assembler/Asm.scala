package assembler

case class Asm(ty: AsmType, loc: Loc)

abstract class AsmType
case class ACmd(addr: TokenType) extends AsmType
case class ACmdLabel(label: TokenType) extends AsmType
case class DummyACmd(label: TokenType) extends AsmType
case class CCmd(dest: TokenType, comp: TokenType, jump: TokenType)
    extends AsmType
case class Label(label: String) extends AsmType

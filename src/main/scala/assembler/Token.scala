package assembler

// Token
sealed case class Token(var ty: TokenType, var loc: Loc)

// TokenType
abstract class TokenType
// illegal character
case class Illegal(ch: Char) extends TokenType
// Eof
case object Eof extends TokenType
// '\n'
case object NewLine extends TokenType
// ","
case object Comma extends TokenType
// ";
case object SemiColon extends TokenType
// "("
case object LParen extends TokenType
// ")"
case object RParen extends TokenType

/* A命令 */
// "@"
case object At extends TokenType
// "[a-zA-Z0-9]+
case class Symbol(s: String) extends TokenType
// [0-9]+
case class Number(i: Int) extends TokenType

/* C命令 */
// "A"
case object A extends TokenType
// "D"
case object D extends TokenType
// "M"
case object M extends TokenType
// "AM"
case object AM extends TokenType
// "AD"
case object AD extends TokenType
// "MD"
case object MD extends TokenType
// "AMD"
case object AMD extends TokenType

/* 演算子 */
// "+"
case object Plus extends TokenType
// "-"
case object Minus extends TokenType
// "&"
case object And extends TokenType
// "|"
case object Or extends TokenType
// "!"
case object Not extends TokenType
// "="
case object Eq extends TokenType

/* ジャンプ命令 */
// "jgt"
case object Jgt extends TokenType
// "jeq"
case object Jeq extends TokenType
// "jge"
case object Jge extends TokenType
// "jlt"
case object Jlt extends TokenType
// "jne"
case object Jne extends TokenType
// "jle"
case object Jle extends TokenType
// "jmp"
case object Jmp extends TokenType

/* 定義済みシンボル */
case object Sp extends TokenType
case object Lcl extends TokenType
case object Arg extends TokenType
case object This extends TokenType
case object That extends TokenType
case object R0 extends TokenType
case object R1 extends TokenType
case object R2 extends TokenType
case object R3 extends TokenType
case object R4 extends TokenType
case object R5 extends TokenType
case object R6 extends TokenType
case object R7 extends TokenType
case object R8 extends TokenType
case object R9 extends TokenType
case object R10 extends TokenType
case object R11 extends TokenType
case object R12 extends TokenType
case object R13 extends TokenType
case object R14 extends TokenType
case object R15 extends TokenType
case object Screen extends TokenType
case object KeyBoard extends TokenType

case object UartRx1 extends TokenType // Player1
case object UartRx2 extends TokenType // Player2
case object Spi extends TokenType // LCD monitor
case object LED7Seg extends TokenType // 7 segment LED

// ニーモニック
// "null"
case object Null extends TokenType
// "-1"
case object MinusOne extends TokenType
// "!A"
case object BangA extends TokenType
// "!D"
case object BangD extends TokenType
// "!M"
case object BangM extends TokenType
// "-A"
case object MinusA extends TokenType
// "-D"
case object MinusD extends TokenType
// "-M"
case object MinusM extends TokenType
// "A+1"
case object APlusOne extends TokenType
// "D+1"
case object DPlusOne extends TokenType
// "M+1"
case object MPlusOne extends TokenType
// "A-1"
case object AMinusOne extends TokenType
// "D-1"
case object DMinusOne extends TokenType
// "M-1"
case object MMinusOne extends TokenType
// "D+A"
case object DPlusA extends TokenType
// "D+M"
case object DPlusM extends TokenType
// "D-A"
case object DMinusA extends TokenType
// "D-M"
case object DMinusM extends TokenType
// "A-D"
case object AMinusD extends TokenType
// "M-D"
case object MMinusD extends TokenType
// "D&A"
case object DAndA extends TokenType
// "D&M"
case object DAndM extends TokenType
// "D|A"
case object DOrA extends TokenType
// "D|M"
case object DOrM extends TokenType

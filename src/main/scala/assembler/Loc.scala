package assembler

// Tokenの位置を表すクラス
// """line: 10
//    @001xx
//     ^^^^^
//   error: expected identifier, but got illegal token"""
case class Loc(line: Int, left: Int, len: Int)

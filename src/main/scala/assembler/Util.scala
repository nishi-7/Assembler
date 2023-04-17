package assembler

import error.SyntaxError

object Util {
  /* Utility functions for errors */
  def throwTokenError(got: Token) = {
    throw new SyntaxError(s"illegal token, ${got.ty}", got.loc)
  }

  def throwSyntaxError(expect: TokenType, got: Token) = {
    throw new SyntaxError(
      s"expected ${expect} token, but got ${got.ty}",
      got.loc
    )
  }

  def throwError(msg: String, got: Token) = {
    throw new SyntaxError(s"expected ${msg} token, but got ${got.ty}", got.loc)
  }

  def throwCodeGenError(msg: String, got: String, loc: Loc) = {
    throw new SyntaxError(
      s"Code generate: expected ${msg}, but got ${got}",
      loc
    )
  }

  def throwOutOfWidthError(n: Int, upper: Int, symbol: String, loc: Loc) = {
    throw new SyntaxError(
      s"${n} is out of width ${upper}, the symbol is ${symbol}",
      loc
    )
  }

  def checkWidthOfData(width: Int, n: Int, tt: TokenType, loc: Loc) = {
    val upper = (1 << width) - 1
    if (n > upper) {
      throwOutOfWidthError(
        n,
        upper,
        tt.toString(),
        loc
      )
    }
  }
}

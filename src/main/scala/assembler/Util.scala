package assembler

import error.SyntaxError

object Util {
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
}

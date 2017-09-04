package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues(se => Signal(eval(se(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def inner(expr: Expr)(implicit visited: Set[String]): Double = expr match {
      case Literal(v) => v
      case Ref(name) if visited contains name => Double.NaN
      case Ref(name) => inner(getReferenceExpr(name, references))(visited + name)
      case Plus(a, b) => inner(a) + inner(b)
      case Minus(a, b) => inner(a) - inner(b)
      case Times(a, b) => inner(a) * inner(b)
      case Divide(a, b) => inner(a) / inner(b)
    }

    inner(expr)(Set())
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}

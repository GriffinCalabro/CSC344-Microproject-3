import scala.util.matching.Regex

// Unambiguous grammar
// S -> E$
// E -> T + E | T
// T -> Const | Var

// Easier to parse grammar
// S -> E$
// E -> Terminal E2
// E2 -> + E
// E2 -> NIL
// Terminal -> Const
// Terminal -> Var

// Grammar to be implemented
// Change to E -> T E2 ???
// T -> Terminal T2
// T2 -> * T
// T2 -> NIL

abstract class S{
  def eval(env: Main.Environment): Int
}
abstract class Terminal extends S
case class E(l: T, r: Option[E2]) extends S{
  def eval(env: Main.Environment): Int = l.eval(env)
}
case class E2(l:E) extends S{
  def eval(env: Main.Environment): Int = l.eval(env)
}
case class T(l: Terminal, r: Option[T2]) extends S{
  def eval(env: Main.Environment): Int = {
    val leftside = l match {
      case v:Var => v.eval(env)
      case c:Const => c.eval(env)
    }
    r match {
      case Some(r) => leftside + r.eval(env)
      case None => leftside
    }
  }
}
case class T2(l:T) extends S{
  def eval(env: Main.Environment): Int = l.eval(env)
}
case class Var(n: String) extends Terminal{
  def eval(env: Main.Environment): Int = env(n)
}
case class Const(v: Int) extends Terminal{
  def eval(env: Main.Environment): Int = v
}

class RecursiveDescent(input: String) {
  val constregex: Regex = "^[0-9]+".r
  val varregex: Regex = "^[A-Za-z]+".r

  var index = 0
  def parseS(): S = parseE()
  def parseE(): E = E(parseT(), parseE2())
  def parseE2(): Option[E2] = {
    //check for +
    if (index < input.length && input(index) == '+') {
      //advance past +
      index += 1
      Some(E2(parseE()))
    }
    else None //NIL
  }
  def parseT(): T = T(parseTerminal(), parseT2())
  def parseT2(): Option[T2] = {
    //check for *
    if (index < input.length && input(index) == '*') {
      //advance past *
      index += 1
      Some(T2(parseT()))
    }
    else None //NIL
  }

  def parseTerminal(): Terminal = {
    //check if we have a Const or Var
    //get unparsed String
    val currString = input.substring(index)

    //check if we have a const
    val consts = constregex.findAllIn(currString)
    if (consts.hasNext) {
      val const: String = consts.next()
      index += const.length()
      Const(const.toInt)
    }
    else {
      val vars = varregex.findAllIn(currString)
      val varname = vars.next()
      index += varname.length()
      Var(varname)
    }
  }
}

object Main {
  type Environment = String => Int

  def main(args: Array[String]): Unit = {
    val env: Environment = {
      case "x" => 5
      case "y" => 7
    }

    val rd = new RecursiveDescent("x+x+7+y") //5 + 5 + 7 + 7 = 24
    val exp2rd:S = rd.parseE()
    println(exp2rd)
    println(exp2rd.eval(env))
  }
}
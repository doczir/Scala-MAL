package hu.lexan.mal.ast

object AstPrinter {
  private def unescape(c: Char): String = c match {
    case '\n' => "\\n"
    case '\\' => "\\\\"
    case '"' => "\\\""
    case anychar => anychar.toString
  }

  def print(expr: MalExpr, readable: Boolean = true): String = expr match {
    case MSymbol(name) => name
    case MKeyword(kw) => s":$kw"
    case MString(str) => if (readable) s""""${str.flatMap(unescape)}"""" else str
    case MInteger(value) => value.toString
    case MList(nodes) => s"(${
      nodes.map {
        print(_, readable)
      }.mkString(" ")
    })"
    case MVector(nodes) => s"[${
      nodes.map {
        print(_, readable)
      }.mkString(" ")
    }]"
    case MMap(elems) => s"{${
      elems.map { case (key, value) => s"${print(key)} ${print(value)}" }.mkString(" ")
    }}"
    case MNil => "nil"
    case MTrue => "true"
    case MFalse => "false"
    case _: MFunction => "#<function>"
  }
}

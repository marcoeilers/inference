package inference.marco

import viper.silver.ast

object Extension {
  val relevantDomainFuncNames: Map[String, ast.Type] = Map("ShStructget0of2<Ref>" -> ast.Ref, "ShStructget1of2<Ref>" -> ast.Ref)
  def domainFuncAppTypeMap(funcName: String, typ: ast.Type): Map[ast.TypeVar, ast.Type] = {
    if (funcName == "ShStructget0of2") {
      Map(ast.TypeVar("T0") -> typ, ast.TypeVar("T1") -> typ)
    } else if (funcName == "ShStructget1of2") {
      Map(ast.TypeVar("T0") -> typ, ast.TypeVar("T1") -> typ)
    } else {
      Map.empty
    }
  }

}

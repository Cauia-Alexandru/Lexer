import scala.collection.mutable.{ListBuffer, Stack}

class AST (var right: AST, var left: AST, var operation: String){

  //functie care verifica daca un nod e atom
  def isAtom(): Boolean = {
    operation != "CONCAT" && operation != "UNION" && operation != "STAR" && operation != "void" && operation != "eps"
  }

  //verific daca nodul e plin
  def ASTisFull(): Boolean = {
    if (operation == "UNION") {
      if (right != null && left != null)
        return true
    }
    else if (operation == "CONCAT") {
      if (right != null && left != null)
        return true
    }
    else if (operation == "STAR") {
      if (left != null)
        return true
    }
    else if (operation != "UNION" && operation != "CONCAT" && operation != "STAR")
      return true

    false
  }

}

object AST {
//comprim toate ast-urile din stiva in unul
  //recursia se termina cand a ramas un fullAst in stiva si il returnez

  def verifyStack(stack: Stack[AST]): AST = {
    val AST = stack.pop()
    if (AST.ASTisFull() && stack.isEmpty) {
      stack.push(AST)
      return AST
    } else if (AST.ASTisFull()) {
      var ASTparent = stack.pop()
      if (ASTparent.left == null) {
        ASTparent.left = AST
        stack.push(ASTparent)
      }
      else {
        ASTparent.right = AST
        stack.push(ASTparent)
      }
    }
    else {
      stack.push(AST)
      return AST
    }

    verifyStack(stack)
  }

  def mySplit(str: String): ListBuffer[String] = {
    var word: String = new String()
    var toReturn: ListBuffer[String] = new ListBuffer[String]()
    var i = 0
    if(str.length == 1){
      toReturn += str
      return toReturn
    }

    while(i < str.length){
      if(str(i) == ' ' && word.nonEmpty){
        toReturn += word
        word = new String()
      }
      else if(str(i) == '\''){
        word += str(i + 1)
        i += 1

        if(i == str.length - 2){
          toReturn += word
          return toReturn
        }

        if(str(i + 1) != '\''){
          word += str(i + 1)
          i += 1
        }
        toReturn += word
        word = new String()
        i += 1
      }
      else if(str(i) != '\''&& str(i) != ' '){
        word += str(i)
      }

      if(i == str.length - 1)
        toReturn += word

      i += 1
    }
    toReturn
  }

  //operatiile PLUS si MAYBE le tratez aparte
  //pun pe stiva operatiile si alfabetul apo refac stiva cu functia de mai sus
  def createAST(str: String): AST = {
    val splitPrenex = mySplit(str)
    var stack = Stack[AST]()
    var i = 0
    while (i < splitPrenex.length) {
      if (splitPrenex(i) == "PLUS") {
        var AST = new AST(null, null, "CONCAT")
        var ASTatom = new AST(null, null, splitPrenex(i + 1))
        AST.left = ASTatom
        stack.push(AST)
        var AST2 = new AST(null, null, "STAR")
        stack.push(AST2)
        var ASTatom2 = new AST(null, null, splitPrenex(i + 1))
        stack.push(ASTatom2)
        i += 2
      }
      else if (splitPrenex(i) == "MAYBE") {
        var AST = new AST(null, null, "UNION")
        var ASTeps = new AST(null, null, "eps")
        AST.left = ASTeps
        stack.push(AST)
        AST = new AST(null, null, splitPrenex(i + 1))
        stack.push(AST)
        i += 2
      }
      else {
        var AST = new AST(null, null, splitPrenex(i))
        stack.push(AST)
        i += 1
      }
      verifyStack(stack)
    }
    stack.pop
  }

  def main(args: Array[String]): Unit = {
    var ast = new AST(null, null, null)
    var res = mySplit("CONCAT ' ' 'a'")
    println(res)
  }
}



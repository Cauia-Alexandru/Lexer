import scala.collection.mutable

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  //def preprocess(s:List[Char]): List[Either[Char,Char]] = {}

  // This function should construct a prenex expression out of a normal one.
  def priority(char: Char): Int = {
    if(char == '(' || char == ')')
      return 0
    else if(char == '*' || char == '+' || char == '?')
      return  3
    else if(char == '~')
      return 2
    else
      return 1

  }

  //proceses stringul ca sa obtin infixul in forma de care am nevoie
  //am grija si sa escapez caracterul special
  def preprocess(str: String): String = {
    var newStr: String = new String()
    var alphabet: String = new String()
    alphabet ++= "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\'@"
    val operators = "*?+"
    var i = 0
    if(str == "eps") return str
    while(i < str.length){
      var nextIndex = i + 1
      if (str(i) == '\'') {
        nextIndex = i + 3
        if (nextIndex == str.length) {
          newStr += str(i)
          newStr += str(i + 1)
          newStr += str(i + 2)
          i = str.length
        }
      }

      if (i < str.length) {
        if (i == str.length - 1) {
          newStr += str(i)
        }
          //daca am 2 litere, pun concat intre ele
        else if(alphabet.contains(str(i)) && alphabet.contains(str(nextIndex))){
          newStr += str(i)
          if (str(i) == '\'') {
            newStr += str(i + 1)
            newStr += str(i + 2)
            i += 2
          }
          newStr += '~'
        }
          //intre ) si litera pun concat
        else if(str(i) == ')' && alphabet.contains(str(nextIndex))){
          newStr += ')'
          newStr += '~'
        }
          //intre litera si (
        else if(alphabet.contains(str(i)) && str(nextIndex) == '('){
          newStr += str(i)
          if (str(i) == '\'') {
            newStr += str(i + 1)
            newStr += str(i + 2)
            i += 2
          }
          newStr += '~'
        }
          //intre )(
        else if(str(i) == ')' && str(nextIndex) == '('){
          newStr += ')'
          newStr += '~'
        }
          //intre operator si litera sau (
        else if(operators.contains(str(i)) && (alphabet.contains(str(nextIndex)) || str(nextIndex) == '(')){
          newStr += str(i)
          newStr += '~'
        }
          //union intre toate elementele dintre []
        else if(str(i) == '['){
          newStr += '('
          for(num <- str(i + 1) to str(i + 3)){
            newStr += num
            newStr += '|'
          }
          newStr = newStr.substring(0, newStr.length - 1)
          newStr += ')'
          i += 4
        }
        else {
          newStr += str(i)
          if (str(i) == '\'') {
            newStr += str(i + 1)
            newStr += str(i + 2)
            i += 2
          }
        }
      }
      i += 1
    }
    return newStr
  }

  def tranformPrefix(str: String): String = {
    var prefix = new String()
    var i = 0
    if(str == "eps") return str
    while (i < str.length) {
      val character = str(i)
      if (character == '*') {
        prefix ++= "STAR "
      }
      else if (character == '|') {
        prefix ++= "UNION "
      }
      else if (character == '~') {
        prefix ++= "CONCAT "
      }
      else if (character == '+') {
        prefix ++= "PLUS "
      }
      else if (character == '?') {
        prefix ++= "MAYBE "
      }
      else {
        if (character == '\'') {
          prefix += character
          prefix += str(i + 1)
          prefix += str(i + 2)
          prefix += " "
          i += 2
        } else {
          prefix ++= (character + " ")
        }
      }

      i += 1
    }

    prefix.take(prefix.length - 1)
  }

  def toPrenex(str: String): String = {
    var alphabet: String = new String()

    alphabet ++= "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\'@"
    val operators = "+*|~?"
    var prefix = new String()
    var stack = mutable.Stack[Char]()
    var infix = preprocess(str)
    infix = infix.reverse
    var i = 0
    while(i < infix.length){
      if(alphabet.contains(infix(i))){
        prefix += infix(i)
        if (infix(i) == '\'') {
          prefix += infix(i + 1)
          prefix += infix(i + 2)
          i += 2
        }
      }
      else if(infix(i) == ')'){
        stack.push(infix(i))
      }
      else if(infix(i) == '('){
        while(stack.top != ')'){
          prefix += stack.pop()
        }
        stack.pop()
      }
      else if(operators.contains(infix(i))){
        if(stack.isEmpty){
          stack.push(infix(i))
        }
        else{
          if(priority(infix(i)) > priority(stack.top))
            stack.push(infix(i))
          else if(priority(infix(i)) == priority(stack.top)){
            stack.push(infix(i))
          }
          else if(priority(infix(i)) < priority(stack.top)){
            while(stack.nonEmpty && priority(infix(i)) < priority(stack.top)){
              prefix += stack.pop()
            }
            stack.push(infix(i))
          }
        }
      }
      i += 1
    }
    while(stack.nonEmpty){
      prefix += stack.pop()
    }
    prefix = prefix.reverse

    tranformPrefix(prefix)
  }



  def main(args: Array[String]): Unit = {
    var d = preprocess("eps")
    val c = Dfa.fromPrenex(d)
    printf(d)
  }
}

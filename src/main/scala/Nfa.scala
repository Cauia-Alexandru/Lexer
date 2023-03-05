
class Nfa[A](val startState: A, val endState: A, val transitions: Map[A, Set[(A, String)]]) {

  //functia f va fi aplicata pe int-ul din tuplu interior si pe setul de int-uri din al 2 tuplu
  def map[B](f: A => B) : Nfa[B] = {
    new Nfa[B](
      f(startState),
      f(endState),
      transitions.map(x => Tuple2(f(x._1), x._2.map(y => Tuple2(f(y._1), y._2))))
    )
  }



  def next(state:A, c: Char): Set[A] = {
    myNext(state, "" + c)
  }

//aflu next states sarind peste eps, adica daca vad eps, apelez recursiv
  //si continui, altfel daca gasesc stringul, adaug starea in rezultat
  def myNext(state:A, c: String): Set[A] = {
    var visited = Set[A]()
    var res = Set[A]()

    def next(state: A, str: String): Set[A] = {
      if (!visited.contains(state)) {
        visited += state
        var nextStates: Set[(A, String)] = Set()
        nextStates = transitions(state)

        nextStates foreach {
          case (nextS, str1) => {

            if (str1.equalsIgnoreCase("eps") && !isFinal(nextS)) {
              next(nextS, str)
            }
            else if (str1 == str) {
              res += nextS
            }
          }
        }
        res
      } else {
        res
      }

    }
    next(state, c)
  }
  //aproape la fel cu myNext, numai ca adaug in rezultat starile de pe eps
  //si merg pe eps cat de mult pot
  def nextEps(state: A, c: String): Set[A] = {
    var visited = Set[A]()
    var res = Set[A]()

    def next(state: A, str: String): Set[A] = {
      if (!visited.contains(state)) {
        visited += state
        var nextStates: Set[(A, String)] = Set()
        nextStates = transitions(state)

        nextStates foreach {
          case (nextS, str1) => {
            if (!transitions.contains(nextS) && str1 == "eps") {
              res += nextS
              return res
            }
            if (str1 == "eps") {
              next(nextS, str)
            }
          }
        }
        res
      } else {
        res
      }
    }
    next(state, c)
  }

  def aux(states: Set[A], charac: Char): Set[A] = {
    var currentStates: Set[A] = Set()

    for(state <- states){
      if(!isFinal(state))
      currentStates ++= next(state, charac)
    }
    currentStates
  }

  /**
   *
   * @param str
   * @return daca accept
   * daca str e gol merg cat pot pe eps, daca ajung in stare finala, accept
   *pentru fiecare caracter stochez starile in care pot ajunge, apoi
   * cu nextEps merg cat pot pe eps si la urma verific daca e stare finala
   * daca da, intorc true
   */
  def accepts(str: String): Boolean = {
    if(transitions.isEmpty)
      return false

    var currentStates: Set[A] = Set()
    if(str.isEmpty) {
      val statesByEps = nextEps(startState, "eps")
      for(state <- statesByEps) {
        if (isFinal(state)) {
          return true
        }
      }
      return false
    }

    currentStates = next(startState, str(0))

    for(c <- str.tail){
      currentStates = aux(currentStates, c)
    }
    var currentStates2: Set[A] = Set()
    for(state <- currentStates){
      if(isFinal(state)) {
        return true
      }
      currentStates2 ++= nextEps(state, "eps")
    }
    for(state <- currentStates2){
      if(isFinal(state))
        return true
    }
    false
  }

  def getStates: Set[A] = {
    transitions.map(x => x._2.map(x => x._1) + x._1).reduce((x, y) => x ++ y)
  }

  def isFinal(state: A): Boolean = {
    state == endState
  }

}


// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class


object Nfa { // TODO implement Prenex -> Nfa transformation.
  def isAtom(ast: AST): Boolean = {
    ast.operation != "CONCAT" && ast.operation != "UNION" && ast.operation != "STAR" && ast.operation != "void"
  }

  def fromPrenex(str: String): Nfa[Int] = {
    var ast = new AST(null, null, null)
    ast = AST.createAST(str)

    def voidNfa(): Nfa[Int] = {
      new Nfa[Int](0, 1, Map())
    }

    def epsNfa(): Nfa[Int] = {
      new Nfa[Int](0, 0, Map())
    }

    /**
     *
     * @param ast
     * @param startPos
     * @return nfa
     * daca e atom sunt 2 stari cu o conexiune intre ele si returnez
     * pentru fiecare operatie apelez recursiv pe copii si fac tranzitiile, returnez nfa nou construit
     */
    def toNFA(ast: AST, startPos: Int): Nfa[Int] = {
      var connect: Map[Int, Set[(Int, String)]] = Map()
      if (isAtom(ast)) {
        connect += Tuple2(startPos, Set(Tuple2(startPos + 1, ast.operation)))
        return new Nfa[Int](startPos, startPos + 1, connect)
      }
      else {
        if (ast.operation == "CONCAT") {
          var left = toNFA(ast.left, startPos)
          var right = toNFA(ast.right, left.endState + 1)
          var transitions: Set[(Int, String)] = Set()
          transitions += Tuple2(right.startState, "eps")
          connect += Tuple2(left.endState, transitions)
          connect ++= left.transitions
          connect ++= right.transitions
          return new Nfa[Int](startPos, right.endState, connect)

        }
        if(ast.operation.equals("UNION")) {
          var upper = toNFA(ast.left, startPos + 1)
          var lower = toNFA(ast.right, upper.endState + 1)
          var transitions: Set[(Int, String)] = Set()
          // start state -> to each nfa
          transitions += Tuple2(upper.startState, "eps")
          transitions += Tuple2(lower.startState, "eps")
          connect += Tuple2(startPos, transitions)
          // merge two maps
          connect ++= upper.transitions
          connect ++= lower.transitions
          // each nfa to final state
          transitions = Set()
          transitions += Tuple2(lower.endState + 1, "eps")

          connect += Tuple2(lower.endState, transitions)
          connect += Tuple2(upper.endState, transitions)
          new Nfa[Int](startPos, lower.endState + 1, connect)
        }else if(ast.operation == "STAR"){
          var child = toNFA(ast.left, startPos + 1)
          var transitions: Set[(Int, String)] = Set()
          transitions += Tuple2(child.startState, "eps")
          transitions += Tuple2(child.endState + 1, "eps")
          connect += Tuple2(startPos, transitions)

          transitions = Set()
          transitions += Tuple2(child.endState + 1, "eps")

          transitions += Tuple2(child.startState, "eps")
          connect += Tuple2(child.endState, transitions)
          connect ++= child.transitions
          new Nfa[Int](startPos, child.endState + 1, connect)
        }
        else if ("eps" == ast.operation) {
          return epsNfa()
        }
        else {
          return voidNfa()
        }

      }
    }
    return toNFA(ast, 0)
  }

  def main(args: Array[String]): Unit = {
    val nfa: Nfa[Int] = fromPrenex("STAR a")
    var myStates = nfa.accepts("aaa")
    val st = nfa.getStates


  }
}






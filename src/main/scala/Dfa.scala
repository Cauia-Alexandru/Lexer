import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Dfa[A] (var initialState: A, var finalStates: Set[A], var transitions: Map[A, Set[(A, String)]], var sinkState: A){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  //aplic f starea initiala si pe starile finale
  //iau toate cheile din mapa si pentru toate tranzitiile din fiecare nod
  //aplic f pe ._1 din tuplu, adica pe A
  def map[B](f: A => B) : Dfa[B] = {
    val initialSt = f(initialState)
    var finalSts = Set[B]()
    for (elem <- finalStates) {
      finalSts += f(elem)
    }
    val sinkSt = f(sinkState)
    var tr = Map[B, Set[(B, String)]]()
    val keys = transitions.keySet
    for (key <- keys) {
      var bTransitionsForThisKey = Set[(B, String)]()
      val aTransitionsForThisKey = transitions(key)
      for (aTransition <- aTransitionsForThisKey) {
        bTransitionsForThisKey += Tuple2(f(aTransition._1), aTransition._2)
      }
      tr += (f(key) -> bTransitionsForThisKey)
    }

    new Dfa[B](initialSt, finalSts, tr, sinkSt)
  }

  //verific daca e sink si daca se afla prin starile vecine
  //stochez tranzitiile de pe state, si pentru fiecare verific c
  //daca e acel care il caut il pun in toReturn
  def next(state:A, c: Char): A = {
    if (state == sinkState) {
      return sinkState
    }
    if(!transitions.keySet.contains(state)) {
      return sinkState
    }

    val statesToVerify = transitions(state)
    var toReturn = Set[A]()

    for(currSt <- statesToVerify) {
      if (currSt._2 == c.toString) {
        toReturn = Set(currSt._1)
      }
    }

    if (toReturn.nonEmpty) {
      toReturn.head
    } else {
      sinkState
    }
  }

  //pentru fiecare caracter din str apelez next
  //la sfarsit verific daca e finala starea currentState
  def accepts(str: String): Boolean = {
    var currentState = Set[A](initialState)
    for(s <- str) {
      currentState = Set(next(currentState.head, s))
    }
    if (isFinal(currentState.head)) {
      true
    } else {
      false
    }
  }


  def getStates : Set[A] = {
    var toReturn = Set[A]()
    val statesToGoThrough = transitions.keySet
    toReturn = statesToGoThrough
    for (stateToCheck <- statesToGoThrough) {
      for (adjacent <- transitions(stateToCheck)) {
        toReturn += adjacent._1
      }
    }
    toReturn
  }


  def isFinal(state: A): Boolean = {
    finalStates.contains(state)
  }


}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {

  //returnez starile care au intre ele eps
  //apelez recursiv pana pana ajung la starea finala sau pana cand
  //starea nu se afla in mapa
  //cand gasesc eps adaug starea in rezultat
  def epsilonClosure(state: Int, nfa: Nfa[Int]): Set[Int] = {
    var visited = Set[Int]()
    var res = Set[Int]()

    def epsilon(state: Int): Set[Int] = {
      if (!visited.contains(state)) {
        visited += state
        var nextStates: Set[(Int, String)] = Set()
        if(nfa.isFinal(state))
          return res

        if(!nfa.transitions.keySet.contains(state))
          return res
        nextStates = nfa.transitions(state)

        nextStates foreach {
          case (nextS, str) => {
            if (str.equalsIgnoreCase("eps")) {
              res += nextS
              epsilon(nextS)
            }
            else
              return res
          }
        }
        res
      }
      else
        res
    }
    res += state
    epsilon(state)
  }


  /**
   *
   * @param nfa
   * @param alphabet
   * @return dfa
   *cu epsilonClosure aflu starile initiale
   * am un set de visited pentru a evita buclele
   * dfa se construieste in 3 for-uri unde parcurg fiecare str din alphabet,
   * fiecare stare din currentStates(in al 3 for intru daca state nu e final, ca sa aiba tranzitii)
   * si fiecare tranzitie din state(unde cu epsilonClosure grupez starile care au eps intre ele)
   * ls sfarsit fac legaturile si apelez recursiv pe starile noi grupate
   */
  def createDfa(nfa: Nfa[Int], alphabet: ListBuffer[String]): Dfa[Int] = {
    var startState = epsilonClosure(nfa.startState, nfa)
    var visited = Set[Set[Int]]()
    var dfa = new Dfa[Set[Int]](startState, Set[Set[Int]](), Map[Set[Int], Set[(Set[Int], String)]](), Set[Int](-100))
    if(startState.contains(nfa.endState)){
      dfa.finalStates += startState
    }
    def toDfa(currentState: Set[Int]): Dfa[Set[Int]] = {
      if (nfa.transitions.isEmpty)
        return dfa
      if(!visited.contains(currentState)) {
        visited += currentState
        for (str <- alphabet) {
          var newStates = Set[Int]()
          for (state <- currentState) {
            if (!nfa.isFinal(state)) {
            for (transition <- nfa.transitions(state)) {
              if (transition._2 == str) {
                newStates ++= epsilonClosure(transition._1, nfa)
                if (newStates.contains(nfa.endState)) {
                    dfa.finalStates += newStates
                }

              }
            }
          }
          }
          var arr: Set[(Set[Int], String)] = Set()
          if (dfa.transitions.contains(currentState))
            arr = dfa.transitions(currentState)
          if(newStates.nonEmpty)
            dfa.transitions += (currentState -> (arr + Tuple2(newStates, str)))
          toDfa(newStates)
        }
      }
      return dfa
    }
    dfa = toDfa(startState)
    //hash function
    dfa.map(x => (((x.max * 2) + x.head + x.size) * 3) * (x.min + 2 * x.last))
  }


  def fromPrenex(str: String): Dfa[Int] = {

    val nfa = Nfa.fromPrenex(str)
    val alphabet = AST.mySplit(str).filter(x => x != "CONCAT" && x != "UNION" && x != "STAR")
    var res = createDfa(nfa, alphabet)
    res
  } // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

  // You can add more methods to this object

  def main(args: Array[String]): Unit = {
    val s = "CONCAT ' ' 'a'"
    val s1 = Nfa.fromPrenex(s)
    val b = Dfa.fromPrenex(s)
    val c = b.accepts(" a")
    println (b)
  }
}



import scala.collection.mutable.Stack

import scala.collection.mutable.Stack

class Dfa[A] (states:Set[A], alphabet:Set[String], transitions:Set[(A, String, A)], init:A, end:Set[A]){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = { // TODO implement map
    new Dfa[B](states.map(x => f(x)), alphabet, transitions.map(x => (f(x._1),x._2,f(x._3))), f(init), end.map(x => f(x)));
  }

  def next(state:A, c: Char): A = { // TODO implement next
    var res = -1;
    for(e <- transitions)
      if(e._1 == state && e._2 == c.toString)
        res = e._3.asInstanceOf[Int];
    return res.asInstanceOf[A];
  }

  def accepts(str: String): Boolean = { // TODO implement accepts
    if (str == "")
      if (isFinal(init))
        return true;
      else
        return false;

    var str1 = str.replaceAll(" ","@");
    var mystate = next(init, str1.charAt(0));
    str1 = str1.substring(1);
    while (!str1.isEmpty && mystate != (-1).asInstanceOf[A]){
      mystate = next(mystate, str1.charAt(0));
      str1 = str1.substring(1);
    }

    isFinal(mystate);

  }

  def getStates : Set[A] = states // TODO implement getStates

  def isFinal(state: A): Boolean = end.contains(state)  // TODO implement isFinal
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {// TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

    var str2 = str;
    if (str2.contains("\' \'"))
      str2 = str2.replaceAll("\' \'", "@");

    if(str2 == "void") {
      new Dfa[Int](Set(1, 2), Set("void"), Set(), 1, Set(2));
    } else if(str2 == "eps") {
      new Dfa[Int](Set(1), Set("eps"), Set((1, "eps", 1)), 1, Set(1));
    } else if (str2.length == 1) {
      new Dfa[Int](Set(1), Set(str2), Set((1, str2, 2)), 1, Set(2));
    }  else {
      var nfa = Nfa.fromPrenex(str2);
      var aux = nfa.getStates;
      var new_states = Set[Set[Int]]();

      var new_alphabet = nfa.getAlphabet - "eps";
      var init = Set(nfa.getinit) ++ nfa.eps_closure(nfa.getinit);

      var new_transitions = Set[(Set[Int], String, Set[Int])]();
      var st = Stack[Set[Int]]();
      var added = Set[Set[Int]]();
      st.push(init);
      added += init;
      new_states += init;
      while (!st.isEmpty) {
        var aux = st.pop();
        var closure = Set[Int]();
        for (q <- aux)
          for (c <- new_alphabet) {
            closure = Set[Int]();

            for (t <- nfa.getTransitions)
              if (t._1 == q && t._2 == c.toString) {
                closure ++= nfa.eps_closure(t._3);
                closure += t._3;
              }

            if (!closure.isEmpty) {

              new_transitions += ((aux, c, closure));
              if (!added.contains(closure)) {
                st.push(closure);
                new_states += closure;
                added += closure;
              }

            }

          }
      }

      var finals = Set[Set[Int]]();
      for (e <- new_states)
        for (fin <- nfa.getfinals)
          if (e.contains(fin))
            finals += e

      var aux2 = Set[(Set[Int], String, Set[Int])]();
      for (e <- new_transitions)
        if (!e._1.isEmpty && !e._3.isEmpty) {
          aux2 += e;
        }

      new_transitions = aux2;

      var aux_list = new_states.toList;
      var final_states = Set[Int]();
      var final_ends = Set[Int]();
      var final_init = aux_list.indexOf(init);
      var final_transitions = Set[(Int, String, Int)]();

      for (e <- new_states)
        final_states += aux_list.indexOf(e);

      for (e <- finals)
        final_ends += aux_list.indexOf(e);

      for (e <- new_transitions)
        final_transitions += ((aux_list.indexOf(e._1), e._2, aux_list.indexOf(e._3)))


      new Dfa[Int](final_states, new_alphabet, final_transitions, final_init, final_ends)

    }


  }
  // You can add more methods to this object
}
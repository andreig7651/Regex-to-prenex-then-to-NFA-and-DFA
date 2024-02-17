import scala.collection.mutable.Stack

class Nfa[A](states:Set[A], alphabet:Set[String], transitions:Set[(A, String, A)], init:A, end:Set[A]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = { //new Nfa[Int] (states.map(f), alphabet, f(init), end.map(f)) // TODO implement map
    var new_states = states.map(x => f(x));
    var new_transitions = transitions.map(x => (f(x._1), x._2, f(x._3)));
    var new_init = f(init);
    var new_end = states.map(x => f(x));
    return new Nfa[B](new_states, alphabet,new_transitions, new_init, new_end);
  }

  def next_eps(state: A): Set[A] = {
    var myset = Set[Int]();
    for (e <- transitions)
      if (e._1 == state && e._2 == "eps")
        myset += e._3.asInstanceOf[Int];

    return myset.asInstanceOf[Set[A]];
  }

  def eps_closure(state: A): Set[A] = { // TODO implement next
    var res = Set[A]();
    var curr = Set(state);
    var next = Set[A]();
    while (!curr.equals(next)) {
      var aux = curr;
      curr = next;
      next = Set[A]();
      for (e <- aux) {
        next = next ++ next_eps(e)
        if(next_eps(e).isEmpty)
          res += e;
      }

    }
    return res;
  }

  def next(state: A, c: Char): Set[A] = { // TODO implement next
    var myset = Set[A]();
    var aux = eps_closure(state)
    for (e <- transitions)
      if (e._1 == state && e._2 == c.toString) {
        myset += e._3.asInstanceOf[A];
      }

    for (e <- aux) {
      for (e1 <- transitions) {
        if (e1._1 == e && e1._2 == c.toString) {
          myset ++= eps_closure(e1._3.asInstanceOf[A])

        }

      }
    }
    myset.asInstanceOf[Set[A]];
  }

  def final_next(state: A): Boolean = {
    for (e <- eps_closure(state))
      if (isFinal(e))
        return true;
    return false;
  }

  def accepts(str: String): Boolean = { // TODO implement accepts

    if (str == "")
      if (isFinal(init) || final_next(init))
        return true;
      else
        return false;

    var myset = next(init, str.charAt(0));;
    var str1 = str;

    var aux = Set[A]();
    str1 = str1.substring(1)

    while(!str1.isEmpty) {

      aux = Set[A]();
      for(e <- myset) {
        aux ++= next(e ,str1.charAt(0));
      }
      myset = aux;
      str1 = str1.substring(1);
    }

    for(e <- myset)
      if(isFinal(e))
        return true;

    return false;
  }

  def getStates : Set[A] = states // TODO implement getStates

  def getAlphabet : Set[String] = alphabet // TODO implement getStates
  def getTransitions : Set[(A, String, A)] = transitions // TODO implement getStates
  def getinit : A = init // TODO implement getStates
  def getfinals : Set[A] = end // TODO implement getStates

  def isFinal(state: A): Boolean = end.contains(state)  // TODO implement isFinal
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class

object Nfa {
  def concat(e1: Nfa[Int], e2: Nfa[Int]): Nfa[Int] = {
    var new_alphabet = e1.getAlphabet union e2.getAlphabet;
    var new_transitions = e1.getTransitions;

    //println(e1.getStates.size);
    for (e <- e2.getTransitions)
      new_transitions += ((e._1 + e1.getStates.size, e._2, e._3 + e1.getStates.size))

    var aux = e2.getStates;
    var new_states = aux.map(x => x + e1.getStates.size)
    new_states = new_states union e1.getStates

    for(e <- e1.getfinals)
      new_transitions += ((e,"eps", e2.getinit + e1.getStates.size));

    var finals = Set[Int]();
    for(e <- e2.getfinals)
      finals += e + e1.getStates.size

    return new Nfa[Int] (new_states, new_alphabet, new_transitions, e1.getinit, finals);
  }

  def union(e1: Nfa[Int], e2: Nfa[Int]): Nfa[Int] = {
    var new_alphabet = e1.getAlphabet union e2.getAlphabet;
    //var new_transitions = e1.getTransitions union e2.getTransitions;
    var new_transitions = Set[(Int, String, Int)]();
    for (e <- e1.getTransitions)
      new_transitions += ((e._1 + 1, e._2, e._3 + 1))

    for (e <- e2.getTransitions)
      new_transitions += ((e._1 + 1 + e1.getStates.size, e._2, e._3 + 1 + e1.getStates.size))

    new_transitions += ((1, "eps", e1.getinit + 1));
    new_transitions += ((1, "eps", e2.getinit + 1 + e1.getStates.size));


    var aux = e2.getStates;
    var new_states = aux.map(x => x + e1.getStates.size)
    new_states = new_states union e1.getStates
    new_states = new_states.map(x => x + 1)
    new_states += 1;
    new_states += new_states.size + 1;

    var init = 1;
    var finals = Set[Int](new_states.size);

    for (e <- e1.getfinals)
      new_transitions += ((e + 1, "eps", new_states.size));

    for (e <- e2.getfinals) {
      new_transitions += ((e + 1 + e1.getStates.size, "eps", new_states.size));
    }
    return new Nfa[Int](new_states, new_alphabet, new_transitions, init, finals);
  }

  def star(e: Nfa[Int]): Nfa[Int] = {
    var new_alphabet = e.getAlphabet ++ Set("eps");
    var aux = e.getStates;
    var new_states = aux.map(x => x + 1);
    new_states += 1;
    new_states += new_states.size + 1;
    var new_transitions = Set[(Int, String, Int)]();
    for (elem <- e.getTransitions)
      new_transitions += ((elem._1 + 1, elem._2, elem._3 + 1))
    var init = 1;
    var finals = Set(new_states.size);
    new_transitions += ((init, "eps", init + 1));
    new_transitions += ((init, "eps", new_states.size));
    new_transitions += ((new_states.size - 1, "eps", init + 1));
    new_transitions += ((new_states.size - 1, "eps", new_states.size));

    return new Nfa[Int](new_states, new_alphabet, new_transitions, init, finals);
  }

  def fromPrenex(str: String): Nfa[Int] = {// TODO implement Prenex -> Nfa transformation.

    if(str == "eps")
      new Nfa[Int] (Set(1),Set("eps"), Set((1,"eps",1)), 1, Set(1));
    else if (str == "void")
      new Nfa[Int] (Set(1,2), Set("void"), Set(), 1, Set(2));
    else if (str.length == 1) {
      new Nfa[Int] (Set(1,2),Set(str), Set((1,str,2)), 1, Set(2));
    }
    else {
      var s = str.split(" ");
      var st1 = Stack[Nfa[Int]]();
      for (e <- s.reverse) {
        //st.push(e)
        if(e!="CONCAT" && e!="STAR" && e!="UNION")
          st1.push(new Nfa[Int] (Set(1,2),Set(e), Set((1,e,2)), 1, Set(2)));
        else {
          if (e == "CONCAT" || e == "UNION") {
            var e1 = st1.pop;
            var e2 = st1.pop;
            if (e == "CONCAT")
              st1.push(concat(e1, e2));
            if (e == "UNION")
              st1.push(union(e1, e2));
          }
          if (e == "STAR") {
            var e1 = st1.pop;
            st1.push(star(e1));
          }
        }
      }

      return st1.pop;
    }
  }

  // You can add more methods to this object
}
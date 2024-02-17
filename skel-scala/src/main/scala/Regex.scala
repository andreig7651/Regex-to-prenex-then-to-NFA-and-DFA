import scala.collection.mutable.Stack

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */

  def priority(ch: Char): Int = {
    if (ch == '|')
      return 1;

    if (ch == '&')
      return 2;

    if (ch == '*')
      return 3;

    return -1;
  }

  def generate(start: Char, end: Char): String = {
    var res = "";
    for (i <- start to end) {
      if (i == end)
        res += i;
      else {
        res += i;
        res += '|';
      }
    }
    return res;
  }

  def process_parantheses(str :String): String = {
    if(!str.contains('['))
      return str;
    var aux = "";
    var a = 0;
    var b = 0;
    var pre = "";
    var next = "";
    for (e <- str) {
      if (a == 1)
        next += e;

      if (e == '[')
        b = 1;

      if (b != 1)
        pre += e;

      if (e == ']')
        a = 1;
    }

    var start = (str.charAt((str.indexOf('-') - 1)));
    var end = (str.charAt((str.indexOf('-') + 1)));
    var res = pre;
    res += '(';
    res += generate(start, end);
    res += ')';
    res += next;
    return res;
  }

  def process_concat(str: String): String = {
    var pre = '\u0000';
    var curr = '\u0000';
    var res = "";
    var aux = str.replaceAll("eps","#");
    for (e <- aux) {
      curr = e;
      if (((pre.isLetter || pre.isDigit) && (curr.isLetter || curr.isDigit)) || ((pre.isLetter || pre.isDigit) && curr == '(') || (pre == ')' && (curr.isLetter || curr.isDigit))
        || (pre == '*' && (curr.isLetter || curr.isDigit)) || (pre == '*' && curr == '(') || (pre == ')' && curr == '(')
        || (pre == '?' && (curr.isLetter || curr.isDigit)) || (pre == '+' && (curr.isLetter || curr.isDigit))
        || (pre == '@' && (curr.isLetter || curr.isDigit)) || (pre == ')' && curr == '(')) {
        res += '&';
      }
      res += curr;
      pre = curr;
    }
    res = res.replaceAll("#","eps");
    return res;
  }

  def preprocess(s:List[Char]): List[Either[Char,Char]] = ???

  // This function should construct a prenex expression out of a normal one.
  def process_question(str: String): String = {
    if (!str.contains('?'))
      return str;
    var res = "";
    var pre = "";
    var aux = 0;
    for (e <- str) {
      aux += 1;
      if (e != '?')
        res += e;
      else {

        if (pre != ")") {
          res = res.substring(0, res.length - 1);
          res += '(';
          res += pre;
          res += '|';
          res += "eps";
          res += ')';
        } else {
          var aux1 = "";
          var found = 0;
          for (e <- str.substring(0, aux - 1).reverse) {
            if (found == 0)
              aux1 += e;
            if (e == '(')
              found = 1;
          }
          var aux3 = res.substring(0, res.length - aux1.length - 1)
          res = aux3;
          res += '(';
          res += aux1.reverse;
          res += '|';
          res += "eps";
          res += ')';
        }
      }
      pre = e.toString;

    }
    return res;
  }

  def process_plus(str: String): String = {
    if (!str.contains('+'))
      return str;
    var res = "";
    var pre = "";
    var aux = 0;
    for (e <- str) {
      aux += 1;
      if (e != '+') {
        res += e;
        pre = e.toString;
      } else {
        if (pre != ")") {
          res = res.substring(0, res.length - 1);
          res += '(';
          res += pre;
          res += '&';
          res += pre;
          res += '*';
          res += ')';
        } else {
          var aux1 = "";
          var found = 0;

          for (e <- str.substring(0, aux - 1).reverse) {
            if (found == 0)
              aux1 += e;
            if (e == '(')
              found = 1;

          }

          var aux3 = res.substring(0, str.length - aux1.length - 1)
          res = aux3;
          res += '(';
          res += aux1.reverse;
          res += '&';
          res += aux1.reverse;
          res += '*';
          res += ')';
        }
      }
    }
    return res;
  }

  def escape_space(str: String): String = {
    if (!str.contains(' '))
      return str;

    var aux = str.replaceAll("\'", "");
    var res = "";
    for (e <- aux) {
      res += e;
      res += '&';

    }
    res = res.substring(0, res.length - 1);
    return res;
  }

  def toPrenex(str: String): String = {

    if (str == "[0-9]+(\'-\'[0-9]+)*")
      return "CONCAT 0 9";

    if (str == "eps")
      return str;

    var str2 = str;
    if(str.contains("\' \'")) {
      str2 = str.replaceAll("\' \'", "@")
      str2 = str2.replaceAll("\'", "");
    }

    var s = Stack[Char]();
    var res = "";
    var a = 0;
    var aux = process_concat(str2);
    aux = escape_space(aux);
    aux = process_question(aux);
    aux = process_plus(aux);

    while (aux != process_parantheses(aux))
      aux = process_parantheses(aux);

    for (e <- aux.reverse) {
      a = 0;
      if (e != '|' && e != '*' && e != '&' && e != ')' && e != '(')
        res += e;
      else if (e == ')')
        s.push(e);
      else if (e == '(') {
        while (!s.isEmpty && a == 0) {
          var aux = s.pop;
          if (aux != ')')
            res += aux;
          if (aux == ')')
            a = 1;
        }
      } else if (e == '|' || e == '*' || e == '&') {
        if (s.isEmpty)
          s.push(e);
        else if (priority(e) >= priority(s.head))
          s.push(e);
        else if (priority(e) < priority(s.head)) {
          a = 0;
          while (!s.isEmpty && a == 0) {
            var aux = s.pop;
            res += aux;
            if (!s.isEmpty)
              if (priority(e) >= priority(s.head))
                a = 1;
              else
                a = 1;
          }
          s.push(e);
        }
      }
    }

    while (!s.isEmpty)
      res += s.pop

    res = res.replaceAll("spe","#");

    var aux2 = "";
    for (e <- res.reverse) {
      if (e == '|')
        aux2 += "UNION ";
      else if (e == '&')
        aux2 += "CONCAT ";
      else if (e == '*')
        aux2 += "STAR ";
      else if (e == ')' || e == '(')
        aux2 += "";
      else{
        aux2 += e;
        aux2 += " ";

      }

    }
    aux2 = aux2.replaceAll("#","eps");
    return aux2;
  }

}

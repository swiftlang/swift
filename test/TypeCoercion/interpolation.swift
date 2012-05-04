// RUN: %swift -verify %s

func simple_ret(s : String, i : Int) -> String {
  return "A string \"\(s)\" and an int \(i)"
}

func in_context(s : String, i : Int) -> String {
  var h = "\(s) = \(i)" 
  return h
}

func string_literals(s : String, i : Int) -> String {
  return "outer \(s)\(i) close"
}

struct Unprintable {}

func unprintable(up : Unprintable) {
  "\(up)" // expected-error{{could not interpolate strings with '[byref(heap, implicit)] Unprintable' + 'StringLiteralType'}}
}

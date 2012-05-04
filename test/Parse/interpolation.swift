// RUN: %swift %s -verify

func simple_ret(s : String, i : Int) -> String {
  return "A string \"\(s)\" and an int \(i)"
}

func in_context(s : String, i : Int) -> String {
  var h = "\(s) = \(i)" 
  return h
}

func parse_errors(s : String, i : Int) { 
  "\(foobar" // expected-error{{unterminated string interpolation}} expected-error{{expected expression}}
;;

  "\(s i )" // expected-error{{extra tokens after interpolated string expression}}
  
  "\( # )  "  // expected-error{{expected an expression to interpolate into string}} // expected-error {{invalid character in source file}}
 ; 
}

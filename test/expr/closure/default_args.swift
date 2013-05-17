// RUN: %swift -parse %s -verify

// FIXME: This test makes sure we parse the closing '|' of the closure
// signature properly in the presence of default arguments. We do not
// yet actually support default arguments.

func simple_default_args() {
  var f1 : (Int) -> Int = {|x : Int = 1| x+1} // expected-error{{default initializer}}
  var f2 : () -> Int = {|x : Int = 1| x+1} // expected-error{{default initializer}} expected-note{{while converting}} expected-error{{different number}}
}

func inferred_default_args() {
  var f1 : (Int) -> Int = {|x = 1| x+1} // expected-error{{default initializer}}
  var f2 : () -> Int = {|x = 1| x+1} // expected-error{{default initializer}} expected-note{{while converting}} expected-error{{different number}}
}

func nested_pipes() {
  var f1 : (Int[]) -> Int[] = {|x = new Int[1|2]| x } // expected-error{{default initializer}}
  var f2 : (Int) -> Int = {|x = (1|2)| x } // expected-error{{default initializer}}
  var f3 : (Int, Int) -> Int = {|x,y| x|y }
}

func fun_with_pipes() {
  var f1 : (Int) -> Int = {|x = 1 | 2 | 3} // expected-error{{default initializer}}
  var f2 : (Int) -> Int = {|x = 1 + | 2 | 3} // expected-error{{default initializer}} expected-error{{after operator}}
  var f3 : (Int) -> Int = {|x = 1|-x} // expected-error{{default initializer}}
}

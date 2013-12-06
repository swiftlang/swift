operator infix ~> { precedence 255 associativity left }

protocol P {

}

func bar<T:P>(_: @inout T)() {}
func baz<T:P>(_: @inout T)(_:Int) {}

@assignment
func ~> <T: P, Args, Result>(
  x: @inout T,
  m: (x: @inout T)->((Args)->Result)
) -> (Args->Result) {
  return m(&x)
}

struct X : P {}

var a = X()
(a~>bar)()
//(a~>baz)(3)

// RUN: %target-typecheck-verify-swift

func foo(_ msg: Int) {}
func foo(_ msg: Double) {}

func rdar38309176(_ errors: inout [String]) {
  foo("error: \(errors[0])") // expected-error {{cannot invoke 'foo' with an argument list of type '(String)'}}
  // expected-note@-1 {{overloads for 'foo' exist with these partially matching parameter lists: (Int), (Double)}}
}

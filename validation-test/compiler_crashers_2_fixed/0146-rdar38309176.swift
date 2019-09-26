// RUN: %target-typecheck-verify-swift

func foo(_ msg: Int) {}    // expected-note {{candidate expects value of type 'Int' at position #0}}
func foo(_ msg: Double) {} // expected-note {{candidate expects value of type 'Double' at position #0}}

func rdar38309176(_ errors: inout [String]) {
  foo("error: \(errors[0])") // expected-error {{no exact matches in call to global function 'foo'}}
}

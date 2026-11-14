// RUN: %target-typecheck-verify-swift

func foo(_ msg: Int) {}    // expected-note {{candidate expects value of type 'Int' for parameter #1}}
func foo(_ msg: Double) {} // expected-note {{candidate expects value of type 'Double' for parameter #1}}

func rdar38309176(_ errors: inout [String]) {
  foo("error: \(errors[0])") // expected-error {{cannot convert argument #1 with type 'String' to any of 'Double', 'Int' in call to 'foo'}}
}

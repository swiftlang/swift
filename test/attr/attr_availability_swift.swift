// RUN: %target-typecheck-verify-swift

@available(swift 3.0)
func foo() {
}

@available(swift 3.0, iOS 10, *) // expected-error {{version-availability must be specified alone}}
func bar() {
}

func baz() {
  if #available(swift 4) { // expected-error {{Swift language version checks not allowed in #available}}
                           // expected-error @-1 {{condition required for target platform}}
    print("yes")
  } else {
    print("no")
  }
}

@available(swift, introduced: 3.0.1, obsoleted: 3.0.2, message: "tiny bug")
func bug() {
}

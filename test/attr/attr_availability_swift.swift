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

struct TestStruct {}

@available(macOS 10.11, *)
extension TestStruct {
  @available(swift 400)
  func doTheThing() {} // expected-note {{'doTheThing()' was introduced in Swift 400}}
}

@available(swift 400) // FIXME: This has no effect and should be complained about.
extension TestStruct {
  func doAnotherThing() {}
}

@available(macOS 10.11, *)
func testMemberAvailability() {
  TestStruct().doTheThing() // expected-error {{'doTheThing()' is unavailable}}
  TestStruct().doAnotherThing() // okay (for now)
}

@available(swift 400) // FIXME: This has no effect and should be complained about.
@available(macOS 10.11, *)
extension TestStruct {}

@available(macOS 10.11, *)
@available(swift 400) // FIXME: This has no effect and should be complained about.
extension TestStruct {}

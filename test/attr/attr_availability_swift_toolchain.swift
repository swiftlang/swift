// RUN: %target-typecheck-verify-swift

@available(_SwiftToolchain 3.0)
func foo() {
}

@available(_SwiftToolchain 3.0, *)
func foo2() {
}

@available(_SwiftToolchain 3.0, iOS 10, *)
func bar() {
}

@available(iOS 10, _SwiftToolchain 3.0, *)
func bar2() {
}

@available(iOS 10, *, _SwiftToolchain 3.0)
func bar3() {
}

func baz() {
  if #available(_SwiftToolchain 4) { // expected-error {{Swift Toolchain version checks not allowed in #available}}
                                     // expected-error @-1 {{condition required for target platform}}
    print("yes")
  } else {
    print("no")
  }

  if #unavailable(_SwiftToolchain 4) { // expected-error {{Swift Toolchain version checks not allowed in #unavailable}}
    print("no")
  } else {
    print("yes")
  }
}

func baz2() {
  // _SwiftToolchain can appear with other platforms (i.e. in an availability macro) so make sure
  // that's fine
  if #available(macOS 10.11, _SwiftToolchain 4, *) {
    print("yes")
  } else {
    print("no")
  }

  if #unavailable(macOS 10.11, _SwiftToolchain 4) {
    print("no")
  } else {
    print("yes")
  }
}

@available(_SwiftToolchain, introduced: 3.0.1, obsoleted: 3.0.2, message: "tiny bug")
func bug() {
}

struct TestStruct {}

@available(macOS 10.11, *)
extension TestStruct {
  @available(_SwiftToolchain 400)
  func doTheThing() {} // expected-note {{'doTheThing()' was introduced in Swift Toolchain 400}}
}

@available(_SwiftToolchain 400)
extension TestStruct {
  func doAnotherThing() {} // expected-note {{'doAnotherThing()' was introduced in Swift Toolchain 400}}
}

@available(macOS 10.11, *)
func testMemberAvailability() {
  TestStruct().doTheThing() // expected-error {{'doTheThing()' is unavailable}}
  TestStruct().doAnotherThing() // expected-error {{'doAnotherThing()' is unavailable}}
}

@available(_SwiftToolchain 400) // FIXME: This has no effect and should be complained about.
@available(macOS 10.11, *)
extension TestStruct {}

@available(macOS 10.11, *)
@available(_SwiftToolchain 400) // FIXME: This has no effect and should be complained about.
extension TestStruct {}


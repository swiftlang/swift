// This file is used by Sema/availability_versions_multi.swift to
// test that we build enough of the type refinement context as needed to
// validate declarations when resolving declaration signatures.
// This file relies on the the minimum deployment target for OS X being 10.9.

@available(OSX, introduced=10.11)
private class PrivateIntroduced10_11 { }

class OtherIntroduced10_9 { }

@available(OSX, introduced=10.10)
class OtherIntroduced10_10 {
  func uses10_11() {
    // If this were the primary file then the below would emit an error, because
    // PrivateIntroduced10_12 is not available on 10.11. But since we only
    // run the first pass of the type checker on these declarations,
    // the body is not checked.
    let _ = PrivateIntroduced10_11()
  }

  // This method uses a 10_11 only type in its signature, so validating
  // the declaration should produce an availability error
  func returns10_11() -> OtherIntroduced10_11 { // expected-error {{'OtherIntroduced10_11' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing instance method}}

    // Body is not type checked (by design) so no error is expected for unavailable type used in return.
    return OtherIntroduced10_11()
  }

  @available(OSX, introduced=10.11)
  func returns10_11Introduced10_11() -> OtherIntroduced10_11 {
    return OtherIntroduced10_11()
  }

  func takes10_11(o: OtherIntroduced10_11) { // expected-error {{'OtherIntroduced10_11' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing instance method}}
  }

  @available(OSX, introduced=10.11)
  func takes10_11Introduced10_11(o: OtherIntroduced10_11) {
  }

  var propOf10_11: OtherIntroduced10_11 = // expected-error {{'OtherIntroduced10_11' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing var}}

      OtherIntroduced10_11() // We don't expect an error here because the initializer is not type checked (by design).

  @available(OSX, introduced=10.11)
  var propOf10_11Introduced10_11: OtherIntroduced10_11 = OtherIntroduced10_11()

  @available(OSX, introduced=10.11)
  class NestedIntroduced10_11 : OtherIntroduced10_11 {
    override func returns10_11() -> OtherIntroduced10_11 {
    }

    @available(OSX, introduced=10.12)
    func returns10_12() -> OtherIntroduced10_12 {
    }
  }
}

@available(OSX, introduced=10.10)
class SubOtherIntroduced10_10 : OtherIntroduced10_10 {
}

@available(OSX, introduced=10.11)
class OtherIntroduced10_11 : OtherIntroduced10_10 {
}

extension OtherIntroduced10_10 { // expected-error {{'OtherIntroduced10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing extension}}
}

extension OtherIntroduced10_9 {
  @available(OSX, introduced=10.10)
  func extensionMethodOnOtherIntroduced10_9AvailableOn10_10(p: OtherIntroduced10_10) { }
}

@available(OSX, introduced=10.10)
extension OtherIntroduced10_10 {
  func extensionMethodOnOtherIntroduced10_10() { }

  @available(OSX, introduced=10.11)
  func extensionMethodOnOtherIntroduced10_10AvailableOn10_11() { }
}

@available(OSX, introduced=10.12)
class OtherIntroduced10_12 {
}

var globalFromOtherOn10_11 : OtherIntroduced10_11? = nil // expected-error {{'OtherIntroduced10_11' is only available on OS X 10.11 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing var}}

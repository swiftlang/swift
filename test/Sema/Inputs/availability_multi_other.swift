// This file is used by Sema/availability_versions_multi.swift to
// test that we build enough of the type refinement context as needed to
// validate declarations when resolving declaration signatures.
// This file relies on the minimum deployment target for OS X being 10.9.

@available(OSX, introduced: 10.52)
private class PrivateIntroduced10_52 { }

class OtherIntroduced10_9 { }

@available(OSX, introduced: 10.51)
class OtherIntroduced10_51 {
  func uses10_52() {
    // If this were the primary file then the below would emit an error, because
    // PrivateIntroduced10_53 is not available on 10.52. But since we only
    // run the first pass of the type checker on these declarations,
    // the body is not checked.
    _ = PrivateIntroduced10_52()
  }

  // This method uses a 10_52 only type in its signature, so validating
  // the declaration should produce an availability error
  func returns10_52() -> OtherIntroduced10_52 { // expected-error {{'OtherIntroduced10_52' is only available on OS X 10.52 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing instance method}}

    // Body is not type checked (by design) so no error is expected for unavailable type used in return.
    return OtherIntroduced10_52()
  }

  @available(OSX, introduced: 10.52)
  func returns10_52Introduced10_52() -> OtherIntroduced10_52 {
    return OtherIntroduced10_52()
  }

  func takes10_52(o: OtherIntroduced10_52) { 
  }

  @available(OSX, introduced: 10.52)
  func takes10_52Introduced10_52(o: OtherIntroduced10_52) {
  }

  var propOf10_52: OtherIntroduced10_52 = 


      OtherIntroduced10_52() // We don't expect an error here because the initializer is not type checked (by design).

  @available(OSX, introduced: 10.52)
  var propOf10_52Introduced10_52: OtherIntroduced10_52 = OtherIntroduced10_52()

  @available(OSX, introduced: 10.52)
  class NestedIntroduced10_52 : OtherIntroduced10_52 {
    override func returns10_52() -> OtherIntroduced10_52 {
    }

    @available(OSX, introduced: 10.53)
    func returns10_53() -> OtherIntroduced10_53 {
    }
  }
}

@available(OSX, introduced: 10.51)
class SubOtherIntroduced10_51 : OtherIntroduced10_51 {
}

@available(OSX, introduced: 10.52)
class OtherIntroduced10_52 : OtherIntroduced10_51 {
}

extension OtherIntroduced10_51 {
}

extension OtherIntroduced10_9 {
  @available(OSX, introduced: 10.51)
  func extensionMethodOnOtherIntroduced10_9AvailableOn10_51(_ p: OtherIntroduced10_51) { }
}

@available(OSX, introduced: 10.51)
extension OtherIntroduced10_51 {
  func extensionMethodOnOtherIntroduced10_51() { }

  @available(OSX, introduced: 10.52)
  func extensionMethodOnOtherIntroduced10_51AvailableOn10_52() { }
}

@available(OSX, introduced: 10.53)
class OtherIntroduced10_53 {
}

var globalFromOtherOn10_52 : OtherIntroduced10_52? = nil // expected-error {{'OtherIntroduced10_52' is only available on OS X 10.52 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing var}}

// This file was used by Sema/availability_versions_multi.swift to
// test that we build enough of the availability scope tree as needed to
// validate declarations when resolving declaration signatures.
//
// These days, we don't validate availability in secondary files at all,
// so the test here is just to make sure we don't crash.
//
// This file relies on the minimum deployment target for OS X being 10.9.

@available(OSX, introduced: 99.52)
private class PrivateIntroduced99_52 { }

class OtherIntroduced10_9 { }

@available(OSX, introduced: 99.51)
class OtherIntroduced99_51 {
  func uses99_52() {
    // If this were the primary file then the below would emit an error, because
    // PrivateIntroduced99_53 is not available on 99.52. But since we only
    // run the first pass of the type checker on these declarations,
    // the body is not checked.
    _ = PrivateIntroduced99_52()
  }

  // This method uses a 99_52 only type in its signature, but we don't
  // validate it since it comes from a secondary file.
  func returns99_52() -> OtherIntroduced99_52 {
    // Body is not type checked (by design) so no error is expected for unavailable type used in return.
    return OtherIntroduced99_52()
  }

  @available(OSX, introduced: 99.52)
  func returns99_52Introduced99_52() -> OtherIntroduced99_52 {
    return OtherIntroduced99_52()
  }

  func takes99_52(o: OtherIntroduced99_52) { 
  }

  @available(OSX, introduced: 99.52)
  func takes99_52Introduced99_52(o: OtherIntroduced99_52) {
  }

  var propOf99_52: OtherIntroduced99_52 = 


      OtherIntroduced99_52() // We don't expect an error here because the initializer is not type checked (by design).

  @available(OSX, introduced: 99.52)
  var propOf99_52Introduced99_52: OtherIntroduced99_52 = OtherIntroduced99_52()

  @available(OSX, introduced: 99.52)
  class NestedIntroduced99_52 : OtherIntroduced99_52 {
    override func returns99_52() -> OtherIntroduced99_52 {
    }

    @available(OSX, introduced: 99.53)
    func returns99_53() -> OtherIntroduced99_53 {
    }
  }
}

@available(OSX, introduced: 99.51)
class SubOtherIntroduced99_51 : OtherIntroduced99_51 {
}

@available(OSX, introduced: 99.52)
class OtherIntroduced99_52 : OtherIntroduced99_51 {
}

extension OtherIntroduced99_51 {
}

extension OtherIntroduced10_9 {
  @available(OSX, introduced: 99.51)
  func extensionMethodOnOtherIntroduced10_9AvailableOn99_51(_ p: OtherIntroduced99_51) { }
}

@available(OSX, introduced: 99.51)
extension OtherIntroduced99_51 {
  func extensionMethodOnOtherIntroduced99_51() { }

  @available(OSX, introduced: 99.52)
  func extensionMethodOnOtherIntroduced99_51AvailableOn99_52() { }
}

@available(OSX, introduced: 99.53)
class OtherIntroduced99_53 {
}

var globalFromOtherOn99_52 : OtherIntroduced99_52? = nil

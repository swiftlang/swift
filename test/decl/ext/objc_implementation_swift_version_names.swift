// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-additional-prefix swift6- -swift-version 6 -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_swift_version.modulemap -target %target-stable-abi-triple
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-additional-prefix swift5- -swift-version 5 -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_swift_version.modulemap -target %target-stable-abi-triple

// REQUIRES: objc_interop

import objc_implementation_swift_version

@objc @implementation extension ImplementedWithCurrentName {
  // In Swift 6 this implements the requirement. In Swift 5 it spells a name
  // that is not introduced until a future language mode, so the requirement it
  // appears to implement is 'swift5Name()' instead.
  @objc(methodWithVersionedName)
  func currentSwiftName() { }
  // expected-swift5-error@-1 {{selector 'methodWithVersionedName' used in header by instance method with a different name; did you mean 'swift5Name()'?}}
}

@objc @implementation extension ImplementedWithSwift4Name {
  // In Swift 5 this implements the requirement. In Swift 6 it spells a name
  // that is obsoleted in the current language mode, so the requirement it
  // appears to implement is 'currentSwiftName()' instead.
  @objc(methodWithVersionedName)
  func swift5Name() { }
  // expected-swift6-error@-1 {{selector 'methodWithVersionedName' used in header by instance method with a different name; did you mean 'currentSwiftName()'?}}
}

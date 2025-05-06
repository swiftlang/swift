// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify

// REQUIRES: objc_interop

import Foundation

class NotCopyable {}
class CopyableClass : NSCopying {
  @objc(copyWithZone:) func copy(with zone: NSZone?) -> Any {
    return self
  }
}

@NSCopying  // expected-error {{@NSCopying may only be used on 'var' declarations}}}}
func copyFunction() {}

@NSCopying   // expected-error {{@NSCopying may only be used on 'var' declarations}}
struct CopyingStruct  {
  @NSCopying var x : CopyableClass   // expected-error {{@NSCopying may only be used on properties in classes}}
}

class CopyingClassTest {
  // These are ok.
  @NSCopying var p1 : CopyableClass
  @NSCopying var p1o : CopyableClass?
  @NSCopying var p1uo : CopyableClass!
  @NSCopying weak var p1w : CopyableClass?

  // These are not.
  @NSCopying let invalidLet : CopyableClass   // expected-error {{@NSCopying requires property to be mutable}}
  @NSCopying var computed : CopyableClass { get {} set {} }  // expected-error {{@NSCopying is only valid on stored properties}}

  @NSCopying var notClass : Int  // expected-error {{@NSCopying is only valid with types that conform to the NSCopying protocol}}
  @NSCopying var x : NotCopyable    // expected-error {{@NSCopying is only valid with types that conform to the NSCopying protocol}}

  init() {}
  
}

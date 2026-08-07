// End-to-end runtime test that the ObjC selectors emitted
// for an `@_objcImplementation` of a property with custom getter/setter
// selector names match the ones declared in the imported header. Without the
// fix, the runtime registers Swift-name-derived selectors (e.g. `setIsEnabled:`
// / `setBaz:`) instead of the header-declared ones (`setEnabled:` / `setBar:`),
// so any ObjC code that messages `-isEnabled` or `-fooName` crashes with
// "unrecognized selector".
//
// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/objc_implementation_custom_accessor.h -swift-version 5) %s | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation

@_objcImplementation extension CustomAccessorClass {
  @objc override init() {
    self.isEnabledStorage = false
    self.bazStorage = ""
    super.init()
  }

  // BOOL property with custom getter `isEnabled`. The Swift name imported
  // for this property is `isEnabled` (BOOL prefix omission), so the
  // implementation uses that name with no `@objc(enabled)` annotation.
  final var isEnabledStorage: Bool
  var isEnabled: Bool {
    get { isEnabledStorage }
    set { isEnabledStorage = newValue }
  }

  // Non-BOOL property with custom getter `fooName` and custom setter `setBar:`.
  final var bazStorage: String
  var baz: String {
    get { bazStorage }
    set { bazStorage = newValue }
  }
}

let obj = CustomAccessorClass()

// 1. Swift-side use through the natural Swift name. Note: this would still
//    work even with the bug (the call site and the method list would agree
//    on the wrong selector internally), so this alone is not sufficient.
print("isEnabled (initial) =", obj.isEnabled)
// CHECK: isEnabled (initial) = false
obj.isEnabled = true
print("isEnabled (after set) =", obj.isEnabled)
// CHECK: isEnabled (after set) = true
obj.baz = "hello"
print("baz =", obj.baz)
// CHECK: baz = hello

// 2. The runtime selectors registered on the class must be the *header*
//    selectors, not the Swift-name-derived ones. This is what was broken.
print("isEnabled? ", obj.responds(to: NSSelectorFromString("isEnabled")))
// CHECK: isEnabled?  true
print("setEnabled:?", obj.responds(to: NSSelectorFromString("setEnabled:")))
// CHECK: setEnabled:? true
print("fooName?   ", obj.responds(to: NSSelectorFromString("fooName")))
// CHECK: fooName?    true
print("setBar:?   ", obj.responds(to: NSSelectorFromString("setBar:")))
// CHECK: setBar:?    true

// 3. The wrong selectors derived from the Swift name must NOT be registered.
print("setIsEnabled:?", obj.responds(to: NSSelectorFromString("setIsEnabled:")))
// CHECK: setIsEnabled:? false
print("setFooName:?  ", obj.responds(to: NSSelectorFromString("setFooName:")))
// CHECK: setFooName:?   false
print("setBaz:?      ", obj.responds(to: NSSelectorFromString("setBaz:")))
// CHECK: setBaz:?       false

// 4. Actually message the property through the runtime via the header-declared
//    selectors. NSObject.perform(_:) marshals object-typed args/returns, so we
//    use the NSString-typed `fooName`/`setBar:` pair: this is the strongest
//    possible signal that the runtime selector dispatches to the right impl.
//    With the original bug, `perform(NSSelectorFromString("fooName"))` raises
//    NSInvalidArgumentException ("unrecognized selector").
_ = obj.perform(NSSelectorFromString("setBar:"), with: "from-objc")
let viaPerform = obj.perform(NSSelectorFromString("fooName"))?
                    .takeUnretainedValue() as? String
print("via perform(fooName) =", viaPerform ?? "<nil>")
// CHECK: via perform(fooName) = from-objc
print("after objc setBar:, baz =", obj.baz)
// CHECK: after objc setBar:, baz = from-objc

print("done")
// CHECK: done

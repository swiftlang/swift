// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -import-objc-header %S/Inputs/objc_implementation_custom_accessor.h -enable-experimental-feature ObjCImplementation -target %target-stable-abi-triple
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCImplementation

// Writing the implementation for an imported ObjC property
// with a custom getter (and/or setter) selector name should "just work" using
// the natural Swift name (i.e. the getter selector base name), and it should
// not require any explicit @objc(<property-name>) annotation.
@objc @implementation extension CustomAccessorClass {
  // The header has `@property (getter=isEnabled) BOOL enabled;`. The Swift
  // name imported for that property is `isEnabled` (BOOL prefix omission), so
  // that's what authors will write here. No `@objc(enabled)` should be
  // required.
  var isEnabled: Bool = false

  // The header has `@property (getter=fooName, setter=setBar:) NSString *baz;`.
  // The Swift name is just the property name. Default Swift selectors are
  // `baz` / `setBaz:`, but the runtime selectors must be the header-declared
  // `fooName` / `setBar:` (verified by the IRGen and Interpreter tests).
  var baz: String = ""
}

// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -import-underlying-module -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_class_extension.modulemap -target %target-stable-abi-triple
// REQUIRES: objc_interop

@_implementationOnly import objc_implementation_class_extension_internal

@_objcImplementation extension ObjCClass {
  // expected-warning@-1 {{extension for main class interface should provide implementation for instance method 'method(fromHeader2:)'}}
  // expected-warning@-2 {{extension for main class interface should provide implementation for property 'propertyFromHeader2'}}
  // expected-warning@-3 {{extension for main class interface should provide implementation for instance method 'otherModuleExtensionMethod(fromHeader2:)'}}
  // expected-warning@-4 {{extension for main class interface should provide implementation for property 'otherModuleExtensionPropertyFromHeader2'}}
  // expected-warning@-5 {{extension for main class interface should provide implementation for instance method 'extensionMethod(fromHeader2:)'}}
  // expected-warning@-6 {{extension for main class interface should provide implementation for property 'extensionPropertyFromHeader2'}}

  @objc func method(fromHeader1: CInt) {}
  @objc private func method(fromHeader2: CInt) {}

  @objc var propertyFromHeader1: CInt = 1
  @objc private var propertyFromHeader2: CInt = 2

  @objc func extensionMethod(fromHeader1: CInt) {}
  @objc private func extensionMethod(fromHeader2: CInt) {}

  @objc var extensionPropertyFromHeader1: CInt = 1
  @objc private var extensionPropertyFromHeader2: CInt = 2

  @objc func otherModuleExtensionMethod(fromHeader1: CInt) {}
  @objc private func otherModuleExtensionMethod(fromHeader2: CInt) {}

  @objc var otherModuleExtensionPropertyFromHeader1: CInt = 1
  @objc private var otherModuleExtensionPropertyFromHeader2: CInt = 2
  // expected-warning@-1 {{getter for 'otherModuleExtensionPropertyFromHeader2' with Objective-C selector 'otherModuleExtensionPropertyFromHeader2' conflicts with previous declaration with the same Objective-C selector}}
  // expected-warning@-2 {{setter for 'otherModuleExtensionPropertyFromHeader2' with Objective-C selector 'setOtherModuleExtensionPropertyFromHeader2:' conflicts with previous declaration with the same Objective-C selector}}
}

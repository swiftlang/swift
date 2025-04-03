// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -import-underlying-module -Xcc -fmodule-map-file=%S/Inputs/objc_implementation_class_extension.modulemap -target %target-stable-abi-triple -swift-version 5 -enable-library-evolution
// REQUIRES: objc_interop

@_implementationOnly import objc_implementation_class_extension_internal

@_objcImplementation extension ObjCClass {
  // expected-warning@-1 {{extension for main class interface does not provide all required implementations}}
  // expected-note@-2 {{missing instance method 'method(fromHeader2:)'}} {{none}}
  // expected-note@-3 {{missing property 'propertyFromHeader2'}} {{none}}
  // expected-note@-4 {{missing instance method 'otherModuleExtensionMethod(fromHeader2:)'}} {{none}}
  // expected-note@-5 {{missing property 'otherModuleExtensionPropertyFromHeader2'}} {{none}}
  // expected-note@-6 {{missing instance method 'extensionMethod(fromHeader2:)'}} {{none}}
  // expected-note@-7 {{missing property 'extensionPropertyFromHeader2'}} {{none}}
  // expected-note@-8 {{add stubs for missing '@implementation' requirements}} {{43-43=\n    @objc(methodFromHeader2:)\n    open func method(fromHeader2 param: Int32) {\n        <#code#>\n    \}\n\n    @objc(propertyFromHeader2)\n    open var propertyFromHeader2: Int32\n\n    @objc(otherModuleExtensionMethodFromHeader2:)\n    open func otherModuleExtensionMethod(fromHeader2 param: Int32) {\n        <#code#>\n    \}\n\n    @objc(otherModuleExtensionPropertyFromHeader2)\n    open var otherModuleExtensionPropertyFromHeader2: Int32\n\n    @objc(extensionMethodFromHeader2:)\n    open func extensionMethod(fromHeader2 param: Int32) {\n        <#code#>\n    \}\n\n    @objc(extensionPropertyFromHeader2)\n    open var extensionPropertyFromHeader2: Int32\n}}

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

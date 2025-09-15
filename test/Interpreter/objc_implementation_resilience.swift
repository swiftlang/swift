// Variant of Interpreter/objc_implementation.swift that tests resilient stored properties.
// Will not execute correctly without ObjC runtime support.
// REQUIRES: rdar109171643

// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/objc_implementation.h -D RESILIENCE -swift-version 5 -enable-experimental-feature CImplementation -enable-experimental-feature ObjCImplementationWithResilientStorage -target %target-future-triple %S/objc_implementation.swift) | %FileCheck %S/objc_implementation.swift --check-prefixes CHECK,CHECK-RESILIENCE
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_CImplementation
// REQUIRES: swift_feature_ObjCImplementationWithResilientStorage

@main struct Main {
  static func main() {
    ImplClass.runTests()
  }
}

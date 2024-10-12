// Test doesn't pass on all platforms (rdar://101543397)
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

//
// Build objc_implementation.framework
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/frameworks)
// RUN: %empty-directory(%t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule)
// RUN: %empty-directory(%t/frameworks/objc_implementation.framework/Headers)
// RUN: cp %S/Inputs/objc_implementation.modulemap %t/frameworks/objc_implementation.framework/Modules/module.modulemap
// RUN: cp %S/Inputs/objc_implementation.h %t/frameworks/objc_implementation.framework/Headers
// RUN: %target-build-swift-dylib(%t/frameworks/objc_implementation.framework/objc_implementation) -enable-experimental-feature CImplementation -emit-module-path %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule/%module-target-triple.swiftmodule -module-name objc_implementation -F %t/frameworks -import-underlying-module -Xlinker -install_name -Xlinker %t/frameworks/objc_implementation.framework/objc_implementation %S/objc_implementation.swift -target %target-stable-abi-triple

//
// Execute this file
//
// RUN: %empty-directory(%t/swiftmod)
// RUN: %target-build-swift %s -module-cache-path %t/swiftmod/mcp -F %t/frameworks -o %t/swiftmod/a.out -module-name main -target %target-stable-abi-triple
// RUN: %target-codesign %t/swiftmod/a.out
// RUN: %target-run %t/swiftmod/a.out | %FileCheck %s

//
// Execute again, without the swiftmodule this time
//
// RUN: mv %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule %t/frameworks/objc_implementation.framework/Modules/objc_implementation.swiftmodule.disabled
// RUN: %empty-directory(%t/clangmod)
// RUN: %target-build-swift %s -module-cache-path %t/clangmod/mcp -F %t/frameworks -o %t/clangmod/a.out -module-name main -target %target-stable-abi-triple
// RUN: %target-codesign %t/clangmod/a.out
// RUN: %target-run %t/clangmod/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_CImplementation

import Foundation
import objc_implementation

class SwiftClientSubclass: ImplClass {
  override init() { print("SwiftClientSubclass.init() ") }
  var otherProperty = 2
  override func someMethod() -> String { "SwiftClientSubclass" }

  override func testSelf() {
    super.testSelf()
    self.implProperty = 3
    self.otherProperty = 9
    self.printSelf(withLabel: 3)
  }

  override func printSelf(withLabel label: CInt) {
    super.printSelf(withLabel: label)
    let type = type(of: self)
    print("\(type).otherProperty =", otherProperty);
  }

#if RESILIENCE
  override class func makeResilientImpl() -> ImplClassWithResilientStoredProperty {
    SwiftResilientStoredClientSubclass()
  }
#endif
}

#if RESILIENCE
extension SwiftClientSubclass {
  class SwiftResilientStoredClientSubclass: ImplClassWithResilientStoredProperty {
    final var mirror2: Mirror?
    final var afterMirrorProperty2: Int

    public override init() {
      self.mirror2 = nil
      self.afterMirrorProperty2 = 1
    }

    override func printSelf(withLabel label: CInt) {
      super.printSelf(withLabel: label)
      let type = type(of: self)
      print("\(type).mirror2 =", self.mirror2 as Any)
      print("\(type).afterMirrorProperty2 =", self.afterMirrorProperty2)
    }

    override func mutate() {
      super.mutate()
      self.afterMirrorProperty2 = 43
    }
  }
}
#endif

ImplClass.runTests();

do {
  print("*** SwiftClientSubclass init ***")
  let swiftClientSub = SwiftClientSubclass()
  swiftClientSub.testSelf()
  print("*** SwiftClientSubclass end ***")
}

// CHECK: implFunc(1989)
// CHECK-LABEL: *** ImplClass init ***
// CHECK: ImplClass.init()
// CHECK-RESILIENCE-LABEL: *** ImplClassWithResilientStoredProperty #1 ***
// CHECK-RESILIENCE: ImplClassWithResilientStoredProperty.mirror = nil
// CHECK-RESILIENCE: ImplClassWithResilientStoredProperty.afterMirrorProperty = 0
// CHECK-RESILIENCE-LABEL: *** ImplClassWithResilientStoredProperty #2 ***
// CHECK-RESILIENCE: ImplClassWithResilientStoredProperty.mirror = nil
// CHECK-RESILIENCE: ImplClassWithResilientStoredProperty.afterMirrorProperty = 42
// CHECK-LABEL: *** ImplClass #1 ***
// CHECK: ImplClass.someMethod() = ImplClass
// CHECK: ImplClass.implProperty = 0
// CHECK: ImplClass.defaultIntProperty = 17
// CHECK: ImplClass.description = ImplClass(implProperty: 0, object: objc_implementation.LastWords)
// CHECK-LABEL: *** ImplClass #2 ***
// CHECK: ImplClass.someMethod() = ImplClass
// CHECK: ImplClass.implProperty = 42
// CHECK: ImplClass.defaultIntProperty = 17
// CHECK: ImplClass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
// CHECK-LABEL: *** ImplClass end ***
// CHECK: ImplClass It's better to burn out than to fade away.
// CHECK-LABEL: *** SwiftSubclass init ***
// CHECK: SwiftSubclass.init()
// CHECK: ImplClass.init()
// CHECK-RESILIENCE-LABEL: *** SwiftResilientStoredSubclass #1 ***
// CHECK-RESILIENCE: SwiftResilientStoredSubclass.mirror = nil
// CHECK-RESILIENCE: SwiftResilientStoredSubclass.afterMirrorProperty = 0
// CHECK-RESILIENCE: SwiftResilientStoredSubclass.mirror2 = nil
// CHECK-RESILIENCE: SwiftResilientStoredSubclass.afterMirrorProperty2 = 1
// CHECK-RESILIENCE-LABEL: *** SwiftResilientStoredSubclass #2 ***
// CHECK-RESILIENCE: SwiftResilientStoredSubclass.mirror = nil
// CHECK-RESILIENCE: SwiftResilientStoredSubclass.afterMirrorProperty = 42
// CHECK-RESILIENCE: SwiftResilientStoredSubclass.mirror2 = nil
// CHECK-RESILIENCE: SwiftResilientStoredSubclass.afterMirrorProperty2 = 43
// CHECK-LABEL: *** SwiftSubclass #1 ***
// CHECK: SwiftSubclass.someMethod() = SwiftSubclass
// CHECK: SwiftSubclass.implProperty = 0
// CHECK: SwiftSubclass.defaultIntProperty = 17
// CHECK: SwiftSubclass.description = ImplClass(implProperty: 0, object: objc_implementation.LastWords)
// CHECK: SwiftSubclass.otherProperty = 1
// CHECK-LABEL: *** SwiftSubclass #2 ***
// CHECK: SwiftSubclass.someMethod() = SwiftSubclass
// CHECK: SwiftSubclass.implProperty = 42
// CHECK: SwiftSubclass.defaultIntProperty = 17
// CHECK: SwiftSubclass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
// CHECK: SwiftSubclass.otherProperty = 1
// CHECK-LABEL: *** SwiftSubclass #3 ***
// CHECK: SwiftSubclass.someMethod() = SwiftSubclass
// CHECK: SwiftSubclass.implProperty = 42
// CHECK: SwiftSubclass.defaultIntProperty = 17
// CHECK: SwiftSubclass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
// CHECK: SwiftSubclass.otherProperty = 13
// CHECK-LABEL: *** SwiftSubclass end ***
// CHECK: SwiftSubclass It's better to burn out than to fade away.
// CHECK-LABEL: *** SwiftClientSubclass init ***
// CHECK: SwiftClientSubclass.init()
// CHECK: ImplClass.init()
// CHECK-RESILIENCE-LABEL: *** SwiftResilientStoredClientSubclass #1 ***
// CHECK-RESILIENCE: SwiftResilientStoredClientSubclass.mirror = nil
// CHECK-RESILIENCE: SwiftResilientStoredClientSubclass.afterMirrorProperty = 0
// CHECK-RESILIENCE: SwiftResilientStoredClientSubclass.mirror2 = nil
// CHECK-RESILIENCE: SwiftResilientStoredClientSubclass.afterMirrorProperty2 = 1
// CHECK-RESILIENCE-LABEL: *** SwiftResilientStoredClientSubclass #2 ***
// CHECK-RESILIENCE: SwiftResilientStoredClientSubclass.mirror = nil
// CHECK-RESILIENCE: SwiftResilientStoredClientSubclass.afterMirrorProperty = 42
// CHECK-RESILIENCE: SwiftResilientStoredClientSubclass.mirror2 = nil
// CHECK-RESILIENCE: SwiftResilientStoredClientSubclass.afterMirrorProperty2 = 43
// CHECK-LABEL: *** SwiftClientSubclass #1 ***
// CHECK: SwiftClientSubclass.someMethod() = SwiftClientSubclass
// CHECK: SwiftClientSubclass.implProperty = 0
// CHECK: SwiftClientSubclass.defaultIntProperty = 17
// CHECK: SwiftClientSubclass.description = ImplClass(implProperty: 0, object: objc_implementation.LastWords)
// CHECK: SwiftClientSubclass.otherProperty = 2
// CHECK-LABEL: *** SwiftClientSubclass #2 ***
// CHECK: SwiftClientSubclass.someMethod() = SwiftClientSubclass
// CHECK: SwiftClientSubclass.implProperty = 42
// CHECK: SwiftClientSubclass.defaultIntProperty = 17
// CHECK: SwiftClientSubclass.description = ImplClass(implProperty: 42, object: objc_implementation.LastWords)
// CHECK: SwiftClientSubclass.otherProperty = 2
// CHECK-LABEL: *** SwiftClientSubclass #3 ***
// CHECK: SwiftClientSubclass.someMethod() = SwiftClientSubclass
// CHECK: SwiftClientSubclass.implProperty = 3
// CHECK: SwiftClientSubclass.defaultIntProperty = 17
// CHECK: SwiftClientSubclass.description = ImplClass(implProperty: 3, object: objc_implementation.LastWords)
// CHECK: SwiftClientSubclass.otherProperty = 9
// CHECK-LABEL: *** SwiftClientSubclass end ***
// CHECK: SwiftClientSubclass It's better to burn out than to fade away.

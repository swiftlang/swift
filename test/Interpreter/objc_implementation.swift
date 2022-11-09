// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/objc_implementation.h -D TOP_LEVEL_CODE -swift-version 5) %s | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// A class whose deallocation is detectable, so we can verify that deinits
// are correctly synthesized.
class LastWords {
  var text: String
  
  init(text: String) {
    self.text = text
  }
  
  deinit {
    print(text, "It's better to burn out than to fade away.")
  }
}

@_objcImplementation extension ImplClass {
  @objc override init() {
    self.implProperty = 0
    self.object = LastWords(text: String(describing: type(of: self)))
    super.init()
  }

  @objc var implProperty: Int
  final var object: LastWords

  @objc class func runTests() {
    do {
      let impl = ImplClass()
      print("someMethod =", impl.someMethod())
      print("implProperty =", impl.implProperty)
      impl.implProperty = 42
      print("implProperty =", impl.implProperty)
      print("description =", impl.description)
    }

    do {
      let swiftSub = SwiftSubclass()
      print("someMethod =", swiftSub.someMethod())
      print("implProperty =", swiftSub.implProperty)
      swiftSub.implProperty = 42
      print("implProperty =", swiftSub.implProperty)

      print("otherProperty =", swiftSub.otherProperty)
      swiftSub.otherProperty = 13
      print("otherProperty =", swiftSub.otherProperty)
      print("implProperty =", swiftSub.implProperty)
    }
  }

  @objc func someMethod() -> String { "ImplClass.someMethod()" }

  open override var description: String {
    "ImplClass(implProperty: \(implProperty), object: \(object))"
  }
}

class SwiftSubclass: ImplClass {
  @objc var otherProperty: Int = 1

  override init() {
    super.init()
  }

  override func someMethod() -> String { "SwiftSubclass.someMethod()" }
}

// `#if swift` to ignore the inactive branch's contents
#if swift(>=5.0) && TOP_LEVEL_CODE
ImplClass.runTests()
// CHECK: someMethod = ImplClass.someMethod()
// CHECK: implProperty = 0
// CHECK: implProperty = 42
// CHECK: description = ImplClass(implProperty: 42, object: main.LastWords)
// CHECK: ImplClass It's better to burn out than to fade away.
// CHECK: someMethod = SwiftSubclass.someMethod()
// CHECK: implProperty = 0
// CHECK: implProperty = 42
// CHECK: otherProperty = 1
// CHECK: otherProperty = 13
// CHECK: implProperty = 42
// CHECK: SwiftSubclass It's better to burn out than to fade away.
#endif

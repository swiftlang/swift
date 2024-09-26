// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/objc_implementation.h -D TOP_LEVEL_CODE -swift-version 5 -enable-experimental-feature CImplementation -target %target-stable-abi-triple) %s | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_CImplementation

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
    print("ImplClass.init()")
    self.implProperty = 0
    self.object = LastWords(text: String(describing: type(of: self)))
    super.init()
  }

  @objc var implProperty: Int
  final var object: LastWords
  final weak var defaultNilProperty: AnyObject?
  @objc var defaultIntProperty: Int = 17

  @objc class func runTests() {
    implFunc(2023 - 34)

    do {
      print("*** ImplClass init ***")
      let impl = ImplClass()
      impl.testSelf()
      print("*** ImplClass end ***")
    }

    do {
      print("*** SwiftSubclass init ***")
      let swiftSub = SwiftSubclass()
      swiftSub.testSelf()
      print("*** SwiftSubclass end ***")
    }
  }

  @objc func testSelf() {
#if RESILIENCE
    let resilientImpl = Self.makeResilientImpl()
    resilientImpl.printSelf(withLabel: 1)
    resilientImpl.mutate()
    resilientImpl.printSelf(withLabel: 2)
#endif

    self.printSelf(withLabel: 1)
    self.implProperty = 42
    self.printSelf(withLabel: 2)
  }

  @objc func printSelf(withLabel label: CInt) {
    let type = type(of: self)
    print("*** \(type) #\(label) ***")
    print("\(type).someMethod() =", self.someMethod())
    print("\(type).implProperty =", self.implProperty)
    print("\(type).defaultIntProperty =", self.defaultIntProperty)
    print("\(type).description =", self.description)
  }

  @objc func someMethod() -> String { "ImplClass" }

  @objc class func makeResilientImpl() -> ImplClassWithResilientStoredProperty {
#if RESILIENCE
    ImplClassWithResilientStoredProperty()
#else
    fatalError()
#endif
  }

  open override var description: String {
    "ImplClass(implProperty: \(implProperty), object: \(object))"
  }
}

class SwiftSubclass: ImplClass {
  @objc var otherProperty: Int = 1

  override init() {
    print("SwiftSubclass.init()")
    super.init()
  }

  override func someMethod() -> String { "SwiftSubclass" }

  override func testSelf() {
    super.testSelf()
    self.otherProperty = 13
    self.printSelf(withLabel: 3)
  }

  override func printSelf(withLabel label: CInt) {
    super.printSelf(withLabel: label)
    let type = type(of: self)
    print("\(type).otherProperty =", self.otherProperty)
  }

#if RESILIENCE
  override class func makeResilientImpl() -> ImplClassWithResilientStoredProperty {
    SwiftResilientStoredSubclass()
  }
#endif
}

#if RESILIENCE
@_objcImplementation extension ImplClassWithResilientStoredProperty {
  final var mirror: Mirror?
  final var afterMirrorProperty: Int

  public override init() {
    self.mirror = nil
    self.afterMirrorProperty = 0
  }

  @objc func printSelf(withLabel label: CInt) {
    let type = type(of: self)
    print("*** \(type) #\(label) ***")
    print("\(type).mirror =", self.mirror as Any)
    print("\(type).afterMirrorProperty =", self.afterMirrorProperty)
  }

  @objc func mutate() {
    self.afterMirrorProperty = 42
  }
}

extension SwiftSubclass {
  class SwiftResilientStoredSubclass: ImplClassWithResilientStoredProperty {
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

@_objcImplementation @_cdecl("implFunc") public func implFunc(_ param: Int32) {
  print("implFunc(\(param))")
}

// `#if swift` to ignore the inactive branch's contents
#if swift(>=5.0) && TOP_LEVEL_CODE
ImplClass.runTests()
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
// CHECK: ImplClass.description = ImplClass(implProperty: 0, object: main.LastWords)
// CHECK-LABEL: *** ImplClass #2 ***
// CHECK: ImplClass.someMethod() = ImplClass
// CHECK: ImplClass.implProperty = 42
// CHECK: ImplClass.defaultIntProperty = 17
// CHECK: ImplClass.description = ImplClass(implProperty: 42, object: main.LastWords)
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
// CHECK: SwiftSubclass.description = ImplClass(implProperty: 0, object: main.LastWords)
// CHECK: SwiftSubclass.otherProperty = 1
// CHECK-LABEL: *** SwiftSubclass #2 ***
// CHECK: SwiftSubclass.someMethod() = SwiftSubclass
// CHECK: SwiftSubclass.implProperty = 42
// CHECK: SwiftSubclass.defaultIntProperty = 17
// CHECK: SwiftSubclass.description = ImplClass(implProperty: 42, object: main.LastWords)
// CHECK: SwiftSubclass.otherProperty = 1
// CHECK-LABEL: *** SwiftSubclass #3 ***
// CHECK: SwiftSubclass.someMethod() = SwiftSubclass
// CHECK: SwiftSubclass.implProperty = 42
// CHECK: SwiftSubclass.defaultIntProperty = 17
// CHECK: SwiftSubclass.description = ImplClass(implProperty: 42, object: main.LastWords)
// CHECK: SwiftSubclass.otherProperty = 13
// CHECK-LABEL: *** SwiftSubclass end ***
// CHECK: SwiftSubclass It's better to burn out than to fade away.
#endif

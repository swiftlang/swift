// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - -disable-objc-attr-requires-foundation-module | %FileCheck %s


// CHECK: internal struct GenericStruct<T> {
internal struct GenericStruct<T> {
  // CHECK-NEXT: internal let _: T
  internal let x: T

  // CHECK-NOT: internal func shouldNotAppear()
  internal func shouldNotAppear() {}

// CHECK: }
}

// CHECK: internal struct UsedFromGenericEnum {
// CHECK-NEXT: }
internal struct UsedFromGenericEnum {}

// CHECK: internal enum GenericEnum<T> {
internal enum GenericEnum<T> {
  // CHECK-NEXT: case a(T)
  // CHECK-NEXT: case c(T)
  case a(T), c(T)

  // CHECK-NEXT: case b({{.*}}UsedFromGenericEnum)
  case b(UsedFromGenericEnum)

  // CHECK-NOT: internal func notExposed()
  internal func notExposed() {}

// CHECK-NEXT: }
}

// CHECK: private struct OnlyReferencedFromPrivateType {
// CHECK-NEXT: }
private struct OnlyReferencedFromPrivateType {}

// CHECK: @_fixed_layout public class Foo {
@_fixed_layout
public class Foo {
  // CHECK-NEXT: internal typealias MyCustomInt = {{.*}}Int
  internal typealias MyCustomInt = Int
  // CHECK-NEXT: internal typealias MyCustomBool = {{.*}}Bool
  internal typealias MyCustomBool = Bool
  // CHECK-NEXT: internal typealias MyCustomTuple = ({{.*}}MyCustomInt, {{.*}}MyCustomBool)
  internal typealias MyCustomTuple = (MyCustomInt, MyCustomBool)
  // CHECK-NEXT: internal typealias MyCustomString = {{.*}}String
  internal typealias MyCustomString = String
  // CHECK-NOT: internal typealias NotReferenced = {{.*}}String
  internal typealias NotReferenced = String

  // CHECK: private class Bar {
  private class Bar {
    // CHECK-NEXT: private let _: {{.*}}MyCustomInt
    private let x: MyCustomInt

    // CHECK-NEXT: private let _: [{{.*}}MyCustomTuple]
    private let y: [MyCustomTuple]

    // CHECK-NEXT: private let _: {{.*}}OnlyReferencedFromPrivateType
    private let z: OnlyReferencedFromPrivateType

    // CHECK-NOT: internal func shouldNotAppear(_ type: {{.*}}NotReferenced)
    internal func shouldNotAppear(_ type: NotReferenced) {}

    // CHECK-NOT: init
    init() {
      self.x = 0
      self.y = []
      self.z = OnlyReferencedFromPrivateType()
    }

  // CHECK: }
  }

  // CHECK-NEXT: private let _: {{.*}}Bar
  private let privateLet: Bar

  // CHECK-NEXT: internal var _: ({{.*}}MyCustomInt, {{.*}}MyCustomInt)
  internal var internalVar: (MyCustomInt, MyCustomInt)

  // CHECK-NEXT: private var _: {{.*}}GenericStruct<{{.*}}MyCustomString>
  private var privateVar: GenericStruct<MyCustomString>

  // CHECK-NEXT: private var privateVarHasInitialValue: {{.*}}MyCustomInt = 0
  private var privateVarHasInitialValue: MyCustomInt = 0

  // CHECK-NEXT: public var publicVar: {{.*}}Int
  public var publicVar: Int

  // CHECK-NEXT: private var _: {{.*}}GenericEnum<{{.*}}MyCustomInt>
  private var privateEnumVar: GenericEnum<MyCustomInt>

  // CHECK-NOT: {{^}}  init
  init() {
    self.privateLet = Bar()
    self.internalVar = (0, 0)
    self.publicVar = 0
    self.privateVar = GenericStruct<MyCustomString>(x: "Hello")
    self.privateEnumVar = .a(0)
  }

// CHECK: }
}

// CHECK: private protocol InheritedProtocol {
// CHECK-NEXT: }
private protocol InheritedProtocol {
}

// CHECK: private protocol UsableAsExistential : InheritedProtocol {
// CHECK-NEXT: }
private protocol UsableAsExistential: InheritedProtocol {
}

// CHECK: @objc private protocol ObjCUsableAsExistential {
// CHECK-NEXT: }
@objc private protocol ObjCUsableAsExistential {
}

// CHECK: private protocol ClassUsableAsExistential : AnyObject {
// CHECK-NEXT: }
private protocol ClassUsableAsExistential: class {
  func foo()
}

// CHECK: @objc @_fixed_layout public class ContainsExistentials {
@objc
@_fixed_layout
public class ContainsExistentials {
// CHECK-NEXT: private var _: UsableAsExistential
  private var existential: UsableAsExistential

// CHECK-NEXT: private var _: ObjCUsableAsExistential
  private var objcExistential: ObjCUsableAsExistential

// CHECK-NEXT: private var _: ClassUsableAsExistential
  private var classExistential: ClassUsableAsExistential

// CHECK-NOT: private init(a: UsableAsExistential, b: ObjCUsableAsExistential, c: ClassUsableAsExistential)
  private init(a: UsableAsExistential, b: ObjCUsableAsExistential, c: ClassUsableAsExistential) {
    existential = a
    objcExistential = b
    classExistential = c
  }
// CHECK: }
}

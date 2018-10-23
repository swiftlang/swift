// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s
// RUN: %FileCheck %s < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - | %FileCheck %s


// CHECK: internal struct GenericStruct<T> {
internal struct GenericStruct<T> {
  // CHECK-NEXT: internal let _: T
  internal let x: T

  // CHECK-NOT: internal func shouldNotAppear()
  internal func shouldNotAppear() {}

// CHECK: }
}

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

  // CHECK-NOT: {{^}}  init
  init() {
    self.privateLet = Bar()
    self.internalVar = (0, 0)
    self.publicVar = 0
    self.privateVar = GenericStruct<MyCustomString>(x: "Hello")
  }

// CHECK: }
}

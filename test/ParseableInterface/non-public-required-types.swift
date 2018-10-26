// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s -check-prefix COMMON -check-prefix NONRESILIENT < %t.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s -check-prefix COMMON -implicit-check-not NONRESILIENT< %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s -check-prefix COMMON -check-prefix NONRESILIENT

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - -disable-objc-attr-requires-foundation-module | %FileCheck %s -check-prefix COMMON -implicit-check-not NONRESILIENT


// COMMON: internal struct GenericStruct<T> {
internal struct GenericStruct<T> {
  // COMMON-NEXT: internal let _: T
  internal let x: T

  // COMMON-NOT: internal func shouldNotAppear()
  internal func shouldNotAppear() {}

// COMMON: }
}

// COMMON: internal struct UsedFromGenericEnum {
// COMMON-NEXT: }
internal struct UsedFromGenericEnum {}

// COMMON: internal enum GenericEnum<T> {
internal enum GenericEnum<T> {
  // COMMON-NEXT: case a(T)
  // COMMON-NEXT: case c(T)
  case a(T), c(T)

  // COMMON-NEXT: case b({{.*}}UsedFromGenericEnum)
  case b(UsedFromGenericEnum)

  // COMMON-NOT: internal func notExposed()
  internal func notExposed() {}

// COMMON-NEXT: }
}

// COMMON: private struct OnlyReferencedFromPrivateType {
// COMMON-NEXT: }
private struct OnlyReferencedFromPrivateType {}

// COMMON: @_fixed_layout public class Foo {
@_fixed_layout
public class Foo {
  // COMMON-NEXT: internal typealias MyCustomInt = {{.*}}Int
  internal typealias MyCustomInt = Int
  // COMMON-NEXT: internal typealias MyCustomBool = {{.*}}Bool
  internal typealias MyCustomBool = Bool
  // COMMON-NEXT: internal typealias MyCustomTuple = ({{.*}}MyCustomInt, {{.*}}MyCustomBool)
  internal typealias MyCustomTuple = (MyCustomInt, MyCustomBool)
  // COMMON-NEXT: internal typealias MyCustomString = {{.*}}String
  internal typealias MyCustomString = String
  // COMMON-NOT: internal typealias NotReferenced = {{.*}}String
  internal typealias NotReferenced = String

  // COMMON: private class Bar {
  private class Bar {
    // COMMON-NEXT: private let _: {{.*}}MyCustomInt
    private let x: MyCustomInt

    // COMMON-NEXT: private let _: [{{.*}}MyCustomTuple]
    private let y: [MyCustomTuple]

    // COMMON-NEXT: private let _: {{.*}}OnlyReferencedFromPrivateType
    private let z: OnlyReferencedFromPrivateType

    // COMMON-NOT: internal func shouldNotAppear(_ type: {{.*}}NotReferenced)
    internal func shouldNotAppear(_ type: NotReferenced) {}

    // COMMON-NOT: init
    init() {
      self.x = 0
      self.y = []
      self.z = OnlyReferencedFromPrivateType()
    }

  // COMMON: }
  }

  // COMMON-NEXT: private let _: {{.*}}Bar
  private let privateLet: Bar

  // COMMON-NEXT: internal var _: ({{.*}}MyCustomInt, {{.*}}MyCustomInt)
  internal var internalVar: (MyCustomInt, MyCustomInt)

  // COMMON-NEXT: private var _: {{.*}}GenericStruct<{{.*}}MyCustomString>
  private var privateVar: GenericStruct<MyCustomString>

  // COMMON-NEXT: private var privateVarHasInitialValue: {{.*}}MyCustomInt = 0
  private var privateVarHasInitialValue: MyCustomInt = 0

  // COMMON-NEXT: public var publicVar: {{.*}}Int
  public var publicVar: Int

  // COMMON-NEXT: private var _: {{.*}}GenericEnum<{{.*}}MyCustomInt>
  private var privateEnumVar: GenericEnum<MyCustomInt>

  // COMMON-NOT: {{^}}  init
  init() {
    self.privateLet = Bar()
    self.internalVar = (0, 0)
    self.publicVar = 0
    self.privateVar = GenericStruct<MyCustomString>(x: "Hello")
    self.privateEnumVar = .a(0)
  }

// COMMON: }
}

// NONRESILIENT: private protocol InheritedProtocol {
// NONRESILIENT-NEXT: }
private protocol InheritedProtocol {
}

// NONRESILIENT: private protocol UsableAsExistential : InheritedProtocol {
// NONRESILIENT-NEXT: }
private protocol UsableAsExistential: InheritedProtocol {
}

// NONRESILIENT: @objc private protocol ObjCUsableAsExistential {
// NONRESILIENT-NEXT: }
@objc private protocol ObjCUsableAsExistential {
}

// NONRESILIENT: private protocol ClassUsableAsExistential : AnyObject {
// NONRESILIENT-NEXT: }
private protocol ClassUsableAsExistential: class {
  func foo()
}

// NONRESILIENT: internal protocol CompositionA {
// NONRESILIENT-NEXT: }
internal protocol CompositionA {
}

// NONRESILIENT: internal protocol CompositionB {
// NONRESILIENT-NEXT: }
internal protocol CompositionB {
}

// COMMON-NOT: internal protocol NeverReferenced {
// COMMON-NOT: }
internal protocol NeverReferenced {
}

// COMMON: @objc public class ContainsExistentials {
@objc public class ContainsExistentials {
// NONRESILIENT: private var _: UsableAsExistential
  private var existential: UsableAsExistential

// NONRESILIENT-NEXT: private var _: ObjCUsableAsExistential
  private var objcExistential: ObjCUsableAsExistential

// NONRESILIENT-NEXT: private var _: ClassUsableAsExistential
  private var classExistential: ClassUsableAsExistential

// NONRESILIENT-NEXT: private var _: CompositionA & CompositionB
  private var composition: CompositionA & CompositionB

// COMMON-NOT: private init
  private init(a: UsableAsExistential, b: ObjCUsableAsExistential,
               c: ClassUsableAsExistential, d: CompositionA & CompositionB) {
    existential = a
    objcExistential = b
    classExistential = c
    composition = d
  }
// COMMON: }
}

// NONRESILIENT: internal protocol GenericConstraint {
// NONRESILIENT-NEXT: }
internal protocol GenericConstraint {
}

// NONRESILIENT: internal class Superclass {
// NONRESILIENT-NEXT: }
internal class Superclass {
}

// NONRESILIENT: internal class HasConstraint<T> : {{.*}}Superclass where T : GenericConstraint {
// NONRESILIENT-NEXT: }
internal class HasConstraint<T: GenericConstraint> : Superclass {
}

// NONRESILIENT: internal struct SatisfiesConstraint : GenericConstraint {
// NONRESILIENT-NEXT: }
internal struct SatisfiesConstraint: GenericConstraint {
}

// COMMON: public struct HasGenericClassMember {
public struct HasGenericClassMember {
  // NONRESILIENT: internal let _: {{.*}}HasConstraint<{{.*}}SatisfiesConstraint>
  internal let member: HasConstraint<SatisfiesConstraint>
// COMMON: }
}

// NONRESILIENT: internal protocol ExtensionConformance {
internal protocol ExtensionConformance {
  // NONRESILIENT-NOT: associatedtype MyType
  associatedtype MyType
// NONRESILIENT: }
}

// NONRESILIENT internal class GetsConformanceThroughExtension {
// NONRESILIENT: }
internal class GetsConformanceThroughExtension {
}

// NONRESILIENT: extension GetsConformanceThroughExtension : ExtensionConformance {
extension GetsConformanceThroughExtension: ExtensionConformance {
  // COMMON-NOT: typealias MyType = {{.*}}Int
  typealias MyType = Int
// NONRESILIENT: }
}

// COMMON: public struct HasClassMemberWithExtensionConformance {
public struct HasClassMemberWithExtensionConformance {
  // NONRESILIENT: internal let _: {{.*}}GetsConformanceThroughExtension
  let member: GetsConformanceThroughExtension
// COMMON: }
}

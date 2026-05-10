// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=default -print-implicit-attrs -module-to-print=VirtualMethods -I %S/Inputs -source-filename=x | %FileCheck %s

// CHECK: struct Base {
// CHECK-NEXT:   @available(*, unavailable, message: "constructors of abstract C++ classes are unavailable in Swift")
// CHECK-NEXT:   init()
// CHECK-NEXT:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK-NEXT:   mutating func foo()
// CHECK-NEXT: }
//
// CHECK: struct Derived<CInt> {
// CHECK-NEXT:  init()
// CHECK-NEXT:  mutating func foo()
// CHECK-NEXT:  mutating func callMe()
// CHECK-NEXT: }
//
// CHECK: struct Unused<CInt> {
// CHECK-NEXT:  init()
// CHECK-NEXT:  @_addressableSelf mutating func foo()
// CHECK-NEXT: }

// CHECK:       struct Base2 {
// CHECK-NEXT:    @available(*, unavailable, message: "constructors of abstract C++ classes are unavailable in Swift")
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK-NEXT:    @_addressableSelf mutating func f() -> Int32
// CHECK-NEXT:  }
//
// CHECK:       struct Derived2 {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    @_addressableSelf mutating func f() -> Int32
// CHECK-NEXT:  }
//
// CHECK:       struct Base3 {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    @_addressableSelf mutating func f() -> Int32
// CHECK-NEXT:  }
//
// CHECK:       struct Derived3 {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    @_addressableSelf mutating func f() -> Int32
// CHECK-NEXT:  }
//
// CHECK:       struct Derived4 {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    mutating func f() -> Int32
// CHECK-NEXT:  }
//
// CHECK:       struct DerivedFromDerived2 {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    mutating func f() -> Int32
// CHECK-NEXT:  }
//
// CHECK:       struct VirtualRenamedBase {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    @_addressableSelf func swiftName() -> Int32
// CHECK-NEXT:  }
// CHECK:       struct VirtualRenamedInherited {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    func swiftName() -> Int32
// CHECK-NEXT:  }
// CHECK:       struct VirtualRenamedOverridden {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    @_addressableSelf func swiftName() -> Int32
// CHECK-NEXT:  }
//
// CHECK:       struct PureVirtualRenamedBase {
// CHECK-NEXT:    @available(*, unavailable, message: "constructors of abstract C++ classes are unavailable in Swift")
// CHECK-NEXT:    init()
// CHECK-NEXT:    @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK-NEXT:    @_addressableSelf func swiftName() -> Int32
// CHECK-NEXT:  }
// CHECK:       struct PureVirtualRenamedOverridden {
// CHECK-NEXT:    init()
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    @_addressableSelf func swiftName() -> Int32
// CHECK-NEXT:  }
//
// CHECK: struct VirtualNonAbstractBase {
// CHECK-NEXT:  init()
// CHECK-NEXT:  func nonAbstractMethod()
// CHECK-NEXT: }

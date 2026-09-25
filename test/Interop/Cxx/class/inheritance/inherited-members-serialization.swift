// Inlinable code that uses members inherited from a C++ base class, or calls
// `super` on a foreign reference type, references functions that the importer
// synthesizes and that are not in the C++ header. Make sure a client can
// deserialize those references.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -O -module-name Library %t/Library.swift -emit-module-path %t/Library.swiftmodule -I %t/Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t -I %t/Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple | %FileCheck %s

//--- Inputs/module.modulemap
module Inherited {
  header "inherited.h"
  requires cplusplus
}

//--- Inputs/inherited.h
struct Base {
  int field = 1;
  int add(int amount) { return field += amount; }
  int read() const { return field; }
  int operator[](int i) const { return field + i; }
  int operator()(int i) const { return field * i; }
};
struct Derived : Base {};
struct TwiceDerived : Derived {};

// Both overloads get a __synthesizedBaseCall_ helper for the derived type, so
// the reference to the non-const one must not resolve to the const one. (Using
// both helpers in one module already fails with "function type mismatch",
// without serialization.)
struct OverloadBase {
  int value = 1;
  int get() { return value += 10; }
  int get() const { return value; }
};
struct OverloadDerived : OverloadBase {};

// Setting a field of the second base casts to that base at a nonzero offset.
struct OtherBase {
  int other = 2;
};
struct MultiDerived : Base, OtherBase {};

// Class template specializations in a namespace, as in
// https://github.com/swiftlang/swift/issues/74578.
namespace future {
template <class T>
struct FutureBase {
  T errorValue = 7;
  T error() const { return errorValue; }
};
template <class T>
struct Future : FutureBase<T> {};
template <class T>
struct FutureWrapper : Future<T> {};
} // namespace future
using IntFuture = future::Future<int>;
using IntFutureWrapper = future::FutureWrapper<int>;

struct MutableSubscriptBase {
  int values[2] = {1, 2};
  int &operator[](int i) { return values[i]; }
};
struct MutableSubscriptDerived : MutableSubscriptBase {};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) VirtualBase {
  virtual int virtualMethod() const { return 100; }
  virtual ~VirtualBase() = default;
};
struct VirtualDerived : VirtualBase {
  int virtualMethod() const override { return 200; }
};
struct VirtualDerivedNoOverride : VirtualBase {};
struct VirtualLeaf : VirtualDerivedNoOverride {
  int virtualMethod() const override { return 300; }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) ReferenceBase {
  int field = 3;
  int get() const { return field; }
};
struct ReferenceDerived : ReferenceBase {};

//--- Library.swift
import Inherited

@inlinable public func callMethod(_ value: inout TwiceDerived) -> CInt {
  value.add(1)
}
@inlinable public func captureMethod(_ value: Derived) -> () -> CInt {
  value.read
}
@inlinable public func callOperator(_ value: Derived) -> CInt {
  value(2)
}
@inlinable public func getField(_ value: TwiceDerived) -> CInt {
  value.field
}
@inlinable public func setField(_ value: inout Derived) {
  value.field = 42
}
@inlinable public func getSubscript(_ value: Derived) -> CInt {
  value[1]
}
@inlinable public func setSubscript(_ value: inout MutableSubscriptDerived) {
  value[1] = 42
}
@inlinable public func callMutatingOverload(_ value: inout OverloadDerived) -> CInt {
  value.getMutating()
}
@inlinable public func setSecondBaseField(_ value: inout MultiDerived) {
  value.other = 42
}
@inlinable public func callTemplateBase(_ value: IntFuture) -> CInt {
  value.error()
}
@inlinable public func callTwiceTemplateBase(_ value: IntFutureWrapper) -> CInt {
  value.error()
}
@inlinable public func callReferenceMethod(_ value: ReferenceDerived) -> CInt {
  value.get()
}
@inlinable public func getReferenceField(_ value: ReferenceDerived) -> CInt {
  value.field
}
@inlinable public func setReferenceField(_ value: ReferenceDerived) {
  value.field = 42
}

extension VirtualDerived {
  @inlinable public func callSuper() -> CInt {
    super.virtualMethod()
  }
  @inlinable public func captureSuper() -> () -> CInt {
    super.virtualMethod
  }
}

extension VirtualLeaf {
  @inlinable public func callInheritedSuper() -> CInt {
    super.virtualMethod()
  }
}

//--- Client.swift
import Inherited
import Library

// CHECK-LABEL: sil @$s6Client10testMethod
// CHECK: function_ref @{{.*}}TwiceDerived{{.*}}__synthesizedBaseCall_add
// CHECK: end sil function
public func testMethod(_ value: inout TwiceDerived) -> CInt {
  callMethod(&value)
}

// CHECK-LABEL: sil @$s6Client18testCapturedMethod
// CHECK: function_ref @{{.*}}Derived{{.*}}__synthesizedBaseCall_read
// CHECK: end sil function
public func testCapturedMethod(_ value: Derived) -> CInt {
  captureMethod(value)()
}

// CHECK-LABEL: sil @$s6Client12testOperator
// CHECK: function_ref @{{.*}}Derived{{.*}}__synthesizedBaseCall_operatorCall
// CHECK: end sil function
public func testOperator(_ value: Derived) -> CInt {
  callOperator(value)
}

// CHECK-LABEL: sil @$s6Client12testGetField
// CHECK: function_ref @{{.*}}TwiceDerived{{.*}}__synthesizedBaseGetterAccessor_field
// CHECK: end sil function
public func testGetField(_ value: TwiceDerived) -> CInt {
  getField(value)
}

// CHECK-LABEL: sil @$s6Client12testSetField
// CHECK: function_ref @{{.*}}__swift_interopStaticCast__ZTS7Derived_to__ZTS4Base
// CHECK: end sil function
public func testSetField(_ value: inout Derived) {
  setField(&value)
}

// CHECK-LABEL: sil @$s6Client16testGetSubscript
// CHECK: function_ref @{{.*}}Derived{{.*}}__synthesizedBaseCall_operatorSubscript
// CHECK: end sil function
public func testGetSubscript(_ value: Derived) -> CInt {
  getSubscript(value)
}

// CHECK-LABEL: sil @$s6Client16testSetSubscript
// CHECK: function_ref @{{.*}}MutableSubscriptDerived{{.*}}__synthesizedBaseCall_operator
// CHECK: end sil function
public func testSetSubscript(_ value: inout MutableSubscriptDerived) {
  setSubscript(&value)
}

// Both `super` calls dispatch statically to the base class implementation.
// CHECK-LABEL: sil @$s6Client9testSuper
// CHECK: [[STATIC_CALL:%.*]] = function_ref @$sSo11VirtualBaseV26__staticCall_virtualMethod
// CHECK: apply [[STATIC_CALL]]
// CHECK: apply [[STATIC_CALL]]
// CHECK: end sil function
public func testSuper(_ value: VirtualDerived) -> CInt {
  value.callSuper() + value.captureSuper()()
}

// CHECK-LABEL: sil @$s6Client20testMutatingOverload
// CHECK: function_ref @{{.*}}OverloadDerived{{.*}}__synthesizedBaseCall_get{{.*}} : $@convention(cxx_method) (@inout OverloadDerived) -> Int32
// CHECK: end sil function
public func testMutatingOverload(_ value: inout OverloadDerived) -> CInt {
  callMutatingOverload(&value)
}

// CHECK-LABEL: sil @$s6Client22testSetSecondBaseField
// CHECK: function_ref @{{.*}}__swift_interopStaticCast__ZTS12MultiDerived_to__ZTS9OtherBase
// CHECK: end sil function
public func testSetSecondBaseField(_ value: inout MultiDerived) {
  setSecondBaseField(&value)
}

// CHECK-LABEL: sil @$s6Client16testTemplateBase
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_error{{.*}} : $@convention(cxx_method) (@in_guaranteed future.Future<CInt>) -> Int32
// CHECK: end sil function
public func testTemplateBase(_ value: IntFuture) -> CInt {
  callTemplateBase(value)
}

// CHECK-LABEL: sil @$s6Client21testTwiceTemplateBase
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_error{{.*}} : $@convention(cxx_method) (@in_guaranteed future.FutureWrapper<CInt>) -> Int32
// CHECK: end sil function
public func testTwiceTemplateBase(_ value: IntFutureWrapper) -> CInt {
  callTwiceTemplateBase(value)
}

// Members inherited by a foreign reference type are used through an upcast to
// the base class, without a synthesized helper.
// CHECK-LABEL: sil @$s6Client19testReferenceMethod
// CHECK: upcast %0 to $ReferenceBase
// CHECK: function_ref @$sSo13ReferenceBaseV3get
// CHECK: end sil function
public func testReferenceMethod(_ value: ReferenceDerived) -> CInt {
  callReferenceMethod(value)
}

// CHECK-LABEL: sil @$s6Client21testGetReferenceField
// CHECK: [[BASE:%.*]] = upcast %0 to $ReferenceBase
// CHECK: ref_element_addr [[BASE]], #ReferenceBase.field
// CHECK: end sil function
public func testGetReferenceField(_ value: ReferenceDerived) -> CInt {
  getReferenceField(value)
}

// CHECK-LABEL: sil @$s6Client21testSetReferenceField
// CHECK: [[BASE:%.*]] = upcast %0 to $ReferenceBase
// CHECK: ref_element_addr [[BASE]], #ReferenceBase.field
// CHECK: end sil function
public func testSetReferenceField(_ value: ReferenceDerived) {
  setReferenceField(value)
}

// The parent of VirtualLeaf does not override virtualMethod, so `super` calls
// the implementation in VirtualBase.
// CHECK-LABEL: sil @$s6Client18testInheritedSuper
// CHECK: [[INHERITED_STATIC_CALL:%.*]] = function_ref @$sSo11VirtualBaseV26__staticCall_virtualMethod
// CHECK: apply [[INHERITED_STATIC_CALL]]
// CHECK: end sil function
public func testInheritedSuper(_ value: VirtualLeaf) -> CInt {
  value.callInheritedSuper()
}

// CHECK: sil {{.*}}[asmname "{{_ZNK11VirtualBase13virtualMethodEv|\?virtualMethod@VirtualBase@@UEBAHXZ}}"] {{.*}}@$sSo11VirtualBaseV26__staticCall_virtualMethod


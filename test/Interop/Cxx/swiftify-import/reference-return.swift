// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Cells
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-sil -o /dev/null -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Cells -enable-experimental-feature Lifetimes \
// RUN:   -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift -verify -eager-macro-checking
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Cells -enable-experimental-feature Lifetimes \
// RUN:   -strict-memory-safety -Xcc -Werror %t/test.swift -dump-macro-expansions 2>&1 | %FileCheck %s --match-full-lines --strict-whitespace --implicit-check-not __swiftmacro

// Wrappers borrowing from a C++ reference take it by address, so that the
// returned Ref points to the caller's value rather than to a temporary copy.
// RUN: %target-swift-frontend -emit-silgen -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Cells -enable-experimental-feature Lifetimes \
// RUN:   %t/test.swift -D SILGEN | %FileCheck %s --check-prefix=SIL
// SIL-DAG: sil {{.*}}@$sSC9constPick{{.*}} : $@convention(thin) (@in_guaranteed Int32) -> @lifetime(borrow address 0) @owned Ref<ConstCell<Int32>> {
// SIL-DAG: sil {{.*}}@$sSo7CounterV4cget{{.*}} : $@convention(method) (@in_guaranteed Counter) -> @lifetime(borrow address 0) @owned Ref<ConstCell<Int32>> {

// Without the Cells feature, no wrappers are generated for reference return values.
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature SafeInteropWrappers \
// RUN:   -strict-memory-safety -Xcc -Werror %t/test.swift -D NO_CELLS -dump-macro-expansions 2>&1 | %FileCheck %s --check-prefix=NO-CELLS --allow-empty
// NO-CELLS-NOT: __swiftmacro

// CHECK-LABEL:@__swiftmacro_So4pick15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_lifetime(&a) @_disfavoredOverload public func pick(_ a: inout CInt, _ b: inout CInt) -> Swift::Ref<Swift::Cell<CInt>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe pick(&a, &b)).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-LABEL:@__swiftmacro_So9constPick15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_lifetime(borrow a) @_disfavoredOverload public func constPick(_ a: borrowing @_addressable CInt) -> Swift::Ref<Swift::ConstCell<CInt>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe constPick(a)).assumingMemoryBound(to: Swift::ConstCell<CInt>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-LABEL:@__swiftmacro_So7CounterV3get15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_lifetime(&self) @_disfavoredOverload
// CHECK-NEXT:public mutating func get() -> Swift::Ref<Swift::Cell<CInt>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe get()).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-LABEL:@__swiftmacro_So7CounterV4cget15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_addressableSelf @_lifetime(borrow self) @_disfavoredOverload
// CHECK-NEXT:public borrowing func cget() -> Swift::Ref<Swift::ConstCell<CInt>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe cget()).assumingMemoryBound(to: Swift::ConstCell<CInt>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-LABEL:@__swiftmacro_So10counterRef15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_lifetime(&c) @_disfavoredOverload public func counterRef(_ c: inout Counter) -> Swift::Ref<Swift::Cell<Counter>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe counterRef(&c)).assumingMemoryBound(to: Swift::Cell<Counter>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-LABEL:@__swiftmacro_So10typedefRef15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_lifetime(&a) @_disfavoredOverload public func typedefRef(_ a: inout CInt) -> Swift::Ref<Swift::Cell<CInt>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe typedefRef(&a)).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-LABEL:@__swiftmacro_So9ContainerV5front15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_lifetime(&self) @_disfavoredOverload
// CHECK-NEXT:public mutating func front() -> Swift::Ref<Swift::Cell<Container.value_type>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe front()).assumingMemoryBound(to: Swift::Cell<Container.value_type>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-LABEL:@__swiftmacro_So9ContainerV6cfront15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_addressableSelf @_lifetime(borrow self) @_disfavoredOverload
// CHECK-NEXT:public borrowing func cfront() -> Swift::Ref<Swift::ConstCell<Container.value_type>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe cfront()).assumingMemoryBound(to: Swift::ConstCell<Container.value_type>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-LABEL:@__swiftmacro_So10asProperty15_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "getter:Counter.property(self:_:)") @_alwaysEmitIntoClient @inline(always) {{(@available\(.*\) )?}}@_lifetime(&c) @_disfavoredOverload
// CHECK-NEXT:public func asProperty(_ c: inout Counter) -> Swift::Ref<Swift::Cell<CInt>> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe asProperty(&c)).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------

//--- Inputs/module.modulemap
module RefReturn {
  header "ref-return.h"
  requires cplusplus
}

//--- Inputs/ref-return.h
#pragma once

struct Counter {
  int value = 0;
  int &get() [[clang::lifetimebound]] { return value; }
  const int &cget() const [[clang::lifetimebound]] { return value; }
  // No lifetime info: no wrapper.
  int &noLifetime() { return value; }
  // Operators are hidden behind subscripts and operators: no wrapper.
  int &operator[](int) [[clang::lifetimebound]] { return value; }
};

inline int &pick(int &a [[clang::lifetimebound]], int &b) { return a; }
inline const int &constPick(const int &a [[clang::lifetimebound]]) { return a; }
inline Counter &counterRef(Counter &c [[clang::lifetimebound]]) { return c; }

// Reference types behind typedefs.
typedef int &IntRef;
inline IntRef typedefRef(int &a [[clang::lifetimebound]]) { return a; }
struct Container {
  using value_type = int;
  using reference = value_type &;
  using const_reference = const value_type &;
  value_type v = 0;
  reference front() [[clang::lifetimebound]] { return v; }
  const_reference cfront() const [[clang::lifetimebound]] { return v; }
};

// Pointers may be null or refer to arrays: no wrapper.
inline int *pointer(int *a [[clang::lifetimebound]]) { return a; }

// Imported as a property accessor: no wrapper.
inline int &asProperty(Counter &c [[clang::lifetimebound]])
    __attribute__((swift_name("getter:Counter.property(self:)"))) {
  return c.value;
}

//--- test.swift
import RefReturn

@available(SwiftStdlib 6.2, *)
func test(_ c: inout Counter, _ x: inout CInt, _ y: inout CInt, _ k: inout Container) {
#if !NO_CELLS
  do {
    let r: Ref<Cell<CInt>> = pick(&x, &y)
    r.value.value = 3
  }
  do {
    let r: Ref<ConstCell<CInt>> = constPick(x)
    _ = r.value.value
  }
  do {
    let r: Ref<Cell<CInt>> = c.get()
    r.value.value += 1
  }
  do {
    let r: Ref<ConstCell<CInt>> = c.cget()
    _ = r.value.value
  }
  do {
    let r: Ref<Cell<Counter>> = counterRef(&c)
    _ = r.value.value.value
  }
  do {
    let r: Ref<Cell<CInt>> = typedefRef(&x)
    r.value.value = 4
  }
  do {
    let r: Ref<Cell<CInt>> = k.front()
    r.value.value = 5
  }
  do {
    let r: Ref<ConstCell<CInt>> = k.cfront()
    _ = r.value.value
  }
#endif
  // Unwrapped functions only have the original overload.
  let _: UnsafeMutablePointer<CInt> = unsafe c.noLifetime()
  let _: UnsafeMutablePointer<CInt>! = unsafe pointer(&x)
  let _: CInt = c[0]
  let _: UnsafeMutablePointer<CInt> = unsafe c.property
  // The originals behind typedefs are still callable.
  let _: IntRef = unsafe typedefRef(&x)
  let _: Container.reference = unsafe k.front()
}

#if !NO_CELLS && !SILGEN
// A non-addressable borrowing parameter has no stable address to refer to.
@available(SwiftStdlib 6.2, *)
@_lifetime(borrow x)
func forward(_ x: borrowing CInt) -> Ref<ConstCell<CInt>> { // expected-note {{it depends on the lifetime of argument 'x'}}
  return constPick(x) // expected-error {{lifetime-dependent value escapes its scope}} expected-note {{this use causes the lifetime-dependent value to escape}}
}
#endif

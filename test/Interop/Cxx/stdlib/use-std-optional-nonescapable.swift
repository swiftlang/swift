// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -I %t%{fs-sep}Inputs -emit-sil -verify %t%{fs-sep}test.swift -enable-experimental-feature Lifetimes -cxx-interoperability-mode=default -strict-memory-safety -diagnostic-style llvm
// RUN: %target-swift-frontend -I %t%{fs-sep}Inputs -emit-sil -verify %t%{fs-sep}escapes.swift -enable-experimental-feature Lifetimes -cxx-interoperability-mode=default -diagnostic-style llvm
// RUN: %target-swift-frontend -I %t%{fs-sep}Inputs -emit-silgen %t%{fs-sep}test.swift -enable-experimental-feature Lifetimes -cxx-interoperability-mode=default | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

// Constructors of a non-Escapable 'std::optional' copy the lifetime of the
// wrapped value, and the 'std::nullopt_t' tag carries none.
// CHECK-DAG: [clang std{{.*}}optional<View>.init] {{.*}} : $@convention(c) () -> @lifetime(immortal) @out
// CHECK-DAG: [clang std{{.*}}optional<View>.init] {{.*}} : $@convention(c) ({{.*}}nullopt_t) -> @lifetime(immortal) @out
// The constructor synthesized for 'CxxOptional' copies the wrapped value.
// CHECK-DAG: [clang std{{.*}}optional<View>.init] {{.*}} : $@convention(c) (@in_guaranteed View) -> @lifetime(copy 0) @out
// 'pointee' projects the wrapped value out of the optional.
// CHECK-DAG: @{{.*}}optionalView{{.*}}7pointeeSo4ViewVvg : $@convention(method) (@in_guaranteed std{{.*}}optional<View>) -> @lifetime(borrow address_for_deps 0) @owned View
// An Escapable optional has no dependencies.
// CHECK-DAG: [clang std{{.*}}optional<CInt>.init] {{.*}} : $@convention(c) (@in_guaranteed Int32) -> @out std{{.*}}optional<CInt>

// Unannotated 'operator*' and 'operator[]' returning a reference to a
// non-Escapable element are imported with addressors.
// CHECK-DAG: @{{.*}}6HolderV7pointeeSo4ViewVvlu : $@convention(method) (@guaranteed Holder) -> UnsafePointer<View>
// CHECK-DAG: @{{.*}}6HolderVySo4ViewVs5Int32Vcilu : $@convention(method) (Int32, @guaranteed Holder) -> UnsafePointer<View>

//--- Inputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
  export *
}

//--- Inputs/test.h
#include <optional>

#define SWIFT_NONESCAPABLE __attribute__((swift_attr("~Escapable")))

struct SWIFT_NONESCAPABLE View {
  const int *p;
};

inline View makeView(const int &x [[clang::lifetimebound]]) { return {&x}; }

using OptionalView = std::optional<View>;
using OptionalInt = std::optional<int>;

struct SWIFT_NONESCAPABLE Holder {
  View v;
  const View &operator*() const { return v; }
  const View &operator[](int) const { return v; }
};

// Inherited operators are called through synthesized forwarding methods, which
// need the same treatment as the operators they forward to. Some standard
// libraries declare the operators of 'std::optional' in a base class.
struct SWIFT_NONESCAPABLE DerivedHolder : Holder {};

struct SWIFT_NONESCAPABLE ViewIterator {
  const int *p;
  ViewIterator &operator++() { ++p; return *this; }
};

struct SWIFT_NONESCAPABLE DerivedViewIterator : ViewIterator {};

//--- test.swift
import Test
import CxxStdlib

func takeCxxOptional<T: CxxOptional & ~Escapable>(_ x: borrowing T) -> Bool
where T.Wrapped: ~Escapable {
  x.hasValue
}

@_lifetime(immortal)
func makeEmpty() -> OptionalView {
  OptionalView()
}

@_lifetime(immortal)
func makeNil() -> OptionalView {
  nil
}

@_lifetime(immortal)
func makeNullopt() -> OptionalView {
  OptionalView(std.nullopt)
}

@_lifetime(copy v)
func wrap(_ v: View) -> OptionalView {
  OptionalView(v)
}

@_lifetime(copy o)
func unwrap(_ o: borrowing OptionalView) -> View? {
  o.value
}

@_lifetime(copy o)
func toSwiftOptional(_ o: borrowing OptionalView) -> View? {
  Optional(fromCxx: o)
}

func hasValue(_ o: borrowing OptionalView) -> Bool {
  o.hasValue && takeCxxOptional(o)
}

@_lifetime(o: copy v)
func assign(_ o: inout OptionalView, _ v: View) {
  o.value = v
}

func escapableOptionalIsUnchanged() -> OptionalInt {
  var o = OptionalInt(42)
  o.value = nil
  return o
}

@_lifetime(borrow h)
func deref(_ h: borrowing Holder) -> View {
  h.pointee
}

@_lifetime(borrow h)
func subscripted(_ h: borrowing Holder) -> View {
  h[0]
}

@_lifetime(borrow h)
func derefInherited(_ h: borrowing DerivedHolder) -> View {
  h.pointee
}

@_lifetime(borrow h)
func subscriptedInherited(_ h: borrowing DerivedHolder) -> View {
  h[0]
}

// Not calling 'successor()' on the base type here: once the base has one, the
// derived type inherits it instead of getting its own.
@_lifetime(copy it)
func advanceInherited(_ it: borrowing DerivedViewIterator)
  -> DerivedViewIterator {
  it.successor()
}

//--- escapes.swift
import Test
import CxxStdlib

@_lifetime(immortal)
func escapesThroughInit() -> OptionalView {
  let x: Int32 = 42
  let v = makeView(x)
  return OptionalView(v)
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-4 {{it depends on the lifetime of variable 'x'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(immortal)
func escapesThroughValue() -> View? {
  let x: Int32 = 42
  let o = OptionalView(makeView(x))
  return o.value
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-4 {{it depends on the lifetime of variable 'x'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(immortal)
func escapesThroughFromCxx() -> View? {
  let x: Int32 = 42
  let o = OptionalView(makeView(x))
  return Optional(fromCxx: o)
  // expected-error @-1 {{lifetime-dependent value escapes its scope}}
  // expected-note @-4 {{it depends on the lifetime of variable 'x'}}
  // expected-note @-3 {{this use causes the lifetime-dependent value to escape}}
}

@_lifetime(o: copy o)
func escapesThroughValueSetter(_ o: inout OptionalView) {
  // expected-error @-1 {{lifetime-dependent variable 'o' escapes its scope}}
  let x: Int32 = 42
  // expected-note @-1 {{it depends on the lifetime of variable 'x'}}
  o.value = makeView(x)
} // expected-note {{this use causes the lifetime-dependent value to escape}}

@_lifetime(copy h)
func escapesThroughPointee(_ h: borrowing Holder) -> View {
  // expected-error @-1 {{lifetime-dependent variable 'h' escapes its scope}}
  // expected-note @-2 {{it depends on the lifetime of argument 'h'}}
  h.pointee
} // expected-note {{this use causes the lifetime-dependent value to escape}}

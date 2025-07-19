
// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %swift_src_root/lib/ClangImporter/SwiftBridging -Xcc -std=c++20 -I %t/Inputs  %t/test.swift -strict-memory-safety -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1

// REQUIRES: objc_interop
// REQUIRES: swift_feature_LifetimeDependence

//--- Inputs/module.modulemap
module Test {
    header "nonescapable.h"
    requires cplusplus
}

//--- Inputs/nonescapable.h
#include "swift/bridging"
#include <span>
#include <vector>
#include <tuple>

struct SWIFT_NONESCAPABLE View {
    __attribute__((swift_attr("@lifetime(immortal)")))
    View() : member(nullptr) {}
    __attribute__((swift_attr("@lifetime(copy p)")))
    View(const int *p [[clang::lifetimebound]]) : member(p) {}
    View(const View&) = default;
private:
    const int *member;
};

struct SWIFT_ESCAPABLE Owner {};

struct Unannotated {
    Unannotated();
    int *pointer;
};

struct SWIFT_UNSAFE_REFERENCE UnsafeReference {};

struct SafeEscapableAggregate {
    int a;
    float b[5];
};

struct UnknownEscapabilityAggregate {
    SafeEscapableAggregate agg;
    Unannotated unann;
};

struct MyContainer {
    int begin() const { return 0; }
    int end() const { return -1; }
};

using SpanOfInt = std::span<int>;
using SpanOfIntAlias = SpanOfInt;
using VecOfPtr = std::vector<int*>;
using VecOfInt = std::vector<int>;
using SafeTuple = std::tuple<int, int, int>;
using UnsafeTuple = std::tuple<int, int*, int>;

View safeFunc(View v1 [[clang::noescape]], View v2 [[clang::lifetimebound]]);
// Second non-escapable type is not annotated in any way.
void unsafeFunc(View v1 [[clang::noescape]], View v2);

class SharedObject {
private:
  int *p;
} SWIFT_SHARED_REFERENCE(retainSharedObject, releaseSharedObject);

inline void retainSharedObject(SharedObject *) {}
inline void releaseSharedObject(SharedObject *) {}

struct DerivedFromSharedObject : SharedObject {};

//--- test.swift

import Test
import CoreFoundation
import CxxStdlib

func useUnsafeParam(x: Unannotated) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
 _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

@available(SwiftStdlib 5.8, *)
func useUnsafeParam2(x: UnsafeReference) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useUnsafeParam3(x: UnknownEscapabilityAggregate) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useSafeParams(x: Owner, y: View, z: SafeEscapableAggregate, c: MyContainer) {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    let _ = c.__beginUnsafe() // expected-note{{reference to unsafe instance method '__beginUnsafe()'}}
}

func useCfType(x: CFArray) {
  _ = x
}

func useString(x: std.string) {
  _ = x
}

func useVecOfPtr(x: VecOfPtr) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useVecOfInt(x: VecOfInt) {
  _ = x
}

func useSafeTuple(x: SafeTuple) {
  _ = x
}

func useUnsafeTuple(x: UnsafeTuple) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useCppSpan(x: SpanOfInt) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useCppSpan2(x: SpanOfIntAlias) {
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
  _ = x // expected-note{{reference to parameter 'x' involves unsafe type}}
}

func useSafeLifetimeAnnotated(v: View) {
    let _ = safeFunc(v, v)
}

func useUnsafeLifetimeAnnotated(v: View) {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    unsafeFunc(v, v) // expected-note{{reference to unsafe global function 'unsafeFunc'}}
}

@available(SwiftStdlib 5.8, *)
func useSharedReference(frt: SharedObject) {
  let _ = frt
}

@available(SwiftStdlib 5.8, *)
func useSharedReference(frt: DerivedFromSharedObject) {
  let _ = frt
}

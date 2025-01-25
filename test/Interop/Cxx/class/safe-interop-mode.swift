
// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %swift_src_root/lib/ClangImporter/SwiftBridging -Xcc -std=c++20 -I %t/Inputs  %t/test.swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe  -enable-experimental-feature SafeInterop -enable-experimental-feature LifetimeDependence -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1

// REQUIRES: objc_interop
// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_SafeInterop
// REQUIRES: swift_feature_WarnUnsafe
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
    __attribute__((swift_attr("@lifetime(p)")))
    View(const int *p [[clang::lifetimebound]]) : member(p) {}
    View(const View&) = default;
private:
    const int *member;
};

struct SWIFT_ESCAPABLE Owner {};

struct Unannotated {
    Unannotated();
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

//--- test.swift

import Test
import CoreFoundation
import CxxStdlib

// expected-warning@+3{{global function 'useUnsafeParam' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{{1-1=@safe }}
func useUnsafeParam(x: Unannotated) { // expected-note{{reference to unsafe struct 'Unannotated'}}
}

// expected-warning@+4{{global function 'useUnsafeParam2' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{{1-1=@safe }}
@available(SwiftStdlib 5.8, *)
func useUnsafeParam2(x: UnsafeReference) { // expected-note{{reference to unsafe class 'UnsafeReference'}}
}

// expected-warning@+3{{global function 'useUnsafeParam3' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{{1-1=@safe }}
func useUnsafeParam3(x: UnknownEscapabilityAggregate) { // expected-note{{reference to unsafe struct 'UnknownEscapabilityAggregate'}}
}

func useSafeParams(x: Owner, y: View, z: SafeEscapableAggregate, c: MyContainer) {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    let _ = c.__beginUnsafe() // expected-note{{reference to unsafe instance method '__beginUnsafe()'}}
}

func useCfType(x: CFArray) {
}

func useString(x: std.string) {
}

// expected-warning@+3{{global function 'useVecOfPtr' has an interface}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func useVecOfPtr(x: VecOfPtr) { // expected-note{{reference to unsafe type alias 'VecOfPtr'}}
}

func useVecOfInt(x: VecOfInt) {
}

func useSafeTuple(x: SafeTuple) {
}

// expected-warning@+3{{global function 'useUnsafeTuple' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func useUnsafeTuple(x: UnsafeTuple) { // expected-note{{reference to unsafe type alias 'UnsafeTuple'}}
}

// expected-warning@+3{{global function 'useCppSpan' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func useCppSpan(x: SpanOfInt) { // expected-note{{reference to unsafe type alias 'SpanOfInt'}}
}

// expected-warning@+3{{global function 'useCppSpan2' has an interface that involves unsafe types}}
// expected-note@+2{{add '@unsafe' to indicate that this declaration is unsafe to use}}{{1-1=@unsafe }}
// expected-note@+1{{add '@safe' to indicate that this declaration is memory-safe to use}}{1-1=@safe }}
func useCppSpan2(x: SpanOfIntAlias) { // expected-note{{reference to unsafe type alias 'SpanOfIntAlias'}}
}

func useSafeLifetimeAnnotated(v: View) {
    let _ = safeFunc(v, v)
}

func useUnsafeLifetimeAnnotated(v: View) {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}
    unsafeFunc(v, v) // expected-note{{reference to unsafe global function 'unsafeFunc'}}
}

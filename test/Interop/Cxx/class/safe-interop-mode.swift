
// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs  %t/test.swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -enable-experimental-feature NonescapableTypes -enable-experimental-feature SafeInterop -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1

// REQUIRES: objc_interop

//--- Inputs/module.modulemap
module Test {
    header "nonescapable.h"
    requires cplusplus
}

//--- Inputs/nonescapable.h
#include "swift/bridging"

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

//--- test.swift

import Test
import CoreFoundation

func useUnsafeParam(x: Unannotated) { // expected-warning{{reference to unsafe struct 'Unannotated'}}
}

@available(SwiftStdlib 5.8, *)
func useUnsafeParam2(x: UnsafeReference) { // expected-warning{{reference to unsafe class 'UnsafeReference'}}
}

func useUnsafeParam3(x: UnknownEscapabilityAggregate) { // expected-warning{{reference to unsafe struct 'UnknownEscapabilityAggregate'}}
}

func useSafeParams(x: Owner, y: View, z: SafeEscapableAggregate) {
}

func useCfType(x: CFArray) {
}

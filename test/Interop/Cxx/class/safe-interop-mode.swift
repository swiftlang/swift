
// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %swift_src_root/lib/ClangImporter/SwiftBridging  -I %t/Inputs  %t/test.swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -enable-experimental-feature NonescapableTypes -enable-experimental-feature SafeInterop -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1

// REQUIRES: objc_interop

// rdar://136620623
// REQUIRES: rdar136620623

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

struct Unannotated {};

struct SWIFT_UNSAFE_REFERENCE UnsafeReference {};

//--- test.swift

import Test
import CoreFoundation

func useUnsafeParam(x: Unannotated) { // expected-warning{{reference to unsafe struct 'Unannotated'}}
}

@available(macOS 13.4, *)
func useUnsafeParam2(x: UnsafeReference) { // expected-warning{{reference to unsafe class 'UnsafeReference'}}
}

func useSafeParams(x: Owner, y: View) {
}

func useCfType(x: CFArray) {
}

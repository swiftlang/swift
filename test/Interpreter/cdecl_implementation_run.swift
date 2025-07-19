// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Build Swift dylib and compatibility header.
// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) %t/Lib.swift \
// RUN:   -emit-module-path %t/Lib.swiftmodule \
// RUN:   -emit-clang-header-path %t/cdecl.h \
// RUN:   -import-objc-header %t/BridgingHeader.h \
// RUN:   -enable-experimental-feature CDecl \
// RUN:   -enable-experimental-feature CImplementation
// RUN: %target-codesign %t/%target-library-name(Lib)

/// Build a C client against cdecl.h.
// RUN: %clang-no-modules %t/Client.c -o %t/a.out -target %target-triple \
// RUN:   -I %clang-include-dir -Werror -isysroot %sdk \
// RUN:   -I %t -l Lib -L %t %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(Lib) > %t/run.log
// RUN: %FileCheck %t/Client.c --check-prefix=PRINTS --input-file %t/run.log

/// Build a Swift client against cdecl.h.
// RUN: %target-build-swift %t/Client.swift -o %t/a.out \
// RUN:   -I %t -l Lib -L %t %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out %t/%target-library-name(Lib) > %t/run.log
// RUN: %FileCheck %t/Client.swift --check-prefix=PRINTS --input-file %t/run.log

// REQUIRES: swift_feature_CDecl
// REQUIRES: swift_feature_CImplementation
// REQUIRES: executable_test

//--- BridgingHeader.h
#include <stddef.h>
#include <stdbool.h>

extern int simple(int x, int y);

extern void primitiveTypes(ptrdiff_t i, int ci, long l, char c, float f, double d, bool b);

extern void sameName();

__attribute__((swift_name("renamed_swiftSide(_:)")))
extern void renamed_clangSide(ptrdiff_t arg);

//--- Lib.swift

@cdecl @implementation
public func simple(_ x: Int32, _ y: Int32) -> Int32 {
    print(x, y)
    return x
}

@implementation @cdecl
public func primitiveTypes(_ i: Int, _ ci: CInt, _ l: CLong, _ c: CChar, _ f: Float, _ d: Double, _ b: Bool) {
    print(i, ci, l, c, f, d, b)
}

@cdecl(sameName) @implementation
public func sameName() {
    print("sameName")
}

@cdecl(renamed_clangSide) @implementation
public func renamed_swiftSide(_ arg: Int) {
    print(arg)
}

//--- Client.c

#include "cdecl.h"
#include "BridgingHeader.h"

int main() {
    ptrdiff_t x = simple(42, 43);
    // PRINTS: 42 43
    primitiveTypes(1, 2, 3, 'a', 1.0f, 2.0, true);
    // PRINTS: 1 2 3 97 1.0 2.0 true
    sameName();
    // PRINTS: sameName
    renamed_clangSide(11);
    // PRINTS: 11
}

//--- Client.swift
import Lib

let _ = simple(42, 43)
// PRINTS: 42 43
primitiveTypes(1, 2, 3, 97, 1.0, 2.0, true)
// PRINTS: 1 2 3 97 1.0 2.0 true
sameName();
// PRINTS: sameName
renamed_swiftSide(11)
// PRINTS: 11

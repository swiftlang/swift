// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Generate an internal cdecl.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/Lib.swift -emit-module -verify -o %t -emit-module-doc \
// RUN:   -emit-clang-header-path %t/cdecl.h \
// RUN:   -enable-experimental-feature CDecl

/// Build and run a binary from Swift and C code.
// RUN: %clang-no-modules -c %t/Client.c -o %t/Client.o \
// RUN:   -I %t -I %clang-include-dir -Werror -isysroot %sdk
// RUN: %target-build-swift %t/Lib.swift %t/Client.o -O -o %t/a.out \
// RUN:   -enable-experimental-feature CDecl -parse-as-library
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out > %t/run.log
// RUN: %FileCheck %s --input-file %t/run.log

// REQUIRES: swift_feature_CDecl
// REQUIRES: executable_test

//--- Lib.swift

/// My documentation
@cdecl(simple) public func simpleNameSwiftSide(x: CInt, bar y: CInt) -> CInt {
    print(x, y)
    return x
}

@cdecl func defaultName(x: Int) {
    print(x)
}

@cdecl public func primitiveTypes(i: Int, ci: CInt, l: CLong, c: CChar, f: Float, d: Double, b: Bool) {
    print(i, ci, l, c, f, d, b)
}

@cdecl enum CEnum: CInt { case A, B }

@cdecl func useEnum(e: CEnum) -> CEnum {
    print(e)
    return e
}

//--- Client.c

#include <stdio.h>
#include "cdecl.h"

int main() {
    int x = simple(42, 43);
    // CHECK: 42 43
    printf("%d\n", x);
    // CHECK-NEXT: 42

    defaultName(121);
    // CHECK-NEXT: 121

    primitiveTypes(1, 2, 3, 'a', 1.0f, 2.0, true);
    // CHECK-NEXT: 1 2 3 97 1.0 2.0 true

    CEnum e = useEnum(CEnumB);
    // CHECK-NEXT: B
    printf("%d\n", e);
    // CHECK-NEXT: 1
}

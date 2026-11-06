// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -module-name test -emit-ir %t/test.swift -I %t -Xcc -fno-builtin | %FileCheck %s
// RUN: %target-swift-frontend -module-name test -emit-ir %t/test.swift -I %t | %FileCheck %s --check-prefix=NEGATIVE
// RUN: %target-swift-frontend -module-name test -emit-ir %t/test.swift -I %t -Xcc -fno-builtin-calloc -Xcc -fno-builtin-stpcpy | %FileCheck %s --check-prefix=CHECK-FUNCS

// Check that a C inline function is still inlined into Swift code when builtins are disabled
// (the caller's and callee's no-builtin attributes must be compatible).
// RUN: %target-swift-frontend -module-name test -emit-ir %t/test.swift -I %t -O -Xcc -fno-builtin | %FileCheck %s --check-prefix=INLINE
// RUN: %target-swift-frontend -module-name test -emit-ir %t/test.swift -I %t -O -Xcc -fno-builtin-calloc -Xcc -fno-builtin-stpcpy | %FileCheck %s --check-prefix=INLINE

//--- module.modulemap
module CTable {
  header "ctable.h"
  export *
}

//--- ctable.h
#include <stddef.h>

extern const unsigned char cTable[16];

static inline unsigned char cTableLookup(size_t i) {
  return cTable[i];
}

//--- test.swift
import CTable

// CHECK: define {{.*}}swiftcc void @"$s4test3fooyyF"() [[ATTRS:#[0-9]+]]
// CHECK-FUNCS: define {{.*}}swiftcc void @"$s4test3fooyyF"() [[ATTRS:#[0-9]+]]
public func foo() {}

// INLINE-LABEL: define {{.*}} @"$s4test7lookupCys5UInt8VSiF"(
// INLINE-NOT:     call
// INLINE:         load i8, ptr
// INLINE-NOT:     call
// INLINE:         ret i8
// INLINE-NEXT:  }
public func lookupC(_ i: Int) -> UInt8 {
  cTableLookup(i & 15)
}

// CHECK: [[ATTRS]] = {{{.*}}"no-builtins"
// CHECK-FUNCS: [[ATTRS]] = {{{.*}}"no-builtin-calloc"{{.*}}"no-builtin-stpcpy"

// Without -fno-builtin, the attribute must not be added.
// NEGATIVE-NOT: "no-builtins"

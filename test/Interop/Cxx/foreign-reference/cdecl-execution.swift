// RUN: %empty-directory(%t)
// RUN: %target-clang -x c -c %S/Inputs/cdecl.c -I %S/Inputs -o %t/cdecl-c.o

// Build with C interoperability
// RUN: %target-build-swift %s -I %S/Inputs -o %t/cdecl %t/cdecl-c.o -Xfrontend -disable-availability-checking
// RUN: %target-codesign %t/cdecl
// RUN: %target-run %t/cdecl | %FileCheck %s

// Build with C++ interoperability
// RUN: %target-build-swift %s -I %S/Inputs -o %t/cdecl %t/cdecl-c.o -Xfrontend -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking
// RUN: %target-codesign %t/cdecl
// RUN: %target-run %t/cdecl | %FileCheck %s

// REQUIRES: executable_test

import CDeclFRT

@implementation @c
func CImplTakesImmortal(_ value: Immortal) {
  print("CImplTakesImmortal: \(value.value)")
}

@implementation @c
func CImplReturnsImmortal() -> Immortal {
  return getImmortal()
}

@implementation @c
func CImplGetSharedValue(_ s: Shared) -> Int32 {
  return s.value
}

let s = makeShared(21)

// CHECK: CImplTakesImmortal: 42
// CHECK-NEXT: CImplReturnsImmortal: 42
// CHECK-NEXT: value: 21
print("value: \(callSwiftImplementations(s))")

// The Swift caller retains 's' for the duration of the call, and the C code
// hands it back unchanged.
// CHECK-NEXT: refCount: 1
print("refCount: \(sharedRefCount(s))")

// CHECK-NEXT: DONE
print("DONE")

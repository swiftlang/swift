// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-ir %t/test.swift -I %t/Inputs -enable-experimental-cxx-interop -Xcc -fignore-exceptions | %FileCheck %s
// RUN: %target-swift-emit-ir %t/test.swift -I %t/Inputs -enable-experimental-cxx-interop -Xcc -fno-exceptions -Xcc -fno-objc-exceptions  -Xcc -DNOEXCEPTION | %FileCheck %s

//--- Inputs/module.modulemap
module CxxModule {
    header "cxxHeader.h"
    requires cplusplus
}

//--- Inputs/cxxHeader.h

inline int freeFunctionThrows(int x) {
#ifndef NOEXCEPTION
  if (x > 0)
    throw 2;
#endif
  return -x;
}

inline int freeFunctionNoThrow(int x) noexcept {
  return -x;
}

//--- test.swift

import CxxModule

func makeCInt() -> CInt {
  return 42
}

func testFreeFunctionNoThrowOnly() -> CInt {
  return freeFunctionNoThrow(makeCInt())
}

func testFreeFunctionCalls() -> CInt {
  let p = freeFunctionThrows(0)
  freeFunctionNoThrow(1)
  freeFunctionThrows(makeCInt())
  return p
}

let _ = testFreeFunctionNoThrowOnly()
let _ = testFreeFunctionCalls()

// CHECK: define {{.*}} @"$s4test0A23FreeFunctionNoThrowOnlys5Int32VyF"() #[[#SWIFTMETA:]] {
// CHECK-NEXT: :
// CHECK-NEXT:  call swiftcc i32 @"$s4test8makeCInts5Int32VyF"()
// CHECK-NEXT:  call i32 @{{_Z19freeFunctionNoThrowi|"\?freeFunctionNoThrow@@YAHH@Z"}}(i32 {{.*}})
// CHECK-NEXT:  ret i32
// CHECK-NEXT: }

// CHECK: define {{.*}} @"$s4test0A17FreeFunctionCallss5Int32VyF"() #[[#SWIFTMETA]] {
// CHECK: call i32 @{{_Z18freeFunctionThrowsi|"\?freeFunctionThrows@@YAHH@Z"}}(i32 0)
// CHECK: call i32 @{{_Z19freeFunctionNoThrowi|"\?freeFunctionNoThrow@@YAHH@Z"}}(i32 1)
// CHECK: call swiftcc i32 @"$s4test8makeCInts5Int32VyF"()
// CHECK: call i32 @{{_Z18freeFunctionThrowsi|"\?freeFunctionThrows@@YAHH@Z"}}(i32
// CHECK: ret i32
// CHECK-NEXT: }

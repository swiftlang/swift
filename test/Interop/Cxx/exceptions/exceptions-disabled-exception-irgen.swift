// RUN: %empty-directory(%t)
// RUN: split-file %S/trap-on-exception-irgen-itanium.swift %t

// RUN: %target-swift-emit-ir %t/test.swift -I %t -enable-experimental-cxx-interop -Xcc -fignore-exceptions | %FileCheck %s

// note: The test sources are in 'trap-on-exception-irgen-itanium.swift'

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

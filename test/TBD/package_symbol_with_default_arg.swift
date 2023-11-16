// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift-dylib(%t/%target-library-name(Utils)) %t/Utils.swift \
// RUN:  -module-name Utils -package-name mypkg \
// RUN:  -emit-module -emit-module-path %t/Utils.swiftmodule \
// RUN:  -emit-tbd -emit-tbd-path %t/libUtils.tbd -Xfrontend -tbd-install_name=%t/libUtils.dylib -Xfrontend -validate-tbd-against-ir=all

// RUN: %llvm-nm %t/libUtils.tbd | %FileCheck %s --check-prefix CHECK-TBD 
// CHECK-TBD-NOT: $s5Utils6pubBar3argS2i_tFfA_
// CHECK-TBD-NOT: $s5Utils11internalBar3argS2i_tF
// CHECK-TBD-NOT: $s5Utils11internalBar3argS2i_tFfA_
// CHECK-TBD-NOT: $s5Utils11internalFoo3argS2i_tF
// CHECK-TBD: $s5Utils6pkgBar3argS2i_tF
// CHECK-TBD: $s5Utils6pkgBar3argS2i_tFfA_
// CHECK-TBD: $s5Utils6pkgFoo3argS2i_tF
// CHECK-TBD: $s5Utils6pubBar3argS2i_tF
// CHECK-TBD: $s5Utils6pubFoo3argS2i_tF

// RUN: %target-build-swift-dylib(%t/%target-library-name(UtilsForTesting)) %t/Utils.swift \
// RUN:  -module-name UtilsForTesting -package-name testpkg \
// RUN:  -emit-module -emit-module-path %t/UtilsForTesting.swiftmodule \
// RUN:  -emit-tbd -emit-tbd-path %t/libUtilsForTesting.tbd -Xfrontend -tbd-install_name=%t/libUtilsForTesting.dylib \
// RUN:  -enable-testing -Xfrontend -validate-tbd-against-ir=all


// RUN: %llvm-nm %t/libUtilsForTesting.tbd | %FileCheck %s --check-prefix CHECK-TEST 
// CHECK-TEST-NOT: $s15UtilsForTesting6pubBar3argS2i_tFfA_
// CHECK-TEST: $s15UtilsForTesting11internalBar3argS2i_tF
// CHECK-TEST: $s15UtilsForTesting11internalBar3argS2i_tFfA_
// CHECK-TEST: $s15UtilsForTesting11internalFoo3argS2i_tF
// CHECK-TEST: $s15UtilsForTesting6pkgBar3argS2i_tF
// CHECK-TEST: $s15UtilsForTesting6pkgBar3argS2i_tFfA_
// CHECK-TEST: $s15UtilsForTesting6pkgFoo3argS2i_tF
// CHECK-TEST: $s15UtilsForTesting6pubBar3argS2i_tF
// CHECK-TEST: $s15UtilsForTesting6pubFoo3argS2i_tF

// RUN: %target-swift-frontend -typecheck %t/main.swift -I %t -L %t -lUtils -package-name mypkg -verify


//--- Utils.swift
public func pubBar(arg: Int = 1) -> Int { return arg + 11 }
package func pkgBar(arg: Int = 1) -> Int { return arg + 12 }
func internalBar(arg: Int = 1) -> Int { return arg + 13 }

public func pubFoo(arg: Int) -> Int { return arg + 1 }
package func pkgFoo(arg: Int) -> Int { return arg + 2 }
func internalFoo(arg: Int) -> Int { return arg + 3 }

//--- main.swift
import Utils

let a = pubBar()
let b = pkgBar()

let c = pubFoo(arg: 3)
let d = pkgFoo(arg: 5)

print(a, b, c, d)

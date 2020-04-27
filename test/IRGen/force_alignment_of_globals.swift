// RUN: %target-swift-frontend -emit-ir -Xllvm -force-align-globals-to=8 %s | %FileCheck -check-prefix=CHECK-EIGHT %s
// RUN: %target-swift-frontend -emit-ir -Xllvm -force-align-globals-to=16 %s | %FileCheck -check-prefix=CHECK-SIXTEEN %s

// CHECK-EIGHT: @"$s26force_alignment_of_globals6globalSivp" ={{( dllexport| protected)?}} global %TSi zeroinitializer, align 8
// CHECK-SIXTEEN: @"$s26force_alignment_of_globals6globalSivp" ={{( dllexport| protected)?}} global %TSi zeroinitializer, align 16
public var global: Int = 0

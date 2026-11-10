// At -Onone under LTO, we leave removing Clang's ARC intrinsics to the
// link-time code generator, as Clang does. Make sure they reach the link, and
// that linking succeeds (rdar://192359751).

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -Onone -Xcc -O2 -lto=llvm-thin -import-objc-header %S/Inputs/StaticInline.h %s -emit-bc -o %t/thin.bc
// RUN: %llvm-dis -o - %t/thin.bc | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -Onone -Xcc -O2 -lto=llvm-full -import-objc-header %S/Inputs/StaticInline.h %s -emit-bc -o %t/full.bc
// RUN: %llvm-dis -o - %t/full.bc | %FileCheck %s
// RUN: %use_just_built_liblto %target-build-swift -parse-as-library -Onone -Xcc -O2 -lto=llvm-thin -import-objc-header %S/Inputs/StaticInline.h %s -emit-library -o %t/libthin.dylib %lto_flags
// RUN: %use_just_built_liblto %target-build-swift -parse-as-library -Onone -Xcc -O2 -lto=llvm-full -import-objc-header %S/Inputs/StaticInline.h %s -emit-library -o %t/libfull.dylib %lto_flags

// CHECK: call void (...) @llvm.objc.clang.arc.noop.use

// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// REQUIRES: no_asan

import Foundation

public func bar() -> String {
  return staticInlineFun()
}

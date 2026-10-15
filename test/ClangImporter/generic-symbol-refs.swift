// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -module-name generic_symbol_refs -I %S/Inputs/custom-modules -emit-ir %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation
import GenericModule

func foo() {
  _ = GenericClass<NSString>(value: "hello" as NSString)
  // CHECK: @"OBJC_CLASS_REF_$_GenericClass" = private externally_initialized global ptr @"OBJC_CLASS_$_GenericClass", section "__DATA,__objc_classrefs,regular,no_dead_strip"
  // CHECK: @"OBJC_CLASS_$_GenericClass" = external global %objc_class
  // CHECK: @"symbolic So12GenericClassC" = linkonce_odr hidden constant <{ [17 x i8], i8, i32 }> <{ [17 x i8] c"So12GenericClassC", i8 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"OBJC_CLASS_REF_$_GenericClass" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ [17 x i8], i8, i32 }>, ptr @"symbolic So12GenericClassC", i32 0, i32 2) to i64)) to i32) }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address
  // CHECK-NOT: @"symbolic So12GenericClassC" = linkonce_odr hidden constant <{ [17 x i8], i8 }> <{ [17 x i8] c"So12GenericClassC", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address

  _ = GenericClass<AnotherGenericClass<NSString>>(value: .init(value: "hello" as NSString))
  // CHECK: @"OBJC_CLASS_REF_$_AnotherGenericClass" = private externally_initialized global ptr @"OBJC_CLASS_$_AnotherGenericClass", section "__DATA,__objc_classrefs,regular,no_dead_strip"
  // CHECK: @"OBJC_CLASS_$_AnotherGenericClass" = external global %objc_class
  // CHECK: @"symbolic So19AnotherGenericClassC" = linkonce_odr hidden constant <{ [24 x i8], i8, i32 }> <{ [24 x i8] c"So19AnotherGenericClassC", i8 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"OBJC_CLASS_REF_$_AnotherGenericClass" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ [24 x i8], i8, i32 }>, ptr @"symbolic So19AnotherGenericClassC", i32 0, i32 2) to i64)) to i32) }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address
  // CHECK-NOT: @"symbolic So19AnotherGenericClassC" = linkonce_odr hidden constant <{ [24 x i8], i8 }> <{ [24 x i8] c"So19AnotherGenericClassC", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address

  // CHECK: @llvm.compiler.used =
  // CHECK-DAG: ptr @"OBJC_CLASS_REF_$_GenericClass"
  // CHECK-DAG: ptr @"OBJC_CLASS_REF_$_AnotherGenericClass"
  // CHECK-SAME: , section "llvm.metadata"
}

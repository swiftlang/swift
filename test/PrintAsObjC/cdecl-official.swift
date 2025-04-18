// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Generate cdecl.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/Lib.swift -emit-module -verify -o %t -emit-module-doc \
// RUN:   -emit-objc-header-path %t/cdecl.h \
// RUN:   -enable-experimental-feature CDecl

/// Check cdecl.h directly
// RUN: %FileCheck %s --input-file %t/cdecl.h
// RUN: %check-in-clang %t/cdecl.h
// RUN: %check-in-clang-c %t/cdecl.h -Wnullable-to-nonnull-conversion
// RUN: %check-in-clang-cxx %t/cdecl.h

/// Build a client against cdecl.h
// RUN: %clang -c %t/Client.c -fmodules -I %t \
// RUN:   -F %S/../Inputs/clang-importer-sdk-path/frameworks \
// RUN:   -I %clang-include-dir -Werror \
// RUN:   -isysroot %S/../Inputs/clang-importer-sdk

// REQUIRES: swift_feature_CDecl

//--- Lib.swift

// CHECK: #if defined(__cplusplus)
// CHECK: extern "C" {
// CHECK: #endif

/// My documentation
@cdecl("simple")
func a_simple(x: Int, bar y: Int) -> Int { return x }
// CHECK-LABEL: // My documentation
// CHECK-LABEL: SWIFT_EXTERN ptrdiff_t simple(ptrdiff_t x, ptrdiff_t y) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

@cdecl("primitiveTypes")
public func b_primitiveTypes(i: Int, ci: CInt, l: CLong, c: CChar, f: Float, d: Double, b: Bool) {}
// CHECK-LABEL: SWIFT_EXTERN void primitiveTypes(ptrdiff_t i, int ci, long l, char c, float f, double d, bool b) SWIFT_NOEXCEPT;

@cdecl("has_keyword_arg_names")
func c_keywordArgNames(auto: Int, union: Int) {}
// CHECK-LABEL: SWIFT_EXTERN void has_keyword_arg_names(ptrdiff_t auto_, ptrdiff_t union_) SWIFT_NOEXCEPT;

@cdecl("return_never")
func d_returnNever() -> Never { fatalError() }
// CHECK-LABEL: SWIFT_EXTERN void return_never(void) SWIFT_NOEXCEPT SWIFT_NORETURN;

@cdecl("block_nightmare")
public func s_block_nightmare(x: @convention(block) (Int) -> Float)
  -> @convention(block) (CChar) -> Double { return { _ in 0 } }
// CHECK-LABEL: SWIFT_EXTERN double (^ _Nonnull block_nightmare(SWIFT_NOESCAPE float (^ _Nonnull x)(ptrdiff_t)))(char) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

@cdecl("block_recurring_nightmare")
public func t_block_recurring_nightmare(x: @escaping @convention(block) (@convention(block) (Double) -> Int) -> Float)
  -> @convention(block) (_ asdfasdf: @convention(block) (CUnsignedChar) -> CChar) -> Double {
  fatalError()
}
// CHECK-LABEL: SWIFT_EXTERN double (^ _Nonnull block_recurring_nightmare(float (^ _Nonnull x)(SWIFT_NOESCAPE ptrdiff_t (^ _Nonnull)(double))))(SWIFT_NOESCAPE char (^ _Nonnull)(unsigned char)) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

@cdecl("function_pointer_nightmare")
func u_function_pointer_nightmare(x: @convention(c) (Int) -> Float)
  -> @convention(c) (CChar) -> Double { return { _ in 0 } }
// CHECK-LABEL: SWIFT_EXTERN double (* _Nonnull function_pointer_nightmare(float (* _Nonnull x)(ptrdiff_t)))(char) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

@cdecl("function_pointer_recurring_nightmare")
public func v_function_pointer_recurring_nightmare(x: @escaping @convention(c) (@convention(c) (Double) -> Int) -> Float)
  -> @convention(c) (@convention(c) (CUnsignedChar) -> CChar) -> Double {
  fatalError()
}
// CHECK-LABEL: SWIFT_EXTERN double (* _Nonnull function_pointer_recurring_nightmare(float (* _Nonnull x)(ptrdiff_t (* _Nonnull)(double))))(char (* _Nonnull)(unsigned char)) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

// CHECK:      #if defined(__cplusplus)
// CHECK-NEXT: }
// CHECK-NEXT: #endif

//--- Client.c

#include "cdecl.h"

int main() {
    ptrdiff_t x = simple(42, 43);
    primitiveTypes(1, 2, 3, 'a', 1.0f, 2.0, true);
    has_keyword_arg_names(1, 2);
    return_never();
}

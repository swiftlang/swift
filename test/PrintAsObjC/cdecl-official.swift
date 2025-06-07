// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Generate cdecl.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   %t/Lib.swift -emit-module -verify -o %t -emit-module-doc \
// RUN:   -emit-clang-header-path %t/cdecl.h \
// RUN:   -enable-experimental-feature CDecl

/// Check cdecl.h directly
// RUN: %FileCheck %s --input-file %t/cdecl.h
// RUN: %check-in-clang-c %t/cdecl.h -Wnullable-to-nonnull-conversion

/// Build a client against cdecl.h
// RUN: %clang-no-modules -c %t/Client.c -I %t \
// RUN:   -F %S/../Inputs/clang-importer-sdk-path/frameworks \
// RUN:   -I %clang-include-dir -Werror \
// RUN:   -isysroot %S/../Inputs/clang-importer-sdk

// REQUIRES: swift_feature_CDecl

//--- Lib.swift

// CHECK-NOT: assume_nonnull

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

/// Pointer types
// CHECK: /// Pointer types

@cdecl("pointers")
func f_pointers(_ x: UnsafeMutablePointer<Int>,
                  y: UnsafePointer<Int>,
                  z: UnsafeMutableRawPointer,
                  w: UnsafeRawPointer,
                  u: OpaquePointer) {}
// CHECK: SWIFT_EXTERN void pointers(ptrdiff_t * _Nonnull x, ptrdiff_t const * _Nonnull y, void * _Nonnull z, void const * _Nonnull w, void * _Nonnull u) SWIFT_NOEXCEPT;

@cdecl("nullable_pointers")
func g_nullablePointers(_ x: UnsafeMutableRawPointer,
                          y: UnsafeMutableRawPointer?,
                          z: UnsafeMutableRawPointer!) {}
// CHECK: SWIFT_EXTERN void nullable_pointers(void * _Nonnull x, void * _Nullable y, void * _Null_unspecified z) SWIFT_NOEXCEPT;

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

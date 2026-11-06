// Tests function templates with C array parameters.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/test.swift -I %t/include -cxx-interoperability-mode=default -verify-additional-prefix legacy-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -cxx-interoperability-mode=default -o /dev/null

// RUN: %target-swift-frontend -typecheck -verify %t/test.swift -I %t/include -cxx-interoperability-mode=default -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix modern-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -cxx-interoperability-mode=default -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -o /dev/null

// REQUIRES: swift_feature_ModernImportedCArrays

//--- include/module.modulemap
module CArrays {
  header "c-arrays.h"
  requires cplusplus
  export *
}

//--- include/c-arrays.h
void takesArrayPointer(int (*array)[4]);

template <typename T>
void takesArrayPointerTemplate(int (*array)[4], T value) {}

//--- test.swift
import CArrays

@available(anyAppleOS 26, *)
func test(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>
) {
  takesArrayPointer(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  takesArrayPointer(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  takesArrayPointerTemplate(tuple, 1)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  takesArrayPointerTemplate(array, 1)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}
}

//--- codegen.swift
import CArrays

#if MODERN_C_ARRAYS
typealias CArray = InlineArray<4, Int32>
#else
typealias CArray = (Int32, Int32, Int32, Int32)
#endif

@available(anyAppleOS 26, *)
func test(_ pointer: UnsafeMutablePointer<CArray>) {
  takesArrayPointer(pointer)
  takesArrayPointerTemplate(pointer, 1)
}

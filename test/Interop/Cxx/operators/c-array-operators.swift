// Test C++ operators (including subscripts) that involve C arrays.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -cxx-interoperability-mode=default -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix modern-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -cxx-interoperability-mode=default -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -o /dev/null

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -cxx-interoperability-mode=default -verify-additional-prefix legacy-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -cxx-interoperability-mode=default -o /dev/null

// RUN: %target-swift-ide-test -print-module -module-to-print=CArrays -source-filename=x -I %t/include -cxx-interoperability-mode=default --access-filter-private -enable-experimental-feature ModernImportedCArraysOnly -target %target-has-inline-array-triple | %FileCheck %s %if VENDOR=apple %{ --check-prefixes CHECK,APPLE %}

// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: swift_feature_ModernImportedCArraysOnly

//--- include/module.modulemap
module CArrays {
  header "c-arrays.h"
  requires cplusplus
  export *
}

//--- include/c-arrays.h
struct SubIndex {
  int operator[](int (*array)[4]) const;
};

struct SubResult {
  int (*operator[](int index) const)[4];
};

struct Ops {
  bool operator==(int (*array)[4]) const;
  Ops operator+(int (*array)[4]) const;
};

//--- typecheck.swift
import CArrays

@available(anyAppleOS 26, *)
func test(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    subIndex: SubIndex, subResult: inout SubResult, ops: Ops
) {
  _ = subIndex[tuple]
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = subIndex[array]
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  let _: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>? = subResult[0]
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>') to type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>?'}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('[4 of CInt]' (aka 'InlineArray<4, Int32>') and '(Int32, Int32, Int32, Int32)') are expected to be equal}}
  let _: UnsafeMutablePointer<InlineArray<4, Int32>>? = subResult[0]
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>') to type 'UnsafeMutablePointer<InlineArray<4, Int32>>?'}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and 'InlineArray<4, Int32>') are expected to be equal}}

  _ = ops == tuple
  // expected-modern-c-array-error@-1 {{referencing operator function '==' on '_Pointer' requires that 'Ops' conform to '_Pointer'}}
  _ = ops == array
  // expected-legacy-c-array-error@-1 {{referencing operator function '==' on '_Pointer' requires that 'Ops' conform to '_Pointer'}}

  _ = ops + tuple
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'Ops' to expected argument type 'Int'}}
  _ = ops + array
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'Ops' to expected argument type 'Int'}}
}

//--- codegen.swift
import CArrays

#if MODERN_C_ARRAYS
typealias CArray = InlineArray<4, Int32>
#else
typealias CArray = (Int32, Int32, Int32, Int32)
#endif

@available(anyAppleOS 26, *)
func test(
    _ pointer: UnsafeMutablePointer<CArray>,
    _ subIndex: SubIndex, _ subResult: inout SubResult, _ ops: Ops
) {
  _ = subIndex[pointer]
  let _: UnsafeMutablePointer<CArray>? = subResult[0]

  _ = ops == pointer
  _ = ops + pointer
}

// CHECK-LABEL: struct SubIndex
// CHECK: func __operatorSubscriptConst
// APPLE-NEXT: @available({{.*}}, *)
// CHECK-NEXT: subscript(array: UnsafeMutablePointer<[4 of CInt]>!) -> CInt

// CHECK-LABEL: struct SubResult
// CHECK: func __operatorSubscript
// APPLE-NEXT: @available({{.*}}, *)
// CHECK-NEXT: subscript(index: CInt) -> UnsafeMutablePointer<[4 of CInt]>!

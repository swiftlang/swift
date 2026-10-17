// Tests that C++ class members with dual C array projections are inherited
// correctly.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/typecheck.swift -I %t/include -cxx-interoperability-mode=default -verify-additional-prefix legacy-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -cxx-interoperability-mode=default -o /dev/null
// RUN: %target-swift-ide-test -print-module -module-to-print=CArrays -source-filename=x -I %t/include -cxx-interoperability-mode=default --access-filter-private | %FileCheck %s --check-prefixes=MEMBERS,MEMBERS-LEGACY --implicit-check-not='[4 of CInt]'

// RUN: %target-swift-frontend -typecheck -verify %t/typecheck.swift -I %t/include -cxx-interoperability-mode=default -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix modern-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -cxx-interoperability-mode=default -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -o /dev/null
// RUN: %target-swift-ide-test -print-module -module-to-print=CArrays -source-filename=x -I %t/include -cxx-interoperability-mode=default --access-filter-private -enable-experimental-feature ModernImportedCArraysOnly | %FileCheck %s --check-prefixes=MEMBERS,MEMBERS-MODERN --implicit-check-not='(CInt, CInt, CInt, CInt)'

// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: swift_feature_ModernImportedCArraysOnly

//--- include/module.modulemap
module CArrays {
  header "c-arrays.h"
  requires cplusplus
  export *
}

//--- include/c-arrays.h
// --implicit-check-not will catch if both projections are printed.

// MEMBERS-LABEL: struct Base {
struct Base {
  // MEMBERS-LEGACY: func takesArrayPointer(_ array: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>!)
  // MEMBERS-MODERN: func takesArrayPointer(_ array: UnsafeMutablePointer<[4 of CInt]>!)
  void takesArrayPointer(int (*array)[4]) const;
};

// MEMBERS-LABEL: struct PublicDerived {
struct PublicDerived : Base {
  // MEMBERS-LEGACY: func takesArrayPointer(_ array: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>!)
  // MEMBERS-MODERN: func takesArrayPointer(_ array: UnsafeMutablePointer<[4 of CInt]>!)
};

// MEMBERS-LABEL: struct UsingDerived {
struct UsingDerived : private Base {
  // MEMBERS-LEGACY: func takesArrayPointer(_ array: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>!)
  // MEMBERS-MODERN: func takesArrayPointer(_ array: UnsafeMutablePointer<[4 of CInt]>!)
  using Base::takesArrayPointer;
};

// MEMBERS-LABEL: struct CtorBase {
struct CtorBase {
  // MEMBERS-LEGACY: init(_ array: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>!)
  // MEMBERS-MODERN: init(_ array: UnsafeMutablePointer<[4 of CInt]>!)
  CtorBase(int (*array)[4]);
};

// MEMBERS-LABEL: struct CtorDerived {
struct CtorDerived : CtorBase {
  // MEMBERS-LEGACY: init(_: UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>!)
  // MEMBERS-MODERN: init(_: UnsafeMutablePointer<[4 of CInt]>!)
  using CtorBase::CtorBase;
};

//--- typecheck.swift
import CArrays

@available(anyAppleOS 26, *)
func test(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    base: Base, publicDerived: PublicDerived, usingDerived: UsingDerived
) {
  base.takesArrayPointer(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  base.takesArrayPointer(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  publicDerived.takesArrayPointer(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  publicDerived.takesArrayPointer(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  usingDerived.takesArrayPointer(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  usingDerived.takesArrayPointer(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  _ = CtorBase(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = CtorBase(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  _ = CtorDerived(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = CtorDerived(array)
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
func test(
    _ pointer: UnsafeMutablePointer<CArray>,
    _ base: Base, _ publicDerived: PublicDerived, _ usingDerived: UsingDerived
) {
  base.takesArrayPointer(pointer)
  publicDerived.takesArrayPointer(pointer)
  usingDerived.takesArrayPointer(pointer)

  _ = CtorBase(pointer)
  _ = CtorDerived(pointer)
}

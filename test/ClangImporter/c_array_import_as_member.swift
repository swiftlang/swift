// Test import-as-member with C array projections.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/typecheck.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix modern-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -o /dev/null

// RUN: %target-swift-frontend -typecheck -verify %t/typecheck.swift -I %t/include -verify-additional-prefix legacy-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -o /dev/null

// REQUIRES: swift_feature_ModernImportedCArrays

//--- include/module.modulemap
module CArrays {
  header "c-arrays.h"
  export *
}

//--- include/c-arrays.h
typedef struct Prop { int x; } Prop;

Prop makeProp(int (*buf)[4])
    __attribute__((swift_name("Prop.init(buf:)")));

void propMethod(Prop *prop, int (*buf)[4])
    __attribute__((swift_name("Prop.method(self:_:)")));

int (*getBuf(const Prop *prop))[4]
    __attribute__((swift_name("getter:Prop.buf(self:)")));
void setBuf(Prop *prop, int (*buf)[4])
    __attribute__((swift_name("setter:Prop.buf(self:_:)")));

//--- typecheck.swift
import CArrays

@available(anyAppleOS 26, *)
func test(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    prop: inout Prop
) {
  // Initializer

  _ = Prop(buf: tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = Prop(buf: array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  // Method

  prop.method(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  prop.method(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  // Property
  let _: UnsafeMutablePointer<InlineArray<4, Int32>> = prop.buf
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>') to type 'UnsafeMutablePointer<InlineArray<4, Int32>>'}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and 'InlineArray<4, Int32>') are expected to be equal}}
  let _: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)> = prop.buf
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>') to type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>'}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('[4 of CInt]' (aka 'InlineArray<4, Int32>') and '(Int32, Int32, Int32, Int32)') are expected to be equal}}

  prop.buf = tuple
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  prop.buf = array
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>')}}
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
func test(_ pointer: UnsafeMutablePointer<CArray>, _ prop: inout Prop) {
  prop = Prop(buf: pointer)
  prop.method(pointer)
  prop.buf = pointer
  let _: UnsafeMutablePointer<CArray> = prop.buf
}

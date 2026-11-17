// Test C++ operators (including subscripts) that involve C arrays.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -cxx-interoperability-mode=default -Xcc -std=c++23 -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -verify-additional-prefix modern-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -cxx-interoperability-mode=default -Xcc -std=c++23 -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -o /dev/null

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -cxx-interoperability-mode=default -Xcc -std=c++23 -verify-additional-prefix legacy-c-array-
// RUN: %target-swift-frontend -emit-sil %t/codegen.swift -I %t/include -cxx-interoperability-mode=default -Xcc -std=c++23 -o /dev/null

// Availability can only be tested on Apple platforms.
// RUN: %if VENDOR=apple %{ %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/availability.swift -I %t/include -cxx-interoperability-mode=default -Xcc -std=c++23 -enable-experimental-feature ModernImportedCArraysOnly %}

// RUN: %target-swift-ide-test -print-module -module-to-print=CArrays -source-filename=x -I %t/include -cxx-interoperability-mode=default -Xcc -std=c++23 --access-filter-private -enable-experimental-feature ModernImportedCArraysOnly -target %target-has-inline-array-triple | %FileCheck %s %if VENDOR=apple %{ --check-prefixes CHECK,APPLE %}

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

// Declared availability that is lower than InlineArray's is raised to it, on
// the subscript as well as its accessors.
// (anyAppleOS can't express a version below InlineArray's.)
#define OLD_AVAILABILITY \
    __attribute__((availability(macos, introduced = 12.0))) \
    __attribute__((availability(ios, introduced = 15.0))) \
    __attribute__((availability(tvos, introduced = 15.0))) \
    __attribute__((availability(watchos, introduced = 8.0))) \
    __attribute__((availability(visionos, introduced = 1.0)))

struct OldSubIndex {
  int operator[](int (*array)[4]) const OLD_AVAILABILITY;
};

struct OldSubResult {
  int (*operator[](int index) const)[4] OLD_AVAILABILITY;
};

struct Ops {
  bool operator==(int (*array)[4]) const;
  Ops operator+(int (*array)[4]) const;
};

struct GlobalOps {};
bool operator==(GlobalOps, int (*array)[4]);

typedef char (&Marker)[1];
Marker operator!=(GlobalOps, GlobalOps);

// Operators with C arrays in their results, and a compound assignment operator.
struct ResultOps {
  int (*operator+(int amount) const)[4];
  int (*operator-() const)[4];
  ResultOps &operator+=(int (*array)[4]);
};

// Call and dereference operators are imported as methods and properties
// instead of being wrapped in Swift operators.
struct CallOps {
  int operator()(int (*array)[4]) const;
  int (*operator()(int index) const)[4];
};

struct DerefOps {
  int (*operator*() const)[4];
};

struct DerefRefOps {
  int (&operator*())[4];
};

// Operators inherited from a base class are cloned onto the derived class.
struct BaseOps {
  bool operator==(int (*array)[4]) const;
  int operator()(int (*array)[4]) const;
};

struct DerivedOps : BaseOps {};

struct UsingOps : private BaseOps {
  using BaseOps::operator==;
  using BaseOps::operator();
};

// C++23 allows `operator()` to be static.
struct StaticCallParam {
  static int operator()(int (*array)[4]);
};

struct StaticCallResult {
  static int (*operator()(int index))[4];
};

//--- typecheck.swift
import CArrays

@available(anyAppleOS 26, *)
func test(
    tuple: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>,
    array: UnsafeMutablePointer<InlineArray<4, Int32>>,
    subIndex: SubIndex, subResult: inout SubResult, ops: Ops,
    globalOps: GlobalOps, resultOps: inout ResultOps, callOps: CallOps,
    derivedOps: DerivedOps, usingOps: UsingOps, derefOps: DerefOps,
    derefRefOps: inout DerefRefOps
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

  _ = globalOps == tuple
  // expected-modern-c-array-error@-1 {{referencing operator function '==' on '_Pointer' requires that 'GlobalOps' conform to '_Pointer'}}
  _ = globalOps == array
  // expected-legacy-c-array-error@-1 {{referencing operator function '==' on '_Pointer' requires that 'GlobalOps' conform to '_Pointer'}}

  _ = globalOps != globalOps

  let _: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>? = resultOps + 1
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'ResultOps' to expected argument type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>'}}
  let _: UnsafeMutablePointer<InlineArray<4, Int32>>? = resultOps + 1
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'ResultOps' to expected argument type 'UnsafeMutablePointer<InlineArray<4, Int32>>'}}

  let _: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>? = -resultOps
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>') to type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>?'}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('[4 of CInt]' (aka 'InlineArray<4, Int32>') and '(Int32, Int32, Int32, Int32)') are expected to be equal}}
  let _: UnsafeMutablePointer<InlineArray<4, Int32>>? = -resultOps
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>') to type 'UnsafeMutablePointer<InlineArray<4, Int32>>?'}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and 'InlineArray<4, Int32>') are expected to be equal}}

  resultOps += tuple
  // expected-modern-c-array-error@-1 {{binary operator '+=' cannot be applied to operands of type 'ResultOps' and 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>'}}
  resultOps += array
  // expected-legacy-c-array-error@-1 {{binary operator '+=' cannot be applied to operands of type 'ResultOps' and 'UnsafeMutablePointer<InlineArray<4, Int32>>'}}

  _ = callOps(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'CInt' (aka 'Int32')}}
  _ = callOps(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'CInt' (aka 'Int32')}}

  let _: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>? = callOps(0)
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>') to type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>?'}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('[4 of CInt]' (aka 'InlineArray<4, Int32>') and '(Int32, Int32, Int32, Int32)') are expected to be equal}}
  let _: UnsafeMutablePointer<InlineArray<4, Int32>>? = callOps(0)
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>') to type 'UnsafeMutablePointer<InlineArray<4, Int32>>?'}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and 'InlineArray<4, Int32>') are expected to be equal}}

  _ = derivedOps(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = derivedOps(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  _ = usingOps == tuple
  // expected-modern-c-array-error@-1 {{referencing operator function '==' on '_Pointer' requires that 'UsingOps' conform to '_Pointer'}}
  _ = usingOps == array
  // expected-legacy-c-array-error@-1 {{referencing operator function '==' on '_Pointer' requires that 'UsingOps' conform to '_Pointer'}}

  _ = usingOps(tuple)
  // expected-modern-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>' to expected argument type 'UnsafeMutablePointer<[4 of CInt]>' (aka 'UnsafeMutablePointer<InlineArray<4, Int32>>')}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(Int32, Int32, Int32, Int32)' and '[4 of CInt]' (aka 'InlineArray<4, Int32>')) are expected to be equal}}
  _ = usingOps(array)
  // expected-legacy-c-array-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<InlineArray<4, Int32>>' to expected argument type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>' (aka 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>')}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('InlineArray<4, Int32>' and '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)')) are expected to be equal}}

  let _: UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>? = derefOps.pointee
  // expected-modern-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<[4 of CInt]>?' (aka 'Optional<UnsafeMutablePointer<InlineArray<4, Int32>>>') to type 'UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>?'}}
  // expected-modern-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('[4 of CInt]' (aka 'InlineArray<4, Int32>') and '(Int32, Int32, Int32, Int32)') are expected to be equal}}
  let _: UnsafeMutablePointer<InlineArray<4, Int32>>? = derefOps.pointee
  // expected-legacy-c-array-error@-1 {{cannot assign value of type 'UnsafeMutablePointer<(CInt, CInt, CInt, CInt)>?' (aka 'Optional<UnsafeMutablePointer<(Int32, Int32, Int32, Int32)>>') to type 'UnsafeMutablePointer<InlineArray<4, Int32>>?'}}
  // expected-legacy-c-array-note@-2 {{arguments to generic parameter 'Pointee' ('(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') and 'InlineArray<4, Int32>') are expected to be equal}}

  let _: (Int32, Int32, Int32, Int32) = derefRefOps.pointee
  // expected-modern-c-array-error@-1 {{cannot convert value of type '[4 of CInt]' (aka 'InlineArray<4, Int32>') to specified type '(Int32, Int32, Int32, Int32)'}}
  let _: InlineArray<4, Int32> = derefRefOps.pointee
  // expected-legacy-c-array-error@-1 {{cannot convert value of type '(CInt, CInt, CInt, CInt)' (aka '(Int32, Int32, Int32, Int32)') to specified type 'InlineArray<4, Int32>'}}
}

//--- availability.swift
import CArrays

func testOldAvailability(_ index: OldSubIndex, _ result: inout OldSubResult) {
  // expected-note@-1 3 {{add '@available' attribute to enclosing global function}}
  _ = index[nil]
  // expected-error@-1 {{'subscript(_:)' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  // expected-error@-2 {{getter for 'subscript(_:)' is only available in}} expected-note@-2 {{add 'if #available' version check}}
  _ = result[0]
  // expected-error@-1 {{'subscript(_:)' is only available in}} expected-note@-1 {{add 'if #available' version check}}
}

func testOperatorAvailability(
    _ call: CallOps, _ base: BaseOps, _ derived: DerivedOps, _ using: UsingOps,
    _ result: inout ResultOps, _ deref: DerefOps, _ derefRef: inout DerefRefOps
) {
  // expected-note@-4 11 {{add '@available' attribute to enclosing global function}}
  _ = call(nil)
  // expected-error@-1 {{'callAsFunction' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = base(nil)
  // expected-error@-1 {{'callAsFunction' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = derived(nil)
  // expected-error@-1 {{'callAsFunction' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = using(nil)
  // expected-error@-1 {{'callAsFunction' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = base == UnsafeMutablePointer(bitPattern: 8)
  // expected-error@-1 {{'==' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = using == UnsafeMutablePointer(bitPattern: 8)
  // expected-error@-1 {{'==' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = result + 1
  // expected-error@-1 {{'+' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = -result
  // expected-error@-1 {{'-' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  result += nil
  // expected-error@-1 {{'+=' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = deref.pointee
  // expected-error@-1 {{'pointee' is only available in}} expected-note@-1 {{add 'if #available' version check}}
  _ = derefRef.pointee
  // expected-error@-1 {{'pointee' is only available in}} expected-note@-1 {{add 'if #available' version check}}
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
    _ subIndex: SubIndex, _ subResult: inout SubResult, _ ops: Ops,
    _ globalOps: GlobalOps, _ resultOps: inout ResultOps, _ callOps: CallOps,
    _ derivedOps: DerivedOps, _ usingOps: UsingOps, _ derefOps: DerefOps,
    _ derefRefOps: inout DerefRefOps, _ staticCallParam: StaticCallParam, _ staticCallResult: StaticCallResult
) {
  _ = subIndex[pointer]
  let _: UnsafeMutablePointer<CArray>? = subResult[0]

  _ = ops == pointer
  _ = ops + pointer

  _ = globalOps == pointer
  _ = globalOps != globalOps

  let _: UnsafeMutablePointer<CArray>? = resultOps + 1
  let _: UnsafeMutablePointer<CArray>? = -resultOps
  resultOps += pointer

  _ = callOps(pointer)
  let _: UnsafeMutablePointer<CArray>? = callOps(0)

  _ = derivedOps(pointer)
  _ = usingOps == pointer
  _ = usingOps(pointer)

  let _: UnsafeMutablePointer<CArray>? = derefOps.pointee
  let _: CArray = derefRefOps.pointee
  derefRefOps.pointee = derefRefOps.pointee

  _ = staticCallParam(pointer)
  let _: UnsafeMutablePointer<CArray>? = staticCallResult(0)
}

// CHECK-LABEL: struct SubIndex
// CHECK: func __operatorSubscriptConst
// APPLE-NEXT: @available({{.*}}, *)
// CHECK-NEXT: subscript(array: UnsafeMutablePointer<[4 of CInt]>!) -> CInt

// CHECK-LABEL: struct SubResult
// CHECK: func __operatorSubscript
// APPLE-NEXT: @available({{.*}}, *)
// CHECK-NEXT: subscript(index: CInt) -> UnsafeMutablePointer<[4 of CInt]>!

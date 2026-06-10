// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by_or_null parameters.

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))

// expected-expansion@+7:55{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe simple(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void simple(int len, int * __counted_by_or_null(len) p);

// expected-expansion@+7:62{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func simpleFlipped(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe simpleFlipped(p.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void simpleFlipped(int * __counted_by_or_null(len) p, int len);

// expected-expansion@+7:31{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe swiftAttr(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedByOrNull(pointer: .param(2), count: \"len\"))")));

// expected-expansion@+10:92{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: UnsafeMutableBufferPointer<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe shared(len, p1.baseAddress, p2.baseAddress)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
void shared(int len, int * __counted_by_or_null(len) p1, int * __counted_by_or_null(len) p2);

// expected-expansion@+9:81{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != len - offset {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\(len - offset) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe complexExpr(len, offset, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void complexExpr(int len, int offset, int * __counted_by_or_null(len - offset) p);

// expected-expansion@+7:82{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullUnspecified(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nullUnspecified(int len, int * __counted_by_or_null(len) _Null_unspecified p);

// expected-warning@+8{{combining '__counted_by_or_null' and '_Nonnull'; did you mean '__counted_by' instead?}}
// expected-expansion@+7:65{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nonnull(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nonnull(int len, int * __counted_by_or_null(len) _Nonnull p);

// expected-expansion@+7:67{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ p: UnsafeMutableBufferPointer<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: unsafe p?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullable(len, p?.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nullable(int len, int * __counted_by_or_null(len) _Nullable p);

// expected-expansion@+6:54{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<Int32>(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by_or_null(len) returnPointer(int len);

// expected-expansion@+9:61{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func offByOne(_ len: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != len + 1 {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in offByOne: expected \\(len + 1) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe offByOne(len, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void offByOne(int len, int * __counted_by_or_null(len + 1) p);

// expected-expansion@+9:85{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func offBySome(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != len + (1 + offset) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in offBySome: expected \\(len + (1 + offset)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe offBySome(len, offset, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void offBySome(int len, int offset, int * __counted_by_or_null(len + (1 + offset)) p);

// expected-expansion@+9:62{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func scalar(_ m: Int32, _ n: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != m * n {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in scalar: expected \\(m * n) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe scalar(m, n, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void scalar(int m, int n, int * __counted_by_or_null(m * n) p);

// expected-expansion@+9:75{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bitwise(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != m & n | ~o {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in bitwise: expected \\(m & n | ~o) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe bitwise(m, n, o, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void bitwise(int m, int n, int o, int * __counted_by_or_null(m & n | ~o) p);

// expected-expansion@+9:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bitshift(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != m << (n >> o) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in bitshift: expected \\(m << (n >> o)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe bitshift(m, n, o, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void bitshift(int m, int n, int o, int * __counted_by_or_null(m << (n >> o)) p);

// expected-expansion@+9:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func constInt(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != 420 {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in constInt: expected \\(420) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe constInt(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void constInt(int * __counted_by_or_null(42 * 10) p);

// expected-expansion@+9:74{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func constFloatCastedToInt(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != 0 {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in constFloatCastedToInt: expected \\(0) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe constFloatCastedToInt(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void constFloatCastedToInt(int * __counted_by_or_null((int) (4.2 / 12)) p);

void sizeofType(int * __counted_by_or_null(sizeof(int *)) p);

void sizeofParam(int * __counted_by_or_null(sizeof(p)) p);

void derefLen(int * len, int * __counted_by_or_null(*len) p);

void lNot(int len, int * __counted_by_or_null(!len) p);

void lAnd(int len, int * __counted_by_or_null(len && len) p);

void lOr(int len, int * __counted_by_or_null(len || len) p);

void floatCastToInt(float meters, int * __counted_by_or_null((int) meters) p);

void pointerCastToInt(int *square, int * __counted_by_or_null((int) square) p);

void nanAsInt(int * __counted_by_or_null((int) (0 / 0)) p);

void unsignedLiteral(int * __counted_by_or_null(2u) p);

void longLiteral(int * __counted_by_or_null(2l) p);

// expected-expansion@+9:51{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func hexLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != 250 {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in hexLiteral: expected \\(250) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe hexLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void hexLiteral(int * __counted_by_or_null(0xfa) p);

// expected-expansion@+9:54{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func binaryLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != 2 {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in binaryLiteral: expected \\(2) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe binaryLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void binaryLiteral(int * __counted_by_or_null(0b10) p);

// expected-expansion@+9:53{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func octalLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    if p.count != 511 {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in octalLiteral: expected \\(511) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe octalLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void octalLiteral(int * __counted_by_or_null(0777) p);

void variadic(int len, int * __counted_by_or_null(len) p, ...);

// Mixed nullability shared counts: __counted_by_or_null _Nullable presents
// the buffer as Optional, while a non-_or_null sharer (here: __counted_by
// alone) presents non-Optional. Generated wrapper should pick the
// non-Optional sharer as the count extractor regardless of declaration
// order, and emit a nil-aware check on the Optional one.

// expected-expansion@+10:99{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func mixedShared(_ p1: UnsafeMutableBufferPointer<Int32>?, _ p2: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if let _p1Count = unsafe p1?.count, _p1Count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in mixedShared: expected \\(len) but got \\(_p1Count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe mixedShared(len, p1?.baseAddress, p2.baseAddress)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
void mixedShared(int len, int * __counted_by_or_null(len) _Nullable p1, int * __counted_by(len) p2);

// Three sharers with mixed nullability: the lone non-Optional p2 should be
// the extractor; both Optional sharers get nil-aware checks.
// expected-expansion@+13:144{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func mixedThree(_ p1: UnsafeMutableBufferPointer<Int32>?, _ p2: UnsafeMutableBufferPointer<Int32>, _ p3: UnsafeMutableBufferPointer<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if let _p1Count = unsafe p1?.count, _p1Count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in mixedThree: expected \\(len) but got \\(_p1Count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    if let _p3Count = unsafe p3?.count, _p3Count != len {|}}
//   expected-remark@8{{macro content: |      fatalError("bounds check failure in mixedThree: expected \\(len) but got \\(_p3Count)")|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe mixedThree(len, p1?.baseAddress, p2.baseAddress, p3?.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void mixedThree(int len, int * __counted_by_or_null(len) _Nullable p1, int * __counted_by(len) p2, int * __counted_by_or_null(len) _Nullable p3);

// All sharers Optional: ??-chain extraction, nil-aware checks on the rest.
// expected-expansion@+10:115{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func allOrNull(_ p1: UnsafeMutableBufferPointer<Int32>?, _ p2: UnsafeMutableBufferPointer<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: unsafe p2?.count ?? p1?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    if let _p1Count = unsafe p1?.count, _p1Count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in allOrNull: expected \\(len) but got \\(_p1Count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe allOrNull(len, p1?.baseAddress, p2?.baseAddress)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
void allOrNull(int len, int * __counted_by_or_null(len) _Nullable p1, int * __counted_by_or_null(len) _Nullable p2);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 7ff35b9ab50ae75a7f0ee983328b18de19a13f5feb1e4c0fdc38e0ad6596564d
import Test

func call_simple(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe simple(len, p)
}

func call_simpleFlipped(_ p: UnsafeMutablePointer<Int32>!, _ len: Int32) {
  return unsafe simpleFlipped(p, len)
}

func call_swiftAttr(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe swiftAttr(len, p)
}

func call_shared(_ len: Int32, _ p1: UnsafeMutablePointer<Int32>!, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe shared(len, p1, p2)
}

func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe complexExpr(len, offset, p)
}

func call_nullUnspecified(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe nullUnspecified(len, p)
}

func call_nonnull(_ len: Int32, _ p: UnsafeMutablePointer<Int32>) {
  return unsafe nonnull(len, p)
}

func call_nullable(_ len: Int32, _ p: UnsafeMutablePointer<Int32>?) {
  return unsafe nullable(len, p)
}

func call_returnPointer(_ len: Int32) -> UnsafeMutablePointer<Int32>! {
  return unsafe returnPointer(len)
}

func call_offByOne(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe offByOne(len, p)
}

func call_offBySome(_ len: Int32, _ offset: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe offBySome(len, offset, p)
}

func call_scalar(_ m: Int32, _ n: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe scalar(m, n, p)
}

func call_bitwise(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe bitwise(m, n, o, p)
}

func call_bitshift(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe bitshift(m, n, o, p)
}

func call_constInt(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe constInt(p)
}

func call_constFloatCastedToInt(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe constFloatCastedToInt(p)
}

func call_sizeofType(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe sizeofType(p)
}

func call_sizeofParam(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe sizeofParam(p)
}

func call_derefLen(_ len: UnsafeMutablePointer<Int32>!, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe derefLen(len, p)
}

func call_lNot(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe lNot(len, p)
}

func call_lAnd(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe lAnd(len, p)
}

func call_lOr(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe lOr(len, p)
}

func call_floatCastToInt(_ meters: Float, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe floatCastToInt(meters, p)
}

func call_pointerCastToInt(_ square: UnsafeMutablePointer<Int32>!, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe pointerCastToInt(square, p)
}

func call_nanAsInt(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe nanAsInt(p)
}

func call_unsignedLiteral(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe unsignedLiteral(p)
}

func call_longLiteral(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe longLiteral(p)
}

func call_hexLiteral(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe hexLiteral(p)
}

func call_binaryLiteral(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe binaryLiteral(p)
}

func call_octalLiteral(_ p: UnsafeMutablePointer<Int32>!) {
  return unsafe octalLiteral(p)
}

func call_mixedShared(_ len: Int32, _ p1: UnsafeMutablePointer<Int32>?, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe mixedShared(len, p1, p2)
}

func call_mixedThree(_ len: Int32, _ p1: UnsafeMutablePointer<Int32>?, _ p2: UnsafeMutablePointer<Int32>!, _ p3: UnsafeMutablePointer<Int32>?) {
  return unsafe mixedThree(len, p1, p2, p3)
}

func call_allOrNull(_ len: Int32, _ p1: UnsafeMutablePointer<Int32>?, _ p2: UnsafeMutablePointer<Int32>?) {
  return unsafe allOrNull(len, p1, p2)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_allOrNull(_ p1: UnsafeMutableBufferPointer<Int32>?, _ p2: UnsafeMutableBufferPointer<Int32>?) {
  return unsafe allOrNull(p1, p2)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_binaryLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe binaryLiteral(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_bitshift(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe bitshift(m, n, o, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_bitwise(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe bitwise(m, n, o, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe complexExpr(len, offset, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_constFloatCastedToInt(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe constFloatCastedToInt(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_constInt(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe constInt(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_hexLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe hexLiteral(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_mixedShared(_ p1: UnsafeMutableBufferPointer<Int32>?, _ p2: UnsafeMutableBufferPointer<Int32>) {
  return unsafe mixedShared(p1, p2)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_mixedThree(_ p1: UnsafeMutableBufferPointer<Int32>?, _ p2: UnsafeMutableBufferPointer<Int32>, _ p3: UnsafeMutableBufferPointer<Int32>?) {
  return unsafe mixedThree(p1, p2, p3)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe nonnull(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe nullUnspecified(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ p: UnsafeMutableBufferPointer<Int32>?) {
  return unsafe nullable(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_octalLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe octalLiteral(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_offByOne(_ len: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe offByOne(len, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_offBySome(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe offBySome(len, offset, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {
  return unsafe returnPointer(len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_scalar(_ m: Int32, _ n: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe scalar(m, n, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p1: UnsafeMutableBufferPointer<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {
  return unsafe shared(p1, p2)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe simple(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_simpleFlipped(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe simpleFlipped(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_swiftAttr(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe swiftAttr(p)
}

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -verify-additional-prefix macro-
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t -strict-memory-safety -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rclang-importer -verify-additional-prefix diagnose-

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by parameters.

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))

// expected-diagnose-remark@+8{{added safe interop wrapper}}
// expected-macro-expansion@+7:47{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-macro-remark@4{{macro content: |    return unsafe simple(len, p.baseAddress!)|}}
//   expected-macro-remark@5{{macro content: |}|}}
// }}
void simple(int len, int * __counted_by(len) p);

// expected-diagnose-remark@+8{{added safe interop wrapper}}
// expected-macro-expansion@+7:54{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func simpleFlipped(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-macro-remark@4{{macro content: |    return unsafe simpleFlipped(p.baseAddress!, len)|}}
//   expected-macro-remark@5{{macro content: |}|}}
// }}
void simpleFlipped(int * __counted_by(len) p, int len);

// expected-diagnose-remark@+9{{did not add safe interop wrapper}}
// expected-diagnose-note@+8{{no bounds or lifetime information found}}
// expected-macro-expansion@+7:31{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-macro-remark@4{{macro content: |    return unsafe swiftAttr(len, p.baseAddress!)|}}
//   expected-macro-remark@5{{macro content: |}|}}
// }}
void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"))")));

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:76{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: UnsafeMutableBufferPointer<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let len = Int32(exactly: p1.count)!|}}
//   expected-macro-remark@4{{macro content: |    if p2.count != len {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p2.count)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe shared(len, p1.baseAddress!, p2.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void shared(int len, int * __counted_by(len) p1, int * __counted_by(len) p2);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:73{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != len - offset {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\(len - offset) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe complexExpr(len, offset, p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void complexExpr(int len, int offset, int * __counted_by(len - offset) p);

// expected-diagnose-remark@+8{{added safe interop wrapper}}
// expected-macro-expansion@+7:74{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-macro-remark@4{{macro content: |    return unsafe nullUnspecified(len, p.baseAddress!)|}}
//   expected-macro-remark@5{{macro content: |}|}}
// }}
void nullUnspecified(int len, int * __counted_by(len) _Null_unspecified p);

// expected-diagnose-remark@+8{{added safe interop wrapper}}
// expected-macro-expansion@+7:57{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-macro-remark@4{{macro content: |    return unsafe nonnull(len, p.baseAddress!)|}}
//   expected-macro-remark@5{{macro content: |}|}}
// }}
void nonnull(int len, int * __counted_by(len) _Nonnull p);

// expected-diagnose-remark@+8{{added safe interop wrapper}}
// expected-macro-expansion@+7:59{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ p: UnsafeMutableBufferPointer<Int32>?) {|}}
//   expected-macro-remark@3{{macro content: |    let len = Int32(exactly: unsafe p?.count ?? 0)!|}}
//   expected-macro-remark@4{{macro content: |    return unsafe nullable(len, p?.baseAddress)|}}
//   expected-macro-remark@5{{macro content: |}|}}
// }}
void nullable(int len, int * __counted_by(len) _Nullable p);

// expected-diagnose-remark@+7{{added safe interop wrapper}}
// expected-macro-expansion@+6:46{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {|}}
//   expected-macro-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<Int32>(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-macro-remark@4{{macro content: |}|}}
// }}
int * __counted_by(len) returnPointer(int len);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:53{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func offByOne(_ len: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != len + 1 {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in offByOne: expected \\(len + 1) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe offByOne(len, p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void offByOne(int len, int * __counted_by(len + 1) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:77{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func offBySome(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != len + (1 + offset) {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in offBySome: expected \\(len + (1 + offset)) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe offBySome(len, offset, p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void offBySome(int len, int offset, int * __counted_by(len + (1 + offset)) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:54{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func scalar(_ m: Int32, _ n: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != m * n {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in scalar: expected \\(m * n) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe scalar(m, n, p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void scalar(int m, int n, int * __counted_by(m * n) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:67{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bitwise(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != m & n | ~o {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in bitwise: expected \\(m & n | ~o) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe bitwise(m, n, o, p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void bitwise(int m, int n, int o, int * __counted_by(m & n | ~o) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:71{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bitshift(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != m << (n >> o) {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in bitshift: expected \\(m << (n >> o)) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe bitshift(m, n, o, p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void bitshift(int m, int n, int o, int * __counted_by(m << (n >> o)) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:44{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func constInt(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != 420 {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in constInt: expected \\(420) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe constInt(p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void constInt(int * __counted_by(42 * 10) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:66{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func constFloatCastedToInt(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != 0 {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in constFloatCastedToInt: expected \\(0) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe constFloatCastedToInt(p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void constFloatCastedToInt(int * __counted_by((int) (4.2 / 12)) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains integer literal not supported in Swift syntax}}
// expected-diagnose-remark@+2{{did not add safe interop wrapper}}
// expected-diagnose-note@+1{{no bounds or lifetime information found}}
void sizeofType(int * __counted_by(sizeof(int *)) p);

// expected-diagnose-note@+4{{count parameter contains integer literal not supported in Swift syntax}}
// expected-diagnose-remark@+3{{ignoring __counted_by attribute}}
// expected-diagnose-note@+2{{no bounds or lifetime information found}}
// expected-diagnose-remark@+1{{did not add safe interop wrapper}}
void sizeofParam(int * __counted_by(sizeof(p)) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains unsupported expression kind UnaryOperator}}
// expected-diagnose-remark@+2{{did not add safe interop wrapper}}
// expected-diagnose-note@+1{{no bounds or lifetime information found}}
void derefLen(int * len, int * __counted_by(*len) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains unsupported expression kind UnaryOperator}}
// expected-diagnose-remark@+2{{did not add safe interop wrapper}}
// expected-diagnose-note@+1{{no bounds or lifetime information found}}
void lNot(int len, int * __counted_by(!len) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains unsupported expression kind BinaryOperator}}
// expected-diagnose-note@+2{{no bounds or lifetime information found}}
// expected-diagnose-remark@+1{{did not add safe interop wrapper}}
void lAnd(int len, int * __counted_by(len && len) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains unsupported expression kind BinaryOperator}}
// expected-diagnose-note@+2{{no bounds or lifetime information found}}
// expected-diagnose-remark@+1{{did not add safe interop wrapper}}
void lOr(int len, int * __counted_by(len || len) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains unsupported expression kind CStyleCastExpr}}
// expected-diagnose-remark@+2{{did not add safe interop wrapper}}
// expected-diagnose-note@+1{{no bounds or lifetime information found}}
void floatCastToInt(float meters, int * __counted_by((int) meters) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains unsupported expression kind CStyleCastExpr}}
// expected-diagnose-remark@+2{{did not add safe interop wrapper}}
// expected-diagnose-note@+1{{no bounds or lifetime information found}}
void pointerCastToInt(int *square, int * __counted_by((int) square) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains unsupported expression kind CStyleCastExpr}}
// expected-diagnose-remark@+2{{did not add safe interop wrapper}}
// expected-diagnose-note@+1{{no bounds or lifetime information found}}
void nanAsInt(int * __counted_by((int) (0 / 0)) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains integer literal not supported in Swift syntax}}
// expected-diagnose-remark@+2{{did not add safe interop wrapper}}
// expected-diagnose-note@+1{{no bounds or lifetime information found}}
void unsignedLiteral(int * __counted_by(2u) p);

// expected-diagnose-remark@+4{{ignoring __counted_by attribute}}
// expected-diagnose-note@+3{{count parameter contains integer literal not supported in Swift syntax}}
// expected-diagnose-remark@+2{{did not add safe interop wrapper}}
// expected-diagnose-note@+1{{no bounds or lifetime information found}}
void longLiteral(int * __counted_by(2l) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:43{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func hexLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != 250 {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in hexLiteral: expected \\(250) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe hexLiteral(p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void hexLiteral(int * __counted_by(0xfa) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:46{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func binaryLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != 2 {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in binaryLiteral: expected \\(2) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe binaryLiteral(p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void binaryLiteral(int * __counted_by(0b10) p);

// expected-diagnose-remark@+11{{added safe interop wrapper}}
// expected-macro-expansion@+10:45{{
//   expected-macro-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-macro-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func octalLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-macro-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-macro-remark@4{{macro content: |    if _pCount != 511 {|}}
//   expected-macro-remark@5{{macro content: |      fatalError("bounds check failure in octalLiteral: expected \\(511) but got \\(_pCount)")|}}
//   expected-macro-remark@6{{macro content: |    }|}}
//   expected-macro-remark@7{{macro content: |    return unsafe octalLiteral(p.baseAddress!)|}}
//   expected-macro-remark@8{{macro content: |}|}}
// }}
void octalLiteral(int * __counted_by(0777) p);

void variadic(int len, int * __counted_by(len) p, ...);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: dc40e34e2ead0c5c89061ff84e7ecb31d97f258952fb2ff356304ee00b98cb00
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

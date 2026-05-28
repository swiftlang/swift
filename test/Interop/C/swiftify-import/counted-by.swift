// REQUIRES: swift_feature_SafeInteropWrappersNullAsEmptySpan

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature SafeInteropWrappersNullAsEmptySpan -strict-memory-safety -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by parameters.

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))

// expected-expansion@+7:47{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe simple(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void simple(int len, int * __counted_by(len) p);

// expected-expansion@+7:54{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func simpleFlipped(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe simpleFlipped(p.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void simpleFlipped(int * __counted_by(len) p, int len);

// expected-expansion@+7:31{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe swiftAttr(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"))")));

// expected-expansion@+10:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe shared(len, p1.baseAddress, p2.baseAddress)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
void shared(int len, int * __counted_by(len) p1, int * __counted_by(len) p2);

// expected-expansion@+9:73{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (len - offset) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\((len - offset)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe complexExpr(len, offset, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void complexExpr(int len, int offset, int * __counted_by(len - offset) p);

// expected-expansion@+7:74{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullUnspecified(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nullUnspecified(int len, int * __counted_by(len) _Null_unspecified p);

// expected-expansion@+7:57{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nonnull(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nonnull(int len, int * __counted_by(len) _Nonnull p);

// expected-expansion@+7:59{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullable(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nullable(int len, int * __counted_by(len) _Nullable p);

// expected-expansion@+6:46{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(len) returnPointer(int len);

// expected-expansion@+9:53{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func offByOne(_ len: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (len + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in offByOne: expected \\((len + CInt(1))) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe offByOne(len, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void offByOne(int len, int * __counted_by(len + 1) p);

// expected-expansion@+9:77{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func offBySome(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (len + ((CInt(1) + offset))) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in offBySome: expected \\((len + ((CInt(1) + offset)))) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe offBySome(len, offset, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void offBySome(int len, int offset, int * __counted_by(len + (1 + offset)) p);

// expected-expansion@+9:54{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func scalar(_ m: CInt, _ n: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (m * n) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in scalar: expected \\((m * n)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe scalar(m, n, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void scalar(int m, int n, int * __counted_by(m * n) p);

// expected-expansion@+9:67{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bitwise(_ m: CInt, _ n: CInt, _ o: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != ((m & n) | ~o) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in bitwise: expected \\(((m & n) | ~o)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe bitwise(m, n, o, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void bitwise(int m, int n, int o, int * __counted_by(m & n | ~o) p);

// expected-expansion@+9:71{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bitshift(_ m: CInt, _ n: CInt, _ o: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (m << ((n >> o))) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in bitshift: expected \\((m << ((n >> o)))) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe bitshift(m, n, o, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void bitshift(int m, int n, int o, int * __counted_by(m << (n >> o)) p);

// expected-expansion@+9:44{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func constInt(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CInt(420) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in constInt: expected \\(CInt(420)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe constInt(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void constInt(int * __counted_by(42 * 10) p);

// expected-expansion@+9:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func constFloatCastedToInt(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CInt(0) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in constFloatCastedToInt: expected \\(CInt(0)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe constFloatCastedToInt(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void constFloatCastedToInt(int * __counted_by((int) (4.2 / 12)) p);

// expected-expansion@+9:139{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func sizeofType(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CUnsignedLongLong(1) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in sizeofType: expected \\(CUnsignedLongLong(1)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe sizeofType(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void sizeofType(int * __counted_by((unsigned long long /*cast to long long to avoid size_t differences between platforms*/)sizeof(char)) p);

// expected-expansion@+9:139{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func sizeofParam(_ p: UnsafeMutableBufferPointer<CChar>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CUnsignedLongLong(1) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in sizeofParam: expected \\(CUnsignedLongLong(1)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe sizeofParam(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void sizeofParam(char * __counted_by((unsigned long long /*cast to long long to avoid size_t differences between platforms*/)sizeof(*p)) p);

void derefLen(int * len, int * __counted_by(*len) p);

void lNot(int len, int * __counted_by(!len) p);

void lAnd(int len, int * __counted_by(len && len) p);

void lOr(int len, int * __counted_by(len || len) p);

// expected-expansion@+9:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func floatCastToInt(_ meters: CFloat, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CInt(meters) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in floatCastToInt: expected \\(CInt(meters)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe floatCastToInt(meters, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void floatCastToInt(float meters, int * __counted_by((int) meters) p);

void pointerCastToInt(int *square, int * __counted_by((int) square) p);

// expected-expansion@+11:50{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nanAsInt(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != ((CInt(0) / CInt(0))) {|}}
//   expected-error@3{{division by zero}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in nanAsInt: expected \\(((CInt(0) / CInt(0)))) but got \\(p.count)")|}}
//   expected-error@4 2{{division by zero}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe nanAsInt(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void nanAsInt(int * __counted_by((int) (0 / 0)) p);

// expected-expansion@+9:46{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func unsignedLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CUnsignedInt(2) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in unsignedLiteral: expected \\(CUnsignedInt(2)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe unsignedLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void unsignedLiteral(int * __counted_by(2u) p);

// expected-expansion@+9:42{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func longLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CLong(2) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in longLiteral: expected \\(CLong(2)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe longLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void longLiteral(int * __counted_by(2l) p);

// expected-expansion@+9:43{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func hexLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CInt(250) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in hexLiteral: expected \\(CInt(250)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe hexLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void hexLiteral(int * __counted_by(0xfa) p);

// expected-expansion@+9:46{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func binaryLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CInt(2) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in binaryLiteral: expected \\(CInt(2)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe binaryLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void binaryLiteral(int * __counted_by(0b10) p);

// expected-expansion@+9:45{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func octalLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CInt(511) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in octalLiteral: expected \\(CInt(511)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe octalLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void octalLiteral(int * __counted_by(0777) p);

// Regression test: by printing the count expression with C syntax,
// this example would fail to typecheck since it relies on implicit
// casts present in C but not in Swift.
// expected-expansion@+9:83{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func implicitIntCast(_ offset: CLongLong, _ len: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (offset + CLongLong(len)) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in implicitIntCast: expected \\((offset + CLongLong(len))) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe implicitIntCast(offset, len, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void implicitIntCast(long long offset, int len, int * __counted_by(offset + len) p);

// expected-expansion@+9:75{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func castTwiceLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CUnsignedInt(4294967295) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in castTwiceLiteral: expected \\(CUnsignedInt(4294967295)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe castTwiceLiteral(p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void castTwiceLiteral(int * __counted_by((unsigned int)(signed short)-1) p);

// expected-expansion@+9:73{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func castParam(_ p: UnsafeMutableBufferPointer<CInt>, _ len: CShort) {|}}
//   expected-remark@3{{macro content: |    if p.count != CUnsignedInt(truncatingIfNeeded: len) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in castParam: expected \\(CUnsignedInt(truncatingIfNeeded: len)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe castParam(p.baseAddress, len)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void castParam(int * __counted_by((unsigned int)len) p, signed short len);

// expected-expansion@+9:72{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func castParam2(_ p: UnsafeMutableBufferPointer<CInt>, _ len: CUnsignedInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != CInt(truncatingIfNeeded: len) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in castParam2: expected \\(CInt(truncatingIfNeeded: len)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe castParam2(p.baseAddress, len)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void castParam2(int * __counted_by((signed int)len) p, unsigned int len);

void variadic(int len, int * __counted_by(len) p, ...);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: d59a494a7fd5d429b92014384812f2939ec16d6e21861a19a4e84429f023ab5c
import Test

func call_simple(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe simple(len, p)
}

func call_simpleFlipped(_ p: UnsafeMutablePointer<CInt>!, _ len: CInt) {
  return unsafe simpleFlipped(p, len)
}

func call_swiftAttr(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe swiftAttr(len, p)
}

func call_shared(_ len: CInt, _ p1: UnsafeMutablePointer<CInt>!, _ p2: UnsafeMutablePointer<CInt>!) {
  return unsafe shared(len, p1, p2)
}

func call_complexExpr(_ len: CInt, _ offset: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe complexExpr(len, offset, p)
}

func call_nullUnspecified(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe nullUnspecified(len, p)
}

func call_nonnull(_ len: CInt, _ p: UnsafeMutablePointer<CInt>) {
  return unsafe nonnull(len, p)
}

func call_nullable(_ len: CInt, _ p: UnsafeMutablePointer<CInt>?) {
  return unsafe nullable(len, p)
}

func call_returnPointer(_ len: CInt) -> UnsafeMutablePointer<CInt>! {
  return unsafe returnPointer(len)
}

func call_offByOne(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe offByOne(len, p)
}

func call_offBySome(_ len: CInt, _ offset: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe offBySome(len, offset, p)
}

func call_scalar(_ m: CInt, _ n: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe scalar(m, n, p)
}

func call_bitwise(_ m: CInt, _ n: CInt, _ o: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe bitwise(m, n, o, p)
}

func call_bitshift(_ m: CInt, _ n: CInt, _ o: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe bitshift(m, n, o, p)
}

func call_constInt(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe constInt(p)
}

func call_constFloatCastedToInt(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe constFloatCastedToInt(p)
}

func call_sizeofType(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe sizeofType(p)
}

func call_sizeofParam(_ p: UnsafeMutablePointer<CChar>!) {
  return unsafe sizeofParam(p)
}

func call_derefLen(_ len: UnsafeMutablePointer<CInt>!, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe derefLen(len, p)
}

func call_lNot(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe lNot(len, p)
}

func call_lAnd(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe lAnd(len, p)
}

func call_lOr(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe lOr(len, p)
}

func call_floatCastToInt(_ meters: CFloat, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe floatCastToInt(meters, p)
}

func call_pointerCastToInt(_ square: UnsafeMutablePointer<CInt>!, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe pointerCastToInt(square, p)
}

func call_nanAsInt(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe nanAsInt(p)
}

func call_unsignedLiteral(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe unsignedLiteral(p)
}

func call_longLiteral(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe longLiteral(p)
}

func call_hexLiteral(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe hexLiteral(p)
}

func call_binaryLiteral(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe binaryLiteral(p)
}

func call_octalLiteral(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe octalLiteral(p)
}

func call_implicitIntCast(_ offset: CLongLong, _ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe implicitIntCast(offset, len, p)
}

func call_castTwiceLiteral(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe castTwiceLiteral(p)
}

func call_castParam(_ p: UnsafeMutablePointer<CInt>!, _ len: CShort) {
  return unsafe castParam(p, len)
}

func call_castParam2(_ p: UnsafeMutablePointer<CInt>!, _ len: CUnsignedInt) {
  return unsafe castParam2(p, len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_binaryLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe binaryLiteral(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_bitshift(_ m: CInt, _ n: CInt, _ o: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe bitshift(m, n, o, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_bitwise(_ m: CInt, _ n: CInt, _ o: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe bitwise(m, n, o, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_castParam(_ p: UnsafeMutableBufferPointer<CInt>, _ len: CShort) {
  return unsafe castParam(p, len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_castParam2(_ p: UnsafeMutableBufferPointer<CInt>, _ len: CUnsignedInt) {
  return unsafe castParam2(p, len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_castTwiceLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe castTwiceLiteral(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe complexExpr(len, offset, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_constFloatCastedToInt(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe constFloatCastedToInt(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_constInt(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe constInt(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_floatCastToInt(_ meters: CFloat, _ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe floatCastToInt(meters, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_hexLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe hexLiteral(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_implicitIntCast(_ offset: CLongLong, _ len: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe implicitIntCast(offset, len, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_longLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe longLiteral(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nanAsInt(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe nanAsInt(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe nonnull(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe nullUnspecified(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe nullable(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_octalLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe octalLiteral(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_offByOne(_ len: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe offByOne(len, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_offBySome(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe offBySome(len, offset, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointer(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {
  return unsafe returnPointer(len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_scalar(_ m: CInt, _ n: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe scalar(m, n, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p1: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {
  return unsafe shared(p1, p2)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe simple(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_simpleFlipped(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe simpleFlipped(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_sizeofParam(_ p: UnsafeMutableBufferPointer<CChar>) {
  return unsafe sizeofParam(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_sizeofType(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe sizeofType(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_swiftAttr(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe swiftAttr(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_unsignedLiteral(_ p: UnsafeMutableBufferPointer<CInt>) {
  return unsafe unsignedLiteral(p)
}

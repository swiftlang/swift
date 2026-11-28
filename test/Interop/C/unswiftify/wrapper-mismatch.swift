// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -enable-experimental-feature SafeInteropImplementations

//--- test.swift
@c @implementation
// expected-error@+3{{'@implementation' global function 'mismatch' has result type 'Bool', but the corresponding safe wrapper has result type 'CInt' (aka 'Int32')}} {{children:
//   expected-note@#mismatch-wrapper{{this safe wrapper was synthesized for the matching C declaration}}
// }}
public func mismatch(_ x: UnsafeBufferPointer<CInt>) -> Bool {
  return x.count > 0
}

// FIXME: add note about noescape
@c @implementation
// expected-error@+3{{'@implementation' global function 'escapability_mismatch' parameter 1 has type 'Span<CInt>' (aka 'Span<Int32>'), but the corresponding safe wrapper has parameter type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>')}} {{children:
//   expected-note@#escapability_mismatch-wrapper{{this safe wrapper was synthesized for the matching C declaration}}
// }}
public func escapability_mismatch(_ x: Span<CInt>) {}

@c @implementation
// expected-error@+3{{'@implementation' global function 'ownership_mismatch' parameter 1 is declared 'borrowing', but the corresponding safe wrapper parameter is 'inout'}} {{children:
//   expected-note@#ownership_mismatch-wrapper{{this safe wrapper was synthesized for the matching C declaration}}
// }}
public func ownership_mismatch(_ x: borrowing MutableSpan<CInt>) {}


@c @implementation
// expected-error@-1{{cannot lower safe-interop '@implementation' of global function 'unannotated' to a C signature}} {{children:
//   expected-note@#unannotated{{no safe wrapper was generated for the matching C declaration}}
// }}
public func unannotated(_ p: UnsafeBufferPointer<CInt>, _ len: CInt) {}

// FIXME: look up other functions with the same base name and diagnose further
@c @implementation
// expected-error@+1{{'@implementation' global function 'unelided_count' does not match any safe wrapper generated for the matching C declaration}}
public func unelided_count(_ x: Span<CInt>, _ len: CInt) {}

// FIXME: add return buffer support
@c @implementation
// expected-error@+1{{safe-interop '@implementation' of global function 'buffer_return' does not yet support a safe-interop result type 'UnsafeBufferPointer<CInt>' (aka 'UnsafeBufferPointer<Int32>')}}
public func buffer_return(_ x: UnsafeBufferPointer<CInt>) -> UnsafeBufferPointer<CInt> {}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

// expected-expansion@+8:53{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   #mismatch-wrapper@2
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func mismatch(_ x: UnsafeBufferPointer<CInt>) -> CInt {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe mismatch(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
int mismatch(const int *__counted_by(len) x, int len);

// expected-expansion@+8:67{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   #escapability_mismatch-wrapper@2
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func escapability_mismatch(_ x: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe escapability_mismatch(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void escapability_mismatch(const int *__counted_by(len) x, int len);

// expected-expansion@+14:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   #ownership_mismatch-wrapper@2
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func ownership_mismatch(_ x: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe ownership_mismatch(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void ownership_mismatch(int * _Nonnull __counted_by(len) __noescape x, int len);

void unannotated(int *p, int len); // #unannotated

// expected-expansion@+13:81{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func unelided_count(_ x: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe unelided_count(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void unelided_count(const int * _Nonnull __counted_by(len) __noescape x, int len);

// expected-expansion@+7:103{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func buffer_return(_ x: UnsafeBufferPointer<CInt>) -> UnsafeBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe UnsafeBufferPointer<CInt>(start: unsafe buffer_return(x.baseAddress!, len), count: Int(len))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
const int * _Nonnull __counted_by(len) buffer_return(const int * _Nonnull __counted_by(len) x, int len);

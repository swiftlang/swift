// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -enable-experimental-feature SafeInteropImplementations

//--- test.swift
@c @implementation
public func compound(_ p: UnsafeMutableRawBufferPointer, _ size: CInt, _ len: CInt) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func compound(_ p: UnsafeMutableRawPointer?, _ size: CInt, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int((size * len))|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe p != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeMutableRawBufferPointer(start: p, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe compound(_safeArg0, size, len)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

@c @implementation
public func compound_half_shared(_ p: UnsafeMutableRawBufferPointer, _ size: CInt, _ p2: UnsafeMutableBufferPointer<CInt>) {
// expected-expansion@+14:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func compound_half_shared(_ p1: UnsafeMutableRawPointer?, _ size: CInt, _ len: CInt, _ p2: UnsafeMutablePointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int((size * len))|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe p1 != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeMutableRawBufferPointer(start: p1, count: _count0)|}}
//   expected-remark@7{{macro content: |    let _count1 = Int(len)|}}
//   expected-remark@8{{macro content: |    precondition(_count1 >= 0, "buffer with negative count")|}}
//   expected-remark@9{{macro content: |    precondition(unsafe p2 != nil || _count1 == 0, "null buffer with non-zero count")|}}
//   expected-remark@10{{macro content: |    let _safeArg1 = unsafe UnsafeMutableBufferPointer(start: p2, count: _count1)|}}
//   expected-remark@11{{macro content: |    unsafe compound_half_shared(_safeArg0, size, _safeArg1)|}}
//   expected-remark@12{{macro content: |}|}}
// }}
}

@c @implementation
public func neg(_ p: UnsafeMutableRawBufferPointer, _ size: CInt) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func neg(_ p: UnsafeMutableRawPointer?, _ size: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(-size)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe p != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeMutableRawBufferPointer(start: p, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe neg(_safeArg0, size)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

@c @implementation
// expected-expansion@+10:43{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func typed_pointee(_ p: UnsafePointer<CChar>?, _ n: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(n)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe p != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = if unsafe p != nil { unsafe RawSpan(_unsafeStart: p!, byteCount: _count0) } else { RawSpan() }|}}
//   expected-remark@7{{macro content: |    typed_pointee(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
public func typed_pointee(_ p: RawSpan) {}

@c @implementation
// expected-expansion@+10:44{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func opaque_pointee(_ p: OpaquePointer?, _ n: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(n)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe p != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = if unsafe p != nil { unsafe RawSpan(_unsafeStart: UnsafeRawPointer(p!), byteCount: _count0) } else { RawSpan() }|}}
//   expected-remark@7{{macro content: |    opaque_pointee(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
public func opaque_pointee(_ p: RawSpan) {}

//--- test.h
#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

// expected-expansion@+15:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func compound(_ p: UnsafeMutableRawBufferPointer, _ size: CInt, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (size * len) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("compound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((size * len), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe compound(p.baseAddress, size, len)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void compound(void *__sized_by(size * len) p, int size, int len);

// expected-expansion@+16:104{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func compound_half_shared(_ p1: UnsafeMutableRawBufferPointer, _ size: CInt, _ p2: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != (size * len) {|}}
//   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@8{{macro content: |        }|}}
//   expected-remark@9{{macro content: |        _fail("compound_half_shared", expected, actual)|}}
//   expected-remark@10{{macro content: |      }|}}
//   expected-remark@11{{macro content: |      _boundsCheckFailure((size * len), p1.count)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe compound_half_shared(p1.baseAddress, size, len, p2.baseAddress)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
void compound_half_shared(void *__sized_by(size * len) p1, int size, int len, int *__counted_by(len) p2);

// expected-expansion@+15:45{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func neg(_ p: UnsafeMutableRawBufferPointer, _ size: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != -size {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("neg", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure(-size, p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe neg(p.baseAddress, size)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void neg(void *__sized_by(-size) p, int size);

// expected-expansion@+13:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func typed_pointee(_ p: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let n = CInt(exactly: p.byteCount)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe typed_pointee(_pPtr.baseAddress?.assumingMemoryBound(to: CChar.self), n)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void typed_pointee(const char * _Nullable __sized_by(n) __noescape p, int n);

struct fwd_t;
// expected-expansion@+13:75{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func opaque_pointee(_ p: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let n = CInt(exactly: p.byteCount)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe opaque_pointee(OpaquePointer(_pPtr.baseAddress), n)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void opaque_pointee(const struct fwd_t * __sized_by(n) __noescape p, int n);

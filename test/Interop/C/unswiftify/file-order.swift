// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify -I %t -module-name main %t/impl.swift %t/caller.swift \
// RUN:   -enable-experimental-feature SafeInteropImplementations \
// RUN:   -verify-additional-file %t/foo.h -verify-additional-prefix impl-checked-
// RUN: %target-verify-unswiftify -I %t -module-name main %t/caller.swift %t/impl.swift \
// RUN:   -enable-experimental-feature SafeInteropImplementations \
// RUN:   -verify-additional-file %t/foo.h -verify-additional-prefix impl-checked-

// Compiling only one of the two files (the other is non-primary).
// RUN: %target-verify-unswiftify -I %t -module-name main -primary-file %t/caller.swift %t/impl.swift \
// RUN:   -enable-experimental-feature SafeInteropImplementations \
// RUN:   -verify-additional-file %t/foo.h
// RUN: %target-verify-unswiftify -I %t -module-name main -primary-file %t/impl.swift %t/caller.swift \
// RUN:   -enable-experimental-feature SafeInteropImplementations \
// RUN:   -verify-additional-file %t/foo.h -verify-additional-prefix impl-checked-

//--- module.modulemap
module CFoo { header "foo.h" export * }

//--- foo.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
// expected-expansion@+13:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func sum_span(_ buf: Span<CInt>) -> CInt {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: buf.count)!|}}
//   expected-remark@4{{macro content: |    let _bufPtr = buf.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(buf)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe sum_span(_bufPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
int sum_span(const int * _Nonnull __counted_by(len) __noescape buf, int len);

//--- caller.swift
import CFoo

public func qualifiedCall(_ s: Span<CInt>) -> CInt {
  CFoo.sum_span(s)
}
public func qualifiedCallModuleSyntax(_ s: Span<CInt>) -> CInt {
  CFoo::sum_span(s)
}
public func unqualifiedCall(_ s: Span<CInt>) -> CInt {
  sum_span(s)
}

//--- impl.swift
import CFoo

@c @implementation
public func sum_span(_ buf: Span<CInt>) -> CInt {
  var t: CInt = 0
  for i in buf.indices { t &+= buf[i] }
  return t
}
// expected-impl-checked-expansion@-1:2{{
//   expected-impl-checked-remark@1{{macro content: |@c @implementation|}}
//   expected-impl-checked-remark@2{{macro content: |public func sum_span(_ buf: UnsafePointer<CInt>, _ len: CInt) -> CInt {|}}
//   expected-impl-checked-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-impl-checked-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-impl-checked-remark@5{{macro content: |    let _safeArg0 = unsafe Span(_unsafeStart: buf, count: _count0)|}}
//   expected-impl-checked-remark@6{{macro content: |    return sum_span(_safeArg0)|}}
//   expected-impl-checked-remark@7{{macro content: |}|}}
// }}

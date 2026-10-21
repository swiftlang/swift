// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-ignore-unrelated -enable-experimental-feature SafeInteropImplementations

// Only 'compound' detects the duplicate implementation before invoking
// _Unswiftify, because it doesn't elide the size parameters. For the others
// there's no crossover in lookup until the macro is expanded since the
// compound names differ between the safe and unsafe signature. This makes
// _Unswiftify a bit of a leaky abstraction here, which is unfortunate, but not
// a top priority to fix.

//--- test.swift
@c @implementation
public func unsafe_impl_first(_ p: UnsafeRawPointer, _ size: CInt) {} // #unsafe_impl_first_unsafe

// expected-error@+3{{duplicate implementation of imported global function 'unsafe_impl_first'}} {{children:
//   expected-note@-3{{previously implemented here}}
// }}
@c @implementation
// expected-expansion@+9:47{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func unsafe_impl_first(_ p: UnsafeRawPointer, _ size: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(size)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe RawSpan(_unsafeStart: p, byteCount: _count0)|}}
//   expected-remark@6{{macro content: |    unsafe_impl_first(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
public func unsafe_impl_first(_ p: RawSpan) {}

@c @implementation
// expected-expansion@+9:45{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func safe_impl_first(_ p: UnsafeRawPointer, _ size: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(size)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe RawSpan(_unsafeStart: p, byteCount: _count0)|}}
//   expected-remark@6{{macro content: |    safe_impl_first(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
public func safe_impl_first(_ p: RawSpan) {} // #safe_impl_first_safe

// expected-error@+3{{duplicate implementation of imported global function 'safe_impl_first'}} {{children:
//   expected-note@#safe_impl_first_safe{{previously implemented here}}
// }}
@c @implementation
public func safe_impl_first(_ p: UnsafeRawPointer, _ size: CInt) {}

@c @implementation
// expected-expansion@+9:65{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func compound(_ p: UnsafeRawPointer, _ size: CInt, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int((size * len))|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe RawSpan(_unsafeStart: p, byteCount: _count0)|}}
//   expected-remark@6{{macro content: |    compound(_safeArg0, size, len)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
public func compound(_ p: RawSpan, _ size: CInt, _ len: CInt) {} // #compound_safe

// expected-error@+3{{duplicate implementation of imported global function 'compound'}} {{children:
//   expected-note@#compound_safe{{previously implemented here}}
// }}
@c @implementation
public func compound(_ p: UnsafeRawPointer, _ size: CInt, _ len: CInt) {}

//--- test.h
#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __noescape __attribute__((noescape))

void unsafe_impl_first(const void * _Nonnull __sized_by(size) __noescape p, int size);

void safe_impl_first(const void * _Nonnull __sized_by(size) __noescape p, int size);

void compound(const void * _Nonnull __sized_by(size * len) __noescape p, int size, int len);

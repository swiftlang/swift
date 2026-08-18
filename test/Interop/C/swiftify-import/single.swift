// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking \
// RUN:   -enable-experimental-feature Lifetimes -verify-additional-prefix stable-

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking \
// RUN:   -enable-experimental-feature Lifetimes -enable-experimental-feature SafeInteropWrappers -verify-additional-prefix experimental-

//--- test.h
#define __single __attribute__((__single__))
#define __noescape __attribute__((__noescape__))
#define __lifetimebound __attribute__((__lifetimebound__))
#define __counted_by(x) __attribute__((__counted_by__(x)))

int * __single _Null_unspecified lifetimeless(int * _Null_unspecified __single p);

// expected-expansion@+6:56{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ p: UnsafeMutablePointer<CInt>!) {|}}
//   expected-remark@3{{macro content: |    return unsafe nullUnspecified(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nullUnspecified(int * _Null_unspecified __single p __noescape);

// expected-expansion@+6:39{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: UnsafeMutablePointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    return unsafe nonnull(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nonnull(int * __single _Nonnull p __noescape);
// expected-expansion@+6:46{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nonnullFlipped(_ p: UnsafeMutablePointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    return unsafe nonnullFlipped(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nonnullFlipped(int * _Nonnull __single p __noescape);

// expected-expansion@+6:41{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ p: UnsafeMutablePointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe nullable(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nullable(int * __single _Nullable p __noescape);
// expected-expansion@+6:48{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullableFlipped(_ p: UnsafeMutablePointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe nullableFlipped(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nullableFlipped(int * _Nullable __single p __noescape);

// expected-expansion@+6:67{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecifiedConst(_ p: UnsafePointer<CInt>!) {|}}
//   expected-remark@3{{macro content: |    return unsafe nullUnspecifiedConst(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nullUnspecifiedConst(const int * _Null_unspecified __single p __noescape);

// expected-expansion@+6:50{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nonnullConst(_ p: UnsafePointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    return unsafe nonnullConst(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nonnullConst(const int * __single _Nonnull p __noescape);

// expected-expansion@+6:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullableConst(_ p: UnsafePointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe nullableConst(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nullableConst(const int * __single _Nullable p __noescape);

// expected-expansion@+6:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nested(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>!) {|}}
//   expected-remark@3{{macro content: |    return unsafe nested(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nested(int * _Null_unspecified __single * _Null_unspecified __single p __noescape);

// expected-expansion@+6:53{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func voidpointer(_ p: UnsafeMutableRawPointer!) {|}}
//   expected-remark@3{{macro content: |    return unsafe voidpointer(p)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void voidpointer(void * _Null_unspecified __single p __noescape);

struct S;
void forwardDeclared(struct S * _Null_unspecified __single p __noescape);

struct T{};
// expected-expansion@+14:12{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@available(swift, obsoleted: 3, renamed: "T.method(self:_:)") @_alwaysEmitIntoClient @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func method(_ p: UnsafeMutablePointer<T>!, _ q: UnsafeMutablePointer<CInt>!) {|}}
//   expected-remark@4{{macro content: |    return unsafe method(p, q)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+7:99{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public mutating func method(_ q: UnsafeMutablePointer<CInt>!) {|}}
//   expected-remark@4{{macro content: |    return unsafe method(q)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void method(struct T * _Null_unspecified __single p __noescape, int * _Null_unspecified __single q __noescape) __attribute__((swift_name("T.method(self:_:)")));

// expected-experimental-expansion@+12:89{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-error@2{{cannot copy the lifetime of an Escapable type}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func lifetimebound(_ p: UnsafeMutablePointer<CInt>!) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe lifetimebound(p)|}}
//   expected-experimental-remark@4{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@5{{macro content: |      precondition(CInt(2) == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@6{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@7{{macro content: |    }|}}
//   expected-experimental-remark@8{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(CInt(2))), copying: ())|}}
//   expected-experimental-remark@9{{macro content: |}|}}
// }}
int * __counted_by(2) _Null_unspecified lifetimebound(int * _Null_unspecified __single p __lifetimebound);

// expected-experimental-expansion@+25:60{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-error@2{{cannot copy the lifetime of an Escapable type}}
//   expected-experimental-remark@2{{macro content: |@available(swift, obsoleted: 3, renamed: "T.methodLifetimebound(self:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload|}}
//   expected-experimental-remark@3{{macro content: |public func methodLifetimebound(_ p: UnsafeMutablePointer<T>!) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@4{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe methodLifetimebound(p)|}}
//   expected-experimental-remark@5{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@6{{macro content: |      precondition(CInt(2) == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@7{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@8{{macro content: |    }|}}
//   expected-experimental-remark@9{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(CInt(2))), copying: ())|}}
//   expected-experimental-remark@10{{macro content: |}|}}
// }}
// expected-experimental-expansion@+12:100{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&self) @_disfavoredOverload|}}
//   expected-experimental-remark@3{{macro content: |public mutating func methodLifetimebound() -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@4{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe methodLifetimebound()|}}
//   expected-experimental-remark@5{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@6{{macro content: |      precondition(CInt(2) == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@7{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@8{{macro content: |    }|}}
//   expected-experimental-remark@9{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(CInt(2))), copying: ())|}}
//   expected-experimental-remark@10{{macro content: |}|}}
// }}
int * __counted_by(2) _Null_unspecified methodLifetimebound(struct T * _Null_unspecified __single p __lifetimebound) __attribute__((swift_name("T.methodLifetimebound(self:)")));

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: c5df79bc3b1582d25553af8ea1916d0782f1f979c06c20ecd06883b07969f343
import Test

func call_lifetimeless(_ p: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe lifetimeless(p)
}

func call_nullUnspecified(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe nullUnspecified(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nullUnspecified'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe nullUnspecified(p)
}

func call_nonnull(_ p: UnsafeMutablePointer<CInt>) {
  return unsafe nonnull(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nonnull'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ p: UnsafeMutablePointer<CInt>) {
  return unsafe nonnull(p)
}

func call_nonnullFlipped(_ p: UnsafeMutablePointer<CInt>) {
  return unsafe nonnullFlipped(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nonnullFlipped'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnullFlipped(_ p: UnsafeMutablePointer<CInt>) {
  return unsafe nonnullFlipped(p)
}

func call_nullable(_ p: UnsafeMutablePointer<CInt>?) {
  return unsafe nullable(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nullable'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ p: UnsafeMutablePointer<CInt>?) {
  return unsafe nullable(p)
}

func call_nullableFlipped(_ p: UnsafeMutablePointer<CInt>?) {
  return unsafe nullableFlipped(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nullableFlipped'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullableFlipped(_ p: UnsafeMutablePointer<CInt>?) {
  return unsafe nullableFlipped(p)
}

func call_nullUnspecifiedConst(_ p: UnsafePointer<CInt>!) {
  return unsafe nullUnspecifiedConst(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nullUnspecifiedConst'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecifiedConst(_ p: UnsafePointer<CInt>!) {
  return unsafe nullUnspecifiedConst(p)
}

func call_nonnullConst(_ p: UnsafePointer<CInt>) {
  return unsafe nonnullConst(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nonnullConst'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnullConst(_ p: UnsafePointer<CInt>) {
  return unsafe nonnullConst(p)
}

func call_nullableConst(_ p: UnsafePointer<CInt>?) {
  return unsafe nullableConst(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nullableConst'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullableConst(_ p: UnsafePointer<CInt>?) {
  return unsafe nullableConst(p)
}

func call_nested(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>!) {
  return unsafe nested(p)
}

// expected-error@+1{{invalid redeclaration of 'call_nested'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nested(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>!) {
  return unsafe nested(p)
}

func call_voidpointer(_ p: UnsafeMutableRawPointer!) {
  return unsafe voidpointer(p)
}

// expected-error@+1{{invalid redeclaration of 'call_voidpointer'}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_voidpointer(_ p: UnsafeMutableRawPointer!) {
  return unsafe voidpointer(p)
}

func call_forwardDeclared(_ p: OpaquePointer!) {
  return unsafe forwardDeclared(p)
}

extension T {
  mutating func call_method_T(_ q: UnsafeMutablePointer<CInt>!) {
    return unsafe method(q)
  }
  // expected-error@+1{{invalid redeclaration of 'call_method_T'}}
  @_alwaysEmitIntoClient @_disfavoredOverload mutating func call_method_T(_ q: UnsafeMutablePointer<CInt>!) {
    return unsafe method(q)
  }
  mutating func call_methodLifetimebound_T() -> UnsafeMutablePointer<CInt>! {
    return unsafe methodLifetimebound()
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(&self)
    @_alwaysEmitIntoClient @_disfavoredOverload mutating func call_methodLifetimebound_T() -> MutableSpan<CInt> {
    // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
    return methodLifetimebound()
  }
}

func call_lifetimebound(_ p: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe lifetimebound(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
// expected-error@+1{{cannot copy the lifetime of an Escapable type}}
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_lifetimebound(_ p: UnsafeMutablePointer<CInt>!) -> MutableSpan<CInt> {
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return unsafe lifetimebound(p)
}

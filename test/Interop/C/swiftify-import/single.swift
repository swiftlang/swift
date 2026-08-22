// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// Ref/MutableRef are annotated with StdlibDeploymentTarget, which CMake clamps
// to the platform being built, so the availability in the expansions below is
// specific to a non-strict macOS build.
// REQUIRES: OS=macosx
// REQUIRES: !swift_stdlib_strict_availability

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
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nullUnspecified(_ p: inout MutableRef<CInt>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe nullUnspecified(p?._unsafeAddress)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nullUnspecified(int * _Null_unspecified __single p __noescape);

// expected-expansion@+6:39{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nonnull(_ p: inout MutableRef<CInt>) {|}}
//   expected-remark@3{{macro content: |    return unsafe nonnull(p._unsafeAddress)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nonnull(int * __single _Nonnull p __noescape);
// expected-expansion@+6:46{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nonnullFlipped(_ p: inout MutableRef<CInt>) {|}}
//   expected-remark@3{{macro content: |    return unsafe nonnullFlipped(p._unsafeAddress)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nonnullFlipped(int * _Nonnull __single p __noescape);

// expected-expansion@+6:41{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nullable(_ p: inout MutableRef<CInt>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe nullable(p?._unsafeAddress)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nullable(int * __single _Nullable p __noescape);
// expected-expansion@+6:48{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nullableFlipped(_ p: inout MutableRef<CInt>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe nullableFlipped(p?._unsafeAddress)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nullableFlipped(int * _Nullable __single p __noescape);

// expected-expansion@+8:67{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nullUnspecifiedConst(_ p: Ref<CInt>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe _swiftifyWithOptionalPointer(p?.value) { _pPtr in|}}
//   expected-remark@4{{macro content: |        unsafe nullUnspecifiedConst(_pPtr)|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |}|}}
// }}
void nullUnspecifiedConst(const int * _Null_unspecified __single p __noescape);

// expected-expansion@+8:50{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nonnullConst(_ p: Ref<CInt>) {|}}
//   expected-remark@3{{macro content: |    return unsafe withUnsafePointer(to: p.value) { _pPtr in|}}
//   expected-remark@4{{macro content: |        unsafe nonnullConst(_pPtr)|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |}|}}
// }}
void nonnullConst(const int * __single _Nonnull p __noescape);

// expected-expansion@+8:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nullableConst(_ p: Ref<CInt>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe _swiftifyWithOptionalPointer(p?.value) { _pPtr in|}}
//   expected-remark@4{{macro content: |        unsafe nullableConst(_pPtr)|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |}|}}
// }}
void nullableConst(const int * __single _Nullable p __noescape);

// expected-expansion@+6:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload public func nested(_ p: inout MutableRef<UnsafeMutablePointer<CInt>?>?) {|}}
//   expected-remark@3{{macro content: |    return unsafe nested(p?._unsafeAddress)|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void nested(int * _Null_unspecified __single * _Null_unspecified __single p __noescape);

void voidpointer(void * _Null_unspecified __single p __noescape);

struct S;
void forwardDeclared(struct S * _Null_unspecified __single p __noescape);

struct T{};
// expected-expansion@+14:12{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@available(swift, obsoleted: 3, renamed: "T.method(self:_:)") @_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func method(_ p: inout MutableRef<T>?, _ q: inout MutableRef<CInt>?) {|}}
//   expected-remark@4{{macro content: |    return unsafe method(p?._unsafeAddress, q?._unsafeAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+7:99{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public mutating func method(_ q: inout MutableRef<CInt>?) {|}}
//   expected-remark@4{{macro content: |    return unsafe method(q?._unsafeAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void method(struct T * _Null_unspecified __single p __noescape, int * _Null_unspecified __single q __noescape) __attribute__((swift_name("T.method(self:_:)")));

// expected-experimental-expansion@+11:89{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(macOS 13.0, *) @_lifetime(copy p) @_disfavoredOverload public func lifetimebound(_ p: inout MutableRef<CInt>?) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe lifetimebound(p?._unsafeAddress)|}}
//   expected-experimental-remark@4{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@5{{macro content: |      precondition(CInt(2) == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@6{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@7{{macro content: |    }|}}
//   expected-experimental-remark@8{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(CInt(2))), copying: ())|}}
//   expected-experimental-remark@9{{macro content: |}|}}
// }}
int * __counted_by(2) _Null_unspecified lifetimebound(int * _Null_unspecified __single p __lifetimebound);

const int * __counted_by(2) _Null_unspecified lifetimeboundConst(const int * _Null_unspecified __single p __lifetimebound);

struct Big {
  int arr[4];
};
const int * __counted_by(4) _Null_unspecified lifetimeboundConstBig(const struct Big * _Null_unspecified __single p __lifetimebound);

// expected-experimental-expansion@+24:60{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@available(swift, obsoleted: 3, renamed: "T.methodLifetimebound(self:)") @_alwaysEmitIntoClient @available(macOS 13.0, *) @_lifetime(copy p) @_disfavoredOverload|}}
//   expected-experimental-remark@3{{macro content: |public func methodLifetimebound(_ p: inout MutableRef<T>?) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@4{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe methodLifetimebound(p?._unsafeAddress)|}}
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
// GENERATED-HASH: fb7b207d35990bf7c2ed5a830f47d60bad41aed4d2d375127544f5c8244c97e0
import Test

func call_lifetimeless(_ p: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe lifetimeless(p)
}

func call_nullUnspecified(_ p: UnsafeMutablePointer<CInt>!) {
  return unsafe nullUnspecified(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ p: inout MutableRef<CInt>?) {
  return nullUnspecified(&p)
}

func call_nonnull(_ p: UnsafeMutablePointer<CInt>) {
  return unsafe nonnull(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ p: inout MutableRef<CInt>) {
  return nonnull(&p)
}

func call_nonnullFlipped(_ p: UnsafeMutablePointer<CInt>) {
  return unsafe nonnullFlipped(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnullFlipped(_ p: inout MutableRef<CInt>) {
  return nonnullFlipped(&p)
}

func call_nullable(_ p: UnsafeMutablePointer<CInt>?) {
  return unsafe nullable(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ p: inout MutableRef<CInt>?) {
  return nullable(&p)
}

func call_nullableFlipped(_ p: UnsafeMutablePointer<CInt>?) {
  return unsafe nullableFlipped(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullableFlipped(_ p: inout MutableRef<CInt>?) {
  return nullableFlipped(&p)
}

func call_nullUnspecifiedConst(_ p: UnsafePointer<CInt>!) {
  return unsafe nullUnspecifiedConst(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecifiedConst(_ p: Ref<CInt>?) {
  return nullUnspecifiedConst(p)
}

func call_nonnullConst(_ p: UnsafePointer<CInt>) {
  return unsafe nonnullConst(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnullConst(_ p: Ref<CInt>) {
  return nonnullConst(p)
}

func call_nullableConst(_ p: UnsafePointer<CInt>?) {
  return unsafe nullableConst(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullableConst(_ p: Ref<CInt>?) {
  return nullableConst(p)
}

func call_nested(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>!) {
  return unsafe nested(p)
}

@available(macOS 13.0, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nested(_ p: inout MutableRef<UnsafeMutablePointer<CInt>?>?) {
  return unsafe nested(&p)
}

func call_voidpointer(_ p: UnsafeMutableRawPointer!) {
  return unsafe voidpointer(p)
}

func call_forwardDeclared(_ p: OpaquePointer!) {
  return unsafe forwardDeclared(p)
}

extension T {
  mutating func call_method_T(_ q: UnsafeMutablePointer<CInt>!) {
    return unsafe method(q)
  }
  @available(macOS 13.0, *)
    @_alwaysEmitIntoClient @_disfavoredOverload mutating func call_method_T(_ q: inout MutableRef<CInt>?) {
    return method(&q)
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

@available(macOS 13.0, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_lifetimebound(_ p: inout MutableRef<CInt>?) -> MutableSpan<CInt> {
  // expected-stable-error@+2{{cannot convert value of type 'UnsafeMutablePointer<MutableRef<CInt>?>' (aka 'UnsafeMutablePointer<Optional<MutableRef<Int32>>>') to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return lifetimebound(&p)
}

func call_lifetimeboundConst(_ p: UnsafePointer<CInt>!) -> UnsafePointer<CInt>! {
  return unsafe lifetimeboundConst(p)
}

func call_lifetimeboundConstBig(_ p: UnsafePointer<Big>!) -> UnsafePointer<CInt>! {
  return unsafe lifetimeboundConstBig(p)
}

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs %t/method.swift -Rmacro-expansions -emit-module \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}method.h -verify-additional-prefix stable- -eager-macro-checking \
// RUN:   -Xcc -Wno-nullability-completeness -strict-memory-safety

// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs %t/method.swift -Rmacro-expansions -emit-module \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}method.h -verify-additional-prefix experimental- -eager-macro-checking \
// RUN:   -Xcc -Wno-nullability-completeness -strict-memory-safety \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

//--- Inputs/module.modulemap
module Method {
    header "method.h"
}

//--- Inputs/method.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
#define __lifetimebound __attribute__((lifetimebound))

@interface Foo
// expected-expansion@+16:1{{
//   expected-note@1 11{{in expansion of macro '_SwiftifyImport' on instance method 'simple' here}}
// }}
// expected-expansion@+13:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func simple(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe simple(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
- (void) simple:(int)len :(int * __counted_by(len) __noescape)p;

// expected-expansion@+25:1{{
//   expected-note@1 20{{in expansion of macro '_SwiftifyImport' on instance method 'shared' here}}
// }}
// expected-expansion@+22:105{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload public final func shared(_ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p1.count)!|}}
//   expected-remark@4{{macro content: |    if p2.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p2.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = p1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _p2Ptr = p2.withUnsafeMutableBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p2)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe shared(len, _p1Ptr.baseAddress!, _p2Ptr.baseAddress!)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
- (void) shared:(int)len :(int * __counted_by(len) __noescape)p1 :(int * __counted_by(len) __noescape)p2;

// expected-expansion@+19:1{{
//   expected-note@1 14{{in expansion of macro '_SwiftifyImport' on instance method 'complexExpr' here}}
// }}
// expected-expansion@+16:92{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-remark@4{{macro content: |    if _pCount != len - offset {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\(len - offset) but got \\(_pCount)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe complexExpr(len, offset, _pPtr.baseAddress!)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
- (void) complexExpr:(int)len :(int) offset :(int * __counted_by(len - offset) __noescape)p;

// expected-expansion@+16:1{{
//   expected-note@1 11{{in expansion of macro '_SwiftifyImport' on instance method 'nullUnspecified' here}}
// }}
// expected-expansion@+13:91{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func nullUnspecified(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullUnspecified(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
- (void) nullUnspecified:(int)len :(int * __counted_by(len) _Null_unspecified __noescape)p;

// expected-expansion@+16:1{{
//   expected-note@1 11{{in expansion of macro '_SwiftifyImport' on instance method 'nonnull' here}}
// }}
// expected-expansion@+13:74{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func nonnull(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nonnull(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
- (void) nonnull:(int)len :(int * __counted_by(len) __noescape _Nonnull)p;

// expected-expansion@+16:1{{
//   expected-note@1 11{{in expansion of macro '_SwiftifyImport' on instance method 'nullable' here}}
// }}
// expected-expansion@+13:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func nullable(_ p: inout MutableSpan<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullable(len, _pPtr?.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
- (void) nullable:(int)len :(int * __counted_by(len) _Nullable __noescape)p;


// expected-stable-note@+24{{'returnPointerNullable' declared here}}
// expected-experimental-expansion@+23:1{{
//   expected-experimental-note@1 18{{in expansion of macro '_SwiftifyImport' on instance method 'returnPointerNullable' here}}
// }}
// expected-experimental-expansion@+20:125{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public final func returnPointerNullable(_ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>? {|}}
//   expected-experimental-remark@3{{macro content: |    let len = Int32(exactly: p?.count ?? 0)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p?.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime({ () in|}}
//   expected-experimental-remark@11{{macro content: |      let _resultValue = unsafe returnPointerNullable(len, _pPtr?.baseAddress)|}}
//   expected-experimental-remark@12{{macro content: |      if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@13{{macro content: |        return nil|}}
//   expected-experimental-remark@14{{macro content: |      } else {|}}
//   expected-experimental-remark@15{{macro content: |        return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |      }|}}
//   expected-experimental-remark@17{{macro content: |        }(), copying: ())|}}
//   expected-experimental-remark@18{{macro content: |}|}}
// }}
- (int * __counted_by(len) _Nullable) returnPointerNullable:(int)len : (int * __counted_by(len) _Nullable) __lifetimebound p;


// expected-stable-note@+17{{'returnPointerNonnull' declared here}}
// expected-experimental-expansion@+16:1{{
//   expected-experimental-note@1 11{{in expansion of macro '_SwiftifyImport' on instance method 'returnPointerNonnull' here}}
// }}
// expected-experimental-expansion@+13:122{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public final func returnPointerNonnull(_ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {|}}
//   expected-experimental-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe returnPointerNonnull(len, _pPtr.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
- (int * __counted_by(len) _Nonnull) returnPointerNonnull:(int)len : (int * __counted_by(len) _Nonnull) __lifetimebound p;

// expected-expansion@+19:105{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_disfavoredOverload public final func mixedEscapability(_ p1: inout MutableSpan<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p1.count)!|}}
//   expected-remark@4{{macro content: |    if p2.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in mixedEscapability: expected \\(len) but got \\(p2.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = p1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe mixedEscapability(len, _p1Ptr.baseAddress!, p2.baseAddress!)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
// expected-expansion@+3:1{{
//   expected-note@1 14{{in expansion of macro '_SwiftifyImport' on instance method 'mixedEscapability' here}}
// }}
- (void) mixedEscapability:(int)len :(int * __counted_by(len) __noescape)p1 :(int * __counted_by(len))p2;

// expected-expansion@+16:1{{
//   expected-note@1 11{{in expansion of macro '_SwiftifyImport' on class method 'staticMethod' here}}
// }}
// expected-expansion@+13:70{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public class final func staticMethod(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe staticMethod(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
+ (void) staticMethod:(int)len :(int * __counted_by(len) __noescape)p;
@end

//--- method.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Method -plugin-path %swift-plugin-dir -I %t/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers > %t/Test-interface.swift && %swift-function-caller-generator Method %t/Test-interface.swift
// GENERATED-HASH: 4d19800cab5922f4a511285b82fb18b26cf370a59338925cd4a173de62bcdaf4
import Method

func call_simple(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.simple(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p: copy p)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ self: Foo, _ p: inout MutableSpan<Int32>) {
  return self.simple(&p)
}

func call_simple(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe self.simple(len, p)
}
func call_shared(_ len: Int32, _ p1: UnsafeMutablePointer<Int32>!, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.shared(len, p1, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p1: copy p1)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p2: copy p2)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ self: Foo, _ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>) {
  return self.shared(&p1, &p2)
}

func call_shared(_ self: Foo, _ len: Int32, _ p1: UnsafeMutablePointer<Int32>!, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe self.shared(len, p1, p2)
}
func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.complexExpr(len, offset, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p: copy p)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ self: Foo, _ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) {
  return self.complexExpr(len, offset, &p)
}

func call_complexExpr(_ self: Foo, _ len: Int32, _ offset: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe self.complexExpr(len, offset, p)
}
func call_nullUnspecified(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.nullUnspecified(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p: copy p)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ self: Foo, _ p: inout MutableSpan<Int32>) {
  return self.nullUnspecified(&p)
}

func call_nullUnspecified(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe self.nullUnspecified(len, p)
}
func call_nonnull(_ len: Int32, _ p: UnsafeMutablePointer<Int32>) {
  return unsafe Foo.nonnull(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p: copy p)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ self: Foo, _ p: inout MutableSpan<Int32>) {
  return self.nonnull(&p)
}

func call_nonnull(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>) {
  return unsafe self.nonnull(len, p)
}
func call_nullable(_ len: Int32, _ p: UnsafeMutablePointer<Int32>?) {
  return unsafe Foo.nullable(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p: copy p)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ self: Foo, _ p: inout MutableSpan<Int32>?) {
  return self.nullable(&p)
}

func call_nullable(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>?) {
  return unsafe self.nullable(len, p)
}
func call_returnPointerNullable(_ len: Int32, _ p: UnsafeMutablePointer<Int32>?) -> UnsafeMutablePointer<Int32>? {
  return unsafe Foo.returnPointerNullable(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(copy p)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p: copy p)
  // expected-stable-error@+1{{a function cannot return a ~Escapable result}}
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointerNullable(_ self: Foo, _ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>? {
  // expected-stable-error@+6{{missing argument for parameter #2 in call}}
  // expected-stable-note@+5{{arguments to generic parameter 'Pointee' ('MutableSpan<Int32>?' and 'Int32') are expected to be equal}}
  // expected-stable-error@+4{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<Int32>?>' to expected argument type 'UnsafeMutablePointer<Int32>'}}
  // expected-stable-error@+3{{cannot convert value of type 'MutableSpan<Int32>?' to expected argument type 'Int32'}}
  // expected-stable-error@+2{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>?'}}
  // expected-stable-note@+1{{arguments to generic parameter 'Wrapped' ('UnsafeMutablePointer<Int32>' and 'MutableSpan<Int32>') are expected to be equal}}
  return self.returnPointerNullable(&p)
}

func call_returnPointerNullable(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>?) -> UnsafeMutablePointer<Int32>? {
  return unsafe self.returnPointerNullable(len, p)
}
func call_returnPointerNonnull(_ len: Int32, _ p: UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32> {
  return unsafe Foo.returnPointerNonnull(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(copy p)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p: copy p)
  // expected-stable-error@+1{{a function cannot return a ~Escapable result}}
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointerNonnull(_ self: Foo, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
  // expected-stable-error@+5{{missing argument for parameter #2 in call}}
  // expected-stable-error@+4{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<Int32>>' to expected argument type 'UnsafeMutablePointer<Int32>'}}
  // expected-stable-error@+3{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  // expected-stable-note@+2{{arguments to generic parameter 'Pointee' ('MutableSpan<Int32>' and 'Int32') are expected to be equal}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>' to return type 'MutableSpan<Int32>'}}
  return self.returnPointerNonnull(&p)
}

func call_returnPointerNonnull(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32> {
  return unsafe self.returnPointerNonnull(len, p)
}
func call_mixedEscapability(_ len: Int32, _ p1: UnsafeMutablePointer<Int32>!, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.mixedEscapability(len, p1, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p1: copy p1)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_mixedEscapability(_ self: Foo, _ p1: inout MutableSpan<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {
  return unsafe self.mixedEscapability(&p1, p2)
}

func call_mixedEscapability(_ self: Foo, _ len: Int32, _ p1: UnsafeMutablePointer<Int32>!, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe self.mixedEscapability(len, p1, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  // expected-stable-error@+1{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
  @_lifetime(p: copy p)
  @_alwaysEmitIntoClient @_disfavoredOverload public func call_staticMethod(_ p: inout MutableSpan<Int32>) {
  return Foo.staticMethod(&p)
}
func call_staticMethod(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.staticMethod(len, p)
}

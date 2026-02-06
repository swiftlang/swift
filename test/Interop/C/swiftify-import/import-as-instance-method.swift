// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t%{fs-sep}Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}instance.h %t/test.swift -I %bridging-path -DVERIFY
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift -dump-macro-expansions -I %bridging-path 2> %t/out.txt
// RUN: diff --strip-trailing-cr %t/out.txt %t/out.expected

//--- test.swift
import Instance

@available(SwiftStdlib 5.8, *)
func foo(_ p: inout MutableSpan<CInt>, a: A, aa: inout A, c: C, b: B, bb: inout B) {
  aa.basic(&p)
  aa.bar(&p)
  a.constSelf(&p)
  a.valSelf(&p)
  let _: MutableSpan<CInt> = a.lifetimeBoundSelf(3)
  c.refSelf(&p)
  b.nonescaping(&p)
  let _: MutableSpan<CInt> = bb.nonescapingLifetimebound(73)

#if VERIFY
  aa.countedSelf(&p)
  a.basic(&p) // expected-error{{cannot use mutating member on immutable value: 'a' is a 'let' constant}}
#endif
}

//--- Inputs/instance.h
#include <ptrcheck.h>
#include <lifetimebound.h>
#include <swift/bridging>

struct A {};
struct SWIFT_NONESCAPABLE B {};
struct SWIFT_IMMORTAL_REFERENCE C {};

void basic(struct A *a, int * __counted_by(len) p __noescape, int len) __attribute__((swift_name("A.basic(self:_:_:)")));

void renamed(struct A *a, int * __counted_by(len) p __noescape, int len) __attribute__((swift_name("A.bar(self:_:_:)")));

void countedSelf(struct A * __counted_by(len)
  a, // expected-warning{{bounds attribute '__counted_by' ignored on parameter mapped to 'self'}}
  int * __counted_by(len) p __noescape, int len)
  __attribute__((
  swift_name // expected-note{{swift_name maps free function to instance method here}}
  ("A.countedSelf(self:_:_:)")));

void constSelf(const struct A *a, int * __counted_by(len) p __noescape, int len) __attribute__((swift_name("A.constSelf(self:_:_:)")));

void valSelf(struct A a, int * __counted_by(len) p __noescape, int len) __attribute__((swift_name("A.valSelf(self:_:_:)")));

void refSelf(struct C *c, int * __counted_by(len) p __noescape, int len) __attribute__((swift_name("C.refSelf(self:_:_:)")));

int * __counted_by(len) lifetimeBoundSelf(struct A a __lifetimebound, int len) __attribute__((swift_name("A.lifetimeBoundSelf(self:_:)")));

void nonescaping(const struct B *d, int * __counted_by(len) p __noescape, int len) __attribute__((swift_name("B.nonescaping(self:_:_:)")));

int * __counted_by(len) nonescapingLifetimebound(struct B *d __lifetimebound, int len) __attribute__((swift_name("B.nonescapingLifetimebound(self:_:)")));

//--- Inputs/module.modulemap
module Instance {
  header "instance.h"
}

//--- out.expected
@__swiftmacro_So1AV5basic15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public mutating func basic(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe basic(_pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So5basic15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.basic(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func basic(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe basic(a, _pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So1AV3bar15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public mutating func bar(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe bar(_pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So7renamed15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.bar(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func renamed(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe renamed(a, _pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So1AV9constSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func constSelf(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe constSelf(_pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So9constSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.constSelf(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func constSelf(_ a: UnsafePointer<A>!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe constSelf(a, _pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So1AV7valSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func valSelf(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe valSelf(_pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So7valSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.valSelf(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func valSelf(_ a: A, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe valSelf(a, _pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So1AV17lifetimeBoundSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload
public func lifetimeBoundSelf(_ len: Int32) -> MutableSpan<Int32> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe lifetimeBoundSelf(len), count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So17lifetimeBoundSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.lifetimeBoundSelf(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow a) @_disfavoredOverload
public func lifetimeBoundSelf(_ a: A, _ len: Int32) -> MutableSpan<Int32> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe lifetimeBoundSelf(a, len), count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So1CV7refSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func refSelf(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe refSelf(_pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So7refSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "C.refSelf(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func refSelf(_ c: C!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe refSelf(c, _pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So1BV11nonescaping15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func nonescaping(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe nonescaping(_pPtr.baseAddress!, len)
    }
}
------------------------------
@__swiftmacro_So1BV24nonescapingLifetimebound15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&self) @_disfavoredOverload
public mutating func nonescapingLifetimebound(_ len: Int32) -> MutableSpan<Int32> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe nonescapingLifetimebound(len), count: Int(len)), copying: ())
}
------------------------------

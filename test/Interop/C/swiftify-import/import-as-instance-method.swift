// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t%{fs-sep}Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety \
// RUN:    -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}instance.h %t/test.swift -I %bridging-path -DVERIFY
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t%{fs-sep}Inputs -strict-memory-safety \
// RUN:    -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}instance.h %t/test.swift -I %bridging-path -DVERIFY -verify-additional-prefix nolifetimebound-
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift -dump-macro-expansions -I %bridging-path 2> %t/out.txt
// RUN: diff --strip-trailing-cr %t/out.txt %t/out.expected

//--- test.swift
import Instance

@available(SwiftStdlib 5.8, *)
func foo(_ p: inout MutableSpan<CInt>, a: A, aa: inout A, c: C, b: B, bb: inout B, d: D) {
  aa.basic(&p)
  aa.bar(&p)
  a.constSelf(&p)
  a.valSelf(&p)
  // expected-nolifetimebound-error@+1{{cannot convert value of type 'UnsafeMutablePointer<Int32>?' to specified type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  let _: MutableSpan<CInt> = a.lifetimeBoundSelf(3)
  c.refSelf(&p)
  d.refSelf(&p)
  b.nonescaping(&p)
  // expected-nolifetimebound-error@+1{{cannot convert value of type 'UnsafeMutablePointer<Int32>?' to specified type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  let _: MutableSpan<CInt> = bb.nonescapingLifetimebound(73)

#if VERIFY
  aa.countedSelf(&p)
  a.basic(&p) // expected-error{{cannot use mutating member on immutable value: 'a' is a 'let' constant}}
#endif
}

@available(SwiftStdlib 5.8, *)
func testInit(_ p: UnsafeMutablePointer<CInt>, _ len: CInt, _ s: inout MutableSpan<CInt>, _ bp: inout UnsafeMutableBufferPointer<CInt>) {
  let _ = unsafe A(countA: len, pointerA: p)
  let _ = unsafe A(pointerA: bp)
  let _ = A(pointerA2: &s)
  // let _ = unsafe B(len, p)
  // let _ = B(&s)
  let _ = unsafe C(countC: len, pointerC: p)
  let _ = unsafe C(pointerC: bp)
  let _ = unsafe D(countD: len, pointerD: p)

#if VERIFY
  // expected-error@+2{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'UnsafeMutablePointer<Int32>'}}
  // expected-error@+1{{missing argument for parameter 'countD' in call}}
  let _ = unsafe D(pointerD: bp)
#endif
}

//--- Inputs/instance.h
#include <ptrcheck.h>
#include <lifetimebound.h>
#include <swift/bridging>

struct A {};
struct SWIFT_NONESCAPABLE B {};
struct SWIFT_IMMORTAL_REFERENCE C {};
typedef struct __attribute__((objc_bridge(id))) D *DRef;

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

void refSelfCF(DRef d, int * __counted_by(len) p __noescape, int len) __attribute__((swift_name("D.refSelf(self:_:_:)")));

int * __counted_by(len) lifetimeBoundSelf(struct A a __lifetimebound, int len) __attribute__((swift_name("A.lifetimeBoundSelf(self:_:)")));

void nonescaping(const struct B *d, int * __counted_by(len) p __noescape, int len) __attribute__((swift_name("B.nonescaping(self:_:_:)")));

int * __counted_by(len) nonescapingLifetimebound(struct B *d __lifetimebound, int len) __attribute__((swift_name("B.nonescapingLifetimebound(self:_:)")));

struct A * createA(int len, int * __counted_by(len) p) __attribute__((swift_name("A.init(countA:pointerA:)")));
struct A * createA2(int len, int * __counted_by(len) p __noescape) __attribute__((swift_name("A.init(countA2:pointerA2:)")));

// This crashes the compiler rdar://169580475
// struct B * createB(int len, int * __counted_by(len) p __lifetimebound) __attribute__((swift_name("B.init(_:_:)")));

struct C * createC(int len, int * __counted_by(len) p) __attribute__((swift_name("C.init(countC:pointerC:)")));

// This should not generate an overload, because Swift does not allow user-defined initiailizers for CF-style foreign references
// expected-note@+1{{'init(countD:pointerD:)' declared here}}
DRef createD(int len, int * __counted_by(len) p) __attribute__((swift_name("D.init(countD:pointerD:)")));

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
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe basic(_pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So5basic15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.basic(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func basic(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe basic(a, _pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So1AV3bar15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public mutating func bar(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe bar(_pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So7renamed15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.bar(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func renamed(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe renamed(a, _pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So1AV9constSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func constSelf(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe constSelf(_pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So9constSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.constSelf(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func constSelf(_ a: UnsafePointer<A>!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe constSelf(a, _pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So1AV7valSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func valSelf(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe valSelf(_pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So7valSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.valSelf(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func valSelf(_ a: A, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe valSelf(a, _pPtr.baseAddress!, len)
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
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe refSelf(_pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So7refSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "C.refSelf(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func refSelf(_ c: C!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe refSelf(c, _pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So4DRefa7refSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func refSelf(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe refSelf(_pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So9refSelfCF15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "D.refSelf(self:_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func refSelfCF(_ d: D!, _ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe refSelfCF(d, _pPtr.baseAddress!, len)
}
------------------------------
@__swiftmacro_So1BV11nonescaping15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func nonescaping(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe nonescaping(_pPtr.baseAddress!, len)
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
@__swiftmacro_So1AV4init15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public /*not inherited*/ init!(pointerA p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: p.count)!
    unsafe self.init(countA: len, pointerA: p.baseAddress!)
}
------------------------------
@__swiftmacro_So7createA15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.init(countA:pointerA:)") @_alwaysEmitIntoClient @_disfavoredOverload
public func createA(_ p: UnsafeMutableBufferPointer<Int32>) -> UnsafeMutablePointer<A>! {
    let len = Int32(exactly: p.count)!
    return unsafe createA(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So1AV4init15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public /*not inherited*/ init!(pointerA2 p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    unsafe self.init(countA2: len, pointerA2: _pPtr.baseAddress!)
}
------------------------------
@__swiftmacro_So8createA215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.init(countA2:pointerA2:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func createA2(_ p: inout MutableSpan<Int32>) -> UnsafeMutablePointer<A>! {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe createA2(len, _pPtr.baseAddress!)
}
------------------------------
@__swiftmacro_So1CV4init15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public /*not inherited*/ convenience init!(pointerC p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: p.count)!
    unsafe self.init(countC: len, pointerC: p.baseAddress!)
}
------------------------------
@__swiftmacro_So7createC15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "C.init(countC:pointerC:)") @_alwaysEmitIntoClient @_disfavoredOverload
public func createC(_ p: UnsafeMutableBufferPointer<Int32>) -> C! {
    let len = Int32(exactly: p.count)!
    return unsafe createC(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So7createD15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "D.init(countD:pointerD:)") @_alwaysEmitIntoClient @_disfavoredOverload
public func createD(_ p: UnsafeMutableBufferPointer<Int32>) -> Unmanaged<D>! {
    let len = Int32(exactly: p.count)!
    return unsafe createD(len, p.baseAddress!)
}
------------------------------

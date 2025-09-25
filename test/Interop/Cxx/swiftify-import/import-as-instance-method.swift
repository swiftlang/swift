// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift -cxx-interoperability-mode=default -Xcc -std=c++20 -dump-macro-expansions 2> %t/out.txt
// RUN: diff --strip-trailing-cr %t/out.txt %t/out.expected
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift  -cxx-interoperability-mode=default -Xcc -std=c++20 -verify -verify-additional-file %t/Inputs/instance.h %t/test.swift -DVERIFY

//--- test.swift
import Instance

func foo(_ p: inout MutableSpan<CInt>, a: A, aa: inout A, s: IntSpan, cs: ConstIntSpan, asp: AliasingSpan, b: inout baz.B) {
  aa.basic(&p)
  aa.bar(&p)
  a.constSelf(&p)
  a.valSelf(&p)
  aa.refSelf(&p)
  aa.namespaced(&p)
  b.decapseman(&p)

  let _: IntSpan = unsafe s.spanSelf()
  let _: MutableSpan<CInt> = unsafe s.spanSelf()
  let _: IntSpan = unsafe s.spanConstSelf()
  let _: MutableSpan<CInt> = unsafe s.spanConstSelf()
  let _: ConstIntSpan = unsafe cs.constSpanSelf()
  let _: Span<CInt> = unsafe cs.constSpanSelf()
  let _: IntSpan = unsafe asp.spanSelf()
  let _: MutableSpan<CInt> = unsafe asp.spanSelf()
#if VERIFY
  p.spanSelf() // expected-error{{value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') has no member 'spanSelf'}}
#endif
}

//--- Inputs/instance.h
#include <ptrcheck.h>
#include <lifetimebound.h>
#include <span>

struct A {};

using IntSpan = std::span<int>;
using ConstIntSpan = std::span<const int>;
using AliasingSpan = std::span<int>;

void basic(A *a, IntSpan p __noescape) __attribute__((swift_name("A.basic(self:_:)")));

void renamed(A *a, IntSpan p __noescape) __attribute__((swift_name("A.bar(self:_:)")));

void constSelf(const A *a, IntSpan p __noescape) __attribute__((swift_name("A.constSelf(self:_:)")));

void valSelf(A a, IntSpan p __noescape) __attribute__((swift_name("A.valSelf(self:_:)")));

void refSelf(A &a, IntSpan p __noescape) __attribute__((swift_name("A.refSelf(self:_:)")));

IntSpan spanSelf(IntSpan p __lifetimebound) __attribute__((swift_name("IntSpan.spanSelf(self:)")));

const IntSpan spanConstSelf(const IntSpan p __lifetimebound) __attribute__((swift_name("IntSpan.spanConstSelf(self:)")));

ConstIntSpan constSpanSelf(ConstIntSpan p __lifetimebound) __attribute__((swift_name("ConstIntSpan.constSpanSelf(self:)")));

namespace baz {
  void namespaced(A *a, IntSpan p __noescape) __attribute__((swift_name("A.namespaced(self:_:)")));
  struct B {};
}

void decapseman(baz::B *b, IntSpan p __noescape) __attribute__((swift_name("baz.B.decapseman(self:_:)")));

//--- Inputs/module.modulemap
module Instance {
  header "instance.h"
  export std.span
}

//--- out.expected
@__swiftmacro_So1AV5basic15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public mutating func basic(_ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe basic(IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So5basic15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.basic(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func basic(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe basic(a, IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So1AV3bar15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public mutating func bar(_ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe bar(IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So7renamed15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.bar(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func renamed(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe renamed(a, IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So1AV9constSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func constSelf(_ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe constSelf(IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So9constSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.constSelf(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func constSelf(_ a: UnsafePointer<A>!, _ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe constSelf(a, IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So1AV7valSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func valSelf(_ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe valSelf(IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So7valSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.valSelf(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func valSelf(_ a: A, _ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe valSelf(a, IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So1AV7refSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public mutating func refSelf(_ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe refSelf(IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So7refSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.refSelf(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func refSelf(_ a: inout A, _ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe refSelf(a, IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So1AV10namespaced15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public mutating func namespaced(_ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe namespaced(IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So3bazO10namespaced15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "A.namespaced(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
  public static func namespaced(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe namespaced(a, IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So3bazO1BV8InstanceE10decapseman15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public mutating func decapseman(_ p: inout MutableSpan<CInt>)  {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe decapseman(IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So10decapseman15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "baz.B.decapseman(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func decapseman(_ b: UnsafeMutablePointer<baz.B>!, _ p: inout MutableSpan<CInt>) {
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe decapseman(b, IntSpan(_pPtr))
    }
}
------------------------------
@__swiftmacro_So3stdO3__1O0056spanCInt_CUnsignedLong_18446744073709551615_syGJqopasxheV8InstanceE8spanSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload
public func spanSelf() -> MutableSpan<CInt> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe spanSelf()), copying: ())
}
------------------------------
@__swiftmacro_So8spanSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "IntSpan.spanSelf(self:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload
public func spanSelf(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe spanSelf(IntSpan(_pPtr))
            }), copying: ())
}
------------------------------
@__swiftmacro_So3stdO3__1O0056spanCInt_CUnsignedLong_18446744073709551615_syGJqopasxheV8InstanceE13spanConstSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload
public func spanConstSelf() -> MutableSpan<CInt> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe spanConstSelf()), copying: ())
}
------------------------------
@__swiftmacro_So13spanConstSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "IntSpan.spanConstSelf(self:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload
public func spanConstSelf(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe spanConstSelf(IntSpan(_pPtr))
            }), copying: ())
}
------------------------------
@__swiftmacro_So3stdO3__1O0071span__cxxConstCInt_CUnsignedLong_18446744073709551615_ilHEvDsarBakaEjpbV8InstanceE13constSpanSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload
public func constSpanSelf() -> Span<CInt> {
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe constSpanSelf()), copying: ())
}
------------------------------
@__swiftmacro_So13constSpanSelf15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(swift, obsoleted: 3, renamed: "ConstIntSpan.constSpanSelf(self:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload
public func constSpanSelf(_ p: Span<CInt>) -> Span<CInt> {
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe constSpanSelf(ConstIntSpan(p))), copying: ())
}
------------------------------

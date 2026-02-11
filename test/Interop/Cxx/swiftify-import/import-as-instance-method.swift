// REQUIRES: swift_feature_StabilizedSafeInteropWrappers
// REQUIRES: std_span

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -Xcc -Werror %t/test.swift  -cxx-interoperability-mode=default -Xcc -std=c++20 \
// RUN:   -verify -verify-additional-file %t/Inputs/instance.h -DVERIFY
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -strict-memory-safety -Xcc -Werror %t/test.swift  -cxx-interoperability-mode=default -Xcc -std=c++20 \
// RUN:   -verify -verify-additional-file %t/Inputs/instance.h -DVERIFY -verify-additional-prefix nolifetimebound-
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/Test.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror %t/test.swift -cxx-interoperability-mode=default -Xcc -std=c++20 -dump-macro-expansions 2>&1 | %FileCheck %s --match-full-lines --strict-whitespace --implicit-check-not __swiftmacro

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
  baz.bar(&aa, &p)
  baz.this(&aa, &p)

  let _: IntSpan = unsafe s.spanSelf()
  // expected-nolifetimebound-error@+1{{cannot convert value of type 'IntSpan' (aka 'std.__1.span<CInt, _CUnsignedLong_18446744073709551615>') to specified type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  let _: MutableSpan<CInt> = unsafe s.spanSelf()
  let _: IntSpan = unsafe s.spanConstSelf()
  // expected-nolifetimebound-error@+1{{cannot convert value of type 'IntSpan' (aka 'std.__1.span<CInt, _CUnsignedLong_18446744073709551615>') to specified type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  let _: MutableSpan<CInt> = unsafe s.spanConstSelf()
  let _: ConstIntSpan = unsafe cs.constSpanSelf()
  // expected-nolifetimebound-error@+1{{cannot convert value of type 'ConstIntSpan' (aka 'std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>') to specified type 'Span<CInt>' (aka 'Span<Int32>')}}
  let _: Span<CInt> = unsafe cs.constSpanSelf()
  let _: IntSpan = unsafe asp.spanSelf()
  // expected-nolifetimebound-error@+1{{cannot convert value of type 'IntSpan' (aka 'std.__1.span<CInt, _CUnsignedLong_18446744073709551615>') to specified type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
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
  void renamed(A &a, IntSpan p __noescape) __attribute__((swift_name("baz.bar(_:_:)")));
  void that(A &a, IntSpan p __noescape) __attribute__((swift_name("this(_:_:)")));
}

void decapseman(baz::B *b, IntSpan p __noescape) __attribute__((swift_name("baz.B.decapseman(self:_:)")));

//--- Inputs/module.modulemap
module Instance {
  header "instance.h"
  export *
}

//--- out.expected
// CHECK:@__swiftmacro_{{.*}}basic{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public mutating func basic(_ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe basic(IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}basic{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "A.basic(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func basic(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe basic(a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}bar{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public mutating func bar(_ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe bar(IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}renamed{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "A.bar(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func renamed(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe renamed(a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}constSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func constSelf(_ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe constSelf(IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}constSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "A.constSelf(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func constSelf(_ a: UnsafePointer<A>!, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe constSelf(a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}valSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func valSelf(_ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe valSelf(IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}valSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "A.valSelf(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func valSelf(_ a: A, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe valSelf(a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}refSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public mutating func refSelf(_ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe refSelf(IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}refSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "A.refSelf(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func refSelf(_ a: inout A, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe refSelf(&a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}namespaced{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public mutating func namespaced(_ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe namespaced(IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}baz{{.*}}namespaced{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "A.namespaced(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:  public static func namespaced(_ a: UnsafeMutablePointer<A>!, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe namespaced(a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}baz{{.*}}Instance{{.*}}decapseman{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public mutating func decapseman(_ p: inout MutableSpan<CInt>)  {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe decapseman(IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}decapseman{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "baz.B.decapseman(self:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func decapseman(_ b: UnsafeMutablePointer<baz.B>!, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe decapseman(b, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}baz{{.*}}Instance{{.*}}bar{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public static func bar(_ a: inout A, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe bar(&a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}baz{{.*}}renamed{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "baz.bar(_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:  public static func renamed(_ a: inout A, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe renamed(&a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}baz{{.*}}this{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public static func this(_ a: inout A, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe this(&a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}baz{{.*}}that{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "this(_:_:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:  public static func that(_ a: inout A, _ p: inout MutableSpan<CInt>) {
// CHECK-NEXT:    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe that(&a, IntSpan(_pPtr))
// CHECK-NEXT:    }
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}Instance{{.*}}spanSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload
// CHECK-NEXT:public func spanSelf() -> MutableSpan<CInt> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe spanSelf()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_So8spanSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "IntSpan.spanSelf(self:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func spanSelf(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe spanSelf(IntSpan(_pPtr))
// CHECK-NEXT:            }), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}Instance{{.*}}spanConstSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload
// CHECK-NEXT:public func spanConstSelf() -> MutableSpan<CInt> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe spanConstSelf()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}spanConstSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "IntSpan.spanConstSelf(self:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload
// CHECK-NEXT:public func spanConstSelf(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:      return unsafe spanConstSelf(IntSpan(_pPtr))
// CHECK-NEXT:            }), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}Instance{{.*}}constSpanSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow self) @_disfavoredOverload
// CHECK-NEXT:public func constSpanSelf() -> Span<CInt> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe constSpanSelf()), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------
// CHECK-NEXT:@__swiftmacro_{{.*}}constSpanSelf{{.*}}_SwiftifyImportfMp_.swift
// CHECK-NEXT:------------------------------
// CHECK-NEXT:/// This is an auto-generated wrapper for safer interop
// CHECK-NEXT:@available(swift, obsoleted: 3, renamed: "ConstIntSpan.constSpanSelf(self:)") @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload
// CHECK-NEXT:public func constSpanSelf(_ p: Span<CInt>) -> Span<CInt> {
// CHECK-NEXT:    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe constSpanSelf(ConstIntSpan(p))), copying: ())
// CHECK-NEXT:}
// CHECK-NEXT:------------------------------

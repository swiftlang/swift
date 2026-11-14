// REQUIRES: std_span
// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Cells
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Without the legacy SafeInteropWrappers feature, returned spans are exposed as Span<[Const]Cell<T>>.
// RUN: %target-swift-frontend -emit-sil -o /dev/null -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 \
// RUN:   -enable-experimental-feature Cells -enable-experimental-feature Lifetimes \
// RUN:   -strict-memory-safety -warnings-as-errors -Xcc -Werror %t%{fs-sep}test.swift -eager-macro-checking \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}cell-span-return.h -Rmacro-expansions -verify-ignore-macro-note \
// RUN:   -verify-additional-prefix cells- -verify-additional-prefix wrapped-

// The legacy SafeInteropWrappers feature keeps Span<T> and MutableSpan<T>. Each function gets one
// wrapper per configuration: the cells- and legacy- expansions below come from separate runs.
// The legacy wrappers use Span's availability. The Cell wrappers use Cell's, which omits visionOS;
// visionOS inherits it from iOS, so the two are equivalent.
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Cells -enable-experimental-feature Lifetimes \
// RUN:   -Xcc -Werror %t%{fs-sep}test.swift -D LEGACY -eager-macro-checking \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}cell-span-return.h -Rmacro-expansions -verify-ignore-macro-note \
// RUN:   -verify-additional-prefix legacy- -verify-additional-prefix wrapped-

// Without the Cells feature, lifetimebound is not supported outside the legacy mode, so there are no wrappers.
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 \
// RUN:   -Xcc -Werror %t%{fs-sep}test.swift -D NO_CELLS -eager-macro-checking \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}cell-span-return.h -Rmacro-expansions

//--- Inputs/module.modulemap
module CellSpanReturn {
  header "cell-span-return.h"
  requires cplusplus
}

//--- Inputs/cell-span-return.h
#pragma once

#include <ptrcheck.h>
#include <span>

using IntSpan = std::span<int>;
using ConstIntSpan = std::span<const int>;

struct Container {
  int data[4] = {1, 2, 3, 4};

  // expected-cells-expansion@+15:15{{
  //   expected-cells-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-cells-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&self) @_disfavoredOverload|}}
  //   expected-cells-remark@3{{macro content: |public mutating func view() -> Span<Swift::Cell<CInt>> {|}}
  //   expected-cells-remark@4{{macro content: |    let _resultValue = unsafe view()|}}
  //   expected-cells-remark@5{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeElements: UnsafeBufferPointer(start: UnsafeRawPointer(_resultValue.data() as UnsafeMutablePointer<CInt>?)?.assumingMemoryBound(to: Swift::Cell<CInt>.self), count: Int(_resultValue.size()))), copying: ())|}}
  //   expected-cells-remark@6{{macro content: |}|}}
  // }}
  // expected-legacy-expansion@+7:15{{
  //   expected-legacy-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-legacy-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&self) @_disfavoredOverload|}}
  //   expected-legacy-remark@3{{macro content: |public mutating func view() -> MutableSpan<CInt> {|}}
  //   expected-legacy-remark@4{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe view()), copying: ())|}}
  //   expected-legacy-remark@5{{macro content: |}|}}
  // }}
  IntSpan view() [[clang::lifetimebound]] { return IntSpan(data, 4); }

  // expected-cells-expansion@+15:21{{
  //   expected-cells-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-cells-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_addressableSelf @_lifetime(borrow self) @_disfavoredOverload|}}
  //   expected-cells-remark@3{{macro content: |public borrowing func cview() -> Span<Swift::ConstCell<CInt>> {|}}
  //   expected-cells-remark@4{{macro content: |    let _resultValue = unsafe cview()|}}
  //   expected-cells-remark@5{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeElements: UnsafeBufferPointer(start: UnsafeRawPointer(_resultValue.data() as UnsafePointer<CInt>?)?.assumingMemoryBound(to: Swift::ConstCell<CInt>.self), count: Int(_resultValue.size()))), copying: ())|}}
  //   expected-cells-remark@6{{macro content: |}|}}
  // }}
  // expected-legacy-expansion@+7:21{{
  //   expected-legacy-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-legacy-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_addressableSelf @_lifetime(borrow self) @_disfavoredOverload|}}
  //   expected-legacy-remark@3{{macro content: |public borrowing func cview() -> Span<CInt> {|}}
  //   expected-legacy-remark@4{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe cview()), copying: ())|}}
  //   expected-legacy-remark@5{{macro content: |}|}}
  // }}
  ConstIntSpan cview() const [[clang::lifetimebound]] { return ConstIntSpan(data, 4); }
};

// expected-cells-expansion@+13:35{{
//   expected-cells-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-cells-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&c) @_disfavoredOverload public func viewOf(_ c: inout Container) -> Span<Swift::Cell<CInt>> {|}}
//   expected-cells-remark@3{{macro content: |    let _resultValue = unsafe viewOf(&c)|}}
//   expected-cells-remark@4{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeElements: UnsafeBufferPointer(start: UnsafeRawPointer(_resultValue.data() as UnsafeMutablePointer<CInt>?)?.assumingMemoryBound(to: Swift::Cell<CInt>.self), count: Int(_resultValue.size()))), copying: ())|}}
//   expected-cells-remark@5{{macro content: |}|}}
// }}
// expected-legacy-expansion@+6:35{{
//   expected-legacy-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-legacy-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&c) @_disfavoredOverload public func viewOf(_ c: inout Container) -> MutableSpan<CInt> {|}}
//   expected-legacy-remark@3{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe viewOf(&c)), copying: ())|}}
//   expected-legacy-remark@4{{macro content: |}|}}
// }}
inline IntSpan viewOf(Container &c [[clang::lifetimebound]]) { return c.view(); }

// expected-cells-expansion@+22:85{{
//   expected-cells-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-cells-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&c) @_disfavoredOverload public func countedView(_ c: inout Container, _ n: CInt) -> Span<Swift::Cell<CInt>> {|}}
//   expected-cells-remark@3{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe countedView(&c, n)|}}
//   expected-cells-remark@4{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-cells-remark@5{{macro content: |      precondition(n == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-cells-remark@6{{macro content: |      return Span<Swift::Cell<CInt>>()|}}
//   expected-cells-remark@7{{macro content: |    }|}}
//   expected-cells-remark@8{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span<Swift::Cell<CInt>>(_unsafeStart: UnsafeRawPointer(_resultValue!).assumingMemoryBound(to: Swift::Cell<CInt>.self), count: Int(n)), copying: ())|}}
//   expected-cells-remark@9{{macro content: |}|}}
// }}
// expected-legacy-expansion@+11:85{{
//   expected-legacy-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-legacy-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&c) @_disfavoredOverload public func countedView(_ c: inout Container, _ n: CInt) -> MutableSpan<CInt> {|}}
//   expected-legacy-remark@3{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe countedView(&c, n)|}}
//   expected-legacy-remark@4{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-legacy-remark@5{{macro content: |      precondition(n == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-legacy-remark@6{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-legacy-remark@7{{macro content: |    }|}}
//   expected-legacy-remark@8{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(n)), copying: ())|}}
//   expected-legacy-remark@9{{macro content: |}|}}
// }}
inline int *__counted_by(n) countedView(Container &c [[clang::lifetimebound]], int n) { return c.data; }

// expected-cells-expansion@+22:102{{
//   expected-cells-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-cells-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow c) @_disfavoredOverload public func constCountedView(_ c: borrowing @_addressable Container, _ n: CInt) -> Span<Swift::ConstCell<CInt>> {|}}
//   expected-cells-remark@3{{macro content: |    let _resultValue: UnsafePointer<CInt>? = unsafe constCountedView(c, n)|}}
//   expected-cells-remark@4{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-cells-remark@5{{macro content: |      precondition(n == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-cells-remark@6{{macro content: |      return Span<Swift::ConstCell<CInt>>()|}}
//   expected-cells-remark@7{{macro content: |    }|}}
//   expected-cells-remark@8{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span<Swift::ConstCell<CInt>>(_unsafeStart: UnsafeRawPointer(_resultValue!).assumingMemoryBound(to: Swift::ConstCell<CInt>.self), count: Int(n)), copying: ())|}}
//   expected-cells-remark@9{{macro content: |}|}}
// }}
// expected-legacy-expansion@+11:102{{
//   expected-legacy-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-legacy-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow c) @_disfavoredOverload public func constCountedView(_ c: borrowing @_addressable Container, _ n: CInt) -> Span<CInt> {|}}
//   expected-legacy-remark@3{{macro content: |    let _resultValue: UnsafePointer<CInt>? = unsafe constCountedView(c, n)|}}
//   expected-legacy-remark@4{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-legacy-remark@5{{macro content: |      precondition(n == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-legacy-remark@6{{macro content: |      return Span<CInt>()|}}
//   expected-legacy-remark@7{{macro content: |    }|}}
//   expected-legacy-remark@8{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span<CInt>(_unsafeStart: _resultValue!, count: Int(n)), copying: ())|}}
//   expected-legacy-remark@9{{macro content: |}|}}
// }}
inline const int *__counted_by(n) constCountedView(const Container &c [[clang::lifetimebound]], int n) { return c.data; }

// expected-wrapped-expansion@+6:31{{
//   expected-wrapped-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-wrapped-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(anyAppleOS 26.0, *) @_lifetime(&c) @_disfavoredOverload public func refOf(_ c: inout Container) -> Swift::Ref<Swift::Cell<CInt>> {|}}
//   expected-wrapped-remark@3{{macro content: |    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe refOf(&c)).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())|}}
//   expected-wrapped-remark@4{{macro content: |}|}}
// }}
inline int &refOf(Container &c [[clang::lifetimebound]]) { return c.data[0]; }

// A lifetimebound parameter with bounds info becomes a Span.
// expected-cells-expansion@+36:91{{
//   expected-cells-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-cells-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func forward(_ p: inout MutableSpan<CInt>) -> Span<Swift::Cell<CInt>> {|}}
//   expected-cells-remark@3{{macro content: |    let n = CInt(exactly: p.count)!|}}
//   expected-cells-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-cells-remark@5{{macro content: |        unsafe $0|}}
//   expected-cells-remark@6{{macro content: |    }|}}
//   expected-cells-remark@7{{macro content: |    defer {|}}
//   expected-cells-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-cells-remark@9{{macro content: |    }|}}
//   expected-cells-remark@10{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe forward(_pPtr.baseAddress, n)|}}
//   expected-cells-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-cells-remark@12{{macro content: |      precondition(n == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-cells-remark@13{{macro content: |      return Span<Swift::Cell<CInt>>()|}}
//   expected-cells-remark@14{{macro content: |    }|}}
//   expected-cells-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span<Swift::Cell<CInt>>(_unsafeStart: UnsafeRawPointer(_resultValue!).assumingMemoryBound(to: Swift::Cell<CInt>.self), count: Int(n)), copying: ())|}}
//   expected-cells-remark@16{{macro content: |}|}}
// }}
// expected-legacy-expansion@+18:91{{
//   expected-legacy-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-legacy-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func forward(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-legacy-remark@3{{macro content: |    let n = CInt(exactly: p.count)!|}}
//   expected-legacy-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-legacy-remark@5{{macro content: |        unsafe $0|}}
//   expected-legacy-remark@6{{macro content: |    }|}}
//   expected-legacy-remark@7{{macro content: |    defer {|}}
//   expected-legacy-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-legacy-remark@9{{macro content: |    }|}}
//   expected-legacy-remark@10{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe forward(_pPtr.baseAddress, n)|}}
//   expected-legacy-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-legacy-remark@12{{macro content: |      precondition(n == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-legacy-remark@13{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-legacy-remark@14{{macro content: |    }|}}
//   expected-legacy-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(n)), copying: ())|}}
//   expected-legacy-remark@16{{macro content: |}|}}
// }}
inline int *__counted_by(n) forward(int *__counted_by(n) p [[clang::lifetimebound]], int n) { return p; }

// A noescape std::span parameter becomes a Span.
// expected-cells-expansion@+17:73{{
//   expected-cells-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-cells-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&c) @_lifetime(s: copy s) @_disfavoredOverload public func withSpanParam(_ s: inout MutableSpan<CInt>, _ c: inout Container) -> Span<Swift::Cell<CInt>> {|}}
//   expected-cells-remark@3{{macro content: |    let _resultValue = s.withUnsafeMutableBufferPointer { _sPtr in|}}
//   expected-cells-remark@4{{macro content: |      return unsafe withSpanParam(IntSpan(_sPtr), &c)|}}
//   expected-cells-remark@5{{macro content: |    }|}}
//   expected-cells-remark@6{{macro content: |    return unsafe _swiftifyOverrideLifetime(Span(_unsafeElements: UnsafeBufferPointer(start: UnsafeRawPointer(_resultValue.data() as UnsafeMutablePointer<CInt>?)?.assumingMemoryBound(to: Swift::Cell<CInt>.self), count: Int(_resultValue.size()))), copying: ())|}}
//   expected-cells-remark@7{{macro content: |}|}}
// }}
// expected-legacy-expansion@+8:73{{
//   expected-legacy-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-legacy-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(&c) @_lifetime(s: copy s) @_disfavoredOverload public func withSpanParam(_ s: inout MutableSpan<CInt>, _ c: inout Container) -> MutableSpan<CInt> {|}}
//   expected-legacy-remark@3{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: s.withUnsafeMutableBufferPointer { _sPtr in|}}
//   expected-legacy-remark@4{{macro content: |      return unsafe withSpanParam(IntSpan(_sPtr), &c)|}}
//   expected-legacy-remark@5{{macro content: |            }), copying: ())|}}
//   expected-legacy-remark@6{{macro content: |}|}}
// }}
inline IntSpan withSpanParam(IntSpan s [[clang::noescape]], Container &c [[clang::lifetimebound]]) { return c.view(); }

// The count cannot be expressed in Swift, so the return value is not a Span and
// lifetimebound is not supported outside the legacy mode.
inline int *__counted_by(*len) unprintableCount(Container &c [[clang::lifetimebound]], const int *__counted_by(m) p, int m, int *len) { return c.data; }

// Cell requires an Escapable element.
struct __attribute__((swift_attr("~Escapable"))) View {
  const int *p;
};
using ViewSpan = std::span<View>;
inline ViewSpan views(Container &c [[clang::lifetimebound]]) { return {}; }

// A dependence on a class is not supported.
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) RefCounted {
  int data[4] = {1, 2, 3, 4};
  IntSpan view() [[clang::lifetimebound]] { return IntSpan(data, 4); }
};

//--- test.swift
import CellSpanReturn

// Macros on imported functions are only expanded when the functions are looked
// up, so each configuration uses all of them.
func forwardPointer(_ p: UnsafeMutablePointer<CInt>) {
  let _: UnsafeMutablePointer<CInt>! = unsafe forward(p, 4)
}

func spanParam(_ c: inout Container) {
  let _: IntSpan = unsafe withSpanParam(IntSpan(), &c)
}

@available(SwiftStdlib 5.8, *)
func refCounted(_ r: RefCounted) {
  let _: IntSpan = unsafe r.view()
}

#if LEGACY
func test(_ c: inout Container) {
  do {
    let s: MutableSpan<CInt> = c.view()
    _ = s.count
  }
  do {
    let s: MutableSpan<CInt> = viewOf(&c)
    _ = s.count
  }
  do {
    let s: MutableSpan<CInt> = countedView(&c, 4)
    _ = s.count
  }
  let s: Span<CInt> = c.cview()
  _ = s.count
  let t: Span<CInt> = constCountedView(c, 4)
  _ = t.count
}

@available(SwiftStdlib 6.2, *)
func ref(_ c: inout Container) {
  let r: Ref<Cell<CInt>> = refOf(&c)
  _ = r.value.value
}
#else
func unwrapped(_ c: inout Container, _ p: UnsafePointer<CInt>, _ len: UnsafeMutablePointer<CInt>) {
  let _: UnsafeMutablePointer<CInt>! = unsafe unprintableCount(&c, p, 4, len)
  let _: ViewSpan = unsafe views(&c)
}

#if NO_CELLS
func test(_ c: inout Container) {
  let _: IntSpan = c.view()
  let _: IntSpan = viewOf(&c)
  let _: UnsafeMutablePointer<CInt>! = unsafe countedView(&c, 4)
  let _: UnsafeMutablePointer<CInt> = unsafe refOf(&c)
  let _: ConstIntSpan = c.cview()
  let _: UnsafePointer<CInt>! = unsafe constCountedView(c, 4)
}
#else
func test(_ c: inout Container) {
  do {
    let s: Span<Cell<CInt>> = c.view()
    s[0].value = 10
  }
  do {
    let s: Span<Cell<CInt>> = viewOf(&c)
    _ = s.count
  }
  do {
    let s: Span<Cell<CInt>> = countedView(&c, 4)
    s[1].value = 20
  }
  let s: Span<ConstCell<CInt>> = c.cview()
  _ = s[0].value
  let t: Span<ConstCell<CInt>> = constCountedView(c, 4)
  _ = t[1].value
}

@available(SwiftStdlib 6.2, *)
func ref(_ c: inout Container) {
  let r: Ref<Cell<CInt>> = refOf(&c)
  r.value.value = 30
}
#endif
#endif

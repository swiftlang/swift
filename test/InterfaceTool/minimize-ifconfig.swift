// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift-interface-tool -action minimize %t/flat/input.swift > %t/flat/output.swift
// RUN: %diff -u %t/flat/expected.swift %t/flat/output.swift

// RUN: %swift-interface-tool -action minimize %t/nested-import/input.swift > %t/nested-import/output.swift
// RUN: %diff -u %t/nested-import/expected.swift %t/nested-import/output.swift

// RUN: %swift-interface-tool -action minimize %t/nested-canImport-empty-body/input.swift > %t/nested-canImport-empty-body/output.swift
// RUN: %diff -u %t/nested-canImport-empty-body/expected.swift %t/nested-canImport-empty-body/output.swift

// RUN: %swift-interface-tool -action minimize %t/nested-multi-level/input.swift > %t/nested-multi-level/output.swift
// RUN: %diff -u %t/nested-multi-level/expected.swift %t/nested-multi-level/output.swift

// RUN: %swift-interface-tool -action minimize %t/nested-in-else/input.swift > %t/nested-in-else/output.swift
// RUN: %diff -u %t/nested-in-else/expected.swift %t/nested-in-else/output.swift

// RUN: %swift-interface-tool -action minimize %t/complex-conditions/input.swift > %t/complex-conditions/output.swift
// RUN: %diff -u %t/complex-conditions/expected.swift %t/complex-conditions/output.swift

// RUN: %swift-interface-tool -action minimize %t/keep-empty-body/input.swift > %t/keep-empty-body/output.swift
// RUN: %diff -u %t/keep-empty-body/expected.swift %t/keep-empty-body/output.swift

// RUN: %swift-interface-tool -action minimize %t/canImport-as-name/input.swift > %t/canImport-as-name/output.swift
// RUN: %diff -u %t/canImport-as-name/expected.swift %t/canImport-as-name/output.swift

// RUN: %swift-interface-tool -action minimize %t/comments-in-ifconfig/input.swift > %t/comments-in-ifconfig/output.swift
// RUN: %diff -u %t/comments-in-ifconfig/expected.swift %t/comments-in-ifconfig/output.swift

// RUN: %swift-interface-tool -action minimize %t/keep-via-elseif/input.swift > %t/keep-via-elseif/output.swift
// RUN: %diff -u %t/keep-via-elseif/expected.swift %t/keep-via-elseif/output.swift

// RUN: %swift-interface-tool -action minimize %t/mixing-with-top-level/input.swift > %t/mixing-with-top-level/output.swift
// RUN: %diff -u %t/mixing-with-top-level/expected.swift %t/mixing-with-top-level/output.swift

// RUN: %swift-interface-tool -action minimize %t/inside-function-body/input.swift > %t/inside-function-body/output.swift
// RUN: %diff -u %t/inside-function-body/expected.swift %t/inside-function-body/output.swift

// RUN: %swift-interface-tool -action minimize %t/pound-diagnostics/input.swift > %t/pound-diagnostics/output.swift
// RUN: %diff -u %t/pound-diagnostics/expected.swift %t/pound-diagnostics/output.swift

// RUN: %swift-interface-tool -action minimize %t/deeply-nested-canImport/input.swift > %t/deeply-nested-canImport/output.swift
// RUN: %diff -u %t/deeply-nested-canImport/expected.swift %t/deeply-nested-canImport/output.swift

// Check complex cases still parse.
// RUN: %target-swift-frontend -parse %t/flat/output.swift
// RUN: %target-swift-frontend -parse %t/keep-empty-body/output.swift
// RUN: %target-swift-frontend -parse %t/keep-via-elseif/output.swift
// RUN: %target-swift-frontend -parse %t/nested-canImport-empty-body/output.swift

//--- flat/input.swift
import AlphaMod

#if canImport(CondMod)
import CondMod
public func condFunc() {}
#endif

#if canImport(OtherCondMod) && os(macOS)
import OtherCondMod
public func otherCondFunc() {}
#endif

#if !canImport(MissingMod)
import GammaMod
public func missingModFunc() {}
#endif

#if os(macOS)
import PlatformMod
public func platformFunc() {}
#elseif os(Linux)
import OtherPlatformMod
#else
public func otherFunc() {}
#endif

#if DEBUG
public func debugFunc() {}
#endif

#if os(iOS)
public struct FlatFoo {}
#endif
//--- flat/expected.swift
import AlphaMod
#if canImport(CondMod)
import CondMod
#endif
#if canImport(OtherCondMod) && os(macOS)
import OtherCondMod
#endif
#if !canImport(MissingMod)
import GammaMod
#endif
#if os(macOS)
import PlatformMod
#elseif os(Linux)
import OtherPlatformMod
#else
#endif
//--- nested-import/input.swift
#if os(macOS)
#if canImport(CondMod)
import CondMod
public func nestedCondFunc() {}
#endif
#endif
//--- nested-import/expected.swift
#if os(macOS)
#if canImport(CondMod)
import CondMod
#endif
#endif
//--- nested-canImport-empty-body/input.swift
#if FOO
#if canImport(CondMod)
public func condOnlyFunc() {}
internal struct CondThing {}
#endif
#endif
//--- nested-canImport-empty-body/expected.swift
#if FOO
#if canImport(CondMod)
#endif
#endif
//--- nested-multi-level/input.swift
#if A
#if B
#if canImport(C)
import C
#endif
#endif
#endif
//--- nested-multi-level/expected.swift
#if A
#if B
#if canImport(C)
import C
#endif
#endif
#endif
//--- nested-in-else/input.swift
#if os(macOS)
import PlatformMod
#else
#if canImport(OtherPlatformMod)
import OtherPlatformMod
public func otherPlatformOnly() {}
#endif
#endif
//--- nested-in-else/expected.swift
#if os(macOS)
import PlatformMod
#else
#if canImport(OtherPlatformMod)
import OtherPlatformMod
#endif
#endif
//--- complex-conditions/input.swift
#if (canImport(SomeMod) || FORCE) && os(macOS)
import SomeMod
#endif

#if canImport(A) || canImport(B)
import Either
#endif

#if !canImport(MissingMod)
import GammaMod
#endif

#if !(canImport(Foo) && canImport(Bar))
import Fallback
#endif
//--- complex-conditions/expected.swift
#if (canImport(SomeMod) || FORCE) && os(macOS)
import SomeMod
#endif
#if canImport(A) || canImport(B)
import Either
#endif
#if !canImport(MissingMod)
import GammaMod
#endif
#if !(canImport(Foo) && canImport(Bar))
import Fallback
#endif
//--- keep-empty-body/input.swift
#if canImport(CondMod)
public func emptyBodyFunc() {}
internal struct EmptyBodyThing {}
public class EmptyBodyClass {}
#endif
//--- keep-empty-body/expected.swift
#if canImport(CondMod)
#endif
//--- canImport-as-name/input.swift
import MarkerOne

#if FOO
public func canImport(_ s: String) -> Bool { false }
public var canImportFlag = true
#endif

#if BAR
#if BAZ
public func nestedNoCanImport() {}
#endif
#endif

import MarkerTwo
//--- canImport-as-name/expected.swift
import MarkerOne
import MarkerTwo
//--- comments-in-ifconfig/input.swift
// file-level comment before #if
#if /* leading */ canImport(CondMod) /* trailing */
// inside body
import CondMod /* trailing block */
// before endif
#endif // after endif

#if canImport(OtherCondMod)
/// doc on import
@_exported import OtherCondMod
#endif
//--- comments-in-ifconfig/expected.swift
#if canImport(CondMod)
import CondMod
#endif
#if canImport(OtherCondMod)
@_exported import OtherCondMod
#endif
//--- keep-via-elseif/input.swift
#if FOO
public struct ElseifA {}
#elseif canImport(Bar)
import Bar
#else
public struct ElseifC {}
#endif

#if FIRST
public func elseifFirst() {}
#elseif SECOND
import SecondMod
#endif
//--- keep-via-elseif/expected.swift
#if FOO
#elseif canImport(Bar)
import Bar
#else
#endif
#if FIRST
#elseif SECOND
import SecondMod
#endif
//--- mixing-with-top-level/input.swift
import AlphaMod
public func mixingA() {}
#if canImport(CondMod)
import CondMod
#endif
public class MixingB {}
import BetaMod
#if FOO
public func mixingDropped1() {}
#endif
public typealias MixingAlias = Int
import GammaMod
#if BAR
#if BAZ
public func mixingDropped2() {}
#endif
#endif
@_exported import LastMod
//--- mixing-with-top-level/expected.swift
import AlphaMod
#if canImport(CondMod)
import CondMod
#endif
import BetaMod
import GammaMod
@_exported import LastMod
//--- inside-function-body/input.swift
import AlphaMod

@inlinable public func inlinableFunc() {
// FIXME: we currently drop canImports in excluded contexts; we may need to
// instead move them to top-level for correctness. We don't expect this case in
// .swiftinterface files, but it can happen when minimizing .swift files.
#if canImport(CondMod)
  useCondMod()
#endif
}

public struct Wrapper {
  @_transparent public var value: Int {
#if canImport(OtherCondMod)
    otherCondValue()
#else
    0
#endif
  }
}

import BetaMod
//--- inside-function-body/expected.swift
import AlphaMod
import BetaMod
//--- pound-diagnostics/input.swift
#error("top level")
#warning("top level")

#if canImport(CondMod)
#error("inside a kept #if")
#endif

#if DEBUG
#warning("inside a dropped #if")
#endif

import AlphaMod
//--- pound-diagnostics/expected.swift
#if canImport(CondMod)
#endif
import AlphaMod
//--- deeply-nested-canImport/input.swift
// A `canImport` anywhere in the condition keeps the `#if`, however deeply it is
// nested. Each body here is dropped, so the `#if` survives only on the strength
// of its condition.
import Marker

// A ternary is not an operator form, and the branches of one are not reachable
// by walking operands.
#if FOO ? canImport(B) : BAR
public func b() {}
#endif

// A call whose callee is not `canImport` may still have one in its arguments.
#if foo(canImport(C))
public func c() {}
#endif

#if canImport(D) == true
public func d() {}
#endif

#if !(canImport(E) || FOO) && os(macOS)
public func e() {}
#endif

// No `canImport` anywhere: dropped, since the body contributes nothing either.
#if foo(bar(BAZ))
public func f() {}
#endif
//--- deeply-nested-canImport/expected.swift
import Marker
#if FOO ? canImport(B) : BAR
#endif
#if foo(canImport(C))
#endif
#if canImport(D) == true
#endif
#if !(canImport(E) || FOO) && os(macOS)
#endif

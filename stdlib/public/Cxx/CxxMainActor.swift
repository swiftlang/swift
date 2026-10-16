//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Exposes a main-actor-isolated declaration to C++.
///
/// C++ callers are not isolated to any actor, so main-actor-isolated
/// declarations cannot be exposed to C++ directly. This macro introduces a
/// `nonisolated` forwarding thunk that traps at runtime if it is not called on
/// the main actor, and exposes it to C++ under the name of the original
/// declaration (or `cxxName`, if provided). The thunk is unavailable in Swift.
@attached(peer, names: prefixed(_cxx_))
public macro _ExposeMainActorToCxx(_ cxxName: String? = nil) =
  #externalMacro(module: "SwiftMacros", type: "CxxMainActorThunkMacro")

/// Applies `@_ExposeMainActorToCxx` to every eligible member of a type or an
/// extension.
@attached(memberAttribute)
public macro _ExposeMainActorMembersToCxx() =
  #externalMacro(module: "SwiftMacros", type: "CxxMainActorThunkMacro")

/// Implementation detail of `@_ExposeMainActorToCxx`.
///
/// Wraps the values the thunks pass to and from the main actor (owned values,
/// or `Ref`s and `MutableRef`s to borrowed and mutated ones), which are not
/// checked for data-race safety as the thunks are only called from C++.
@frozen
public struct _CxxUncheckedSendable<Value: ~Copyable & ~Escapable>:
  ~Copyable, ~Escapable, @unchecked Sendable
{
  public var value: Value

  @_alwaysEmitIntoClient
  @_lifetime(copy value)
  public init(_ value: consuming Value) { self.value = value }
}

extension _CxxUncheckedSendable: Copyable where Value: Copyable & ~Escapable {}
extension _CxxUncheckedSendable: Escapable where Value: Escapable & ~Copyable {}

extension _CxxUncheckedSendable where Value: ~Copyable {
  @_alwaysEmitIntoClient
  public consuming func take() -> Value { value }
}

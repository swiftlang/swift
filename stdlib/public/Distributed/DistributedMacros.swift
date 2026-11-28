//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// Macros supporting distributed actor features.
//===----------------------------------------------------------------------===//

import Swift
import _Concurrency

// Macros are disabled when Swift is built without swift-syntax.
#if $Macros && hasAttribute(attached)

/// Enables the attached to protocol to be resolved as remote distributed
/// actor reference.
///
/// ### Requirements
///
/// The attached to type must be a protocol that refines the `DistributedActor`
/// protocol. It must either specify a concrete `ActorSystem` or constrain it
/// in such way that the system's `SerializationRequirement` is statically known.
///
/// ### Resolvable protocol stub type
///
/// This macro synthesizes a type with the same name as the protocol, prefixed with a `$` sign, e.g.
/// `@Resolvable protocol Greeter ... {}` results in a `distributed actor $Greeter {}` type.
///
/// This type is referred to as 'resolvable protocol stub' type, and is only interacted with directly when:
/// - resolving a type on a remote host, without knowing the underlying implementation type, like so:
///
/// ```swift
/// let greeter: some Greeter = try $Greeter.resolve(id: ..., using: system)
/// ```
///
/// Alongside the `$`-prefixed stub, the macro can synthesize a typealias linking
/// the protocol to its generated stub:
///
/// ```swift
/// typealias DistributedProtocolStub = $Greeter // or $Greeter<Self.ActorSystem>
/// ```
///
/// This typealias lets callers spell the stub type generically, given only the
/// protocol, e.g. `SomeGreeterProtocol.DistributedProtocolStub`. Its synthesis is
/// gated behind the experimental `DistributedProtocolStubTypealias` feature, so it
/// only materializes when a client enables that feature.
///
/// Combining multiple resolvable protocols is less common, but possible. An actor
/// that conforms to two `@Resolvable` protocols `A` and `B` inherits a
/// `DistributedProtocolStub` typealias from each, which the compiler cannot
/// disambiguate, so the actor must declare a single `DistributedProtocolStub`
/// itself to pick a default. A `@Resolvable` protocol that refines another can
/// avoid contributing a second candidate by opting out of the synthesis with
/// `@Resolvable(_emitStubTypealias: false)`.
///
/// - Parameter _emitStubTypealias: When `true` (the default) the macro synthesizes
///   the `DistributedProtocolStub` typealias (subject to the experimental feature
///   above). Pass `false` to suppress it, e.g. on a protocol that refines another
///   `@Resolvable` protocol and would otherwise introduce a conflicting witness.
@attached(peer, names: prefixed(`$`)) // provides $Greeter concrete stub type
@attached(extension, names: arbitrary) // provides extension for Greeter & _DistributedActorStub
public macro Resolvable() =
  #externalMacro(module: "SwiftMacros", type: "DistributedResolvableMacro")


#if $DistributedProtocolStubTypealias
@attached(peer, names: prefixed(`$`)) // provides $Greeter concrete stub type
@attached(extension, names: arbitrary) // provides extension for Greeter & _DistributedActorStub
public macro Resolvable(_emitStubTypealias: Bool) =
  #externalMacro(module: "SwiftMacros", type: "DistributedResolvableMacro")
#endif

#endif

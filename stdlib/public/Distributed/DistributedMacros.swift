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
@attached(peer, names: prefixed(`$`)) // provides $Greeter concrete stub type
@attached(extension, names: arbitrary) // provides extension for Greeter & _DistributedActorStub
public macro Resolvable() =
  #externalMacro(module: "SwiftMacros", type: "DistributedResolvableMacro")

#endif

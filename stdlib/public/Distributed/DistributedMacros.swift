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

#if $Macros

/// ### Sample usage
///     try #resolve<any Worker, SampleSystem>(id: id, using: system)
@available(SwiftStdlib 9999, *)
@freestanding(expression)
 public macro resolve<DA, DAS: DistributedActorSystem>(
  id: DAS.ActorID, using system: DAS) -> DA =
    #externalMacro(module: "DistributedMacros", type: "DistributedResolveMacro")

@available(SwiftStdlib 9999, *)
@freestanding(expression)
public macro distributedResolveStub<DAS: DistributedActorSystem>(
  stubTypeName: String,
  module: String, protocolName: String,
  id: DAS.ActorID, using system: DAS,
  stubProtocols: String...) -> Any =
    #externalMacro(module: "DistributedMacros", type: "DistributedResolveStubMacro")

/// Macro which expands a list of protocol requirements into their "stub"
/// implementations, which invoke the ``Distributed/_methodStub()`` method.
///
/// Distributed stubs are used during distributed protocol stub synthesis,
/// in order to generate a type that can be used as underlying implementation
/// for a created remote distributed actor reference object, when given only a
/// protocol to create such reference for.
///
/// Distributed method stubs are not used for concrete distributed actors --
/// as those have concrete (local) implementations already provided by the
/// type's author.
@available(SwiftStdlib 9999, *)
@freestanding(declaration, names: arbitrary)
public macro distributedStubs(
  module: String, protocolName: String,
  stubProtocols: [String],
  _ requirements: String...) =
    #externalMacro(module: "DistributedMacros", type: "DistributedRequirementStubsMacro")

@attached(peer, names: prefixed(`$`), prefixed(_distributed_stubs_))
public macro DistributedProtocol() =
  #externalMacro(module: "DistributedMacros", type: "DistributedMakeProtocolStubTypeMacro")

#endif

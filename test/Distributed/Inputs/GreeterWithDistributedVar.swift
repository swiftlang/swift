//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0
//
// See LICENSE.txt for license information
// See CONTRIBUTORS.txt for the list of Swift project authors
//
// SPDX-License-Identifier: Apache-2.0
//
//===----------------------------------------------------------------------===//

import Distributed

// A 'distributed var' declared as a requirement on a DistributedActor-refining
// protocol, satisfied by a concrete 'distributed actor' in the same module.
// The point of housing this in a separate module is to exercise the
// cross-module deserialization path for the synthesized getter thunk
// ('...vgTE') from a client that imports this module.
public protocol Greeter: DistributedActor
  where ActorSystem == LocalTestingDistributedActorSystem {
  distributed var greeting: String { get }
}

public distributed actor GreeterWithDistributedVar: Greeter {
  public typealias ActorSystem = LocalTestingDistributedActorSystem

  public init(actorSystem: ActorSystem) {
    self.actorSystem = actorSystem
  }

  public distributed var greeting: String { "hello" }
}

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

@attached(peer, names: prefixed(`$`)) // provides $Greeter concrete stub type
@attached(extension, names: arbitrary) // provides extension for Greeter & _DistributedActorStub
public macro _DistributedProtocol() =
  #externalMacro(module: "SwiftMacros", type: "DistributedProtocolMacro")

#endif

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Distributed
import EmbeddedFakeActorSystem
import GreeterAPI

public distributed actor GreeterImpl: Greeter {
  public distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }

  public distributed func farewell(name: String) -> String {
    return "Goodbye, \(name)!"
  }

  public distributed func note(_ message: String) {
    print("[swift] server noted: \(message)")
  }

  public distributed func check(_ uid: ComplexRequest) -> ComplexResponse {
    print("[swift] server checked request id: \(uid.id)")
    return ComplexResponse(id: uid.id + 1)
  }
}


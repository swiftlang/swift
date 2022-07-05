//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0
//
// See LICENSE.txt for license information
// See CONTRIBUTORS.txt for the list of Swift project authors
//
// SPDX-License-Identifier: Apache-2.0
//
//===----------------------------------------------------------------------===//

import Distributed

distributed actor Echo /* in the mirror */{
  typealias ActorSystem = LocalTestingDistributedActorSystem

  distributed func echo(_ input: String) -> String {
    return "echo: \(input)"
  }
}

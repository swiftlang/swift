//===--- DestructorAnalysis.swift - the dead-end blocks analysis ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OptimizerBridging
import SIL

struct DestructorAnalysis {
  let bridged: BridgedDestructorAnalysis

  // TODO: swift::isDestructorSideEffectFree() also  handles other cases, such as Arrays and closures.
  func deinitMayHaveEffects(type: Type, in function: Function) -> Bool {
    if type.isBox {
      return type.getBoxFields(in: function).contains(where: { deinitMayHaveEffects(type: $0, in: function) })
    }
    return bridged.mayStoreToMemoryOnDestruction(type.bridged)
  }
}

//===--- DeinitDevirtualizer.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Devirtualizes destroys of non-copyable values.
///
let deinitDevirtualizer = FunctionPass(name: "deinit-devirtualizer") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else {
    return
  }

  for inst in function.instructions {
    switch inst {
    case let destroyValue as DestroyValueInst:
      _ = devirtualizeDeinits(of: destroyValue, context)
    case let destroyAddr as DestroyAddrInst:
      _ = devirtualizeDeinits(of: destroyAddr, context)
    default:
      break
    }
  }
}

//===--- SimplifyClassifyBridgeObject.swift -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

extension ClassifyBridgeObjectInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    // Constant fold `classify_bridge_object` to `(false, false)` if the operand is known
    // to be a swift class.
    var walker = CheckForSwiftClasses();
    if walker.walkUp(value: operand.value, path: UnusedWalkingPath()) == .abortWalk {
      return
    }

    let builder = Builder(before: self, context)
    let falseLiteral = builder.createIntegerLiteral(0, type: context.getBuiltinIntegerType(bitWidth: 1))
    let tp = builder.createTuple(type: self.type, elements: [falseLiteral, falseLiteral])
    uses.replaceAll(with: tp, context)
    context.erase(instruction: self)
  }
}

private struct CheckForSwiftClasses: ValueUseDefWalker {
  mutating func walkUp(value: Value, path: UnusedWalkingPath) -> WalkResult {
    if let nominal = value.type.nominal,
       let classDecl = nominal as? ClassDecl,
       !classDecl.isObjC
    {
      // Stop this use-def walk if the value is known to be a swift class.
      return .continueWalk
    }
    return walkUpDefault(value: value, path: path)
  }

  mutating func rootDef(value: Value, path: UnusedWalkingPath) -> WalkResult {
    return .abortWalk
  }

  var walkUpCache = WalkerCache<UnusedWalkingPath>()
}

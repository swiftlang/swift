//===--- AccessDumper.swift - Dump access information  --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Dumps access information for memory accesses (`load` and `store`)
/// instructions.
///
/// This pass is used for testing `AccessUtils`.
let accessDumper = FunctionPass(name: "dump-access", {
  (function: Function, context: PassContext) in
  print("Accesses for \(function.name)")

  struct PrintArgUses : AccessUseVisitor {
    var walkDownCache = WalkerCache<SmallProjectionPath>()

    mutating func visitUse(value: Operand, path: Path) -> WalkResult {
      print("ValueUse(\(path)): \(value)")
      return .continueWalk
    }
    mutating func visitUse(address: Operand, path: Path) -> WalkResult {
      print("AddressUse(\(path)): \(address)")
      return .continueWalk
    }
  }
  var printArgUses = PrintArgUses()

  for arg in function.arguments {
    if arg.type.isAddress {
      _ = printArgUses.visitAccesses(toAddress: arg)
    } else {
      _ = printArgUses.visitUses(ofValue: arg)
    }
  }

  var apw = AccessPathWalker()
  var arw = AccessStoragePathVisitor()
  for block in function.blocks {
    for instr in block.instructions {
      switch instr {
      case let st as StoreInst:
        printAccessInfo(st.destinationOperand.value, &apw, &arw, context)
      case let load as LoadInst:
        printAccessInfo(load.operand, &apw, &arw, context)
      default:
        break
      }
    }
  }

  print("End accesses for \(function.name)")
})

private struct AccessStoragePathVisitor : AccessStoragePathWalker {
  var walkUpCache = WalkerCache<Path>()
  mutating func visit(access: AccessStoragePath) {
    print("    Storage: \(access.storage)")
    print("    Path: \"\(access.path)\"")
  }
}

private func printAccessInfo(_ value: Value, _ apw: inout AccessPathWalker, _ aspw: inout AccessStoragePathVisitor,
                             _ ctx: PassContext) {
  print("Value: \(value)")
  let (ap, scope) = apw.getAccessPathWithScope(of: value)
  if let scope = scope {
    switch scope {
    case let .scope(ba):
      print("  Scope: \(ba)")
    case .base(_):
      print("  Scope: base")
    }
  }

  if let ap = ap {
    print("  Base: \(ap.base)")
    print("  Path: \"\(ap.projectionPath)\"")

    aspw.getAccessStorage(for: ap)
  }
}

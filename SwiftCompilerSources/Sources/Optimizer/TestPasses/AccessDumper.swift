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

import Basic
import SIL

/// Dumps access information for memory accesses (`load` and `store`)
/// instructions.
/// Also verifies that `AccessPath.isDistinct(from:)` is correct. This does not actually
/// dumps anything, but aborts if the result is wrong.
///
/// This pass is used for testing `AccessUtils`.
let accessDumper = FunctionPass(name: "dump-access") {
  (function: Function, context: FunctionPassContext) in
  print("Accesses for \(function.name)")

  for block in function.blocks {
    for instr in block.instructions {
      switch instr {
      case let st as StoreInst:
        printAccessInfo(address: st.destination)
      case let load as LoadInst:
        printAccessInfo(address: load.address)
      case let apply as ApplyInst:
        guard let callee = apply.referencedFunction else {
          break
        }
        if callee.name == StringRef("_isDistinct") {
          checkAliasInfo(forArgumentsOf: apply, expectDistinct: true)
        } else if callee.name == StringRef("_isNotDistinct") {
          checkAliasInfo(forArgumentsOf: apply, expectDistinct: false)
        }
      default:
        break
      }
    }
  }

  print("End accesses for \(function.name)")
}

private struct AccessStoragePathVisitor : ValueUseDefWalker {
  var walkUpCache = WalkerCache<Path>()
  mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
    print("    Storage: \(value)")
    print("    Path: \"\(path)\"")
    return .continueWalk
  }
}

private func printAccessInfo(address: Value) {
  print("Value: \(address)")

  let (ap, scope) = address.accessPathWithScope
  if let beginAccess = scope {
    print("  Scope: \(beginAccess)")
  } else {
    print("  Scope: base")
  }

  let constAp = address.constantAccessPath
  if constAp == ap {
    print("  Base: \(ap.base)")
    print("  Path: \"\(ap.projectionPath)\"")
  } else {
    print("  nonconst-base: \(ap.base)")
    print("  nonconst-path: \"\(ap.projectionPath)\"")
    print("  const-base: \(constAp.base)")
    print("  const-path: \"\(constAp.projectionPath)\"")
  }

  var arw = AccessStoragePathVisitor()
  if !arw.visitAccessStorageRoots(of: ap) {
    print("   no Storage paths")
  }
}

private func checkAliasInfo(forArgumentsOf apply: ApplyInst, expectDistinct: Bool) {
  let address1 = apply.arguments[0]
  let address2 = apply.arguments[1]

  checkIsDistinct(path1: address1.accessPath,
                  path2: address2.accessPath,
                  expectDistinct: expectDistinct,
                  instruction: apply)

  if !expectDistinct {
    // Also check all combinations with the constant variant of access paths.
    // Note: we can't do that for "isDistinct" because "isDistinct" might be more conservative in one of the variants.
    checkIsDistinct(path1: address1.constantAccessPath,
                    path2: address2.constantAccessPath,
                    expectDistinct: false,
                    instruction: apply)
    checkIsDistinct(path1: address1.accessPath,
                    path2: address2.constantAccessPath,
                    expectDistinct: false,
                    instruction: apply)
    checkIsDistinct(path1: address1.constantAccessPath,
                    path2: address2.accessPath,
                    expectDistinct: false,
                    instruction: apply)
  }
}

private func checkIsDistinct(path1: AccessPath, path2: AccessPath, expectDistinct: Bool, instruction: Instruction) {
  if path1.isDistinct(from: path2) != expectDistinct {
    print("wrong isDistinct result of \(instruction)")
  } else if path2.isDistinct(from: path1) != expectDistinct {
    print("wrong reverse isDistinct result of \(instruction)")
  } else {
    return
  }

  print("in function")
  print(instruction.parentFunction)
  fatalError()
}

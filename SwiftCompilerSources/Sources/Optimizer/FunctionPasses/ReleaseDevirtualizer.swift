//===--- ReleaseDevirtualizer.swift - Devirtualizes release-instructions --===//
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

import AST
import SIL

/// Devirtualizes release instructions which are known to destruct the object.
///
/// This means, it replaces a sequence of
///    %x = alloc_ref [stack] $X
///      ...
///    strong_release %x
///    dealloc_stack_ref %x
/// with
///    %x = alloc_ref [stack] $X
///      ...
///    set_deallocating %x
///    %d = function_ref @dealloc_of_X
///    %a = apply %d(%x)
///    dealloc_stack_ref %x
///
/// The optimization is only done for stack promoted objects because they are
/// known to have no associated objects (which are not explicitly released
/// in the deinit method).
let releaseDevirtualizerPass = FunctionPass(name: "release-devirtualizer") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    if let dealloc = inst as? DeallocStackRefInst {
      if !context.continueWithNextSubpassRun(for: dealloc) {
        return
      }
      tryDevirtualizeRelease(of: dealloc, context)
    }
  }
}

private func tryDevirtualizeRelease(of dealloc: DeallocStackRefInst, _ context: FunctionPassContext) {
  guard let (lastRelease, pathToRelease) = findLastRelease(of: dealloc, context) else {
    return
  }

  if !pathToRelease.isMaterializable {
    return
  }

  let allocRef = dealloc.allocRef
  var upWalker = FindAllocationWalker(allocation: allocRef)
  if upWalker.walkUp(value: lastRelease.operand.value, path: pathToRelease) == .abortWalk {
    return
  }

  let type = allocRef.type

  guard let dealloc = context.calleeAnalysis.getDestructor(ofExactType: type) else {
    return
  }

  let builder = Builder(before: lastRelease, location: lastRelease.location, context)

  var object = lastRelease.operand.value.createProjection(path: pathToRelease, builder: builder)
  if object.type != type {
    object = builder.createUncheckedRefCast(from: object, to: type)
  }

  // Do what a release would do before calling the deallocator: set the object
  // in deallocating state, which means set the RC_DEALLOCATING_FLAG flag.
  let beginDealloc = builder.createBeginDeallocRef(reference: object, allocation: allocRef)

  // Create the call to the destructor with the allocated object as self
  // argument.
  let functionRef = builder.createFunctionRef(dealloc)

  let substitutionMap: SubstitutionMap
  if dealloc.isGeneric {
    substitutionMap = context.getContextSubstitutionMap(for: type)
  } else {
    // In embedded Swift, dealloc might be a specialized deinit, so the substitution map on the old apply isn't valid for the new apply
    substitutionMap = SubstitutionMap()
  }

  builder.createApply(function: functionRef, substitutionMap, arguments: [beginDealloc])
  context.erase(instruction: lastRelease)
}

private func findLastRelease(
  of dealloc: DeallocStackRefInst,
  _ context: FunctionPassContext
) -> (lastRelease: RefCountingInst, pathToRelease: SmallProjectionPath)? {
  let allocRef = dealloc.allocRef

  // Search for the final release in the same basic block of the dealloc.
  for instruction in ReverseInstructionList(first: dealloc.previous) {
    switch instruction {
    case let strongRelease as StrongReleaseInst:
      if let pathToRelease = getPathToRelease(from: allocRef, to: strongRelease) {
        return (strongRelease, pathToRelease)
      }
    case let releaseValue as ReleaseValueInst:
      if releaseValue.value.type.containsSingleReference(in: dealloc.parentFunction) {
        if let pathToRelease = getPathToRelease(from: allocRef, to: releaseValue) {
          return (releaseValue, pathToRelease)
        }
      }
    case is BeginDeallocRefInst, is DeallocRefInst:
      // Check if the last release was already de-virtualized.
      if allocRef.escapes(to: instruction, context) {
        return nil
      }
    default:
      break
    }
    if instruction.mayRelease && allocRef.escapes(to: instruction, context) {
      // This instruction may release the allocRef, which means that any release we find
      // earlier in the block is not guaranteed to be the final release.
      return nil
    }
  }
  return nil
}

// If the release is a release_value it might release a struct which _contains_ the allocated object.
// Return a projection path to the contained object in this case.
private func getPathToRelease(from allocRef: AllocRefInstBase, to release: RefCountingInst) -> SmallProjectionPath? {
  var downWalker = FindReleaseWalker(release: release)
  if downWalker.walkDownUses(ofValue: allocRef, path: SmallProjectionPath()) == .continueWalk {
    return downWalker.result
  }
  return nil
}

private struct FindReleaseWalker : ValueDefUseWalker {
  private let release: RefCountingInst
  private(set) var result: SmallProjectionPath? = nil

  var walkDownCache = WalkerCache<SmallProjectionPath>()

  init(release: RefCountingInst) {
    self.release = release
  }

  mutating func leafUse(value: Operand, path: SmallProjectionPath) -> WalkResult {
    if value.instruction == release {
      if let existingResult = result {
        result = existingResult.merge(with: path)
      } else {
        result = path
      }
    }
    return .continueWalk
  }
}

private extension AllocRefInstBase {
  func escapes(to instruction: Instruction, _ context: FunctionPassContext) -> Bool {
    return self.isEscaping(using: EscapesToInstructionVisitor(target: instruction), context)
  }
}

private struct EscapesToInstructionVisitor : EscapeVisitor {
  let target: Instruction

  mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
    if operand.instruction == target {
      return .abort
    }
    return .continueWalk
  }
}

// Up-walker to find the root of a release instruction.
private struct FindAllocationWalker : ValueUseDefWalker {
  private let allocInst: AllocRefInstBase

  var walkUpCache = WalkerCache<SmallProjectionPath>()

  init(allocation: AllocRefInstBase) { allocInst = allocation }

  mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
    return value == allocInst && path.isEmpty ? .continueWalk : .abortWalk
  }
}

private extension Type {
  func containsSingleReference(in function: Function) -> Bool {
    if isClass {
      return true
    }
    if isStruct {
      return getNominalFields(in: function)?.containsSingleReference(in: function) ?? false
    } else if isTuple {
      return tupleElements.containsSingleReference(in: function)
    } else {
      return false
    }
  }
}

private extension Collection where Element == Type {
  func containsSingleReference(in function: Function) -> Bool {
    var nonTrivialFieldFound = false
    for elementTy in self {
      if !elementTy.isTrivial(in: function) {
        if nonTrivialFieldFound {
          return false
        }
        if !elementTy.containsSingleReference(in: function) {
          return false
        }
        nonTrivialFieldFound = true
      }
    }
    return nonTrivialFieldFound
  }
}

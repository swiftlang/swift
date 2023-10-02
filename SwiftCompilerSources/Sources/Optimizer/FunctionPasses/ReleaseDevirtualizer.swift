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

  for block in function.blocks {
    // The last `release_value`` or `strong_release`` instruction before the
    // deallocation.
    var lastRelease: RefCountingInst?

    for instruction in block.instructions {
      switch instruction {
      case let dealloc as DeallocStackRefInst:
        if let lastRel = lastRelease {
          // We only do the optimization for stack promoted object, because for
          // these we know that they don't have associated objects, which are
          // _not_ released by the deinit method.
          if !context.continueWithNextSubpassRun(for: lastRel) {
            return
          }
          tryDevirtualizeRelease(of: dealloc.allocRef, lastRelease: lastRel, context)
          lastRelease = nil
        }
      case let strongRelease as StrongReleaseInst:
        lastRelease = strongRelease
      case let releaseValue as ReleaseValueInst where releaseValue.value.type.containsSingleReference(in: function):
        lastRelease = releaseValue
      case is DeallocRefInst, is BeginDeallocRefInst:
        lastRelease = nil
      default:
        if instruction.mayRelease {
          lastRelease = nil
        }
      }
    }
  }
}

/// Tries to de-virtualize the final release of a stack-promoted object.
private func tryDevirtualizeRelease(
  of allocRef: AllocRefInstBase,
  lastRelease: RefCountingInst,
  _ context: FunctionPassContext
) {
  var downWalker = FindReleaseWalker(release: lastRelease)
  guard let pathToRelease = downWalker.getPathToRelease(from: allocRef) else {
    return
  }

  if !pathToRelease.isMaterializable {
    return
  }

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
  if dealloc.isGenericFunction {
    substitutionMap = context.getContextSubstitutionMap(for: type)
  } else {
    // In embedded Swift, dealloc might be a specialized deinit, so the substitution map on the old apply isn't valid for the new apply
    substitutionMap = SubstitutionMap()
  }

  builder.createApply(function: functionRef, substitutionMap, arguments: [beginDealloc])
  context.erase(instruction: lastRelease)
}

private struct FindReleaseWalker : ValueDefUseWalker {
  private let release: RefCountingInst
  private var result: SmallProjectionPath? = nil

  var walkDownCache = WalkerCache<SmallProjectionPath>()

  init(release: RefCountingInst) {
    self.release = release
  }

  mutating func getPathToRelease(from allocRef: AllocRefInstBase) -> SmallProjectionPath? {
    if walkDownUses(ofValue: allocRef, path: SmallProjectionPath()) == .continueWalk {
      return result
    }
    return nil
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
      return getNominalFields(in: function).containsSingleReference(in: function)
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

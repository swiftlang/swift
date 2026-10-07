//===--- ImplicitDestroyChecker.swift -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

/// Diagnoses destroys of values that can't be destroyed implicitly (see `Type.isImplicitlyDestroyable`), such as
/// values of `~Deinitable` types.
///
/// This pass runs right after the move-only checker, which guarantees that no path consumes a noncopyable value more
/// than once, and that every path that doesn't consume the value ends its lifetime with an explicit `destroy_value` or
/// `destroy_addr`. A call or any other legal transfer of the value is a consume, not a destroy. "Consumed on every
/// path" therefore reduces to a simple rule: no destroy on a path that doesn't end in `unreachable`.
///
/// Some destroys only destroy a value's stored properties, which is fine if each of them can be destroyed implicitly.
/// That's the case after `discard self`, and when the move-only checker destroys a value before reinitializing all of
/// its stored properties one by one.
///
let implicitDestroyChecker = FunctionPass(name: "implicit-destroy-checker") {
  (function: Function, context: FunctionPassContext) in

  // Don't rerun diagnostics on deserialized functions.
  if function.wasDeserializedCanonical {
    return
  }

  // This runs even without `NondeinitableTypes`, because a client can still drop a `~Deinitable` value that an API
  // returns.

  // If an earlier pass already diagnosed this function, don't add noise.
  if function.hasSemanticsAttribute("sil.optimizer.moveonly.diagnostic.ignore") {
    return
  }

  // Most functions have no destroys to diagnose, so find them before computing the dead-end blocks.
  let destroys = function.instructions.compactMap { ImplicitDestroy(of: $0, in: function) }
  if destroys.isEmpty {
    return
  }

  // Group the destroys by the variable or stored property that they destroy, so that each one gets one error with a
  // note for each path.
  var groups: [(variable: DestroyedVariable, destroys: [Instruction])] = []
  for destroy in destroys where !context.deadEndBlocks.isDeadEnd(destroy.instruction.parentBlock) {
    let variable = DestroyedVariable(destroy)
    if let index = groups.firstIndex(where: { $0.variable == variable }) {
      groups[index].destroys.append(destroy.instruction)
    } else {
      groups.append((variable, [destroy.instruction]))
    }
  }

  for (variable, destroys) in groups {
    diagnose(variable, destroys, in: function, context)
  }
}

/// A destroy of a value that can't be destroyed implicitly.
private struct ImplicitDestroy {
  let instruction: Instruction
  let value: Value

  /// If the destroy only destroys the value's stored properties, the name of the first one that can't be destroyed
  /// implicitly.
  let storedProperty: String?

  init?(of instruction: Instruction, in function: Function) {
    let value: Value
    switch instruction {
    case let destroy as DestroyValueInst:
      if destroy.isDeadEnd {
        return nil
      }
      value = destroy.destroyedValue
    case let destroy as DestroyAddrInst:
      value = destroy.destroyedAddress
    default:
      return nil
    }

    let type = value.type.objectType
    if type.isImplicitlyDestroyable {
      return nil
    }

    // `drop_deinit` only leaves trivially-destroyed stored properties to destroy.
    if value.isResultOfDropDeinit {
      return nil
    }

    var storedProperty: String?
    if let destroy = instruction as? DestroyAddrInst, type.isStruct, destroy.isBeforeMemberwiseReinit,
       let fields = type.getNominalFields(in: function) {
      guard let index = fields.firstIndex(where: { !$0.isImplicitlyDestroyable }) else {
        return nil
      }
      storedProperty = fields.getNameOfField(withIndex: index).string
    }

    self.instruction = instruction
    self.value = value
    self.storedProperty = storedProperty
  }
}

/// The variable, or the stored property of a variable, that a destroy drops.
private struct DestroyedVariable : Equatable {
  /// The value that introduces the variable, or the destroyed value itself if no name could be inferred.
  let root: Value

  /// The user-facing name, such as `x` or `x.y`, if the root is a variable.
  let name: String?

  init(_ destroy: ImplicitDestroy) {
    guard let (name, root) = destroy.value.inferredNameAndRoot else {
      self.root = destroy.value
      self.name = nil
      return
    }
    self.root = root
    if root.isVariable {
      self.name = destroy.storedProperty.map { "\(name).\($0)" } ?? name
    } else {
      self.name = nil
    }
  }

  static func ==(lhs: Self, rhs: Self) -> Bool {
    lhs.root == rhs.root && lhs.name == rhs.name
  }
}

private func diagnose(_ variable: DestroyedVariable, _ destroys: [Instruction], in function: Function,
                      _ context: FunctionPassContext) {
  let loc = variable.diagnosticLoc(in: function) ?? destroys[0].location.sourceLoc
  if let name = variable.name {
    context.diagnosticEngine.diagnose(.sil_implicit_destroy_not_consumed, name, at: loc)
  } else {
    context.diagnosticEngine.diagnose(.sil_implicit_destroy_unnamed_not_consumed,
                                      variable.root.type.rawType, at: loc)
  }

  // Paths can share an exit, as the cases of a `switch` do.
  var exitLocs: [SourceLoc] = []
  for destroy in destroys {
    guard let exitLoc = pathExitLoc(of: destroy, declLoc: loc, context) else {
      continue
    }
    if exitLocs.contains(where: { $0.bridged.raw == exitLoc.bridged.raw }) {
      continue
    }
    exitLocs.append(exitLoc)
    context.diagnosticEngine.diagnose(.sil_implicit_destroy_path_exit, at: exitLoc)
  }
}

private extension DestroyedVariable {
  func diagnosticLoc(in function: Function) -> SourceLoc? {
    if let arg = root as? FunctionArgument, !arg.isClosureCapture, let loc = arg.sourceLoc {
      return loc
    }

    // A closure's captures are declared in the enclosing function, but the obligation to consume them belongs to the
    // closure.
    guard let loc = root.definingInstruction?.location.sourceLoc, function.contains(loc) else {
      return function.location.sourceLoc
    }
    return loc
  }
}

/// Returns the location of the path exit for a note about `destroy`.
///
/// The destroy's own location is best when it points at the code that drops the value, such as `_ = consume x`, an
/// assignment, or a `return`. Some destroys carry the location of the variable's declaration instead, so in that case
/// use the statement that exits the function on that path.
private func pathExitLoc(of destroy: Instruction, declLoc: SourceLoc?, _ context: FunctionPassContext) -> SourceLoc? {
  let function = destroy.parentFunction
  let loc = destroy.location.sourceLoc
  if let loc, function.bodyContains(loc),
     loc.bridged.raw != declLoc?.bridged.raw {
    return loc
  }

  var worklist = BasicBlockWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(destroy.parentBlock)
  while let block = worklist.pop() {
    let terminator = block.terminator
    let terminatorLoc = terminator.location.sourceLoc
    if terminator.location.isReturnOrThrowStatement {
      return terminatorLoc
    }
    if terminator.isFunctionExiting {
      if let terminatorLoc, function.bodyContains(terminatorLoc) {
        return terminatorLoc
      }
      return function.bodyEndLoc
    }
    worklist.pushIfNotVisited(contentsOf: block.successors)
  }
  return loc
}

private extension Value {
  /// True if this value is a variable, so that diagnostics can name it.
  var isVariable: Bool {
    if let arg = self as? FunctionArgument {
      return arg.findVarDecl() != nil
    }
    return definingInstruction is DebugVariableInstruction
  }

  /// True if this value is the result of `drop_deinit`, looking through access markers and moves.
  var isResultOfDropDeinit: Bool {
    switch self {
    case is DropDeinitInst:
      return true
    case let access as BeginAccessInst:
      return access.address.isResultOfDropDeinit
    case let move as MoveValueInst:
      return move.fromValue.isResultOfDropDeinit
    case let mark as MarkUnresolvedNonCopyableValueInst:
      return mark.operand.value.isResultOfDropDeinit
    default:
      return false
    }
  }
}

private extension DestroyAddrInst {
  /// True if this destroys a value that's about to be reinitialized one stored property at a time.
  var isBeforeMemberwiseReinit: Bool {
    let base = destroyedAddress.strippingAccess
    var inst = next
    while let current = inst {
      inst = current.next
      let destination: Value
      switch current {
      case let store as StoreInst:
        destination = store.destinationOperand.value
      case let copy as CopyAddrInst:
        destination = copy.destinationOperand.value
      default:
        continue
      }
      let (destinationBase, isProjected) = destination.strippingAccessAndProjections
      if destinationBase == base {
        return isProjected
      }
    }
    return false
  }
}

private extension Value {
  var strippingAccess: Value {
    if let access = self as? BeginAccessInst {
      return access.address.strippingAccess
    }
    return self
  }

  /// Strips access markers and the projections of stored properties, and reports whether there were projections.
  var strippingAccessAndProjections: (base: Value, isProjected: Bool) {
    switch self {
    case let access as BeginAccessInst:
      return access.address.strippingAccessAndProjections
    case let projection as StructElementAddrInst:
      return (projection.struct.strippingAccessAndProjections.base, true)
    case let projection as TupleElementAddrInst:
      return (projection.tuple.strippingAccessAndProjections.base, true)
    default:
      return (self, false)
    }
  }
}

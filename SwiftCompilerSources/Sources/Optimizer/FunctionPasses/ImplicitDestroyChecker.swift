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

/// Diagnoses destroys of values `~Deinitable` types, which are not allowed to be destroyed implicitly.
///
/// This pass runs right after the move-only checker, which guarantees that no path consumes a noncopyable value more
/// than once, and that every path that doesn't consume the value ends its lifetime with an explicit `destroy_value` or
/// `destroy_addr`. A call or any other legal transfer of the value is a consume, not a destroy. "Consumed on every
/// path" therefore reduces to a simple rule: no destroy on a path that doesn't end in `unreachable`.
///
let implicitDestroyChecker = FunctionPass(name: "implicit-destroy-checker") {
  (function: Function, context: FunctionPassContext) in

  // Don't rerun diagnostics on deserialized functions.
  if function.wasDeserializedCanonical {
    return
  }

  // If an earlier pass already diagnosed this function, don't add noise.
  if function.hasSemanticsAttribute("sil.optimizer.moveonly.diagnostic.ignore") {
    return
  }

  // Find implicit destroys that must be diagnosed. Also, find instances of `discard self` that this checker
  // cannot yet handle.
  var unsupportedDiscards: [DropDeinitInst] = []
  var destroys: [ImplicitDestroy] = []
  for instruction in function.instructions {
    if let dropDeinit = instruction as? DropDeinitInst {
      // FIXME: [deinitable] Support `discard self` in aggregates with members that can't be destroyed implicitly.
      let type = dropDeinit.operand.value.type.objectType
      if !type.isImplicitlyDestroyable(in: function) && !type.hasOnlyImplicitlyDestroyableMembers(in: function) {
        unsupportedDiscards.append(dropDeinit)
      }
    } else if let destroy = ImplicitDestroy(of: instruction, in: function) {
      destroys.append(destroy)
    }
  }

  for dropDeinit in unsupportedDiscards {
    context.diagnosticEngine.diagnose(.sil_implicit_destroy_discard_unsupported,
                                      dropDeinit.operand.value.type.rawType,
                                      at: dropDeinit.location.sourceLoc)
  }

  if destroys.isEmpty {
    return
  }

  // Group the destroys by the variable or stored property that they destroy, so that each one gets one error with a
  // note for each path.
  var groups: [(variable: DestroyedVariable, destroys: [Instruction])] = []
  let endsInUnreachable = BlocksThatEndInUnreachable(in: function, context)
  for destroy in destroys where !endsInUnreachable.contains(destroy.instruction.parentBlock) {
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

/// The blocks from which every path ends in `unreachable`, for example by calling `fatalError()`.
///
/// A destroy in any other block runs, even in a block that can't reach the function's exit because it loops forever.
private struct BlocksThatEndInUnreachable : ~Copyable {
  private var blocks: BasicBlockSet

  init(in function: Function, _ context: FunctionPassContext) {
    blocks = BasicBlockSet(context)
    // A block is added once all of its successors are, so blocks in a loop that never ends aren't added.
    var changed = true
    while changed {
      changed = false
      for block in function.blocks.reversed() where !blocks.contains(block) {
        let successors = block.successors
        if block.terminator is UnreachableInst ||
           (!successors.isEmpty && successors.allSatisfy { blocks.contains($0) }) {
          blocks.insert(block)
          changed = true
        }
      }
    }
  }

  func contains(_ block: BasicBlock) -> Bool { blocks.contains(block) }

  deinit {
    var blocks = blocks
    blocks.deinitialize()
  }
}

/// A destroy of a value that can't be destroyed implicitly.
private struct ImplicitDestroy {
  let instruction: Instruction
  let value: Value

  /// The type of the value that the destroy drops. This is the type of the contents if `value` is a box.
  let type: Type

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
    case let store as StoreInst where store.storeOwnership == .assign:
      value = store.destination
    case let copy as CopyAddrInst where !copy.isInitializationOfDestination:
      value = copy.destination
    default:
      return nil
    }

    let type = value.type.objectType
    if type.isImplicitlyDestroyable(in: function) {
      return nil
    }

    // Destroying the result of a supported `discard self` doesn't destroy anything.
    if value is DropDeinitInst {
      return nil
    }

    self.instruction = instruction
    self.value = value
    self.type = type.isBox ? type.getBoxFields(in: function)[0] : type
  }
}

/// The variable, or the stored property of a variable, that a destroy drops.
private struct DestroyedVariable : Equatable {
  /// The value that introduces the variable, or the destroyed value itself if no name could be inferred.
  let root: Value

  /// The user-facing name, such as `x` or `x.y`, if the root is a variable.
  let name: String?

  /// The type of the value that is dropped, which is the type of a stored property for a name like `x.y`.
  let type: Type

  init(_ destroy: ImplicitDestroy) {
    self.type = destroy.type
    guard let (name, root) = destroy.value.inferredNameAndRoot else {
      self.root = destroy.value
      self.name = nil
      return
    }
    self.root = root
    if root.isVariable {
      self.name = name
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

  // The only way to consume an exactly-once function is to call it.
  let isCall = variable.type.isLoweredFunction ? 1 : 0
  if let name = variable.name {
    context.diagnosticEngine.diagnose(.sil_implicit_destroy_not_consumed, name, isCall, at: loc)
  } else {
    context.diagnosticEngine.diagnose(.sil_implicit_destroy_unnamed_not_consumed,
                                      variable.type.rawType, isCall, at: loc)
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
    context.diagnosticEngine.diagnose(.sil_implicit_destroy_path_exit, isCall, at: exitLoc)
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

  // The path doesn't exit, for example because it's in a loop that never ends. Don't repeat the declaration.
  if let loc, loc.bridged.raw != declLoc?.bridged.raw {
    return loc
  }
  return nil
}

private extension Value {
  /// True if this value is a variable, so that diagnostics can name it.
  var isVariable: Bool {
    if let arg = self as? FunctionArgument {
      return arg.findVarDecl() != nil
    }
    return definingInstruction is DebugVariableInstruction
  }
}

private extension Type {
  /// True if every stored property or enum payload of this type can be destroyed implicitly.
  func hasOnlyImplicitlyDestroyableMembers(in function: Function) -> Bool {
    if isStruct, let fields = getNominalFields(in: function) {
      return fields.allSatisfy { $0.isImplicitlyDestroyable(in: function) }
    }
    if isEnum, let cases = getEnumCases(in: function) {
      return cases.allSatisfy { $0.payload?.isImplicitlyDestroyable(in: function) ?? true }
    }
    return false
  }
}

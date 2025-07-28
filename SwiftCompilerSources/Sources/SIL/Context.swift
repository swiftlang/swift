//===--- Context.swift - defines the context protocols --------------------===//
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

import SILBridging
import AST

public protocol Context {
  var _bridged: BridgedContext { get }
}

/// A context which allows mutation of a function's SIL.
public protocol MutatingContext : Context {
  // Called by all instruction mutations, including inserted new instructions.
  var notifyInstructionChanged: (Instruction) -> () { get }
}

/// Common funcationality of all Contexts.
extension Context {
  public var silStage: SILStage {
    switch _bridged.getSILStage() {
      case .Raw:       return .raw
      case .Canonical: return .canonical
      case .Lowered:   return .lowered
      default:         fatalError("unhandled SILStage case")
    }
  }

  public var currentModuleContext: ModuleDecl {
    _bridged.getCurrentModuleContext().getAs(ModuleDecl.self)
  }

  public var moduleIsSerialized: Bool { _bridged.moduleIsSerialized() }

  public var moduleHasLoweredAddresses: Bool { _bridged.moduleHasLoweredAddresses() }

  public func lookupDeinit(ofNominal: NominalTypeDecl) -> Function? {
    _bridged.lookUpNominalDeinitFunction(ofNominal.bridged).function
  }

  public func getBuiltinIntegerType(bitWidth: Int) -> Type { _bridged.getBuiltinIntegerType(bitWidth).type }

  public var swiftArrayDecl: NominalTypeDecl {
    _bridged.getSwiftArrayDecl().getAs(NominalTypeDecl.self)
  }

  public var swiftMutableSpan: NominalTypeDecl {
    _bridged.getSwiftMutableSpanDecl().getAs(NominalTypeDecl.self)
  }

  public func lookupFunction(name: String) -> Function? {
    name._withBridgedStringRef {
      _bridged.lookupFunction($0).function
    }
  }

  public func lookupWitnessTable(for conformance: Conformance) -> WitnessTable? {
    return _bridged.lookupWitnessTable(conformance.bridged).witnessTable
  }

  public func lookupVTable(for classDecl: NominalTypeDecl) -> VTable? {
    return _bridged.lookupVTable(classDecl.bridged).vTable
  }

  public func lookupSpecializedVTable(for classType: Type) -> VTable? {
    return _bridged.lookupSpecializedVTable(classType.bridged).vTable
  }

  /// Looks up a function in the `Swift` module.
  /// The `name` is the source name of the function and not the mangled name.
  /// Returns nil if no such function or multiple matching functions are found.
  public func lookupStdlibFunction(name: StaticString) -> Function? {
    return name.withUTF8Buffer { (nameBuffer: UnsafeBufferPointer<UInt8>) in
      let nameStr = BridgedStringRef(data: nameBuffer.baseAddress, count: nameBuffer.count)
      return _bridged.lookupStdlibFunction(nameStr).function
    }
  }

  public func getSpecializedConformance(of genericConformance: Conformance,
                                        for type: AST.`Type`,
                                        substitutions: SubstitutionMap) -> Conformance
  {
    let c = _bridged.getSpecializedConformance(genericConformance.bridged, type.bridged, substitutions.bridged)
    return Conformance(bridged: c)
  }
}

extension MutatingContext {
  public func verifyIsTransforming(function: Function) {
    precondition(_bridged.isTransforming(function.bridged), "pass modifies wrong function")
  }

  public func notifyInstructionsChanged() {
    _bridged.notifyChanges(.Instructions)
  }

  public func notifyCallsChanged() {
    _bridged.notifyChanges(.Calls)
  }

  public func notifyBranchesChanged() {
    _bridged.notifyChanges(.Branches)
  }

  public func notifyEffectsChanged() {
    _bridged.notifyChanges(.Effects)
  }

  public func createGlobalVariable(name: String, type: Type, linkage: Linkage, isLet: Bool) -> GlobalVariable {
    let gv = name._withBridgedStringRef {
      _bridged.createGlobalVariable($0, type.bridged, linkage.bridged, isLet)
    }
    return gv.globalVar
  }

  /// Splits the basic block, which contains `inst`, before `inst` and returns the
  /// new block.
  ///
  /// `inst` and all subsequent instructions are moved to the new block, while all
  /// instructions _before_ `inst` remain in the original block.
  public func splitBlock(before inst: Instruction) -> BasicBlock {
    notifyBranchesChanged()
    return _bridged.splitBlockBefore(inst.bridged).block
  }

  /// Splits the basic block, which contains `inst`, after `inst` and returns the
  /// new block.
  ///
  /// All subsequent instructions after `inst` are moved to the new block, while `inst` and all
  /// instructions _before_ `inst` remain in the original block.
  public func splitBlock(after inst: Instruction) -> BasicBlock {
    notifyBranchesChanged()
    return _bridged.splitBlockAfter(inst.bridged).block
  }

  public func createBlock(after block: BasicBlock) -> BasicBlock {
    notifyBranchesChanged()
    return _bridged.createBlockAfter(block.bridged).block
  }

  /// Removes and deletes `instruction`.
  /// If `salvageDebugInfo` is true, compensating `debug_value` instructions are inserted for certain
  /// kind of instructions.
  public func erase(instruction: Instruction, salvageDebugInfo: Bool = true) {
    if !instruction.isInStaticInitializer {
      verifyIsTransforming(function: instruction.parentFunction)
    }
    if instruction is FullApplySite {
      notifyCallsChanged()
    }
    if instruction is TermInst {
      notifyBranchesChanged()
    }
    notifyInstructionsChanged()

    _bridged.eraseInstruction(instruction.bridged, salvageDebugInfo)
  }

  public func erase(instructionIncludingAllUsers inst: Instruction) {
    if inst.isDeleted {
      return
    }
    for result in inst.results {
      for use in result.uses {
        erase(instructionIncludingAllUsers: use.instruction)
      }
    }
    // We rely that after deleting the instruction its operands have no users.
    // Therefore `salvageDebugInfo` must be turned off because we cannot insert debug_value instructions.
    erase(instruction: inst, salvageDebugInfo: false)
  }

  public func erase<S: Sequence>(instructions: S) where S.Element: Instruction {
    for inst in instructions {
      erase(instruction: inst)
    }
  }

  public func erase(instructionIncludingDebugUses inst: Instruction) {
    precondition(inst.results.allSatisfy { $0.uses.ignoreDebugUses.isEmpty })
    erase(instructionIncludingAllUsers: inst)
  }

  public func erase(block: BasicBlock) {
    _bridged.eraseBlock(block.bridged)
  }

}

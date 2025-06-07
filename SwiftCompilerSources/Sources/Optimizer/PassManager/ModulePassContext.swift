//===--- ModulePassContext.swift ------------------------------------------===//
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
import OptimizerBridging

/// The context which is passed to a `ModulePass`'s run-function.
///
/// It provides access to all functions, v-tables and witness tables of a module,
/// but it doesn't provide any APIs to modify functions.
/// In order to modify a function, a module pass must use `transform(function:)`.
struct ModulePassContext : Context, CustomStringConvertible {
  let _bridged: BridgedPassContext

  var description: String {
    return String(taking: _bridged.getModuleDescription())
  }

  struct FunctionList : CollectionLikeSequence, IteratorProtocol {
    private var currentFunction: Function?
    
    fileprivate init(first: Function?) { currentFunction = first }

    mutating func next() -> Function? {
      if let f = currentFunction {
        currentFunction = BridgedPassContext.getNextFunctionInModule(f.bridged).function
        return f
      }
      return nil
    }
  }

  struct GlobalVariableList : CollectionLikeSequence, IteratorProtocol {
    private var currentGlobal: GlobalVariable?

    fileprivate init(first: GlobalVariable?) { currentGlobal = first }

    mutating func next() -> GlobalVariable? {
      if let g = currentGlobal {
        currentGlobal = BridgedPassContext.getNextGlobalInModule(g.bridged).globalVar
        return g
      }
      return nil
    }
  }

  struct VTableArray : BridgedRandomAccessCollection {
    fileprivate let bridgedCtxt: BridgedPassContext

    var startIndex: Int { return 0 }
    var endIndex: Int { return bridgedCtxt.getNumVTables() }

    subscript(_ index: Int) -> VTable {
      assert(index >= startIndex && index < endIndex)
      return VTable(bridged: bridgedCtxt.getVTable(index))
    }
  }

  struct WitnessTableList : CollectionLikeSequence, IteratorProtocol {
    private var currentTable: WitnessTable?
    
    fileprivate init(first: WitnessTable?) { currentTable = first }

    mutating func next() -> WitnessTable? {
      if let t = currentTable {
        currentTable = BridgedPassContext.getNextWitnessTableInModule(t.bridged).witnessTable
        return t
      }
      return nil
    }
  }

  struct DefaultWitnessTableList : CollectionLikeSequence, IteratorProtocol {
    private var currentTable: DefaultWitnessTable?
    
    fileprivate init(first: DefaultWitnessTable?) { currentTable = first }

    mutating func next() -> DefaultWitnessTable? {
      if let t = currentTable {
        currentTable = BridgedPassContext.getNextDefaultWitnessTableInModule(t.bridged).defaultWitnessTable
        return t
      }
      return nil
    }
  }

  var functions: FunctionList {
    FunctionList(first: _bridged.getFirstFunctionInModule().function)
  }
  
  var globalVariables: GlobalVariableList {
    GlobalVariableList(first: _bridged.getFirstGlobalInModule().globalVar)
  }

  var vTables: VTableArray { VTableArray(bridgedCtxt: _bridged) }
  
  var witnessTables: WitnessTableList {
    WitnessTableList(first: _bridged.getFirstWitnessTableInModule().witnessTable)
  }

  var defaultWitnessTables: DefaultWitnessTableList {
    DefaultWitnessTableList(first: _bridged.getFirstDefaultWitnessTableInModule().defaultWitnessTable)
  }

  /// Run a closure with a `PassContext` for a function, which allows to modify that function.
  ///
  /// Only a single `transform` can be alive at the same time, i.e. it's not allowed to nest
  /// calls to `transform`.
  func transform(function: Function, _ runOnFunction: (FunctionPassContext) -> ()) {
    _bridged.beginTransformFunction(function.bridged)
    runOnFunction(FunctionPassContext(_bridged: _bridged))
    _bridged.endTransformFunction();
  }

  func loadFunction(function: Function, loadCalleesRecursively: Bool) -> Bool {
    if function.isDefinition {
      return true
    }
    _bridged.loadFunction(function.bridged, loadCalleesRecursively)
    return function.isDefinition
  }

  func specialize(function: Function, for substitutions: SubstitutionMap) -> Function? {
    return _bridged.specializeFunction(function.bridged, substitutions.bridged).function
  }

  enum DeserializationMode {
    case allFunctions
    case onlySharedFunctions
  }

  func deserializeAllCallees(of function: Function, mode: DeserializationMode) {
    _bridged.deserializeAllCallees(function.bridged, mode == .allFunctions ? true : false)
  }

  @discardableResult
  func createSpecializedWitnessTable(entries: [WitnessTable.Entry],
                          conformance: Conformance,
                          linkage: Linkage,
                          serialized: Bool) -> WitnessTable
  {
    let bridgedEntries = entries.map { $0.bridged }
    let bridgedWitnessTable = bridgedEntries.withBridgedArrayRef {
      _bridged.createSpecializedWitnessTable(linkage.bridged, serialized, conformance.bridged, $0)
    }
    return WitnessTable(bridged: bridgedWitnessTable)
  }

  @discardableResult
  func createSpecializedVTable(entries: [VTable.Entry],
                               for classType: Type,
                               isSerialized: Bool) -> VTable
  {
    let bridgedEntries = entries.map { $0.bridged }
    let bridgedVTable = bridgedEntries.withBridgedArrayRef {
      _bridged.createSpecializedVTable(classType.bridged, isSerialized, $0)
    }
    return VTable(bridged: bridgedVTable)
  }

  func replaceVTableEntries(of vTable: VTable, with entries: [VTable.Entry]) {
    let bridgedEntries = entries.map { $0.bridged }
    bridgedEntries.withBridgedArrayRef {
      vTable.bridged.replaceEntries($0)
    }
    notifyFunctionTablesChanged()
  }

  func createEmptyFunction(
    name: String,
    parameters: [ParameterInfo],
    hasSelfParameter: Bool,
    fromOriginal originalFunction: Function
  ) -> Function {
    return name._withBridgedStringRef { nameRef in
      let bridgedParamInfos = parameters.map { $0._bridged }
      return bridgedParamInfos.withUnsafeBufferPointer { paramBuf in
        _bridged.createEmptyFunction(nameRef, paramBuf.baseAddress, paramBuf.count,
                                     hasSelfParameter, originalFunction.bridged).function
      }
    }
  }

  func moveFunctionBody(from sourceFunc: Function, to destFunc: Function) {
    precondition(!destFunc.isDefinition, "cannot move body to non-empty function")
    _bridged.moveFunctionBody(sourceFunc.bridged, destFunc.bridged)
  }

  func mangleAsyncRemoved(from function: Function) -> String {
    return String(taking: _bridged.mangleAsyncRemoved(function.bridged))
  }

  func mangle(withDeadArguments: [Int], from function: Function) -> String {
    withDeadArguments.withUnsafeBufferPointer { bufPtr in
      bufPtr.withMemoryRebound(to: Int.self) { valPtr in
        String(taking: _bridged.mangleWithDeadArgs(valPtr.baseAddress,
                                                   withDeadArguments.count,
                                                   function.bridged))
      }
    }
  }

  func notifyFunctionTablesChanged() {
    _bridged.asNotificationHandler().notifyChanges(.functionTablesChanged)
  }
}

extension GlobalVariable {
  func setIsLet(to value: Bool, _ context: ModulePassContext) {
    bridged.setLet(value)
  }
}

extension Function {
  func set(linkage: Linkage, _ context: ModulePassContext) {
    bridged.setLinkage(linkage.bridged)
  }

  func set(isSerialized: Bool, _ context: ModulePassContext) {
    bridged.setIsSerialized(isSerialized)
  }
}

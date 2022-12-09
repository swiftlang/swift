//===--- Function.swift - Defines the Function class ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic
import SILBridging

final public class Function : CustomStringConvertible, HasShortDescription, Hashable {
  public private(set) var effects = FunctionEffects()

  public var name: StringRef {
    return StringRef(bridged: SILFunction_getName(bridged))
  }

  final public var description: String {
    let stdString = SILFunction_debugDescription(bridged)
    return String(_cxxString: stdString)
  }

  public var shortDescription: String { name.string }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  public var hasOwnership: Bool { SILFunction_hasOwnership(bridged) != 0 }

  /// Returns true if the function is a definition and not only an external declaration.
  ///
  /// This is the case if the functioun contains a body, i.e. some basic blocks.
  public var isDefinition: Bool { blocks.first != nil }

  public var entryBlock: BasicBlock {
    SILFunction_firstBlock(bridged).block!
  }

  public var blocks : BasicBlockList {
    BasicBlockList(first: SILFunction_firstBlock(bridged).block)
  }

  public var arguments: LazyMapSequence<ArgumentArray, FunctionArgument> {
    entryBlock.arguments.lazy.map { $0 as! FunctionArgument }
  }

  /// All instructions of all blocks.
  public var instructions: LazySequence<FlattenSequence<LazyMapSequence<BasicBlockList, InstructionList>>> {
    blocks.lazy.flatMap { $0.instructions }
  }

  /// The number of indirect result arguments.
  public var numIndirectResultArguments: Int {
    SILFunction_numIndirectResultArguments(bridged)
  }
  
  /// The number of arguments which correspond to parameters (and not to indirect results).
  public var numParameterArguments: Int {
    SILFunction_numParameterArguments(bridged)
  }

  /// The total number of arguments.
  ///
  /// This is the sum of indirect result arguments and parameter arguments.
  /// If the function is a definition (i.e. it has at least an entry block), this is the
  /// number of arguments of the function's entry block.
  public var numArguments: Int { numIndirectResultArguments + numParameterArguments }

  public var hasSelfArgument: Bool {
    SILFunction_getSelfArgumentIndex(bridged) >= 0
  }

  public var selfArgumentIndex: Int {
    let selfIdx = SILFunction_getSelfArgumentIndex(bridged)
    assert(selfIdx >= 0)
    return selfIdx
  }
  
  public var argumentTypes: ArgumentTypeArray { ArgumentTypeArray(function: self) }
  public var resultType: Type { SILFunction_getSILResultType(bridged).type }

  public func getArgumentConvention(for argumentIndex: Int) -> ArgumentConvention {
    if argumentIndex < numIndirectResultArguments {
      return .indirectOut
    }
    return SILFunction_getSILArgumentConvention(bridged, argumentIndex).convention
  }

  public var returnInstruction: ReturnInst? {
    for block in blocks.reversed() {
      if let retInst = block.terminator as? ReturnInst { return retInst }
    }
    return nil
  }

  /// True, if the linkage of the function indicates that it is visible outside the current
  /// compilation unit and therefore not all of its uses are known.
  ///
  /// For example, `public` linkage.
  public var isPossiblyUsedExternally: Bool {
    return SILFunction_isPossiblyUsedExternally(bridged) != 0
  }

  /// True, if the linkage of the function indicates that it has a definition outside the
  /// current compilation unit.
  ///
  /// For example, `public_external` linkage.
  public var isAvailableExternally: Bool {
    return SILFunction_isAvailableExternally(bridged) != 0
  }

  public func hasSemanticsAttribute(_ attr: StaticString) -> Bool {
    attr.withUTF8Buffer { (buffer: UnsafeBufferPointer<UInt8>) in
      SILFunction_hasSemanticsAttr(bridged, llvm.StringRef(buffer.baseAddress!, buffer.count)) != 0
    }
  }

  /// True if the callee function is annotated with @_semantics("programtermination_point").
  /// This means that the function terminates the program.
  public var isProgramTerminationPoint: Bool { hasSemanticsAttribute("programtermination_point") }

  /// Kinds of effect attributes which can be defined for a Swift function.
  public enum EffectAttribute {
    /// No effect attribute is specified.
    case none
    
    /// `[readnone]`
    ///
    /// A readnone function does not have any observable memory read or write operations.
    /// This does not mean that the function cannot read or write at all. For example,
    /// it’s allowed to allocate and write to local objects inside the function.
    ///
    /// A function can be marked as readnone if two calls of the same function with the
    /// same parameters can be simplified to one call (e.g. by the CSE optimization).
    /// Some conclusions:
    /// * A readnone function must not return a newly allocated class instance.
    /// * A readnone function can return a newly allocated copy-on-write object,
    ///   like an Array, because COW data types conceptually behave like value types.
    /// * A readnone function must not release any parameter or any object indirectly
    ///   referenced from a parameter.
    /// * Any kind of observable side-effects are not allowed, like `print`, file IO, etc.
    case readNone
    
    /// `[readonly]`
    ///
    /// A readonly function does not have any observable memory write operations.
    /// Similar to readnone, a readonly function is allowed to contain writes to e.g. local objects, etc.
    ///
    /// A function can be marked as readonly if it’s save to eliminate a call to such
    /// a function if its return value is not used.
    /// The same conclusions as for readnone also apply to readonly.
    case readOnly
    
    /// `[releasenone]`
    ///
    /// A releasenone function must not perform any observable release-operation on an object.
    /// This means, it must not do anything which might let the caller observe any decrement of
    /// a reference count or any deallocations.
    /// Note that it's allowed to release an object if the release is balancing a retain in the
    /// same function. Also, it's allowed to release (and deallocate) local objects which were
    /// allocated in the same function.
    case releaseNone
  }

  /// The effect attribute which is specified in the source code (if any).
  public var effectAttribute: EffectAttribute {
    switch SILFunction_getEffectAttribute(bridged) {
      case EffectKind_none: return .none
      case EffectKind_readNone: return .readNone
      case EffectKind_readOnly: return .readOnly
      case EffectKind_releaseNone: return .releaseNone
      default: fatalError()
    }
  }

  /// True, if the function runs with a swift 5.1 runtime.
  /// Note that this is function specific, because inlinable functions are de-serialized
  /// in a client module, which might be compiled with a different deployment target.
  public var isSwift51RuntimeAvailable: Bool {
    SILFunction_isSwift51RuntimeAvailable(bridged) != 0
  }

  public var needsStackProtection: Bool {
    SILFunction_needsStackProtection(bridged) != 0
  }

  public var isDeinitBarrier: Bool {
    effects.sideEffects?.global.isDeinitBarrier ?? true
  }

  // Only to be called by PassContext
  public func _modifyEffects(_ body: (inout FunctionEffects) -> ()) {
    body(&effects)
  }

  static func register() {
    func checkLayout(_ p: UnsafeMutablePointer<FunctionEffects>,
                     data: UnsafeMutableRawPointer, size: Int) {
      assert(MemoryLayout<FunctionEffects>.size <= size, "wrong FunctionInfo size")
      assert(UnsafeMutableRawPointer(p) == data, "wrong FunctionInfo layout")
    }

    let metatype = unsafeBitCast(Function.self, to: SwiftMetatype.self)
    Function_register(metatype,
      // initFn
      { (f: BridgedFunction, data: UnsafeMutableRawPointer, size: Int) in
        checkLayout(&f.function.effects, data: data, size: size)
        data.initializeMemory(as: FunctionEffects.self, repeating: FunctionEffects(), count: 1)
      },
      // destroyFn
      { (f: BridgedFunction, data: UnsafeMutableRawPointer, size: Int) in
        checkLayout(&f.function.effects, data: data, size: size)
        data.assumingMemoryBound(to: FunctionEffects.self).deinitialize(count: 1)
      },
      // writeFn
      { (f: BridgedFunction, os: BridgedOStream, idx: Int) in
        let s: String
        let effects = f.function.effects
        if idx >= 0 {
          if idx < effects.escapeEffects.arguments.count {
            s = effects.escapeEffects.arguments[idx].bodyDescription
          } else {
            let globalIdx = idx - effects.escapeEffects.arguments.count
            if globalIdx == 0 {
              s = effects.sideEffects!.global.description
            } else {
              let seIdx = globalIdx - 1
              s = effects.sideEffects!.getArgumentEffects(for: seIdx).bodyDescription
            }
          }
        } else {
          s = effects.description
        }
        s._withStringRef { OStream_write(os, $0) }
      },
      // parseFn:
      { (f: BridgedFunction, str: llvm.StringRef, mode: ParseEffectsMode, argumentIndex: Int, paramNames: BridgedArrayRef) -> BridgedParsingError in
        do {
          var parser = StringParser(str.string)
          let function = f.function

          switch mode {
          case ParseArgumentEffectsFromSource:
            let paramToIdx = paramNames.withElements(ofType: llvm.StringRef.self) {
                (buffer: UnsafeBufferPointer<llvm.StringRef>) -> Dictionary<String, Int> in
              let keyValPairs = buffer.enumerated().lazy.map { ($0.1.string, $0.0) }
              return Dictionary(uniqueKeysWithValues: keyValPairs)
            }
            let effect = try parser.parseEffectFromSource(for: function, params: paramToIdx)
            function.effects.escapeEffects.arguments.append(effect)
          case ParseArgumentEffectsFromSIL:
            try parser.parseEffectsFromSIL(argumentIndex: argumentIndex, to: &function.effects)
          case ParseGlobalEffectsFromSIL:
            try parser.parseGlobalSideEffectsFromSIL(to: &function.effects)
          case ParseMultipleEffectsFromSIL:
            try parser.parseEffectsFromSIL(to: &function.effects)
          default:
            fatalError("invalid ParseEffectsMode")
          }
          if !parser.isEmpty() { try parser.throwError("syntax error") }
        } catch let error as ParsingError {
          return BridgedParsingError(message: error.message.utf8Start, position: error.position)
        } catch {
          fatalError()
        }
        return BridgedParsingError(message: nil, position: 0)
      },
      // copyEffectsFn
      { (toFunc: BridgedFunction, fromFunc: BridgedFunction) -> Int in
        let srcFunc = fromFunc.function
        let destFunc = toFunc.function
        let srcResultArgs = srcFunc.numIndirectResultArguments
        let destResultArgs = destFunc.numIndirectResultArguments
        
        // We only support reabstraction (indirect -> direct) of a single
        // return value.
        if srcResultArgs != destResultArgs &&
           (srcResultArgs > 1 || destResultArgs > 1) {
          return 0
        }
        destFunc.effects =
          FunctionEffects(copiedFrom: srcFunc.effects,
                          resultArgDelta: destResultArgs - srcResultArgs)
        return 1
      },
      // getEffectInfo
      {  (f: BridgedFunction, idx: Int) -> BridgedEffectInfo in
        let effects = f.function.effects
        if idx < effects.escapeEffects.arguments.count {
          let effect = effects.escapeEffects.arguments[idx]
          return BridgedEffectInfo(argumentIndex: effect.argumentIndex,
                                   isDerived: effect.isDerived, isEmpty: false, isValid: true)
        }
        if let sideEffects = effects.sideEffects {
          let globalIdx = idx - effects.escapeEffects.arguments.count
          if globalIdx == 0 {
            return BridgedEffectInfo(argumentIndex: -1, isDerived: true, isEmpty: false, isValid: true)
          }
          let seIdx = globalIdx - 1
          if seIdx < sideEffects.arguments.count {
            return BridgedEffectInfo(argumentIndex: seIdx, isDerived: true,
                                     isEmpty: sideEffects.arguments[seIdx].isEmpty, isValid: true)
          }
        }
        return BridgedEffectInfo(argumentIndex: -1, isDerived: false, isEmpty: true, isValid: false)
      },
      // getMemBehaviorFn
      { (f: BridgedFunction, observeRetains: Bool) -> BridgedMemoryBehavior in
        let e = f.function.getSideEffects()
        return e.getMemBehavior(observeRetains: observeRetains)
      }
    )
  }

  public var bridged: BridgedFunction { BridgedFunction(obj: SwiftObject(self)) }
}

public func == (lhs: Function, rhs: Function) -> Bool { lhs === rhs }
public func != (lhs: Function, rhs: Function) -> Bool { lhs !== rhs }

public struct ArgumentTypeArray : RandomAccessCollection, FormattedLikeArray {
  fileprivate let function: Function

  public var startIndex: Int { return 0 }
  public var endIndex: Int { SILFunction_getNumSILArguments(function.bridged) }

  public subscript(_ index: Int) -> Type {
    SILFunction_getSILArgumentType(function.bridged, index).type
  }
}

// Bridging utilities

extension BridgedFunction {
  public var function: Function { obj.getAs(Function.self) }
}

extension OptionalBridgedFunction {
  public var function: Function? { obj.getAs(Function.self) }
}

public extension SideEffects.GlobalEffects {
  func getMemBehavior(observeRetains: Bool) -> BridgedMemoryBehavior {
    if allocates || ownership.destroy || (ownership.copy && observeRetains) {
      return MayHaveSideEffectsBehavior
    }
    switch (memory.read, memory.write) {
    case (false, false): return NoneBehavior
    case (true, false): return MayReadBehavior
    case (false, true): return MayWriteBehavior
    case (true, true): return MayReadWriteBehavior
    }
  }
}

public struct BasicBlockList : CollectionLikeSequence, IteratorProtocol {
  private var currentBlock: BasicBlock?

  public init(first: BasicBlock?) { currentBlock = first }

  public mutating func next() -> BasicBlock? {
    if let block = currentBlock {
      currentBlock = block.next
      return block
    }
    return nil
  }

  public var first: BasicBlock? { currentBlock }

  public func reversed() -> ReverseBasicBlockList {
    if let block = currentBlock {
      let lastBlock = SILFunction_lastBlock(block.function.bridged).block
      return ReverseBasicBlockList(first: lastBlock)
    }
    return ReverseBasicBlockList(first: nil)
  }
}

public struct ReverseBasicBlockList : CollectionLikeSequence, IteratorProtocol {
  private var currentBlock: BasicBlock?

  public init(first: BasicBlock?) { currentBlock = first }

  public mutating func next() -> BasicBlock? {
    if let block = currentBlock {
      currentBlock = block.previous
      return block
    }
    return nil
  }

  public var first: BasicBlock? { currentBlock }
}

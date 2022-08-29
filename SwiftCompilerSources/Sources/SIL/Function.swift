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

  public var entryBlock: BasicBlock {
    SILFunction_firstBlock(bridged).block!
  }

  public var blocks : List<BasicBlock> {
    return List(first: SILFunction_firstBlock(bridged).block)
  }

  public var arguments: LazyMapSequence<ArgumentArray, FunctionArgument> {
    entryBlock.arguments.lazy.map { $0 as! FunctionArgument }
  }

  /// All instructions of all blocks.
  public var instructions: LazySequence<FlattenSequence<LazyMapSequence<List<BasicBlock>, List<Instruction>>>> {
    blocks.lazy.flatMap { $0.instructions }
  }

  public var numIndirectResultArguments: Int {
    SILFunction_numIndirectResultArguments(bridged)
  }

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

  /// True, if the function runs with a swift 5.1 runtime.
  /// Note that this is function specific, because inlinable functions are de-serialized
  /// in a client module, which might be compiled with a different deployment target.
  public var isSwift51RuntimeAvailable: Bool {
    SILFunction_isSwift51RuntimeAvailable(bridged) != 0
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
        let s = f.function.effects.argumentEffects[idx].description
        s._withStringRef { OStream_write(os, $0) }
      },
      // parseFn:
      { (f: BridgedFunction, str: llvm.StringRef, fromSIL: Int, isDerived: Int, paramNames: BridgedArrayRef) -> BridgedParsingError in
        do {
          var parser = StringParser(str.string)
          let effect: ArgumentEffect
          if fromSIL != 0 {
            effect = try parser.parseEffectFromSIL(for: f.function, isDerived: isDerived != 0)
          } else {
            let paramToIdx = paramNames.withElements(ofType: llvm.StringRef.self) {
                (buffer: UnsafeBufferPointer<llvm.StringRef>) -> Dictionary<String, Int> in
              let keyValPairs = buffer.enumerated().lazy.map { ($0.1.string, $0.0) }
              return Dictionary(uniqueKeysWithValues: keyValPairs)
            }
            effect = try parser.parseEffectFromSource(for: f.function, params: paramToIdx)
          }
          if !parser.isEmpty() { try parser.throwError("syntax error") }

          f.function.effects.argumentEffects.append(effect)
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
      // getEffectFlags
      {  (f: BridgedFunction, idx: Int) -> Int in
        let argEffects = f.function.effects.argumentEffects
        if idx >= argEffects.count { return 0 }
        let effect = argEffects[idx]
        var flags = 0
        switch effect.kind {
          case .notEscaping, .escaping:
            flags |= Int(EffectsFlagEscape)
        }
        if effect.isDerived {
          flags |= Int(EffectsFlagDerived)
        }
        return flags
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

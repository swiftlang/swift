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
import AST
import SILBridging

@_semantics("arc.immortal")
final public class Function : CustomStringConvertible, HasShortDescription, Hashable {
  public private(set) var effects = FunctionEffects()

  public var name: StringRef {
    return StringRef(bridged: bridged.getName())
  }

  public var location: Location {
    return Location(bridged: bridged.getLocation())
  }

  final public var description: String {
    return String(taking: bridged.getDebugDescription())
  }

  public var shortDescription: String { name.string }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  public var isTrapNoReturn: Bool { bridged.isTrapNoReturn() }

  public var isAutodiffVJP: Bool { bridged.isAutodiffVJP() }

  public var specializationLevel: Int { bridged.specializationLevel() }
  
  public var hasOwnership: Bool { bridged.hasOwnership() }

  public var hasLoweredAddresses: Bool { bridged.hasLoweredAddresses() }

  /// The lowered function type in the expansion context of self.
  ///
  /// Always expanding a function type means that the opaque result types
  /// have the correct generic signature. For example:
  ///    @substituted <τ_0_0> () -> @out τ_0_0 for <some P>
  /// is lowered to this inside its module:
  ///    @substituted <τ_0_0> () -> @out τ_0_0 for <ActualResultType>
  /// and this outside its module
  ///    @substituted <τ_0_0> () -> @out τ_0_0 for <some P>
  public var loweredFunctionType: CanonicalType {
    CanonicalType(bridged: bridged.getLoweredFunctionTypeInContext())
  }

  /// Returns true if the function is a definition and not only an external declaration.
  ///
  /// This is the case if the functioun contains a body, i.e. some basic blocks.
  public var isDefinition: Bool { blocks.first != nil }

  public var blocks : BasicBlockList { BasicBlockList(first: bridged.getFirstBlock().block) }

  public var entryBlock: BasicBlock { blocks.first! }

  public var arguments: LazyMapSequence<ArgumentArray, FunctionArgument> {
    entryBlock.arguments.lazy.map { $0 as! FunctionArgument }
  }

  public func argument(at index: Int) -> FunctionArgument {
    entryBlock.arguments[index] as! FunctionArgument
  }

  /// All instructions of all blocks.
  public var instructions: LazySequence<FlattenSequence<LazyMapSequence<BasicBlockList, InstructionList>>> {
    blocks.lazy.flatMap { $0.instructions }
  }

  public var reversedInstructions: LazySequence<FlattenSequence<LazyMapSequence<ReverseBasicBlockList, ReverseInstructionList>>>  {
    blocks.reversed().lazy.flatMap { $0.instructions.reversed() }
  }
  
  public var returnInstruction: ReturnInst? {
    for block in blocks.reversed() {
      if let retInst = block.terminator as? ReturnInst { return retInst }
    }
    return nil
  }

  /// True if the callee function is annotated with @_semantics("programtermination_point").
  /// This means that the function terminates the program.
  public var isProgramTerminationPoint: Bool { hasSemanticsAttribute("programtermination_point") }

  public var isTransparent: Bool { bridged.isTransparent() }

  public var isAsync: Bool { bridged.isAsync() }

  /// True if this is a `[global_init]` function.
  ///
  /// Such a function is typically a global addressor which calls the global's
  /// initializer (`[global_init_once_fn]`) via a `builtin "once"`.
  public var isGlobalInitFunction: Bool { bridged.isGlobalInitFunction() }

  /// True if this is a `[global_init_once_fn]` function.
  ///
  /// Such a function allocates a global and stores the global's init value.
  /// It's called from a `[global_init]` function via a `builtin "once"`.
  public var isGlobalInitOnceFunction: Bool { bridged.isGlobalInitOnceFunction() }

  public var isDestructor: Bool { bridged.isDestructor() }

  public var isGeneric: Bool { bridged.isGeneric() }

  public var linkage: Linkage { bridged.getLinkage().linkage }

  /// True, if the linkage of the function indicates that it is visible outside the current
  /// compilation unit and therefore not all of its uses are known.
  ///
  /// For example, `public` linkage.
  public var isPossiblyUsedExternally: Bool {
    return bridged.isPossiblyUsedExternally()
  }

  /// True, if the linkage of the function indicates that it has a definition outside the
  /// current compilation unit.
  ///
  /// For example, `public_external` linkage.
  public var isDefinedExternally: Bool { linkage.isExternal }

  public func hasSemanticsAttribute(_ attr: StaticString) -> Bool {
    attr.withUTF8Buffer { (buffer: UnsafeBufferPointer<UInt8>) in
      bridged.hasSemanticsAttr(BridgedStringRef(data: buffer.baseAddress!, count: buffer.count))
    }
  }
  public var isSerialized: Bool { bridged.isSerialized() }

  public var isAnySerialized: Bool { bridged.isAnySerialized() }

  public enum SerializedKind {
    case notSerialized, serialized, serializedForPackage
  }

  public var serializedKind: SerializedKind {
    switch bridged.getSerializedKind() {
    case .IsNotSerialized: return .notSerialized
    case .IsSerialized: return .serialized
    case .IsSerializedForPackage: return .serializedForPackage
    @unknown default: fatalError()
    }
  }

  private func serializedKindBridged(_ arg: SerializedKind) -> BridgedFunction.SerializedKind {
    switch arg {
    case .notSerialized: return .IsNotSerialized
    case .serialized: return .IsSerialized
    case .serializedForPackage: return .IsSerializedForPackage
    }
  }

  public func canBeInlinedIntoCaller(withSerializedKind callerSerializedKind: SerializedKind) -> Bool {
    switch serializedKind {
    // If both callee and caller are not_serialized, the callee can be inlined into the caller
    // during SIL inlining passes even if it (and the caller) might contain private symbols.
    case .notSerialized:
      return callerSerializedKind == .notSerialized;

    // If Package-CMO is enabled, we serialize package, public, and @usableFromInline decls as
    // [serialized_for_package].
    // Their bodies must not, however, leak into @inlinable functons (that are [serialized])
    // since they are inlined outside of their defining module.
    //
    // If this callee is [serialized_for_package], the caller must be either non-serialized
    // or [serialized_for_package] for this callee's body to be inlined into the caller.
    // It can however be referenced by [serialized] caller.
    case .serializedForPackage:
      return callerSerializedKind != .serialized;
    case .serialized:
      return true;
    }
  }

  public func hasValidLinkageForFragileRef(_ kind: SerializedKind) -> Bool {
    bridged.hasValidLinkageForFragileRef(serializedKindBridged(kind))
  }

  public enum ThunkKind {
    case noThunk, thunk, reabstractionThunk, signatureOptimizedThunk
  }

  public var thunkKind: ThunkKind {
    switch bridged.isThunk() {
    case .IsNotThunk:                return .noThunk
    case .IsThunk:                   return .thunk
    case .IsReabstractionThunk:      return .reabstractionThunk
    case .IsSignatureOptimizedThunk: return .signatureOptimizedThunk
    default:
      fatalError()
    }
  }

  /// True, if the function runs with a swift 5.1 runtime.
  /// Note that this is function specific, because inlinable functions are de-serialized
  /// in a client module, which might be compiled with a different deployment target.
  public var isSwift51RuntimeAvailable: Bool {
    bridged.isSwift51RuntimeAvailable()
  }

  public var needsStackProtection: Bool {
    bridged.needsStackProtection()
  }

  public var isDeinitBarrier: Bool {
    effects.sideEffects?.global.isDeinitBarrier ?? true
  }

  public enum PerformanceConstraints {
    case none
    case noAllocations
    case noLocks
    case noRuntime
    case noExistentials
    case noObjCRuntime
  }

  public var performanceConstraints: PerformanceConstraints {
    switch bridged.getPerformanceConstraints() {
      case .None: return .none
      case .NoAllocation: return .noAllocations
      case .NoLocks: return .noLocks
      case .NoRuntime: return .noRuntime
      case .NoExistentials: return .noExistentials
      case .NoObjCBridging: return .noObjCRuntime
      default: fatalError("unknown performance constraint")
    }
  }

  public enum InlineStrategy {
    case automatic
    case never
    case always
  }

  public var inlineStrategy: InlineStrategy {
    switch bridged.getInlineStrategy() {
      case .InlineDefault: return .automatic
      case .NoInline: return .never
      case .AlwaysInline: return .always
      default:
        fatalError()
    }
  }
}

public func == (lhs: Function, rhs: Function) -> Bool { lhs === rhs }
public func != (lhs: Function, rhs: Function) -> Bool { lhs !== rhs }

// Function conventions.
extension Function {
  public var convention: FunctionConvention {
    FunctionConvention(for: loweredFunctionType, in: self)
  }

  public var argumentConventions: ArgumentConventions {
    ArgumentConventions(convention: convention)
  }

  // FIXME: Change this to argumentConventions.indirectSILResultCount.
  // This is incorrect in two cases: it does not include the indirect
  // error result, and, prior to address lowering, does not include
  // pack results.
  public var numIndirectResultArguments: Int { bridged.getNumIndirectFormalResults() }

  public var hasIndirectErrorArgument: Bool { bridged.hasIndirectErrorResult() }

  /// The number of arguments which correspond to parameters (and not to indirect results).
  public var numParameterArguments: Int { convention.parameters.count }

  /// The slice of arguments starting at argumentConventions.firstParameterIndex.
  public var parameters: LazyMapSequence<Slice<ArgumentArray>, FunctionArgument> {
    let args = arguments
    return args[argumentConventions.firstParameterIndex..<args.count]
  }

  /// The total number of arguments.
  ///
  /// This is the sum of indirect result arguments and parameter arguments.
  /// If the function is a definition (i.e. it has at least an entry block), this is the
  /// number of arguments of the function's entry block.
  public var numArguments: Int { argumentConventions.count }

  public var hasSelfArgument: Bool { argumentConventions.selfIndex != nil }

  public var selfArgumentIndex: Int { argumentConventions.selfIndex! }

  public var selfArgument: FunctionArgument { arguments[selfArgumentIndex] }
  
  public var dynamicSelfMetadata: FunctionArgument? {
    if bridged.hasDynamicSelfMetadata() {
      return arguments.last!
    }
    return nil
  }

  public var argumentTypes: ArgumentTypeArray { ArgumentTypeArray(function: self) }

  public var resultType: Type { bridged.getSILResultType().type }

  public var hasUnsafeNonEscapableResult: Bool {
    return bridged.hasUnsafeNonEscapableResult()
  }

  public var hasResultDependence: Bool {
    convention.resultDependencies != nil
  }
}

public struct ArgumentTypeArray : RandomAccessCollection, FormattedLikeArray {
  fileprivate let function: Function

  public var startIndex: Int { return 0 }
  public var endIndex: Int { function.bridged.getNumSILArguments() }

  public subscript(_ index: Int) -> Type {
    function.bridged.getSILArgumentType(index).type
  }
}

// Function effects.
extension Function {
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
    switch bridged.getEffectAttribute() {
      case .ReadNone: return .readNone
      case .ReadOnly: return .readOnly
      case .ReleaseNone: return .releaseNone
      default: return .none
    }
  }

  // Only to be called by PassContext
  public func _modifyEffects(_ body: (inout FunctionEffects) -> ()) {
    body(&effects)
  }
}

// Bridging utilities

extension Function {
  public var bridged: BridgedFunction {
    BridgedFunction(obj: SwiftObject(self))
  }

  static func register() {
    func checkLayout(_ p: UnsafeMutablePointer<FunctionEffects>,
                     data: UnsafeMutableRawPointer, size: Int) {
      assert(MemoryLayout<FunctionEffects>.size <= size, "wrong FunctionInfo size")
      assert(UnsafeMutableRawPointer(p) == data, "wrong FunctionInfo layout")
    }

    let metatype = unsafeBitCast(Function.self, to: SwiftMetatype.self)
    BridgedFunction.registerBridging(metatype,
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
        s._withBridgedStringRef { $0.write(os) }
      },
      // parseFn:
      { (f: BridgedFunction, str: BridgedStringRef, mode: BridgedFunction.ParseEffectsMode, argumentIndex: Int, paramNames: BridgedArrayRef) -> BridgedFunction.ParsingError in
        do {
          var parser = StringParser(String(str))
          let function = f.function

          switch mode {
          case .argumentEffectsFromSource:
            let paramToIdx = paramNames.withElements(ofType: BridgedStringRef.self) {
                (buffer: UnsafeBufferPointer<BridgedStringRef>) -> Dictionary<String, Int> in
              let keyValPairs = buffer.enumerated().lazy.map { (String($0.1), $0.0) }
              return Dictionary(uniqueKeysWithValues: keyValPairs)
            }
            let effect = try parser.parseEffectFromSource(for: function, params: paramToIdx)
            function.effects.escapeEffects.arguments.append(effect)
          case .argumentEffectsFromSIL:
            try parser.parseEffectsFromSIL(argumentIndex: argumentIndex, to: &function.effects)
          case .globalEffectsFromSIL:
            try parser.parseGlobalSideEffectsFromSIL(to: &function.effects)
          case .multipleEffectsFromSIL:
            try parser.parseEffectsFromSIL(to: &function.effects)
          default:
            fatalError("invalid ParseEffectsMode")
          }
          if !parser.isEmpty() { try parser.throwError("syntax error") }
        } catch let error as ParsingError {
          return BridgedFunction.ParsingError(message: error.message.utf8Start, position: error.position)
        } catch {
          fatalError()
        }
        return BridgedFunction.ParsingError(message: nil, position: 0)
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
      {  (f: BridgedFunction, idx: Int) -> BridgedFunction.EffectInfo in
        let effects = f.function.effects
        if idx < effects.escapeEffects.arguments.count {
          let effect = effects.escapeEffects.arguments[idx]
          return BridgedFunction.EffectInfo(argumentIndex: effect.argumentIndex,
                                            isDerived: effect.isDerived, isEmpty: false, isValid: true)
        }
        if let sideEffects = effects.sideEffects {
          let globalIdx = idx - effects.escapeEffects.arguments.count
          if globalIdx == 0 {
            return BridgedFunction.EffectInfo(argumentIndex: -1, isDerived: true, isEmpty: false, isValid: true)
          }
          let seIdx = globalIdx - 1
          if seIdx < sideEffects.arguments.count {
            return BridgedFunction.EffectInfo(argumentIndex: seIdx, isDerived: true,
                                              isEmpty: sideEffects.arguments[seIdx].isEmpty, isValid: true)
          }
        }
        return BridgedFunction.EffectInfo(argumentIndex: -1, isDerived: false, isEmpty: true, isValid: false)
      },
      // getMemBehaviorFn
      { (f: BridgedFunction, observeRetains: Bool) -> BridgedMemoryBehavior in
        let e = f.function.getSideEffects()
        return e.getMemBehavior(observeRetains: observeRetains)
      },
      // argumentMayRead  (used by the MemoryLifetimeVerifier)
      { (f: BridgedFunction, bridgedArgOp: BridgedOperand, bridgedAddr: BridgedValue) -> Bool in
        let argOp = Operand(bridged: bridgedArgOp)
        let addr = bridgedAddr.value
        let applySite = argOp.instruction as! FullApplySite
        let addrPath = addr.accessPath
        let calleeArgIdx = applySite.calleeArgumentIndex(of: argOp)!
        let convention = applySite.convention(of: argOp)!
        assert(convention.isIndirectIn || convention.isInout)
        let argPath = argOp.value.accessPath
        assert(!argPath.isDistinct(from: addrPath))
        let path = argPath.getProjection(to: addrPath) ?? SmallProjectionPath()
        let effects = f.function.getSideEffects(forArgument: argOp.value.at(path),
                                                atIndex: calleeArgIdx,
                                                withConvention: convention)
        return effects.memory.read
      }
    )
  }
}

extension BridgedFunction {
  public var function: Function { obj.getAs(Function.self) }
}

extension OptionalBridgedFunction {
  public var function: Function? { obj.getAs(Function.self) }
}

public extension SideEffects.GlobalEffects {
  func getMemBehavior(observeRetains: Bool) -> BridgedMemoryBehavior {
    if allocates || ownership.destroy || (ownership.copy && observeRetains) {
      return .MayHaveSideEffects
    }
    switch (memory.read, memory.write) {
    case (false, false): return .None
    case (true, false): return .MayRead
    case (false, true): return .MayWrite
    case (true, true): return .MayReadWrite
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
      let lastBlock = block.parentFunction.bridged.getLastBlock().block
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

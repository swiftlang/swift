import AST
import SIL

extension Type {
  func isBranchTracingEnum(in vjp: Function) -> Bool {
    return self.bridged.isAutodiffBranchTracingEnumInVJP(vjp.bridged)
  }
}

extension EnumCase {
  func enumType(in function: Function) -> Type {
    // Preserve the queried enum's generic arguments. A specialized VJP can
    // have an empty generic environment while its BTE declaration is generic.
    remapBranchTracingEnumType(self.enumType, into: function)
  }
}

extension TupleInst {
  // CFG simplification can duplicate a constructor without changing its saved
  // values. Accept that source shape, without equating different callable plans.
  func hasSamePayloadValues(as other: TupleInst) -> Bool {
    type == other.type && operands.elementsEqual(other.operands, by: { $0.value == $1.value })
  }
}

// Information required to specialize one closure stored in a payload tuple of a branch tracing enum case.
struct ClosureInBTE : Equatable {
  let closure: SingleValueInstruction
  // Ordered from the root closure to its stored callable: ordinary function
  // conversions and supported reabstraction/subset thunk partial applications.
  let adapters: [SingleValueInstruction]
  let optionalWrapper: EnumInst?
  let useInPayload: Operand
  let enumCase: EnumCase

  var payloadTuple: TupleInst { useInPayload.instruction as! TupleInst }
  var indexInPayload: Int { useInPayload.index }
}

private func getCapturedArgTypesTupleForClosure(
  closure: SingleValueInstruction, context: FunctionPassContext
) -> AST.`Type` {
  var capturedArgTypes = [AST.`Type`]()
  if let pai = closure as? PartialApplyInst {
    capturedArgTypes.append(contentsOf: pai.arguments.map{ $0.type.rawType })
  } else {
    assert(closure is ThinToThickFunctionInst)
  }
  return context.getTupleType(elements: capturedArgTypes)
}

// For a given branch tracing enum type, return an array of branch tracing enum types
// which are used for "predecessor" elements of payload tuples of the given branch tracing enum.
// For example, for the enum below, return [_AD__$xxx_bbB__Pred__xxx, _AD__$xxx_bbC__Pred__xxx].
// Note that the "predecessor" element in the payload tuple is optional and might be not present.
//
//   enum _AD__$xxx_bbA__Pred__xxx {
//     case bbB((predecessor: _AD__$xxx_bbB__Pred__xxx, /* closure types */))
//     case bbC((predecessor: _AD__$xxx_bbC__Pred__xxx, /* closure types */))
//   }
private func getBranchTracingEnumPreds(bteType: Type, in vjp: Function) -> Set<Type>? {
  guard let enumCases = bteType.getEnumCases(in: vjp) else {
    return nil
  }

  var btePreds = Set<Type>()
  for enumCase in enumCases {
    guard let payload = enumCase.payload, payload.isTuple else { return nil }
    guard let firstTupleElementType = payload.tupleElements.first else {
      continue
    }
    if firstTupleElementType.isBranchTracingEnum(in: vjp) {
      btePreds.insert(firstTupleElementType)
    }
  }

  return btePreds
}

private func iterateOverBranchTracingEnumPreds(
  bteToPredsDict: inout [Type: Set<Type>],
  currentBTEType: Type,
  in vjp: Function
) -> Bool {
  guard let currentBTEPreds = getBranchTracingEnumPreds(bteType: currentBTEType, in: vjp) else {
    return false
  }
  bteToPredsDict[currentBTEType] = currentBTEPreds
  for currentBTEPred in currentBTEPreds {
    if bteToPredsDict[currentBTEPred] == nil {
      guard iterateOverBranchTracingEnumPreds(
        bteToPredsDict: &bteToPredsDict, currentBTEType: currentBTEPred, in: vjp) else {
        return false
      }
    }
  }
  return true
}

// Get branch tracing enum type queue for further specialization. The types in the queue
// are ordered from most nested ones (which contain no predecessor branch tracing enums in their
// case payloads) to least nested ones (which are not stored as predecessor elements in other
// branch tracing enums).
//
// For the enums below, an example of the correct order might be:
// 1. _AD__$xxx_bbD__Pred__xxx
// 2. _AD__$xxx_bbC__Pred__xxx
// 3. _AD__$xxx_bbB__Pred__xxx
// 4. _AD__$xxx_bbA__Pred__xxx
//
//   enum _AD__$xxx_bbA__Pred__xxx {
//     case bbB((predecessor: _AD__$xxx_bbB__Pred__xxx, /* closure types */))
//     case bbC((predecessor: _AD__$xxx_bbC__Pred__xxx, /* closure types */))
//   }
//   enum _AD__$xxx_bbB__Pred__xxx {
//     case bbD((predecessor: _AD__$xxx_bbD__Pred__xxx, /* closure types */))
//   }
//   enum _AD__$xxx_bbC__Pred__xxx {
//     case bbD((predecessor: _AD__$xxx_bbD__Pred__xxx, /* closure types */))
//   }
//   enum _AD__$xxx_bbD__Pred__xxx {
//     case bbE((/* closure types */))
//   }
func getBranchTracingEnumSpecializationQueue(topBTEType: Type, in vjp: Function) -> [Type]? {
  var bteToPredsDict = [Type: Set<Type>]()
  guard iterateOverBranchTracingEnumPreds(
    bteToPredsDict: &bteToPredsDict,
    currentBTEType: topBTEType,
    in: vjp) else { return nil }
  var bteSpecializationQueue = [Type]()
  let bteCount = bteToPredsDict.count

  for _ in 0..<bteCount {
    let previousCount = bteSpecializationQueue.count
    for (bteType, btePreds) in bteToPredsDict {
      guard btePreds.isEmpty else {
        continue
      }
      bteSpecializationQueue.append(bteType)
      break
    }
    guard bteSpecializationQueue.count > previousCount else { return nil }
    bteToPredsDict.removeValue(forKey: bteSpecializationQueue.last!)
    for bteType in bteToPredsDict.keys {
      bteToPredsDict[bteType]!.remove(bteSpecializationQueue.last!)
    }
  }
  assert(bteSpecializationQueue.count == bteCount)

  return bteSpecializationQueue
}

// The queried type can originate in the pullback's generic environment. Map
// its AST type directly: lowering an intermediate interface type would lose the
// source instantiation or try to lower parameters outside any environment.
func remapBranchTracingEnumType(_ type: Type, into function: Function) -> Type {
  let interfaceType = type.rawType.mapOutOfEnvironment().getReducedType(
    of: function.loweredFunctionType.substitutedGenericSignatureOfFunctionType.genericSignature)
  return function.mapTypeIntoEnvironment(interfaceType.rawType).canonical
    .loweredTypeWithAbstractionPattern(in: function)
}

private func getBranchTracingEnumLoweredType(ed: EnumDecl, vjp: Function) -> Type {
  vjp.mapTypeIntoEnvironment(ed.declaredInterfaceType).canonical.loweredTypeWithAbstractionPattern(in: vjp)
}

private func getSourceFileFor(derivative: Function) -> SourceFile {
  // Imported VJPs have no source file of their own. Prefer the current primary
  // file so that synthesized tape metadata is emitted in its IRGen partition.
  if let sourceFile = SourceFile(bridged: derivative.bridged.getAssociatedSourceFile()) {
    return sourceFile
  }
  if let sourceFile = derivative.sourceFile {
    return sourceFile
  }
  return derivative.bridged.getFilesForModule().withElements(ofType: FileUnit.self) {
    for fileUnit in $0 {
      if let sourceFile = fileUnit.asSourceFile {
        return sourceFile
      }
    }
    assert(false)
    return nil
  }!
}

private func cloneGenericParameters(
  canonicalGenericSig: CanonicalGenericSignature, astContext: ASTContext, declContext: DeclContext
) -> GenericParameterList {
  let params = canonicalGenericSig.genericSignature.genericParameters.map {
    assert($0.isGenericTypeParameter)
    return GenericTypeParamDecl.create(
      declContext: declContext,
      name: $0.nameOfGenericTypeParameter,
      depth: $0.depthOfGenericTypeParameter,
      index: $0.indexOfGenericTypeParameter,
      paramKind: $0.kindOfGenericTypeParameter)
  }

  return GenericParameterList.create(
    leftAngleLoc: nil, parameters: params,
    genericWhereClause: nil,
    rightAngleLoc: nil, astContext)
}

// Create specialized payload tuple type for a given enum case. Specialization implies replacing
// closures with tuples of arguments captured by these closures. Note that the last closure might be
// wrapped in an optional (this is used for supporting derivatives of throwing functions).
// Consider the enum case below:
//   case bbB((predecessor: _AD__$xxx_bbB__Pred__xxx, (Float) -> Float, Optional<() -> Float>))
//
// Assuming that the closure at index 1 is capturing one Float argument and the closure at index 2 is
// capturing two Float arguments, the specialized case would look like this:
//   case bbB((predecessor: _AD__$xxx_bbB__Pred__xxx_spec, (Float), Optional<(Float, Float)>))
private func getSpecializedParamDeclForEnumCase(
  enumCase: EnumCase,
  closuresInBTE: [ClosureInBTE],
  newEDName: inout String,
  newPayloadTypes: inout [AST.`Type`],
  specializedBTEDict: [Type: Type],
  topVJP: Function,
  context: FunctionPassContext
) -> ParamDecl {
  let oldPayloadTupleElementTypes = enumCase.payload!.tupleElements
  let closuresInBTEForCase = closuresInBTE.filter{ $0.enumCase.index == enumCase.index }

  var nameSuffix: String = ""
  var newPayloadTupleElementTypes = [(label: Identifier, type: AST.`Type`)]()

  for (elementIndex, oldElementType) in oldPayloadTupleElementTypes.enumerated() {
    var newElementType: AST.`Type`
    let closuresInBTEForCaseAndPayloadIndex = closuresInBTEForCase.filter({ $0.indexInPayload == elementIndex })

    if let first = closuresInBTEForCaseAndPayloadIndex.first {
      // Duplicated constructors must save the same ordered SSA values.
      // Analysis rejects differing plans for the same bound case.
      let captureTypes = first.capturedArgs.map { $0.type }
      for info in closuresInBTEForCaseAndPayloadIndex.dropFirst() {
        assert(first.payloadTuple.hasSamePayloadValues(as: info.payloadTuple))
        assert(captureTypes == info.capturedArgs.map { $0.type })
      }
    }
    if let closureInBTE = closuresInBTEForCaseAndPayloadIndex.first {
      nameSuffix += "_\(elementIndex)"
      newElementType = getCapturedArgTypesTupleForClosure(
        closure: closureInBTE.closure, context: context)
      if oldElementType.isOptional {
        assert(elementIndex + 1 == oldPayloadTupleElementTypes.count)
        newElementType = newElementType.optionalType
      }
    } else {
      newElementType = oldElementType.rawType
      if elementIndex == 0 && oldElementType.isBranchTracingEnum(in: topVJP) {
        newElementType = specializedBTEDict[oldElementType]!.rawType
      }
    }
    newPayloadTupleElementTypes.append((label: oldPayloadTupleElementTypes.label(at: elementIndex), type: newElementType))
  }

  let enumElementDecl = enumCase.enumElementDecl

  if !nameSuffix.isEmpty {
    newEDName += "_\(enumElementDecl.name)\(nameSuffix)"
  }

  let newParamDecl = enumElementDecl.parameterList.singleElement!.cloneWithoutType()
  let newPayloadType = context.getTupleType(elements: newPayloadTupleElementTypes).mapOutOfEnvironment()
  newPayloadTypes.append(newPayloadType)
  newParamDecl.setInterfaceType(type: newPayloadType)

  return newParamDecl
}

// Create a specialized EnumDecl for a given branch tracing enum. See getSpecializedParamDeclForEnumCase
// for more info on how each enum case payload is specialized.
// The specialized enum name includes the changed payload positions followed by
// a digest of the complete canonical schema and emitting file. For example:
//   _AD__$xxx_bbA__Pred__xxx_spec_bbB_1_bbC_0_1_<schema digest>
// This means that for enum case bbB closure at index 1 in payload tuple was replaced with
// its captured arguments tuple, and for enum case bbC closures at indexes 0 and 1 were
// replaced the same way.
private func autodiffSpecializeBranchTracingEnum(
  bteType: Type, topVJP: Function,
  closuresInBTE: [ClosureInBTE],
  specializedBTEDict: [Type: Type],
  context: FunctionPassContext
) -> Type {
  assert(specializedBTEDict[bteType] == nil)
  closuresInBTE.forEach { assert($0.enumCase.enumType(in: topVJP) == bteType) }

  let oldED = bteType.nominal as! EnumDecl
  // A deserialized tape belongs to the imported module. Its specialized
  // declaration must instead be owned by the file that registers and emits it.
  let sourceFile = getSourceFileFor(derivative: topVJP)
  let declContext = UnknownDeclContext(bridged: sourceFile.getOrCreateSynthesizedFileContext())
  let astContext = declContext.astContext

  var newEDName: String = oldED.name.string + "_spec"

  var newPayloadTypes = [AST.`Type`]()
  let newPLs = bteType.getEnumCases(in: topVJP)!.map{
    ParameterList.create(
      leftParenLoc: nil,
      parameters: [
        getSpecializedParamDeclForEnumCase(
          enumCase: $0,
          closuresInBTE: closuresInBTE,
          newEDName: &newEDName,
          newPayloadTypes: &newPayloadTypes,
          specializedBTEDict: specializedBTEDict,
          topVJP: topVJP,
          context: context
        )
      ],
      rightParenLoc: nil, astContext
    )
  }

  let canonicalGenericSig = topVJP.genericSignature.canonicalSignature
  // Include the original instantiation even when its generic arguments do not
  // occur in a payload. The complete ordered payload schema includes preserved
  // closures, optional wrappers, and already-specialized predecessor enums.
  let schema = context.getTupleType(elements:
    [bteType.rawType.mapOutOfEnvironment()] + newPayloadTypes)
  newEDName = newEDName._withBridgedStringRef {
    String(taking: context.bridgedPassContext.mangleAutoDiffBranchTracingEnum(
      $0, schema.bridged, topVJP.bridged, sourceFile))
  }
  // Generic specialization can produce VJP variants with the same tape layout.
  // Share their declaration to avoid duplicating emitted enum metadata.
  if let existing = newEDName._withBridgedStringRef({
    context.bridgedPassContext.lookupAutoDiffSpecializedBranchTracingEnum($0, topVJP.bridged).getAs(EnumDecl.self)
  }) {
    let existingType = getBranchTracingEnumLoweredType(ed: existing, vjp: topVJP)
    assert(existingType.getEnumCases(in: topVJP)!.map {
      $0.payload!.rawType.mapOutOfEnvironment().canonical
    } == newPayloadTypes.map { $0.canonical })
    return existingType
  }

  let genericParams =
    if canonicalGenericSig.isEmpty { GenericParameterList?(nil) }
    else { cloneGenericParameters(canonicalGenericSig: canonicalGenericSig, astContext: astContext, declContext: declContext) }

  let newED = EnumDecl.create(
    declContext: declContext,
    enumKeywordLoc: nil,
    name: newEDName,
    nameLoc: nil,
    genericParamList: genericParams,
    inheritedTypes: [],
    genericWhereClause: nil,
    braceRange: SourceRange(start: nil),
    astContext)

  newED.setImplicit()
  if !canonicalGenericSig.isEmpty {
    newED.setGenericSignature(canonicalGenericSig.genericSignature)
  }

  for (idx, enumCase) in bteType.getEnumCases(in: topVJP)!.enumerated() {
    let newEED = EnumElementDecl.create(
      declContext: newED,
      name: enumCase.enumElementDecl.baseIdentifier, nameLoc: nil,
      parameterList: newPLs[idx],
      equalsLoc: nil, rawValue: nil, astContext)
    newEED.setImplicit()
    newED.add(member: newEED)
    // AST conformance inference visits case declarations, while SIL lowering
    // visits elements. Keep both views of the enum's storage consistent.
    let newCase = EnumCaseDecl.create(
      declContext: newED, caseKeywordLoc: nil, elements: [newEED])
    newCase.setImplicit()
    newED.add(member: newCase)
  }

  // Like the original branch-tracing enums, specialized tapes must never be
  // resilient, even in a pullback with minimal resilience expansion.
  newED.setAccess(swift.AccessLevel.public)
  newED.setFrozen()
  sourceFile.addTopLevelDecl(newED.bridgedDecl)

  return getBranchTracingEnumLoweredType(ed: newED, vjp: topVJP)
}

// Specialize all branch tracing enums which store control-flow graph info in topVJP.
func autodiffSpecializeBranchTracingEnums(
  topVJP: Function, bteSpecializationQueue: [Type], closuresInBTE: [ClosureInBTE],
  context: FunctionPassContext
) -> [Type: Type]? {
  var closuresInBTEByBTE = [Type: [ClosureInBTE]]()
  for closureInBTE in closuresInBTE {
    let enumType = closureInBTE.enumCase.enumType(in: topVJP)
    closuresInBTEByBTE[enumType] = (closuresInBTEByBTE[enumType] ?? []) + [closureInBTE]
  }

  var specializedBTEDict = [Type: Type]()
  for bteType in bteSpecializationQueue {
    specializedBTEDict[bteType] = autodiffSpecializeBranchTracingEnum(
      bteType: bteType, topVJP: topVJP,
      closuresInBTE: closuresInBTEByBTE[bteType] ?? [],
      specializedBTEDict: specializedBTEDict, context: context)
  }

  return specializedBTEDict
}

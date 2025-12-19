import AST
import SIL

extension Type {
  func isBranchTracingEnum(in vjp: Function) -> Bool {
    return self.bridged.isAutodiffBranchTracingEnumInVJP(vjp.bridged)
  }
}

extension EnumCase {
  func enumType(in function: Function) -> Type {
    remapType(type: self.enumElementDecl.parentEnum.declaredInterfaceType.loweredTypeWithAbstractionPattern(in: function), function: function)
  }
}

// Information required to specialize one closure stored in a payload tuple of a branch tracing enum case.
struct ClosureInBTE : Equatable {
  let closure: SingleValueInstruction
  let subsetThunk: PartialApplyInst?
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
private func getBranchTracingEnumPreds(bteType: Type, in vjp: Function) -> Set<Type> {
  guard let enumCases = bteType.getEnumCases(in: vjp) else {
    return []
  }

  var btePreds = Set<Type>()
  for enumCase in enumCases {
    guard let firstTupleElementType = enumCase.payload!.tupleElements.first else {
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
) {
  let currentBTEPreds = getBranchTracingEnumPreds(bteType: currentBTEType, in: vjp)
  bteToPredsDict[currentBTEType] = currentBTEPreds
  for currentBTEPred in currentBTEPreds {
    if bteToPredsDict[currentBTEPred] == nil {
      iterateOverBranchTracingEnumPreds(
        bteToPredsDict: &bteToPredsDict, currentBTEType: currentBTEPred, in: vjp)
    }
  }
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
private func getBranchTracingEnumSpecializationQueue(topBTEType: Type, in vjp: Function) -> [Type] {
  var bteToPredsDict = [Type: Set<Type>]()
  iterateOverBranchTracingEnumPreds(
    bteToPredsDict: &bteToPredsDict,
    currentBTEType: topBTEType,
    in: vjp)
  var bteSpecializationQueue = [Type]()
  let bteCount = bteToPredsDict.count

  for _ in 0..<bteCount {
    for (bteType, btePreds) in bteToPredsDict {
      guard btePreds.isEmpty else {
        continue
      }
      bteSpecializationQueue.append(bteType)
      break
    }
    bteToPredsDict.removeValue(forKey: bteSpecializationQueue.last!)
    for bteType in bteToPredsDict.keys {
      bteToPredsDict[bteType]!.remove(bteSpecializationQueue.last!)
    }
  }
  assert(bteSpecializationQueue.count == bteCount)

  return bteSpecializationQueue
}

// NOTE: this is adopted from lib/SILOptimizer/Differentiation/PullbackCloner.cpp.
// Remap any archetypes into the current function's context.
private func remapType(type: Type, function: Function) -> Type {
  let silType =
    if type.rawType.hasArchetype { type.mapOutOfEnvironment(in: function) }
    else { type }
  let remappedCanType = silType.rawType.getReducedType(
    of: function.loweredFunctionType.substitutedGenericSignatureOfFunctionType.genericSignature)
  let remappedSILType = remappedCanType.loweredTypeWithAbstractionPattern(in: function)
  if !function.genericSignature.isEmpty {
    return function.mapTypeIntoEnvironment(remappedSILType)
  }
  return remappedSILType
}

private func getBranchTracingEnumLoweredType(ed: EnumDecl, vjp: Function) -> Type {
  ed.declaredInterfaceType.canonical.loweredTypeWithAbstractionPattern(in: vjp)
}

private func getSourceFileFor(derivative: Function) -> SourceFile {
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
    assert(closuresInBTEForCaseAndPayloadIndex.count <= 1)
    if let closureInBTE = closuresInBTEForCaseAndPayloadIndex.singleElement {
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
        let predED = newElementType.nominal as! EnumDecl
        let predBTEType = remapType(
          type: getBranchTracingEnumLoweredType(ed: predED, vjp: topVJP),
          function: topVJP)
        newElementType = specializedBTEDict[predBTEType]!.rawType
      }
    }
    newPayloadTupleElementTypes.append((label: oldPayloadTupleElementTypes.label(at: elementIndex), type: newElementType))
  }

  let enumElementDecl = enumCase.enumElementDecl

  if !nameSuffix.isEmpty {
    newEDName += "_\(enumElementDecl.name)\(nameSuffix)"
  }

  let newParamDecl = enumElementDecl.parameterList.singleElement!.cloneWithoutType()
  newParamDecl.setInterfaceType(type:
    context.getTupleType(elements: newPayloadTupleElementTypes)
    .mapOutOfEnvironment())

  return newParamDecl
}

// Create a specialized EnumDecl for a given branch tracing enum. See getSpecializedParamDeclForEnumCase
// for more info on how each enum case payload is specialized.
// The specialized enum type contains a suffix with info on what was specialized. Consider:
//   _AD__$xxx_bbA__Pred__xxx_spec_bbB_1_bbC_0_1
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
  let declContext = oldED.parentDeclContext!
  let astContext = declContext.astContext

  var newEDName: String = oldED.name.string + "_spec"

  let newPLs = bteType.getEnumCases(in: topVJP)!.map{
    ParameterList.create(
      leftParenLoc: nil,
      parameters: [
        getSpecializedParamDeclForEnumCase(
          enumCase: $0,
          closuresInBTE: closuresInBTE,
          newEDName: &newEDName,
          specializedBTEDict: specializedBTEDict,
          topVJP: topVJP,
          context: context
        )
      ],
      rightParenLoc: nil, astContext
    )
  }

  let canonicalGenericSig = topVJP.genericSignature.canonicalSignature
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
  }

  // TODO: we should probably copy access level from old EnumDecl, but
  // for some reason it results in crashes for many existing tests
  newED.setAccess(swift.AccessLevel.public)
  getSourceFileFor(derivative: topVJP).addTopLevelDecl(newED.bridgedDecl)

  let newBTEType = remapType(
    type: getBranchTracingEnumLoweredType(ed: newED, vjp: topVJP),
    function: topVJP)

  return newBTEType
}

// Specialize all branch tracing enums which store control-flow graph info in topVJP.
func autodiffSpecializeBranchTracingEnums(
  topVJP: Function, topBTE: Type, closuresInBTE: [ClosureInBTE],
  context: FunctionPassContext
) -> [Type: Type] {
  let bteSpecializationQueue: [Type] = getBranchTracingEnumSpecializationQueue(topBTEType: topBTE, in: topVJP)

  var closuresInBTEByBTE = [Type: [ClosureInBTE]]()
  for closureInBTE in closuresInBTE {
    let enumType = closureInBTE.enumCase.enumType(in: topVJP)
    closuresInBTEByBTE[enumType] = (closuresInBTEByBTE[enumType] ?? []) + [closureInBTE]
  }

  var specializedBTEDict = [Type: Type]()
  for bteType in bteSpecializationQueue {
    let ed = bteType.nominal as! EnumDecl
    let remappedBTEType = remapType(
      type: getBranchTracingEnumLoweredType(ed: ed, vjp: topVJP), function: topVJP)

    specializedBTEDict[remappedBTEType] = autodiffSpecializeBranchTracingEnum(
      bteType: remappedBTEType, topVJP: topVJP,
      closuresInBTE: closuresInBTEByBTE[bteType] ?? [],
      specializedBTEDict: specializedBTEDict, context: context)
  }

  return specializedBTEDict
}

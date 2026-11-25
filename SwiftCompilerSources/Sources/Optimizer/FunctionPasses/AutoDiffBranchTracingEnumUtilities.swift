import AST
import ASTBridging
import SIL

extension Type {
  func isBranchTracingEnum(in vjp: Function) -> Bool {
    return self.bridged.isAutodiffBranchTracingEnumInVJP(vjp.bridged)
  }
}

extension EnumCase {
  func enumType(in function: Function) -> Type {
    remapBranchTracingEnumType(self.enumType, into: function)
  }
}

// Information required to specialize one closure stored in a payload tuple of a branch tracing enum case.
struct ClosureInBTE : Equatable {
  let closure: SingleValueInstruction
  // Ordered from the root closure to the stored closure.
  let adapters: [SingleValueInstruction]
  let indirectCaptureStores: [Int: StoreInst]
  let optionalWrapper: EnumInst?
  let useInPayload: Operand
  let enumCase: EnumCase

  var payloadTuple: TupleInst { useInPayload.instruction as! TupleInst }
  var indexInPayload: Int { useInPayload.index }

  func hasSameReconstruction(as other: ClosureInBTE) -> Bool {
    closure == other.closure && adapters == other.adapters
      && optionalWrapper?.caseIndex == other.optionalWrapper?.caseIndex
  }
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

// Get branch tracing enum type queue for further specialization. The types in the queue
// are ordered from most nested ones (which contain no predecessor branch tracing enums in their
// case payloads) to least nested ones (which are not stored as predecessor elements in other
// branch tracing enums). Visit sibling predecessors in enum case order to keep declaration
// order deterministic.
// Note that the "predecessor" element in the payload tuple is optional and might be not present.
//
// For the enums below, the order is:
// 1. _AD__$xxx_bbD__Pred__xxx
// 2. _AD__$xxx_bbB__Pred__xxx
// 3. _AD__$xxx_bbC__Pred__xxx
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
  var active = Set<Type>()
  var finished = Set<Type>()
  var order = [Type]()

  func visit(_ type: Type) -> Bool {
    if finished.contains(type) { return true }
    guard active.insert(type).inserted, let cases = type.getEnumCases(in: vjp) else { return false }

    for enumCase in cases {
      guard let payload = enumCase.payload, payload.isTuple else { return false }
      if let predecessor = payload.tupleElements.first, predecessor.isBranchTracingEnum(in: vjp) {
        guard visit(predecessor) else { return false }
      }
    }
    active.remove(type)
    finished.insert(type)
    order.append(type)
    return true
  }

  return visit(topBTEType) ? order : nil
}

// NOTE: this is adopted from lib/SILOptimizer/Differentiation/PullbackCloner.cpp.
// Remap any archetypes into the current function's context.
// Lower only after mapping into the destination's generic environment.
func remapBranchTracingEnumType(_ type: Type, into function: Function) -> Type {
  let interfaceType = type.rawType.mapOutOfEnvironment().getReducedType(
    of: function.loweredFunctionType.substitutedGenericSignatureOfFunctionType.genericSignature)
  return function.mapTypeIntoEnvironment(interfaceType.rawType).canonical
    .loweredTypeWithAbstractionPattern(in: function)
}

private func getBranchTracingEnumLoweredType(ed: EnumDecl, vjp: Function) -> Type {
  vjp.mapTypeIntoEnvironment(ed.declaredInterfaceType).canonical.loweredTypeWithAbstractionPattern(in: vjp)
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
private func specializedCasePayload(
  _ enumCase: EnumCase, closures: [ClosureInBTE], specializedBTEDict: [Type: Type], context: FunctionPassContext
) -> (type: AST.`Type`, nameSuffix: String) {
  let fields = enumCase.payload!.tupleElements
  let closuresByField = Dictionary(grouping: closures, by: \.indexInPayload)
  var changedFields = ""
  let elements = fields.enumerated().map { index, oldType -> (label: Identifier, type: AST.`Type`) in
    let type: AST.`Type`
    if let matchingClosures = closuresByField[index], let closureInfo = matchingClosures.first {
      assert(matchingClosures.allSatisfy { $0.hasSameReconstruction(as: closureInfo) })
      changedFields += "_\(index)"
      let captures = getCapturedArgTypesTupleForClosure(closure: closureInfo.closure, context: context)
      if oldType.isOptional {
        assert(index + 1 == fields.count)
        type = captures.optionalType
      } else {
        type = captures
      }
    } else {
      type = index == 0 ? specializedBTEDict[oldType]?.rawType ?? oldType.rawType : oldType.rawType
    }
    return (fields.label(at: index), type)
  }
  let suffix = changedFields.isEmpty ? "" : "_\(enumCase.name)\(changedFields)"
  return (context.getTupleType(elements: elements).mapOutOfEnvironment(), suffix)
}

// Create a specialized EnumDecl for a given branch tracing enum. See specializedCasePayload
// for more info on how each enum case payload is specialized.
// The specialized enum type contains a suffix with info on what was specialized
// and a digest of its schema and emitting file. Consider:
//   _AD__$xxx_bbA__Pred__xxx_spec_bbB_1_bbC_0_1_<schema digest>
// This means that for enum case bbB closure at index 1 in payload tuple was replaced with
// its captured arguments tuple, and for enum case bbC closures at indexes 0 and 1 were
// replaced the same way.
private func autodiffSpecializeBranchTracingEnum(
  bteType: Type, topVJP: Function,
  closuresInBTE: [ClosureInBTE],
  specializedBTEDict: [Type: Type],
  context: FunctionPassContext
) -> Type? {
  assert(specializedBTEDict[bteType] == nil)
  closuresInBTE.forEach { assert($0.enumCase.enumType(in: topVJP) == bteType) }

  let cases = bteType.getEnumCases(in: topVJP)!
  guard !closuresInBTE.isEmpty || cases.contains(where: { enumCase in
    guard let first = enumCase.payload!.tupleElements.first else { return false }
    return specializedBTEDict[first] != nil
  }) else { return nil }

  let oldED = bteType.nominal as! EnumDecl
  // An imported enum's specialization belongs to the current module.
  let emittingFile = context.bridgedPassContext.getSynthesizedSourceFile(topVJP.bridged)
  let closuresByCase = Dictionary(grouping: closuresInBTE) { $0.enumCase.index }
  let layouts = cases.map { enumCase in
    specializedCasePayload(enumCase, closures: closuresByCase[enumCase.index] ?? [],
                           specializedBTEDict: specializedBTEDict, context: context)
  }
  let newPayloadTypes = layouts.map(\.type)
  let prefix = oldED.name.string + "_spec" + layouts.map(\.nameSuffix).joined()

  let canonicalGenericSig = topVJP.genericSignature.canonicalSignature
  // Include the original instantiation even when its generic arguments do not
  // occur in a payload.
  let schema = context.getTupleType(elements:
    [bteType.rawType.mapOutOfEnvironment()] + newPayloadTypes)
  let newEDName = prefix._withBridgedStringRef {
    String(taking: context.bridgedPassContext.mangleAutoDiffBranchTracingEnum(
      $0, schema.bridged, topVJP.bridged, emittingFile.sourceFile))
  }
  if let existingDecl = newEDName._withBridgedStringRef({
    context.bridgedPassContext.lookupUniqueModuleValue($0).decl
  }) {
    let existing = existingDecl as! EnumDecl
    let existingType = getBranchTracingEnumLoweredType(ed: existing, vjp: topVJP)
    assert(existingType.getEnumCases(in: topVJP)!.map {
      $0.payload!.rawType.mapOutOfEnvironment().canonical
    } == newPayloadTypes.map { $0.canonical })
    return existingType
  }

  let declContext = UnknownDeclContext(bridged: emittingFile.declContext)
  let astContext = declContext.astContext
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

  for (enumCase, layout) in zip(cases, layouts) {
    let parameter = enumCase.enumElementDecl.parameterList.singleElement!.cloneWithoutType()
    parameter.setInterfaceType(type: layout.type)
    let parameters = ParameterList.create(leftParenLoc: nil, parameters: [parameter], rightParenLoc: nil, astContext)
    let element = BridgedEnumElementDecl.createParsed(
      astContext, declContext: newED.bridgedDeclContext,
      name: enumCase.enumElementDecl.baseIdentifier, nameLoc: .init(),
      parameterList: .init(raw: parameters.bridged.raw), equalsLoc: .init(), rawValue: .init(raw: nil))
    // Conformance inference visits case declarations, while SIL lowering visits elements.
    let caseDecl = [element].withBridgedArrayRef {
      BridgedEnumCaseDecl.createParsed(declContext: newED.bridgedDeclContext, caseKeywordLoc: .init(), elements: $0)
    }
    for bridgedDecl in [element.asDecl, caseDecl.asDecl] {
      let decl = bridgedDecl.declObj.decl
      decl.setImplicit()
      newED.add(member: decl)
    }
  }

  // Only nonserialized derivative bodies use the specialized enum.
  newED.setAccess(swift.AccessLevel.internal)
  emittingFile.sourceFile.addTopLevelDecl(newED.bridgedDecl)

  return getBranchTracingEnumLoweredType(ed: newED, vjp: topVJP)
}

// Specialize the branch tracing enums which store control-flow graph info in topVJP.
func autodiffSpecializeBranchTracingEnums(
  topVJP: Function, bteSpecializationQueue: [Type], closuresInBTE: [ClosureInBTE],
  context: FunctionPassContext
) -> [Type: Type] {
  let closuresInBTEByBTE = Dictionary(grouping: closuresInBTE) {
    $0.enumCase.enumType(in: topVJP)
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

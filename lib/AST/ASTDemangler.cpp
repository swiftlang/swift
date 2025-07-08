//===--- ASTDemangler.cpp ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines a builder concept for the TypeDecoder and MetadataReader which builds
// AST Types, and a utility function wrapper which takes a mangled string and
// feeds it through the TypeDecoder instance.
//
// The RemoteAST library defines a MetadataReader instance that uses this
// concept, together with some additional utilities.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTDemangler.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclNameExtractor.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/USRGeneration.h"
#include "swift/AST/SILLayout.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/ManglingMacros.h"
#include "llvm/ADT/StringSwitch.h"
#include "clang/Tooling/Refactor/USRFinder.h"

using namespace swift;

Decl *swift::Demangle::getDeclForUSR(ASTContext &ctx, StringRef usr,
                                     const DeclContext *lookupDC,
                                     GenericSignature genericSig) {
  if (!usr.starts_with("s:"))
    return nullptr;

  std::string mangling(usr);
  mangling.replace(0, 2, MANGLING_PREFIX_STR);

  Demangle::Context Dem;
  auto node = Dem.demangleSymbolAsNode(mangling);

  ASTBuilder builder(ctx, genericSig);
  return builder.findDecl(node, usr, lookupDC);
}

Type swift::Demangle::getTypeForMangling(ASTContext &ctx,
                                         StringRef mangling,
                                         GenericSignature genericSig) {
  Demangle::Context Dem;
  auto node = Dem.demangleSymbolAsNode(mangling);
  if (!node)
    return Type();

  ASTBuilder builder(ctx, genericSig);
  return builder.decodeMangledType(node);
}

TypeDecl *swift::Demangle::getTypeDeclForMangling(ASTContext &ctx,
                                                  StringRef mangling,
                                                  GenericSignature genericSig) {
  Demangle::Context Dem;
  auto node = Dem.demangleSymbolAsNode(mangling);
  if (!node)
    return nullptr;

  ASTBuilder builder(ctx, genericSig);
  return builder.createTypeDecl(node);
}

TypeDecl *swift::Demangle::getTypeDeclForUSR(ASTContext &ctx,
                                             StringRef usr,
                                             GenericSignature genericSig) {
  if (!usr.starts_with("s:"))
    return nullptr;

  std::string mangling(usr);
  mangling.replace(0, 2, MANGLING_PREFIX_STR);

  return getTypeDeclForMangling(ctx, mangling, genericSig);
}

using ValueDeclPredicate = llvm::function_ref<bool(const ValueDecl *)>;

static Decl *
findTopLevelClangDecl(ClangModuleLoader *importer, DeclName name,
                      ValueDeclPredicate predicate) {
  struct Consumer : VisibleDeclConsumer {
    ValueDecl *Result = nullptr;
    ValueDeclPredicate Predicate;
    
    explicit Consumer(ValueDeclPredicate Predicate) : Predicate(Predicate) {}
    
    void foundDecl(ValueDecl *decl, DeclVisibilityKind reason,
                   DynamicLookupInfo dynamicLookupInfo = {}) override {
      if (Result != nullptr)
        return;
      
      if (Predicate(decl))
        Result = decl;
    }
  } consumer(predicate);
  
  importer->lookupValue(name, consumer);
  
  return consumer.Result;
}

Decl *ASTBuilder::findDecl(NodePointer node, StringRef usr,
                           const DeclContext *lookupDC) {
  if (node == nullptr)
    return nullptr;

  if (auto *TD = createTypeDecl(node))
    return TD;

  switch (node->getKind()) {
  default:
    // We should have probably arrived at a probable declaration node by now
    break;
  case Node::Kind::Global:
  case Node::Kind::Static:
  case Node::Kind::BoundGenericEnum:
  case Node::Kind::BoundGenericClass:
  case Node::Kind::BoundGenericFunction:
  case Node::Kind::BoundGenericProtocol:
  case Node::Kind::BoundGenericStructure:
  case Node::Kind::BoundGenericTypeAlias:
  case Node::Kind::BoundGenericOtherNominalType:
  case Node::Kind::Extension:
    return findDecl(node->getFirstChild(), usr, lookupDC);
  }
  
  DeclNameExtractor NameExtractor(Ctx);
  auto name = NameExtractor.extractDeclName(node);
  
  auto contextNode = node->getFirstChild();
  if (!contextNode)
    return nullptr;
  
  auto hasMatchingUSR = [usr](const ValueDecl *VD) {
    SmallString<128> candidateUSR;
    llvm::raw_svector_ostream OS(candidateUSR);

    if (ide::printValueDeclSwiftUSR(VD, OS))
      return false;

    return usr == candidateUSR;
  };
  
  SmallVector<ValueDecl *, 4> candidates;
  if (contextNode->getKind() == Node::Kind::Module) {
    // If a foreign Clang module, perform lookup in Clang importer
    if (auto kind = getForeignModuleKind(contextNode)) {
      auto *importer = Ctx.getClangModuleLoader();
      return findTopLevelClangDecl(importer, name, hasMatchingUSR);
    }

    ModuleDecl *scratch;
    auto potentialModules = findPotentialModules(contextNode, scratch);

    for (auto *module : potentialModules) {
      auto *moduleScopeContext = module->getModuleScopeContext();
      namelookup::lookupInModule(module, name, candidates,
          NLKind::QualifiedLookup, namelookup::ResolutionKind::Overloadable,
          moduleScopeContext, SourceLoc(), NL_QualifiedDefault);
    }
  } else {
    auto *ownerDC = findDeclContext(contextNode);
    if (!ownerDC)
      return nullptr;

    if (auto *nominal = ownerDC->getSelfNominalTypeDecl()) {
      auto result = nominal->lookupDirect(name);
      candidates.append(result.begin(), result.end());
    } else {
      UnqualifiedLookupDescriptor desc(DeclNameRef(name), ownerDC);
      
      auto result = evaluateOrDefault(Ctx.evaluator,
                                      UnqualifiedLookupRequest(desc),
                                      LookupResult());
      
      for (const auto& entry : result) {
        if (auto *VD = entry.getValueDecl())
          candidates.push_back(VD);
      }
    }
  }
  
  for (auto *candidate : candidates) {
    if (hasMatchingUSR(candidate))
      return candidate;
  }

  return nullptr;
}

Type ASTBuilder::decodeMangledType(NodePointer node, bool forRequirement) {
  return swift::Demangle::decodeMangledType(*this, node, forRequirement)
      .getType();
}

TypeDecl *ASTBuilder::createTypeDecl(NodePointer node) {
  if (node->getKind() == Node::Kind::Global)
    return createTypeDecl(node->getChild(0));

  // Special case: associated types are not DeclContexts.
  if (node->getKind() == Node::Kind::AssociatedTypeRef) {
    if (node->getNumChildren() != 2)
      return nullptr;

    auto *DC = findDeclContext(node->getChild(0));
    auto *proto = dyn_cast_or_null<ProtocolDecl>(DC);
    if (proto == nullptr)
      return nullptr;

    auto name = getIdentifier(node->getChild(1)->getText());
    return proto->getAssociatedType(name);
  }

  auto *DC = findDeclContext(node);
  return dyn_cast_or_null<GenericTypeDecl>(DC);
}

Type
ASTBuilder::createBuiltinType(StringRef builtinName,
                              StringRef mangledName) {
  if (builtinName.starts_with(BUILTIN_TYPE_NAME_PREFIX)) {
    SmallVector<ValueDecl *, 1> decls;

    StringRef strippedName =
        builtinName.drop_front(BUILTIN_TYPE_NAME_PREFIX.size());
    Ctx.TheBuiltinModule->lookupValue(getIdentifier(strippedName),
                                      NLKind::QualifiedLookup, decls);

    if (decls.size() == 1 && isa<TypeDecl>(decls[0]))
      return cast<TypeDecl>(decls[0])->getDeclaredInterfaceType();
  }

  return Type();
}

GenericTypeDecl *ASTBuilder::createTypeDecl(StringRef mangledName,
                                            bool &typeAlias) {
  Demangle::Demangler Dem;
  Demangle::NodePointer node = Dem.demangleType(mangledName);
  if (!node) return nullptr;

  return createTypeDecl(node, typeAlias);
}

ProtocolDecl *
ASTBuilder::createProtocolDecl(NodePointer node) {
  bool typeAlias;
  return dyn_cast_or_null<ProtocolDecl>(
    createTypeDecl(node, typeAlias));
}

Type ASTBuilder::createNominalType(GenericTypeDecl *decl) {
  auto *nominalDecl = dyn_cast<NominalTypeDecl>(decl);
  if (!nominalDecl)
    return Type();

  // If the declaration is generic, fail.
  if (nominalDecl->isGenericContext())
    return Type();

  return nominalDecl->getDeclaredType();
}

Type ASTBuilder::createNominalType(GenericTypeDecl *decl, Type parent) {
  auto *nominalDecl = dyn_cast<NominalTypeDecl>(decl);
  if (!nominalDecl)
    return Type();

  // If the declaration is generic, fail.
  if (nominalDecl->isGeneric())
    return Type();

  // Imported types can be renamed to be members of other (non-generic)
  // types, but the mangling does not have a parent type. Just use the
  // declared type directly in this case and skip the parent check below.
  bool isImported = nominalDecl->hasClangNode() ||
      nominalDecl->getAttrs().hasAttribute<ClangImporterSynthesizedTypeAttr>();
  if (isImported && !nominalDecl->isGenericContext())
    return nominalDecl->getDeclaredInterfaceType();

  // Validate the parent type.
  if (!validateParentType(nominalDecl, parent))
    return Type();

  return NominalType::get(nominalDecl, parent, Ctx);
}

Type ASTBuilder::createTypeAliasType(GenericTypeDecl *decl, Type parent) {
  auto *aliasDecl = dyn_cast<TypeAliasDecl>(decl);
  if (!aliasDecl)
    return Type();

  // If the declaration is generic, fail.
  if (aliasDecl->getGenericParams())
    return Type();

  // Imported types can be renamed to be members of other (non-generic)
  // types, but the mangling does not have a parent type. Just use the
  // declared type directly in this case and skip the parent check below.
  bool isImported = aliasDecl->hasClangNode() ||
      aliasDecl->getAttrs().hasAttribute<ClangImporterSynthesizedTypeAttr>();
  if (isImported && !aliasDecl->isGenericContext())
    return aliasDecl->getDeclaredInterfaceType();

  // Validate the parent type.
  if (!validateParentType(aliasDecl, parent))
    return Type();

  auto declaredType = aliasDecl->getDeclaredInterfaceType();
  if (!parent)
    return declaredType;

  auto *dc = aliasDecl->getDeclContext();
  auto subs = parent->getContextSubstitutionMap(dc);

  return declaredType.subst(subs);
}

static SubstitutionMap
createSubstitutionMapFromGenericArgs(GenericSignature genericSig,
                                     ArrayRef<Type> args,
                                     LookupConformanceFn lookupConformance) {
  if (!genericSig)
    return SubstitutionMap();
  
  if (genericSig.getGenericParams().size() != args.size())
    return SubstitutionMap();

  return SubstitutionMap::get(
      genericSig,
      [&](SubstitutableType *t) -> Type {
        auto *gp = cast<GenericTypeParamType>(t);
        unsigned ordinal = genericSig->getGenericParamOrdinal(gp);
        if (ordinal < args.size())
          return args[ordinal];
        return Type();
      },
      lookupConformance);
}

Type ASTBuilder::createBoundGenericType(GenericTypeDecl *decl,
                                        ArrayRef<Type> args) {
  auto *nominalDecl = dyn_cast<NominalTypeDecl>(decl);
  if (!nominalDecl)
    return Type();

  // If the declaration isn't generic, fail.
  if (!nominalDecl->isGenericContext())
    return Type();

  // Build a SubstitutionMap.
  auto genericSig = nominalDecl->getGenericSignature();
  auto subs = createSubstitutionMapFromGenericArgs(
      genericSig, args, LookUpConformanceInModule());
  if (!subs)
    return Type();
  auto origType = nominalDecl->getDeclaredInterfaceType();

  // FIXME: We're not checking that the type satisfies the generic
  // requirements of the signature here.
  return origType.subst(subs);
}

Type ASTBuilder::resolveOpaqueType(NodePointer opaqueDescriptor,
                                   ArrayRef<ArrayRef<Type>> args,
                                   unsigned ordinal) {
  if (opaqueDescriptor->getKind() == Node::Kind::OpaqueReturnTypeOf) {
    auto definingDecl = opaqueDescriptor->getChild(0);
    auto definingGlobal = Factory.createNode(Node::Kind::Global);
    definingGlobal->addChild(definingDecl, Factory);
    auto mangling = mangleNode(definingGlobal, ManglingFlavor);
    if (!mangling.isSuccess())
      return Type();
    auto mangledName = mangling.result();

    auto moduleNode = findModuleNode(definingDecl);
    if (!moduleNode)
      return Type();
    
    ModuleDecl *scratch;
    auto potentialParentModules = findPotentialModules(moduleNode, scratch);
    if (potentialParentModules.empty())
      return Type();

    OpaqueTypeDecl *opaqueDecl = nullptr;
    for (auto module : potentialParentModules)
      if (auto decl = module->lookupOpaqueResultType(mangledName))
        opaqueDecl = decl;

    if (!opaqueDecl)
      return Type();
    SmallVector<Type, 8> allArgs;
    for (auto argSet : args) {
      allArgs.append(argSet.begin(), argSet.end());
    }

    SubstitutionMap subs = createSubstitutionMapFromGenericArgs(
        opaqueDecl->getGenericSignature(), allArgs,
        LookUpConformanceInModule());
    Type interfaceType = opaqueDecl->getOpaqueGenericParams()[ordinal];
    return OpaqueTypeArchetypeType::get(opaqueDecl, interfaceType, subs);
  }
  
  // TODO: named opaque types
  return Type();
}

Type ASTBuilder::createBoundGenericType(GenericTypeDecl *decl,
                                        ArrayRef<Type> args,
                                        Type parent) {
  // If the declaration isn't generic, fail.
  if (!decl->getGenericParams())
    return Type();

  // Validate the parent type.
  if (!validateParentType(decl, parent))
    return Type();

  if (auto *nominalDecl = dyn_cast<NominalTypeDecl>(decl))
    return BoundGenericType::get(nominalDecl, parent, args);

  auto *aliasDecl = cast<TypeAliasDecl>(decl);
  auto *dc = aliasDecl->getDeclContext();

  SmallVector<Type, 2> subs;

  // Combine the substitutions from our parent type with our generic
  // arguments.
  if (dc->isLocalContext()) {
    for (auto *param : dc->getGenericSignatureOfContext().getGenericParams()) {
      subs.push_back(param);
    }
  } else if (parent) {
    auto parentSubs = parent->getContextSubstitutionMap(
        dc).getReplacementTypes();
    subs.append(parentSubs.begin(), parentSubs.end());
  }

  auto genericSig = aliasDecl->getGenericSignature();
  ASSERT(genericSig.getInnermostGenericParams().size() == args.size());
  subs.append(args.begin(), args.end());

  auto subMap = SubstitutionMap::get(genericSig, subs,
                                     LookUpConformanceInModule());
  return aliasDecl->getDeclaredInterfaceType().subst(subMap);
}

Type ASTBuilder::createTupleType(ArrayRef<Type> eltTypes, ArrayRef<StringRef> labels) {
  // Unwrap unlabeled one-element tuples.
  //
  // FIXME: The behavior of one-element labeled tuples is inconsistent
  // throughout the different re-implementations of type substitution
  // and pack expansion.
  if (eltTypes.size() == 1 &&
      !eltTypes[0]->is<PackExpansionType>() &&
      labels[0].empty()) {
    return eltTypes[0];
  }

  SmallVector<TupleTypeElt, 4> elements;
  elements.reserve(eltTypes.size());
  for (unsigned i : indices(eltTypes)) {
    Identifier label;
    if (!labels[i].empty())
      label = getIdentifier(labels[i]);
    elements.emplace_back(eltTypes[i], label);
  }

  return TupleType::get(elements, Ctx);
}

Type ASTBuilder::createPackType(ArrayRef<Type> eltTypes) {
  return PackType::get(Ctx, eltTypes);
}

Type ASTBuilder::createSILPackType(ArrayRef<Type> eltTypes,
                                   bool isElementAddress) {
  auto extInfo = SILPackType::ExtInfo(isElementAddress);

  SmallVector<CanType, 4> elements;
  for (auto eltType : eltTypes)
    elements.push_back(eltType->getCanonicalType());

  return SILPackType::get(Ctx, extInfo, elements);
}

size_t ASTBuilder::beginPackExpansion(Type countType) {
  ActivePackExpansions.push_back(countType);

  return 1;
}

void ASTBuilder::advancePackExpansion(size_t index) {
  assert(index == 0);
}

Type ASTBuilder::createExpandedPackElement(Type patternType) {
  assert(!ActivePackExpansions.empty());
  auto countType = ActivePackExpansions.back();
  return PackExpansionType::get(patternType, countType);
}

void ASTBuilder::endPackExpansion() {
  ActivePackExpansions.pop_back();
}

Type ASTBuilder::createFunctionType(
    ArrayRef<Demangle::FunctionParam<Type>> params,
    Type output, FunctionTypeFlags flags, ExtendedFunctionTypeFlags extFlags,
    FunctionMetadataDifferentiabilityKind diffKind, Type globalActor,
    Type thrownError) {
  // The result type must be materializable.
  if (!output->isMaterializable()) return Type();

  bool hasIsolatedParameter = false;

  llvm::SmallVector<AnyFunctionType::Param, 8> funcParams;
  for (const auto &param : params) {
    auto type = param.getType();

    // All the argument types must be materializable.
    if (!type->isMaterializable())
      return Type();

    auto label = getIdentifier(param.getLabel());
    auto flags = param.getFlags();
    auto ownership =
      ParamDecl::getParameterSpecifierForValueOwnership(asValueOwnership(flags.getOwnership()));
    auto parameterFlags = ParameterTypeFlags()
                              .withOwnershipSpecifier(ownership)
                              .withVariadic(flags.isVariadic())
                              .withAutoClosure(flags.isAutoClosure())
                              .withNoDerivative(flags.isNoDerivative())
                              .withIsolated(flags.isIsolated())
                              .withSending(flags.isSending());

    hasIsolatedParameter |= flags.isIsolated();
    funcParams.push_back(AnyFunctionType::Param(type, label, parameterFlags));
  }

  FunctionTypeRepresentation representation;
  switch (flags.getConvention()) {
  case FunctionMetadataConvention::Swift:
    representation = FunctionTypeRepresentation::Swift;
    break;
  case FunctionMetadataConvention::Block:
    representation = FunctionTypeRepresentation::Block;
    break;
  case FunctionMetadataConvention::Thin:
    representation = FunctionTypeRepresentation::Thin;
    break;
  case FunctionMetadataConvention::CFunctionPointer:
    representation = FunctionTypeRepresentation::CFunctionPointer;
    break;
  }

  DifferentiabilityKind resultDiffKind;
  switch (diffKind.Value) {
  #define SIMPLE_CASE(CASE) \
      case FunctionMetadataDifferentiabilityKind::CASE: \
        resultDiffKind = DifferentiabilityKind::CASE; break;
  SIMPLE_CASE(NonDifferentiable)
  SIMPLE_CASE(Forward)
  SIMPLE_CASE(Reverse)
  SIMPLE_CASE(Normal)
  SIMPLE_CASE(Linear)
  #undef SIMPLE_CASE
  }

  FunctionTypeIsolation isolation = FunctionTypeIsolation::forNonIsolated();
  if (hasIsolatedParameter) {
    isolation = FunctionTypeIsolation::forParameter();
  } else if (globalActor) {
    isolation = FunctionTypeIsolation::forGlobalActor(globalActor);
  } else if (extFlags.isIsolatedAny()) {
    isolation = FunctionTypeIsolation::forErased();
  } else if (extFlags.isNonIsolatedCaller()) {
    isolation = FunctionTypeIsolation::forNonIsolatedCaller();
  }

  auto noescape =
    (representation == FunctionTypeRepresentation::Swift
     || representation == FunctionTypeRepresentation::Block)
    && !flags.isEscaping();

  const clang::Type *clangFunctionType = nullptr;
  if (shouldStoreClangType(representation))
    clangFunctionType = Ctx.getClangFunctionType(funcParams, output,
                                                 representation);

  // TODO: Handle LifetimeDependenceInfo here.
  auto einfo =
      FunctionType::ExtInfoBuilder(
          representation, noescape, flags.isThrowing(), thrownError,
          resultDiffKind, clangFunctionType, isolation,
          /*LifetimeDependenceInfo*/ std::nullopt, extFlags.hasSendingResult())
          .withAsync(flags.isAsync())
          .withSendable(flags.isSendable())
          .build();

  return FunctionType::get(funcParams, output, einfo);
}

static ParameterConvention
getParameterConvention(ImplParameterConvention conv) {
  switch (conv) {
  case Demangle::ImplParameterConvention::Indirect_In:
  case Demangle::ImplParameterConvention::Indirect_In_Constant:
    return ParameterConvention::Indirect_In;
  case Demangle::ImplParameterConvention::Indirect_In_Guaranteed:
    return ParameterConvention::Indirect_In_Guaranteed;
  case Demangle::ImplParameterConvention::Indirect_Inout:
    return ParameterConvention::Indirect_Inout;
  case Demangle::ImplParameterConvention::Indirect_InoutAliasable:
    return ParameterConvention::Indirect_InoutAliasable;
  case Demangle::ImplParameterConvention::Direct_Owned:
    return ParameterConvention::Direct_Owned;
  case Demangle::ImplParameterConvention::Direct_Unowned:
    return ParameterConvention::Direct_Unowned;
  case Demangle::ImplParameterConvention::Direct_Guaranteed:
    return ParameterConvention::Direct_Guaranteed;
  case Demangle::ImplParameterConvention::Pack_Owned:
    return ParameterConvention::Pack_Owned;
  case Demangle::ImplParameterConvention::Pack_Guaranteed:
    return ParameterConvention::Pack_Guaranteed;
  case Demangle::ImplParameterConvention::Pack_Inout:
    return ParameterConvention::Pack_Inout;
  }
  llvm_unreachable("covered switch");
}

static std::optional<SILParameterInfo::Options>
getParameterOptions(ImplParameterInfoOptions implOptions) {
  SILParameterInfo::Options result;

  if (implOptions.contains(ImplParameterInfoFlags::NotDifferentiable)) {
    implOptions -= ImplParameterInfoFlags::NotDifferentiable;
    result |= SILParameterInfo::NotDifferentiable;
  }

  if (implOptions.contains(ImplParameterInfoFlags::Sending)) {
    implOptions -= ImplParameterInfoFlags::Sending;
    result |= SILParameterInfo::Sending;
  }

  // If we did not handle all flags in implOptions, this code was not updated
  // appropriately. Return None to signal error.
  if (bool(implOptions))
    return {};

  return result;
}

static ResultConvention getResultConvention(ImplResultConvention conv) {
  switch (conv) {
  case Demangle::ImplResultConvention::Indirect:
    return ResultConvention::Indirect;
  case Demangle::ImplResultConvention::Owned:
    return ResultConvention::Owned;
  case Demangle::ImplResultConvention::Unowned:
    return ResultConvention::Unowned;
  case Demangle::ImplResultConvention::UnownedInnerPointer:
    return ResultConvention::UnownedInnerPointer;
  case Demangle::ImplResultConvention::Autoreleased:
    return ResultConvention::Autoreleased;
  case Demangle::ImplResultConvention::Pack:
    return ResultConvention::Pack;
  }
  llvm_unreachable("covered switch");
}

static std::optional<SILResultInfo::Options>
getResultOptions(ImplResultInfoOptions implOptions) {
  SILResultInfo::Options result;

  if (implOptions.contains(ImplResultInfoFlags::NotDifferentiable)) {
    implOptions -= ImplResultInfoFlags::NotDifferentiable;
    result |= SILResultInfo::NotDifferentiable;
  }

  if (implOptions.contains(ImplResultInfoFlags::IsSending)) {
    implOptions -= ImplResultInfoFlags::IsSending;
    result |= SILResultInfo::IsSending;
  }

  // If we did not remove all of the options from implOptions, someone forgot to
  // update this code for a new type of flag. Return none to signal error!
  if (bool(implOptions))
    return {};

  return result;
}

static SILCoroutineKind
getCoroutineKind(ImplCoroutineKind kind) {
  switch (kind) {
  case ImplCoroutineKind::None:
    return SILCoroutineKind::None;
  case ImplCoroutineKind::YieldOnce:
    return SILCoroutineKind::YieldOnce;
  case ImplCoroutineKind::YieldOnce2:
    return SILCoroutineKind::YieldOnce2;
  case ImplCoroutineKind::YieldMany:
    return SILCoroutineKind::YieldMany;
  }
  llvm_unreachable("unknown coroutine kind");
}

Type ASTBuilder::createImplFunctionType(
    Demangle::ImplParameterConvention calleeConvention,
    Demangle::ImplCoroutineKind coroutineKind,
    ArrayRef<Demangle::ImplFunctionParam<Type>> params,
    ArrayRef<Demangle::ImplFunctionYield<Type>> yields,
    ArrayRef<Demangle::ImplFunctionResult<Type>> results,
    std::optional<Demangle::ImplFunctionResult<Type>> errorResult,
    ImplFunctionTypeFlags flags) {
  GenericSignature genericSig;

  ParameterConvention funcCalleeConvention =
    getParameterConvention(calleeConvention);
  SILCoroutineKind funcCoroutineKind =
    getCoroutineKind(coroutineKind);

  SILFunctionTypeRepresentation representation;
  switch (flags.getRepresentation()) {
  case ImplFunctionRepresentation::Thick:
    representation = SILFunctionTypeRepresentation::Thick;
    break;
  case ImplFunctionRepresentation::Block:
    representation = SILFunctionTypeRepresentation::Block;
    break;
  case ImplFunctionRepresentation::Thin:
    representation = SILFunctionTypeRepresentation::Thin;
    break;
  case ImplFunctionRepresentation::CFunctionPointer:
    representation = SILFunctionTypeRepresentation::CFunctionPointer;
    break;
  case ImplFunctionRepresentation::Method:
    representation = SILFunctionTypeRepresentation::Method;
    break;
  case ImplFunctionRepresentation::ObjCMethod:
    representation = SILFunctionTypeRepresentation::ObjCMethod;
    break;
  case ImplFunctionRepresentation::WitnessMethod:
    representation = SILFunctionTypeRepresentation::WitnessMethod;
    break;
  case ImplFunctionRepresentation::Closure:
    representation = SILFunctionTypeRepresentation::Closure;
    break;
  }

  swift::DifferentiabilityKind diffKind;
  switch (flags.getDifferentiabilityKind()) {
  #define SIMPLE_CASE(CASE) \
      case ImplFunctionDifferentiabilityKind::CASE: \
        diffKind = swift::DifferentiabilityKind::CASE; break;
  SIMPLE_CASE(NonDifferentiable)
  SIMPLE_CASE(Forward)
  SIMPLE_CASE(Reverse)
  SIMPLE_CASE(Normal)
  SIMPLE_CASE(Linear)
  #undef SIMPLE_CASE
  }

  auto isolation = SILFunctionTypeIsolation::forUnknown();
  if (flags.hasErasedIsolation())
    isolation = SILFunctionTypeIsolation::forErased();

  // There's no representation of this in the mangling because it can't
  // occur in well-formed programs.
  bool unimplementable = false;

  llvm::SmallVector<SILParameterInfo, 8> funcParams;
  llvm::SmallVector<SILYieldInfo, 8> funcYields;
  llvm::SmallVector<SILResultInfo, 8> funcResults;
  std::optional<SILResultInfo> funcErrorResult;

  for (const auto &param : params) {
    auto type = param.getType()->getCanonicalType();
    auto conv = getParameterConvention(param.getConvention());
    auto options = *getParameterOptions(param.getOptions());
    funcParams.emplace_back(type, conv, options);
  }

  for (const auto &yield : yields) {
    auto type = yield.getType()->getCanonicalType();
    auto conv = getParameterConvention(yield.getConvention());
    auto options = *getParameterOptions(yield.getOptions());
    funcParams.emplace_back(type, conv, options);
  }

  for (const auto &result : results) {
    auto type = result.getType()->getCanonicalType();
    auto conv = getResultConvention(result.getConvention());
    auto options = *getResultOptions(result.getOptions());
    // We currently set sending result at the function level, but we set sending
    // result on each result.
    if (flags.hasSendingResult())
      options |= SILResultInfo::IsSending;
    funcResults.emplace_back(type, conv, options);
  }

  if (errorResult) {
    auto type = errorResult->getType()->getCanonicalType();
    auto conv = getResultConvention(errorResult->getConvention());
    funcErrorResult.emplace(type, conv);
  }

  const clang::Type *clangFnType = nullptr;
  if (shouldStoreClangType(representation)) {
    assert(funcResults.size() <= 1 && funcYields.size() == 0 &&
           "C functions and blocks have at most 1 result and 0 yields.");
    auto result =
        funcResults.empty() ? std::optional<SILResultInfo>() : funcResults[0];
    clangFnType = getASTContext().getCanonicalClangFunctionType(
        funcParams, result, representation);
  }
  auto einfo =
      SILFunctionType::ExtInfoBuilder(
          representation, flags.isPseudogeneric(), !flags.isEscaping(),
          flags.isSendable(), flags.isAsync(), unimplementable, isolation,
          diffKind, clangFnType, /*LifetimeDependenceInfo*/ std::nullopt)
          .build();

  return SILFunctionType::get(genericSig, einfo, funcCoroutineKind,
                              funcCalleeConvention, funcParams, funcYields,
                              funcResults, funcErrorResult,
                              SubstitutionMap(), SubstitutionMap(), Ctx);
}

Type ASTBuilder::createProtocolCompositionType(
    ArrayRef<ProtocolDecl *> protocols,
    Type superclass,
    bool isClassBound,
    bool forRequirement) {
  std::vector<Type> members;
  for (auto protocol : protocols)
    members.push_back(protocol->getDeclaredInterfaceType());
  if (superclass && superclass->getClassOrBoundGenericClass())
    members.push_back(superclass);

  // FIXME: move-only generics
  InvertibleProtocolSet inverses;
  Type composition = ProtocolCompositionType::get(Ctx, members, inverses,
                                                  isClassBound);
  if (forRequirement)
    return composition;

  return ExistentialType::get(composition);
}

Type ASTBuilder::createProtocolTypeFromDecl(ProtocolDecl *protocol) {
  return protocol->getDeclaredInterfaceType();
}

static MetatypeRepresentation
getMetatypeRepresentation(ImplMetatypeRepresentation repr) {
  switch (repr) {
  case Demangle::ImplMetatypeRepresentation::Thin:
    return MetatypeRepresentation::Thin;
  case Demangle::ImplMetatypeRepresentation::Thick:
    return MetatypeRepresentation::Thick;
  case Demangle::ImplMetatypeRepresentation::ObjC:
    return MetatypeRepresentation::ObjC;
  }
  llvm_unreachable("covered switch");
}

Type ASTBuilder::createExistentialMetatypeType(
    Type instance, std::optional<Demangle::ImplMetatypeRepresentation> repr) {
  if (auto existential = instance->getAs<ExistentialType>())
    instance = existential->getConstraintType();
  if (!instance->isAnyExistentialType())
    return Type();
  if (!repr)
    return ExistentialMetatypeType::get(instance);

  return ExistentialMetatypeType::get(instance,
                                      getMetatypeRepresentation(*repr));
}

Type ASTBuilder::createConstrainedExistentialType(
    Type base, ArrayRef<BuiltRequirement> constraints,
    ArrayRef<BuiltInverseRequirement> inverseRequirements) {
  llvm::SmallDenseMap<AssociatedTypeDecl *, Type> primaryAssociatedTypes;
  llvm::SmallDenseSet<AssociatedTypeDecl *> claimed;

  for (const auto &req : constraints) {
    switch (req.getKind()) {
    case RequirementKind::SameShape:
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
    case RequirementKind::Layout:
      break;

    case RequirementKind::SameType: {
      if (auto *memberTy = req.getFirstType()->getAs<DependentMemberType>()) {
        if (memberTy->getBase()->is<GenericTypeParamType>()) {
          // This is the only case we understand so far.
          primaryAssociatedTypes[memberTy->getAssocType()] = req.getSecondType();
          continue;
        }
      }
      break;
    }
    }

    // If we end here, we didn't recognize this requirement.
    return Type();
  }

  auto maybeFormParameterizedProtocolType = [&](ProtocolType *protoTy) -> Type {
    auto *proto = protoTy->getDecl();

    llvm::SmallVector<Type, 4> args;
    for (auto *assocTy : proto->getPrimaryAssociatedTypes()) {
      auto found = primaryAssociatedTypes.find(assocTy);
      if (found == primaryAssociatedTypes.end())
        return protoTy;
      args.push_back(found->second);
      claimed.insert(found->first);
    }

    // We may not have any arguments because the constrained existential is a
    // plain protocol with an inverse requirement.
    if (args.empty())
      return protoTy;

    return ParameterizedProtocolType::get(Ctx, protoTy, args);
  };

  SmallVector<Type, 2> members;
  bool hasExplicitAnyObject = false;
  InvertibleProtocolSet inverses;

  // We're given either a single protocol type, or a composition of protocol
  // types. Transform each protocol type to add arguments, if necessary.
  if (auto protoTy = base->getAs<ProtocolType>()) {
    members.push_back(maybeFormParameterizedProtocolType(protoTy));
  } else {
    auto compositionTy = base->castTo<ProtocolCompositionType>();
    hasExplicitAnyObject = compositionTy->hasExplicitAnyObject();
    ASSERT(compositionTy->getInverses().empty());

    for (auto member : compositionTy->getMembers()) {
      if (auto *protoTy = member->getAs<ProtocolType>()) {
        members.push_back(maybeFormParameterizedProtocolType(protoTy));
        continue;
      }
      ASSERT(member->getClassOrBoundGenericClass());
      members.push_back(member);
    }
  }

  // Make sure that all arguments were actually used.
  ASSERT(claimed.size() == primaryAssociatedTypes.size());

  // Handle inverse requirements.
  if (!inverseRequirements.empty()) {
    for (const auto &inverseReq : inverseRequirements) {
      inverses.insert(inverseReq.getKind());
    }
  }

  return ExistentialType::get(ProtocolCompositionType::get(
      Ctx, members, inverses, hasExplicitAnyObject));
}

Type ASTBuilder::createSymbolicExtendedExistentialType(NodePointer shapeNode,
                                                       ArrayRef<Type> genArgs) {
  return Type();
}

Type ASTBuilder::createMetatypeType(
    Type instance, std::optional<Demangle::ImplMetatypeRepresentation> repr) {
  if (!repr)
    return MetatypeType::get(instance);

  return MetatypeType::get(instance, getMetatypeRepresentation(*repr));
}

void ASTBuilder::pushGenericParams(ArrayRef<std::pair<unsigned, unsigned>> parameterPacks) {
  ParameterPackStack.push_back(ParameterPacks);
  ParameterPacks.clear();
  ParameterPacks.append(parameterPacks.begin(), parameterPacks.end());
}

void ASTBuilder::popGenericParams() {
  ParameterPacks = ParameterPackStack.back();
  ParameterPackStack.pop_back();

  if (!ValueParametersStack.empty()) {
    ValueParameters = ValueParametersStack.back();
    ValueParametersStack.pop_back();
  }
}

Type ASTBuilder::createGenericTypeParameterType(unsigned depth,
                                                unsigned index) {
  if (!ParameterPacks.empty()) {
    for (auto pair : ParameterPacks) {
      if (pair.first == depth && pair.second == index) {
        return GenericTypeParamType::getPack(depth, index, Ctx);
      }
    }
  }

  if (!ValueParameters.empty()) {
    for (auto tuple : ValueParameters) {
      auto pair = std::get<std::pair<unsigned, unsigned>>(tuple);
      auto type = std::get<Type>(tuple);

      if (pair.first == depth && pair.second == index) {
        return GenericTypeParamType::getValue(depth, index, type, Ctx);
      }
    }
  }

  return GenericTypeParamType::getType(depth, index, Ctx);
}

Type ASTBuilder::createDependentMemberType(StringRef member,
                                           Type base) {
  auto identifier = getIdentifier(member);

  if (auto *archetype = base->getAs<ArchetypeType>()) {
      if (Type memberType = archetype->getNestedTypeByName(identifier))
        return memberType;
  }

  if (base->isTypeParameter()) {
    return DependentMemberType::get(base, identifier);
  }

  return Type();
}

Type ASTBuilder::createDependentMemberType(StringRef member,
                                           Type base,
                                           ProtocolDecl *protocol) {
  auto identifier = getIdentifier(member);

  if (auto *archetype = base->getAs<ArchetypeType>()) {
    if (auto assocType = protocol->getAssociatedType(identifier))
      return archetype->getNestedType(assocType);
  }

  if (base->isTypeParameter()) {
    if (auto assocType = protocol->getAssociatedType(identifier))
      return DependentMemberType::get(base, assocType);
  }

  return Type();
}

#define REF_STORAGE(Name, ...) \
Type ASTBuilder::create##Name##StorageType(Type base) { \
  return Name##StorageType::get(base, Ctx); \
}
#include "swift/AST/ReferenceStorage.def"

Type ASTBuilder::createSILBoxType(Type base) {
  return SILBoxType::get(base->getCanonicalType());
}

/// Utility function to produce a Requirement from an InverseRequirement.
static Requirement inverseAsRequirement(const InverseRequirement &inverseReq) {
  ASTContext &ctx = inverseReq.subject->getASTContext();
  InvertibleProtocolSet inverses;
  inverses.insert(inverseReq.getKind());
  Type constraintType = ProtocolCompositionType::get(
      ctx, { }, inverses, /*hasExplicitAnyObject=*/false);
  return Requirement(
      RequirementKind::Conformance, inverseReq.subject, constraintType);
}

/// Utility function to append Requirements produced from the given set of
/// InverseRequirements to the `requirements` vector.
static void appendInversesAsRequirements(
    ArrayRef<InverseRequirement> inverseRequirements,
    SmallVectorImpl<Requirement> &requirements) {
  for (const auto &inverseReq : inverseRequirements)
    requirements.push_back(inverseAsRequirement(inverseReq));
}

Type ASTBuilder::createSILBoxTypeWithLayout(
    ArrayRef<BuiltSILBoxField> fields,
    ArrayRef<BuiltSubstitution> Substitutions,
    ArrayRef<BuiltRequirement> Requirements,
    ArrayRef<BuiltInverseRequirement> InverseRequirements) {
  SmallVector<Type, 4> replacements;
  SmallVector<GenericTypeParamType *, 2> genericTypeParams;
  for (const auto &s : Substitutions) {
    if (auto *t = dyn_cast_or_null<GenericTypeParamType>(s.first.getPointer()))
      genericTypeParams.push_back(t);
    replacements.push_back(s.second);
  }

  GenericSignature signature;
  if (!genericTypeParams.empty()) {
    SmallVector<BuiltRequirement, 2> RequirementsVec(Requirements);
    appendInversesAsRequirements(InverseRequirements, RequirementsVec);
    signature = swift::buildGenericSignature(Ctx,
                                             signature,
                                             genericTypeParams,
                                             std::move(RequirementsVec),
                                             /*allowInverses=*/true);
  }
  SmallVector<SILField, 4> silFields;
  for (auto field: fields)
    silFields.emplace_back(field.getPointer()->getCanonicalType(),
                           field.getInt());
  SILLayout *layout =
      SILLayout::get(Ctx, signature.getCanonicalSignature(), silFields,
                     /*captures generics*/ false);

  SubstitutionMap substs;
  if (signature)
    substs = createSubstitutionMapFromGenericArgs(
        signature, replacements,
        LookUpConformanceInModule());
  return SILBoxType::get(Ctx, layout, substs);
}

Type ASTBuilder::createObjCClassType(StringRef name) {
  auto typeDecl =
      findForeignTypeDecl(name, /*relatedEntityKind*/{},
                          ForeignModuleKind::Imported,
                          Demangle::Node::Kind::Class);
  if (!typeDecl) return Type();
  return typeDecl->getDeclaredInterfaceType();
}

Type ASTBuilder::createBoundGenericObjCClassType(StringRef name,
                                                 ArrayRef<Type> args) {
  auto typeDecl =
      findForeignTypeDecl(name, /*relatedEntityKind*/{},
                          ForeignModuleKind::Imported,
                          Demangle::Node::Kind::Class);
  if (!typeDecl ||
      !isa<ClassDecl>(typeDecl)) return Type();
  if (!typeDecl->getGenericParams() ||
      typeDecl->getGenericParams()->size() != args.size())
    return Type();

  Type parent;
  auto *dc = typeDecl->getDeclContext();
  if (dc->isTypeContext()) {
    if (dc->isGenericContext())
      return Type();
    parent = dc->getDeclaredInterfaceType();
  }

  return BoundGenericClassType::get(cast<ClassDecl>(typeDecl),
                                    parent, args);
}

ProtocolDecl *ASTBuilder::createObjCProtocolDecl(StringRef name) {
  auto typeDecl =
      findForeignTypeDecl(name, /*relatedEntityKind*/{},
                          ForeignModuleKind::Imported,
                          Demangle::Node::Kind::Protocol);
  if (auto *protocolDecl = dyn_cast_or_null<ProtocolDecl>(typeDecl))
    return protocolDecl;
  return nullptr;
}

Type ASTBuilder::createDynamicSelfType(Type selfType) {
  return DynamicSelfType::get(selfType, Ctx);
}

Type ASTBuilder::createForeignClassType(StringRef mangledName) {
  bool typeAlias = false;
  auto typeDecl = createTypeDecl(mangledName, typeAlias);
  if (!typeDecl) return Type();
  return typeDecl->getDeclaredInterfaceType();
}

Type ASTBuilder::getUnnamedForeignClassType() {
  return Type();
}

Type ASTBuilder::getOpaqueType() {
  return Type();
}

Type ASTBuilder::createOptionalType(Type base) {
  return OptionalType::get(base);
}

Type ASTBuilder::createArrayType(Type base) {
  return ArraySliceType::get(base);
}

Type ASTBuilder::createInlineArrayType(Type count, Type element) {
  return InlineArrayType::get(count, element);
}

Type ASTBuilder::createDictionaryType(Type key, Type value) {
  return DictionaryType::get(key, value);
}

Type ASTBuilder::createIntegerType(intptr_t value) {
  return IntegerType::get(std::to_string(value), /*isNegative*/ false, Ctx);
}

Type ASTBuilder::createNegativeIntegerType(intptr_t value) {
  return IntegerType::get(std::to_string(value), /*isNegative*/ true, Ctx);
}

Type ASTBuilder::createBuiltinFixedArrayType(Type size, Type element) {
  return BuiltinFixedArrayType::get(size->getCanonicalType(),
                                    element->getCanonicalType());
}

GenericSignature
ASTBuilder::createGenericSignature(ArrayRef<BuiltType> builtParams,
                                   ArrayRef<BuiltRequirement> requirements) {
  std::vector<GenericTypeParamType *> params;
  for (auto &param : builtParams) {
    auto paramTy = param->getAs<GenericTypeParamType>();
    if (!paramTy)
      return GenericSignature();
    params.push_back(paramTy);
  }
  return GenericSignature::get(params, requirements);
}

SubstitutionMap
ASTBuilder::createSubstitutionMap(BuiltGenericSignature sig,
                                  ArrayRef<BuiltType> replacements) {
  return SubstitutionMap::get(sig, replacements,
                              LookUpConformanceInModule());
}

Type ASTBuilder::subst(Type subject, const BuiltSubstitutionMap &Subs) const {
  return subject.subst(Subs);
}

bool ASTBuilder::validateParentType(TypeDecl *decl, Type parent) {
  auto parentDecl = decl->getDeclContext()->getSelfNominalTypeDecl();

  // If we don't have a parent type, fast-path.
  if (!parent) {
    return parentDecl == nullptr;
  }

  // We do have a parent type. If our type doesn't, it's an error.
  if (!parentDecl) {
    return false;
  }

  if (isa<NominalTypeDecl>(decl)) {
    // The parent should be a nominal type when desugared.
    auto *parentNominal = parent->getAnyNominal();
    if (!parentNominal || parentNominal != parentDecl) {
      return false;
    }
  }

  // FIXME: validate that the parent is a correct application of the
  // enclosing context?
  return true;
}

GenericTypeDecl *
ASTBuilder::getAcceptableTypeDeclCandidate(ValueDecl *decl,
                                           Demangle::Node::Kind kind) {
  if (kind == Demangle::Node::Kind::Class) {
    return dyn_cast<ClassDecl>(decl);
  } else if (kind == Demangle::Node::Kind::Enum) {
    return dyn_cast<EnumDecl>(decl);
  } else if (kind == Demangle::Node::Kind::Protocol) {
    return dyn_cast<ProtocolDecl>(decl);
  } else if (kind == Demangle::Node::Kind::Structure) {
    return dyn_cast<StructDecl>(decl);
  } else {
    assert(kind == Demangle::Node::Kind::TypeAlias);
    return dyn_cast<TypeAliasDecl>(decl);
  }
}

DeclContext *ASTBuilder::getNotionalDC() {
  if (!NotionalDC) {
    NotionalDC = ModuleDecl::createEmpty(getIdentifier(".RemoteAST"), Ctx);
    NotionalDC = new (Ctx) TopLevelCodeDecl(NotionalDC);
  }
  return NotionalDC;
}

GenericTypeDecl *
ASTBuilder::createTypeDecl(NodePointer node,
                           bool &typeAlias) {
  auto DC = findDeclContext(node);
  if (!DC)
    return nullptr;

  typeAlias = isa<TypeAliasDecl>(DC);
  return dyn_cast<GenericTypeDecl>(DC);
}

llvm::ArrayRef<ModuleDecl *>
ASTBuilder::findPotentialModules(NodePointer node, ModuleDecl *&scratch) {
  assert(node->getKind() == Demangle::Node::Kind::Module);
  
  const auto moduleName = node->getText();
  
  if (moduleName == CLANG_HEADER_MODULE_NAME) {
    auto *importer = Ctx.getClangModuleLoader();
    scratch = importer->getImportedHeaderModule();
    return ArrayRef(&scratch, 1);
  }
  
  return Ctx.getModulesByRealOrABIName(moduleName);
}

Demangle::NodePointer
ASTBuilder::findModuleNode(NodePointer node) {
  auto child = node;
  while (child->hasChildren() &&
         child->getKind() != Demangle::Node::Kind::Module) {
    child = child->getFirstChild();
  }

  if (child->getKind() != Demangle::Node::Kind::Module)
    return nullptr;

  return child;
}

std::optional<ASTBuilder::ForeignModuleKind>
ASTBuilder::getForeignModuleKind(NodePointer node) {
  if (node->getKind() == Demangle::Node::Kind::DeclContext)
    return getForeignModuleKind(node->getFirstChild());

  if (node->getKind() != Demangle::Node::Kind::Module)
    return std::nullopt;

  return llvm::StringSwitch<std::optional<ForeignModuleKind>>(node->getText())
      .Case(MANGLING_MODULE_OBJC, ForeignModuleKind::Imported)
      .Case(MANGLING_MODULE_CLANG_IMPORTER,
            ForeignModuleKind::SynthesizedByImporter)
      .Default(std::nullopt);
}

LayoutConstraint ASTBuilder::getLayoutConstraint(LayoutConstraintKind kind) {
  return LayoutConstraint::getLayoutConstraint(kind, getASTContext());
}

LayoutConstraint ASTBuilder::getLayoutConstraintWithSizeAlign(
    LayoutConstraintKind kind, unsigned size, unsigned alignment) {
  return LayoutConstraint::getLayoutConstraint(kind, size, alignment,
                                               getASTContext());
}

InverseRequirement ASTBuilder::createInverseRequirement(
    Type subject, InvertibleProtocolKind kind) {
  auto knownProtoKind = getKnownProtocolKind(kind);
  auto proto = subject->getASTContext().getProtocol(knownProtoKind);
  return InverseRequirement(subject, proto, SourceLoc());
}

CanGenericSignature ASTBuilder::demangleGenericSignature(
    NominalTypeDecl *nominalDecl,
    NodePointer node) {
  auto baseGenericSig = nominalDecl->getGenericSignature();

  // The generic signature is for a constrained extension of nominalDecl, so
  // we introduce the parameter packs from the nominal's generic signature.
  ParameterPackStack.push_back(ParameterPacks);
  ParameterPacks.clear();

  ValueParametersStack.push_back(ValueParameters);
  ValueParameters.clear();
  for (auto *paramTy : baseGenericSig.getGenericParams()) {
    if (paramTy->isParameterPack())
      ParameterPacks.emplace_back(paramTy->getDepth(), paramTy->getIndex());

    if (paramTy->isValue()) {
      auto pair = std::make_pair(paramTy->getDepth(), paramTy->getIndex());
      auto tuple = std::make_tuple(pair, paramTy->getValueType());
      ValueParameters.emplace_back(tuple);
    }
  }
  SWIFT_DEFER { popGenericParams(); };

  // Constrained extensions mangle the subset of requirements not satisfied
  // by the nominal's generic signature.
  SmallVector<Requirement, 2> requirements;
  SmallVector<InverseRequirement, 2> inverseRequirements;
  decodeRequirement<BuiltType, BuiltRequirement, BuiltInverseRequirement,
                    BuiltLayoutConstraint, ASTBuilder>(
      node, requirements, inverseRequirements, *this);
  appendInversesAsRequirements(inverseRequirements, requirements);

  return buildGenericSignature(Ctx, baseGenericSig, {}, std::move(requirements),
                               /*allowInverses=*/true)
      .getCanonicalSignature();
}

DeclContext *
ASTBuilder::findDeclContext(NodePointer node) {
  switch (node->getKind()) {
  case Demangle::Node::Kind::DeclContext:
  case Demangle::Node::Kind::Type:
  case Demangle::Node::Kind::BoundGenericClass:
  case Demangle::Node::Kind::BoundGenericEnum:
  case Demangle::Node::Kind::BoundGenericProtocol:
  case Demangle::Node::Kind::BoundGenericStructure:
  case Demangle::Node::Kind::BoundGenericTypeAlias:
    return findDeclContext(node->getFirstChild());

  case Demangle::Node::Kind::Module: {
    // A Module node is not enough information to find the decl context.
    // The reason being that the module name in a mangled name can either be
    // the module's ABI name, which is potentially not unique (due to the
    // -module-abi-name flag), or the module's real name, if mangling for the
    // debugger or USR together with the OriginallyDefinedIn attribute for
    // example.
    assert(false && "Looked up module as decl context directly!");
    ModuleDecl *scratch;
    auto modules = findPotentialModules(node, scratch);
    return modules.empty() ? nullptr : modules[0];
  }

  case Demangle::Node::Kind::Class:
  case Demangle::Node::Kind::Enum:
  case Demangle::Node::Kind::Protocol:
  case Demangle::Node::Kind::Structure:
  case Demangle::Node::Kind::TypeAlias: {
    const auto &declNameNode = node->getChild(1);

    // Handle local declarations.
    if (declNameNode->getKind() == Demangle::Node::Kind::LocalDeclName) {
      // Find the AST node for the defining module.
      auto moduleNode = findModuleNode(node);
      if (!moduleNode)
        return nullptr;

      ModuleDecl *scratch;
      auto potentialModules = findPotentialModules(moduleNode, scratch);
      if (potentialModules.empty())
        return nullptr;

      // Look up the local type by its mangling.
      auto mangling = Demangle::mangleNode(node, ManglingFlavor);
      if (!mangling.isSuccess())
        return nullptr;
      auto mangledName = mangling.result();

      for (auto *module : potentialModules)
        if (auto *decl = module->lookupLocalType(mangledName))
          return dyn_cast<DeclContext>(decl);

      return nullptr;
    }

    StringRef name;
    StringRef relatedEntityKind;
    Identifier privateDiscriminator;
    if (declNameNode->getKind() == Demangle::Node::Kind::Identifier) {
      name = declNameNode->getText();

    } else if (declNameNode->getKind() ==
                 Demangle::Node::Kind::PrivateDeclName) {
      name = declNameNode->getChild(1)->getText();
      privateDiscriminator =
          getIdentifier(declNameNode->getChild(0)->getText());

    } else if (declNameNode->getKind() ==
                 Demangle::Node::Kind::RelatedEntityDeclName) {
      name = declNameNode->getChild(1)->getText();
      relatedEntityKind = declNameNode->getFirstChild()->getText();

    // Ignore any other decl-name productions for now.
    } else {
      return nullptr;
    }

    // Do some special logic for foreign type declarations.
    if (privateDiscriminator.empty()) {
      if (auto foreignModuleKind = getForeignModuleKind(node->getChild(0))) {
        return findForeignTypeDecl(name, relatedEntityKind,
                                    foreignModuleKind.value(),
                                    node->getKind());
      }
    }

    auto child = node->getFirstChild();
    if (child->getKind() == Node::Kind::Module) {
      ModuleDecl *scratch;
      auto potentialModules = findPotentialModules(child, scratch);
      if (potentialModules.empty())
        return nullptr;

      for (auto *module : potentialModules)
        if (auto typeDecl = findTypeDecl(module, getIdentifier(name),
                                         privateDiscriminator, node->getKind()))
          return typeDecl;
      return nullptr;
    }

    if (auto *dc = findDeclContext(child))
      if (auto typeDecl = findTypeDecl(dc, getIdentifier(name),
                                       privateDiscriminator, node->getKind()))
        return typeDecl;

    return nullptr;
  }

  case Demangle::Node::Kind::Global:
    return findDeclContext(node->getChild(0));

  case Demangle::Node::Kind::Extension: {
    ModuleDecl *scratch;
    auto moduleDecls = findPotentialModules(node->getFirstChild(), scratch);
    if (moduleDecls.empty())
      return nullptr;

    auto *nominalDecl = dyn_cast_or_null<NominalTypeDecl>(
        findDeclContext(node->getChild(1)));
    if (!nominalDecl)
      return nullptr;

    CanGenericSignature genericSig;
    bool genericSigMatchesNominal = false;
    if (node->getNumChildren() > 2) {
      genericSig = demangleGenericSignature(nominalDecl, node->getChild(2));

      // If the generic signature are equivalent to that of the nominal type,
      // we're either in another module or the nominal type is generic and
      // involves inverse requirements on its generic parameters.
      genericSigMatchesNominal = genericSig &&
        genericSig == nominalDecl->getGenericSignatureOfContext().getCanonicalSignature();

      // If the generic signature is equivalent to that of the nominal type,
      // and we're in the same module, it's due to inverse requirements.
      // Just return the nominal declaration.
      for (auto *moduleDecl : moduleDecls) {
        if (genericSigMatchesNominal &&
            nominalDecl->getParentModule() == moduleDecl) {
          return nominalDecl;;
        }
      }
    }

    for (auto *ext : nominalDecl->getExtensions()) {
      bool found = false;
      for (ModuleDecl *module : moduleDecls) {
        auto *extensionModule = ext->getParentModule();
        
        if (extensionModule == module) {
          found = true;
          break;
        }
      }
      if (!found)
        continue;

      if (!ext->isConstrainedExtension()) {
        if (!genericSig || genericSigMatchesNominal)
          return ext;

        continue;
      }

      if (!ext->isWrittenWithConstraints() && !genericSig)
        return ext;

      auto extSig = ext->getGenericSignature().getCanonicalSignature();
      if (extSig == genericSig) {
        return ext;
      }

      // If the extension mangling doesn't include a generic signature, it
      // might be because the nominal type suppresses conformance.
      if (!genericSig) {
        SmallVector<Requirement, 2> requirements;
        SmallVector<InverseRequirement, 2> inverses;
        extSig->getRequirementsWithInverses(requirements, inverses);
        if (requirements.empty())
          return ext;
      }
    }

    return nullptr;
  }

  // Bail out on other kinds of contexts.
  default:
    return nullptr;
  }
}

GenericTypeDecl *
ASTBuilder::findTypeDecl(DeclContext *dc,
                         Identifier name,
                         Identifier privateDiscriminator,
                         Demangle::Node::Kind kind) {
  auto module = dc->getParentModule();

  // When looking into an extension, look into the nominal instead; the
  // important thing is that the module, obtained above, is the module
  // containing the extension and not the module containing the nominal
  if (isa<ExtensionDecl>(dc))
    dc = dc->getSelfNominalTypeDecl();

  SmallVector<ValueDecl *, 4> lookupResults;
  module->lookupMember(lookupResults, dc, name, privateDiscriminator);

  GenericTypeDecl *result = nullptr;
  for (auto decl : lookupResults) {
    // Ignore results that are not the right kind of type declaration.
    auto *candidate = getAcceptableTypeDeclCandidate(decl, kind);
    if (!candidate)
      continue;

    // Ignore results that aren't actually from the defining module.
    if (candidate->getParentModule() != module)
      continue;

    // This is a viable result.

    // If we already have a viable result, it's ambiguous, so give up.
    if (result) return nullptr;
    result = candidate;
  }

  // If we looked into the standard library module, but didn't find anything,
  // try the _Concurrency module, which is also mangled into the Swift module.
  if (!result && !dc->getParent() && module->isStdlibModule()) {
    ASTContext &ctx = module->getASTContext();
    if (auto concurrencyModule = ctx.getLoadedModule(ctx.Id_Concurrency)) {
      return findTypeDecl(concurrencyModule, name, privateDiscriminator, kind);
    }
  }

  return result;
}

static std::optional<ClangTypeKind>
getClangTypeKindForNodeKind(Demangle::Node::Kind kind) {
  switch (kind) {
  case Demangle::Node::Kind::Protocol:
    return ClangTypeKind::ObjCProtocol;
  case Demangle::Node::Kind::Class:
    return ClangTypeKind::ObjCClass;
  case Demangle::Node::Kind::TypeAlias:
    return ClangTypeKind::Typedef;
  case Demangle::Node::Kind::Structure:
  case Demangle::Node::Kind::Enum:
    return ClangTypeKind::Tag;
  default:
    return std::nullopt;
  }
}

GenericTypeDecl *ASTBuilder::findForeignTypeDecl(StringRef name,
                                                 StringRef relatedEntityKind,
                                                 ForeignModuleKind foreignKind,
                                                 Demangle::Node::Kind kind) {
  // Check to see if we have an importer loaded.
  auto importer = Ctx.getClangModuleLoader();
  if (!importer)
    return nullptr;

  // Find the unique declaration that has the right kind.
  struct Consumer : VisibleDeclConsumer {
    Demangle::Node::Kind ExpectedKind;
    GenericTypeDecl *Result = nullptr;
    bool HadError = false;

    explicit Consumer(Demangle::Node::Kind kind) : ExpectedKind(kind) {}

    void foundDecl(ValueDecl *decl, DeclVisibilityKind reason,
                   DynamicLookupInfo dynamicLookupInfo = {}) override {
      if (HadError)
        return;
      if (decl == Result)
        return;
      if (!Result) {
        Result = dyn_cast<GenericTypeDecl>(decl);
        HadError |= !Result;
      } else {
        HadError = true;
        Result = nullptr;
      }
    }
  } consumer(kind);

  auto found = [&](TypeDecl *found) {
    consumer.foundDecl(found, DeclVisibilityKind::VisibleAtTopLevel);
  };

  std::optional<ClangTypeKind> lookupKind = getClangTypeKindForNodeKind(kind);
  if (!lookupKind)
    return nullptr;

  switch (foreignKind) {
  case ForeignModuleKind::SynthesizedByImporter:
    if (!relatedEntityKind.empty()) {
      importer->lookupRelatedEntity(name, *lookupKind, relatedEntityKind,
                                    found);
      break;
    }
    importer->lookupValue(getIdentifier(name), consumer);
    if (consumer.Result)
      consumer.Result = getAcceptableTypeDeclCandidate(consumer.Result, kind);
    break;
  case ForeignModuleKind::Imported:
    importer->lookupTypeDecl(name, *lookupKind, found);
  }

  return consumer.Result;
}

Identifier ASTBuilder::getIdentifier(StringRef name) {
  if (name.size() > 1 && name.front() == '`' && name.back() == '`') {
    // Raw identifiers have backticks affixed before mangling. We need to
    // remove those before creating the Identifier for the AST, which doesn't
    // encode the backticks.
    std::string fixedName;
    for (size_t i = 1; i < name.size() - 1; ++i) {
      unsigned char ch = name[i];
      // Raw identifiers have the space (U+0020) replaced with a non-breaking
      // space (U+00A0, UTF-8: 0xC2 0xA0) in their mangling so that parts of
      // the runtime that use space as a delimiter remain compatible with
      // these identifiers. Flip it back.
      if (ch == 0xc2 && i < name.size() - 2 &&
          (unsigned char)name[i + 1] == 0xa0) {
        fixedName.push_back(' ');
        ++i;
      } else {
        fixedName.push_back(ch);
      }
    }
    return Ctx.getIdentifier(fixedName);
  }
  return Ctx.getIdentifier(name);
}

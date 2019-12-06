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
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/ManglingMacros.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

Type swift::Demangle::getTypeForMangling(ASTContext &ctx,
                                         StringRef mangling) {
  Demangle::Context Dem;
  auto node = Dem.demangleSymbolAsNode(mangling);
  if (!node)
    return Type();

  ASTBuilder builder(ctx);
  return swift::Demangle::decodeMangledType(builder, node);
}

TypeDecl *swift::Demangle::getTypeDeclForMangling(ASTContext &ctx,
                                                  StringRef mangling) {
  Demangle::Context Dem;
  auto node = Dem.demangleSymbolAsNode(mangling);
  if (!node)
    return nullptr;

  ASTBuilder builder(ctx);
  return builder.createTypeDecl(node);
}

TypeDecl *swift::Demangle::getTypeDeclForUSR(ASTContext &ctx,
                                             StringRef usr) {
  if (!usr.startswith("s:"))
    return nullptr;

  std::string mangling(usr);
  mangling.replace(0, 2, MANGLING_PREFIX_STR);

  return getTypeDeclForMangling(ctx, mangling);
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

    auto name = Ctx.getIdentifier(node->getChild(1)->getText());
    return proto->getAssociatedType(name);
  }

  auto *DC = findDeclContext(node);
  return dyn_cast_or_null<GenericTypeDecl>(DC);
}

Type
ASTBuilder::createBuiltinType(StringRef builtinName,
                              StringRef mangledName) {
  if (builtinName.startswith(BUILTIN_TYPE_NAME_PREFIX)) {
    SmallVector<ValueDecl *, 1> decls;

    StringRef strippedName =
        builtinName.drop_front(BUILTIN_TYPE_NAME_PREFIX.size());
    Ctx.TheBuiltinModule->lookupValue(Ctx.getIdentifier(strippedName),
                                      NLKind::QualifiedLookup,
                                      decls);
    
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
  if (nominalDecl->getGenericParams())
    return Type();

  // Imported types can be renamed to be members of other (non-generic)
  // types, but the mangling does not have a parent type. Just use the
  // declared type directly in this case and skip the parent check below.
  bool isImported = nominalDecl->hasClangNode() ||
      nominalDecl->getAttrs().hasAttribute<ClangImporterSynthesizedTypeAttr>();
  if (isImported && !nominalDecl->isGenericContext())
    return nominalDecl->getDeclaredType();

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
  auto subs = parent->getContextSubstitutionMap(dc->getParentModule(), dc);

  return declaredType.subst(subs);
}

static SubstitutionMap
createSubstitutionMapFromGenericArgs(GenericSignature genericSig,
                                     ArrayRef<Type> args,
                                     ModuleDecl *moduleDecl) {
  if (!genericSig)
    return SubstitutionMap();
  
  SmallVector<GenericTypeParamType *, 4> genericParams;
  genericSig->forEachParam([&](GenericTypeParamType *gp, bool canonical) {
    if (canonical)
      genericParams.push_back(gp);
  });
  if (genericParams.size() != args.size())
    return SubstitutionMap();

  return SubstitutionMap::get(
      genericSig,
      [&](SubstitutableType *t) -> Type {
        for (unsigned i = 0, e = genericParams.size(); i < e; ++i) {
          if (t->isEqual(genericParams[i]))
            return args[i];
        }
        return Type();
      },
      LookUpConformanceInModule(moduleDecl));
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
      genericSig, args, decl->getParentModule());
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
    auto mangledName = mangleNode(definingGlobal);
    
    auto moduleNode = findModuleNode(definingDecl);
    if (!moduleNode)
      return Type();
    auto parentModule = findModule(moduleNode);
    if (!parentModule)
      return Type();

    auto opaqueDecl = parentModule->lookupOpaqueResultType(mangledName);
    if (!opaqueDecl)
      return Type();
    // TODO: multiple opaque types
    assert(ordinal == 0 && "not implemented");
    if (ordinal != 0)
      return Type();
    
    SmallVector<Type, 8> allArgs;
    for (auto argSet : args) {
      allArgs.append(argSet.begin(), argSet.end());
    }

    SubstitutionMap subs = createSubstitutionMapFromGenericArgs(
                      opaqueDecl->getGenericSignature(), allArgs, parentModule);
    return OpaqueTypeArchetypeType::get(opaqueDecl, subs);
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

  // Combine the substitutions from our parent type with our generic
  // arguments.
  TypeSubstitutionMap subs;
  if (parent)
    subs = parent->getContextSubstitutions(decl->getDeclContext());

  auto *aliasDecl = cast<TypeAliasDecl>(decl);

  auto genericSig = aliasDecl->getGenericSignature();
  for (unsigned i = 0, e = args.size(); i < e; i++) {
    auto origTy = genericSig->getInnermostGenericParams()[i];
    auto substTy = args[i];

    subs[origTy->getCanonicalType()->castTo<GenericTypeParamType>()] =
      substTy;
  }

  // FIXME: This is the wrong module
  auto *moduleDecl = decl->getParentModule();
  auto subMap = SubstitutionMap::get(genericSig,
                                     QueryTypeSubstitutionMap{subs},
                                     LookUpConformanceInModule(moduleDecl));
  if (!subMap)
    return Type();

  return aliasDecl->getDeclaredInterfaceType().subst(subMap);
}

Type ASTBuilder::createTupleType(ArrayRef<Type> eltTypes,
                                 StringRef labels,
                                 bool isVariadic) {
  // Just bail out on variadic tuples for now.
  if (isVariadic) return Type();

  SmallVector<TupleTypeElt, 4> elements;
  elements.reserve(eltTypes.size());
  for (auto eltType : eltTypes) {
    Identifier label;
    if (!labels.empty()) {
      auto split = labels.split(' ');
      if (!split.first.empty())
        label = Ctx.getIdentifier(split.first);
      labels = split.second;
    }
    elements.emplace_back(eltType, label);
  }

  return TupleType::get(elements, Ctx);
}

Type ASTBuilder::createFunctionType(
    ArrayRef<Demangle::FunctionParam<Type>> params,
    Type output, FunctionTypeFlags flags) {
  // The result type must be materializable.
  if (!output->isMaterializable()) return Type();

  llvm::SmallVector<AnyFunctionType::Param, 8> funcParams;
  for (const auto &param : params) {
    auto type = param.getType();

    // All the argument types must be materializable.
    if (!type->isMaterializable())
      return Type();

    auto label = Ctx.getIdentifier(param.getLabel());
    auto flags = param.getFlags();
    auto ownership = flags.getValueOwnership();
    auto parameterFlags = ParameterTypeFlags()
                              .withValueOwnership(ownership)
                              .withVariadic(flags.isVariadic())
                              .withAutoClosure(flags.isAutoClosure());

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

  auto noescape =
    (representation == FunctionTypeRepresentation::Swift
     || representation == FunctionTypeRepresentation::Block)
    && !flags.isEscaping();

  FunctionType::ExtInfo incompleteExtInfo(
    FunctionTypeRepresentation::Swift,
    noescape, flags.throws(),
    DifferentiabilityKind::NonDifferentiable,
    /*clangFunctionType*/nullptr);

  const clang::Type *clangFunctionType = nullptr;
  if (representation == FunctionTypeRepresentation::CFunctionPointer)
    clangFunctionType = Ctx.getClangFunctionType(funcParams, output,
                                                 incompleteExtInfo,
                                                 representation);

  auto einfo = incompleteExtInfo.withRepresentation(representation)
                                .withClangFunctionType(clangFunctionType);

  return FunctionType::get(funcParams, output, einfo);
}

static ParameterConvention
getParameterConvention(ImplParameterConvention conv) {
  switch (conv) {
  case Demangle::ImplParameterConvention::Indirect_In:
    return ParameterConvention::Indirect_In;
  case Demangle::ImplParameterConvention::Indirect_In_Constant:
    return ParameterConvention::Indirect_In_Constant;
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
  }
  llvm_unreachable("covered switch");
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
  }
  llvm_unreachable("covered switch");
}

Type ASTBuilder::createImplFunctionType(
    Demangle::ImplParameterConvention calleeConvention,
    ArrayRef<Demangle::ImplFunctionParam<Type>> params,
    ArrayRef<Demangle::ImplFunctionResult<Type>> results,
    Optional<Demangle::ImplFunctionResult<Type>> errorResult,
    ImplFunctionTypeFlags flags) {
  GenericSignature genericSig = GenericSignature();

  SILCoroutineKind funcCoroutineKind = SILCoroutineKind::None;
  ParameterConvention funcCalleeConvention =
    getParameterConvention(calleeConvention);

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

  // TODO: [store-sil-clang-function-type]
  auto einfo = SILFunctionType::ExtInfo(
      representation, flags.isPseudogeneric(), !flags.isEscaping(),
      DifferentiabilityKind::NonDifferentiable,
      /*clangFunctionType*/ nullptr);

  llvm::SmallVector<SILParameterInfo, 8> funcParams;
  llvm::SmallVector<SILYieldInfo, 8> funcYields;
  llvm::SmallVector<SILResultInfo, 8> funcResults;
  Optional<SILResultInfo> funcErrorResult;

  for (const auto &param : params) {
    auto type = param.getType()->getCanonicalType();
    auto conv = getParameterConvention(param.getConvention());
    funcParams.emplace_back(type, conv);
  }

  for (const auto &result : results) {
    auto type = result.getType()->getCanonicalType();
    auto conv = getResultConvention(result.getConvention());
    funcResults.emplace_back(type, conv);
  }

  if (errorResult) {
    auto type = errorResult->getType()->getCanonicalType();
    auto conv = getResultConvention(errorResult->getConvention());
    funcErrorResult.emplace(type, conv);
  }
  return SILFunctionType::get(genericSig, einfo, funcCoroutineKind,
                              funcCalleeConvention, funcParams, funcYields,
                              funcResults, funcErrorResult,
                              SubstitutionMap(), false, Ctx);
}

Type ASTBuilder::createProtocolCompositionType(
    ArrayRef<ProtocolDecl *> protocols,
    Type superclass,
    bool isClassBound) {
  std::vector<Type> members;
  for (auto protocol : protocols)
    members.push_back(protocol->getDeclaredType());
  if (superclass && superclass->getClassOrBoundGenericClass())
    members.push_back(superclass);
  return ProtocolCompositionType::get(Ctx, members, isClassBound);
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

Type ASTBuilder::createExistentialMetatypeType(Type instance,
                          Optional<Demangle::ImplMetatypeRepresentation> repr) {
  if (!instance->isAnyExistentialType())
    return Type();
  if (!repr)
    return ExistentialMetatypeType::get(instance);

  return ExistentialMetatypeType::get(instance,
                                      getMetatypeRepresentation(*repr));
}

Type ASTBuilder::createMetatypeType(Type instance,
                         Optional<Demangle::ImplMetatypeRepresentation> repr) {
  if (!repr)
    return MetatypeType::get(instance);

  return MetatypeType::get(instance, getMetatypeRepresentation(*repr));
}

Type ASTBuilder::createGenericTypeParameterType(unsigned depth,
                                                unsigned index) {
  return GenericTypeParamType::get(depth, index, Ctx);
}

Type ASTBuilder::createDependentMemberType(StringRef member,
                                           Type base) {
  if (!base->isTypeParameter())
    return Type();

  return DependentMemberType::get(base, Ctx.getIdentifier(member));
}

Type ASTBuilder::createDependentMemberType(StringRef member,
                                           Type base,
                                           ProtocolDecl *protocol) {
  if (!base->isTypeParameter())
    return Type();

  if (auto assocType = protocol->getAssociatedType(Ctx.getIdentifier(member)))
    return DependentMemberType::get(base, assocType);

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

Type ASTBuilder::createDictionaryType(Type key, Type value) {
  return DictionaryType::get(key, value);
}

Type ASTBuilder::createParenType(Type base) {
  return ParenType::get(Ctx, base);
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
    NotionalDC = ModuleDecl::create(Ctx.getIdentifier(".RemoteAST"), Ctx);
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

ModuleDecl *
ASTBuilder::findModule(NodePointer node) {
  assert(node->getKind() == Demangle::Node::Kind::Module);
  const auto &moduleName = node->getText();
  return Ctx.getModuleByName(moduleName);
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

Optional<ASTBuilder::ForeignModuleKind>
ASTBuilder::getForeignModuleKind(NodePointer node) {
  if (node->getKind() == Demangle::Node::Kind::DeclContext)
    return getForeignModuleKind(node->getFirstChild());

  if (node->getKind() != Demangle::Node::Kind::Module)
    return None;

  return llvm::StringSwitch<Optional<ForeignModuleKind>>(node->getText())
      .Case(MANGLING_MODULE_OBJC, ForeignModuleKind::Imported)
      .Case(MANGLING_MODULE_CLANG_IMPORTER,
            ForeignModuleKind::SynthesizedByImporter)
      .Default(None);
}

CanGenericSignature ASTBuilder::demangleGenericSignature(
    NominalTypeDecl *nominalDecl,
    NodePointer node) {
  SmallVector<Requirement, 2> requirements;

  for (auto &child : *node) {
    if (child->getKind() ==
          Demangle::Node::Kind::DependentGenericParamCount)
      continue;

    if (child->getNumChildren() != 2)
      return CanGenericSignature();
    auto subjectType = swift::Demangle::decodeMangledType(
        *this, child->getChild(0));
    if (!subjectType)
      return CanGenericSignature();

    Type constraintType;
    if (child->getKind() ==
          Demangle::Node::Kind::DependentGenericConformanceRequirement ||
        child->getKind() ==
          Demangle::Node::Kind::DependentGenericSameTypeRequirement) {
      constraintType = swift::Demangle::decodeMangledType(
        *this, child->getChild(1));
      if (!constraintType)
        return CanGenericSignature();
    }

    switch (child->getKind()) {
    case Demangle::Node::Kind::DependentGenericConformanceRequirement: {
      requirements.push_back(
          Requirement(constraintType->isExistentialType()
                        ? RequirementKind::Conformance
                        : RequirementKind::Superclass,
                      subjectType, constraintType));
      break;
    }
    case Demangle::Node::Kind::DependentGenericSameTypeRequirement: {
      requirements.push_back(
          Requirement(RequirementKind::SameType,
                      subjectType, constraintType));
      break;
    }
    case Demangle::Node::Kind::DependentGenericLayoutRequirement: {
      auto kindChild = child->getChild(1);
      if (kindChild->getKind() != Demangle::Node::Kind::Identifier)
        return CanGenericSignature();

      auto kind = llvm::StringSwitch<Optional<
          LayoutConstraintKind>>(kindChild->getText())
        .Case("U", LayoutConstraintKind::UnknownLayout)
        .Case("R", LayoutConstraintKind::RefCountedObject)
        .Case("N", LayoutConstraintKind::NativeRefCountedObject)
        .Case("C", LayoutConstraintKind::Class)
        .Case("D", LayoutConstraintKind::NativeClass)
        .Case("T", LayoutConstraintKind::Trivial)
        .Cases("E", "e", LayoutConstraintKind::TrivialOfExactSize)
        .Cases("M", "m", LayoutConstraintKind::TrivialOfAtMostSize)
        .Default(None);

      if (!kind)
        return CanGenericSignature();

      LayoutConstraint layout;

      if (kind != LayoutConstraintKind::TrivialOfExactSize &&
          kind != LayoutConstraintKind::TrivialOfAtMostSize) {
        layout = LayoutConstraint::getLayoutConstraint(*kind, Ctx);
      } else {
        auto size = child->getChild(2)->getIndex();
        auto alignment = 0;

        if (child->getNumChildren() == 4)
          alignment = child->getChild(3)->getIndex();

        layout = LayoutConstraint::getLayoutConstraint(*kind, size, alignment,
                                                       Ctx);
      }

      requirements.push_back(
          Requirement(RequirementKind::Layout, subjectType, layout));
      break;
    }
    default:
      return CanGenericSignature();
    }
  }

  return evaluateOrDefault(
      Ctx.evaluator,
      AbstractGenericSignatureRequest{
        nominalDecl->getGenericSignature().getPointer(), { },
        std::move(requirements)},
      GenericSignature())->getCanonicalSignature();
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

  case Demangle::Node::Kind::Module:
    return findModule(node);

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
      if (!moduleNode) return nullptr;

      auto module = findModule(moduleNode);
      if (!module) return nullptr;

      // Look up the local type by its mangling.
      auto mangledName = Demangle::mangleNode(node);
      auto decl = module->lookupLocalType(mangledName);
      if (!decl) return nullptr;

      return dyn_cast<DeclContext>(decl);
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
        Ctx.getIdentifier(declNameNode->getChild(0)->getText());

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
                                    foreignModuleKind.getValue(),
                                    node->getKind());
      }
    }

    DeclContext *dc = findDeclContext(node->getChild(0));
    if (!dc) {
      return nullptr;
    }

    return findTypeDecl(dc, Ctx.getIdentifier(name),
                        privateDiscriminator, node->getKind());
  }

  case Demangle::Node::Kind::Global:
    return findDeclContext(node->getChild(0));

  case Demangle::Node::Kind::Extension: {
    auto *moduleDecl = dyn_cast_or_null<ModuleDecl>(
        findDeclContext(node->getChild(0)));
    if (!moduleDecl)
      return nullptr;

    auto *nominalDecl = dyn_cast_or_null<NominalTypeDecl>(
        findDeclContext(node->getChild(1)));
    if (!nominalDecl)
      return nullptr;

    CanGenericSignature genericSig;
    if (node->getNumChildren() > 2)
      genericSig = demangleGenericSignature(nominalDecl, node->getChild(2));

    for (auto *ext : nominalDecl->getExtensions()) {
      if (ext->getParentModule() != moduleDecl)
        continue;

      if (!ext->isConstrainedExtension()) {
        if (!genericSig)
          return ext;
        continue;
      }

      if (ext->getGenericSignature()->getCanonicalSignature()
          == genericSig) {
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

  return result;
}

static Optional<ClangTypeKind>
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
    return None;
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

  Optional<ClangTypeKind> lookupKind = getClangTypeKindForNodeKind(kind);
  if (!lookupKind)
    return nullptr;

  switch (foreignKind) {
  case ForeignModuleKind::SynthesizedByImporter:
    if (!relatedEntityKind.empty()) {
      importer->lookupRelatedEntity(name, *lookupKind, relatedEntityKind,
                                    found);
      break;
    }
    importer->lookupValue(Ctx.getIdentifier(name), consumer);
    if (consumer.Result)
      consumer.Result = getAcceptableTypeDeclCandidate(consumer.Result, kind);
    break;
  case ForeignModuleKind::Imported:
    importer->lookupTypeDecl(name, *lookupKind, found);
  }

  return consumer.Result;
}

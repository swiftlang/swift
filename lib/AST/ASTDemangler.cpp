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
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Demangling/Demangler.h"

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


Type
ASTBuilder::createBuiltinType(StringRef builtinName,
                              StringRef mangledName) {
  if (builtinName.startswith(BUILTIN_TYPE_NAME_PREFIX)) {
    SmallVector<ValueDecl *, 1> decls;

    ModuleDecl::AccessPathTy accessPath;
    StringRef strippedName =
          builtinName.drop_front(strlen(BUILTIN_TYPE_NAME_PREFIX));
    Ctx.TheBuiltinModule->lookupValue(accessPath,
                                      Ctx.getIdentifier(strippedName),
                                      NLKind::QualifiedLookup,
                                      decls);
    
    if (decls.size() == 1 && isa<TypeDecl>(decls[0]))
      return cast<TypeDecl>(decls[0])->getDeclaredInterfaceType();
  }

  return Type();
}

NominalTypeDecl *
ASTBuilder::createNominalTypeDecl(StringRef mangledName) {
  Demangle::Demangler Dem;
  Demangle::NodePointer node = Dem.demangleType(mangledName);
  if (!node) return nullptr;

  return createNominalTypeDecl(node);
}

ProtocolDecl *
ASTBuilder::createProtocolDecl(const Demangle::NodePointer &node) {
  return dyn_cast_or_null<ProtocolDecl>(createNominalTypeDecl(node));
}

Type ASTBuilder::createNominalType(NominalTypeDecl *decl) {
  // If the declaration is generic, fail.
  if (decl->isGenericContext())
    return Type();

  return decl->getDeclaredType();
}

Type ASTBuilder::createNominalType(NominalTypeDecl *decl, Type parent) {
  // If the declaration is generic, fail.
  if (decl->getGenericParams())
    return Type();

  // Imported types can be renamed to be members of other (non-generic)
  // types, but the mangling does not have a parent type. Just use the
  // declared type directly in this case and skip the parent check below.
  if (decl->hasClangNode() && !decl->isGenericContext())
    return decl->getDeclaredType();

  // Validate the parent type.
  if (!validateNominalParent(decl, parent))
    return Type();

  return NominalType::get(decl, parent, Ctx);
}

Type ASTBuilder::createBoundGenericType(NominalTypeDecl *decl,
                                        ArrayRef<Type> args) {
  // If the declaration isn't generic, fail.
  if (!decl->isGenericContext())
    return Type();

  // Build a SubstitutionMap.
  auto *genericSig = decl->getGenericSignature();

  SmallVector<GenericTypeParamType *, 4> genericParams;
  genericSig->forEachParam([&](GenericTypeParamType *gp, bool canonical) {
    if (canonical)
      genericParams.push_back(gp);
  });
  if (genericParams.size() != args.size())
    return Type();

  auto subMap = SubstitutionMap::get(
      genericSig,
      [&](SubstitutableType *t) -> Type {
        for (unsigned i = 0, e = genericParams.size(); i < e; ++i) {
          if (t->isEqual(genericParams[i]))
            return args[i];
        }
        return Type();
      },
      // FIXME: Wrong module
      LookUpConformanceInModule(decl->getParentModule()));

  auto origType = decl->getDeclaredInterfaceType();

  // FIXME: We're not checking that the type satisfies the generic
  // requirements of the signature here.
  auto substType = origType.subst(subMap);
  return substType;
}

Type ASTBuilder::createBoundGenericType(NominalTypeDecl *decl,
                                        ArrayRef<Type> args,
                                        Type parent) {
  // If the declaration isn't generic, fail.
  if (!decl->getGenericParams())
    return Type();

  // Validate the parent type.
  if (!validateNominalParent(decl, parent))
    return Type();

  return BoundGenericType::get(decl, parent, args);
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

  auto einfo = AnyFunctionType::ExtInfo(representation,
                                        /*throws*/ flags.throws());
  if (flags.isEscaping())
    einfo = einfo.withNoEscape(false);
  else
    einfo = einfo.withNoEscape(true);

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

  return FunctionType::get(funcParams, output, einfo);
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

Type ASTBuilder::createExistentialMetatypeType(Type instance) {
  if (!instance->isAnyExistentialType())
    return Type();
  return ExistentialMetatypeType::get(instance);
}

Type ASTBuilder::createMetatypeType(Type instance,
                                    bool wasAbstract) {
  // FIXME: Plumb through metatype representation and generalize silly
  // 'wasAbstract' flag
  return MetatypeType::get(instance);
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

  auto flags = OptionSet<NominalTypeDecl::LookupDirectFlags>();
  flags |= NominalTypeDecl::LookupDirectFlags::IgnoreNewExtensions;
  for (auto member : protocol->lookupDirect(Ctx.getIdentifier(member),
                                            flags)) {
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member))
      return DependentMemberType::get(base, assocType);
  }

  return Type();
}

#define REF_STORAGE(Name, ...) \
Type ASTBuilder::create##Name##StorageType(Type base) { \
  if (!base->allowsOwnership()) \
    return Type(); \
  return Name##StorageType::get(base, Ctx); \
}
#include "swift/AST/ReferenceStorage.def"

Type ASTBuilder::createSILBoxType(Type base) {
  return SILBoxType::get(base->getCanonicalType());
}

Type ASTBuilder::createObjCClassType(StringRef name) {
  auto typeDecl =
      findForeignNominalTypeDecl(name, /*relatedEntityKind*/{},
                                 ForeignModuleKind::Imported,
                                 Demangle::Node::Kind::Class);
  if (!typeDecl) return Type();
  return createNominalType(typeDecl, /*parent*/ Type());
}

ProtocolDecl *ASTBuilder::createObjCProtocolDecl(StringRef name) {
  auto typeDecl =
      findForeignNominalTypeDecl(name, /*relatedEntityKind*/{},
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
  auto typeDecl = createNominalTypeDecl(mangledName);
  if (!typeDecl) return Type();

  return createNominalType(typeDecl, /*parent*/ Type());
}

Type ASTBuilder::getUnnamedForeignClassType() {
  return Type();
}

Type ASTBuilder::getOpaqueType() {
  return Type();
}

bool ASTBuilder::validateNominalParent(NominalTypeDecl *decl,
                                       Type parent) {
  auto parentDecl = decl->getDeclContext()->getSelfNominalTypeDecl();

  // If we don't have a parent type, fast-path.
  if (!parent) {
    return parentDecl == nullptr;
  }

  // We do have a parent type.  If the nominal type doesn't, it's an error.
  if (!parentDecl) {
    return false;
  }

  // FIXME: validate that the parent is a correct application of the
  // enclosing context?
  return true;
}

NominalTypeDecl *
ASTBuilder::getAcceptableNominalTypeCandidate(ValueDecl *decl, 
                                              Demangle::Node::Kind kind) {
  if (kind == Demangle::Node::Kind::Class) {
    return dyn_cast<ClassDecl>(decl);
  } else if (kind == Demangle::Node::Kind::Enum) {
    return dyn_cast<EnumDecl>(decl);
  } else if (kind == Demangle::Node::Kind::Protocol) {
    return dyn_cast<ProtocolDecl>(decl);
  } else {
    assert(kind == Demangle::Node::Kind::Structure);
    return dyn_cast<StructDecl>(decl);
  }
}

DeclContext *ASTBuilder::getNotionalDC() {
  if (!NotionalDC) {
    NotionalDC = ModuleDecl::create(Ctx.getIdentifier(".RemoteAST"), Ctx);
    NotionalDC = new (Ctx) TopLevelCodeDecl(NotionalDC);
  }
  return NotionalDC;
}

NominalTypeDecl *
ASTBuilder::createNominalTypeDecl(const Demangle::NodePointer &node) {
  auto DC = findDeclContext(node);
  if (!DC)
    return nullptr;

  auto decl = dyn_cast<NominalTypeDecl>(DC);
  if (!decl) return nullptr;

  return decl;
}

ModuleDecl *
ASTBuilder::findModule(const Demangle::NodePointer &node) {
  assert(node->getKind() == Demangle::Node::Kind::Module);
  const auto &moduleName = node->getText();
  return Ctx.getModuleByName(moduleName);
}

Demangle::NodePointer
ASTBuilder::findModuleNode(const Demangle::NodePointer &node) {
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
ASTBuilder::getForeignModuleKind(const Demangle::NodePointer &node) {
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
    const Demangle::NodePointer &node) {
  GenericSignatureBuilder builder(Ctx);
  builder.addGenericSignature(nominalDecl->getGenericSignature());

  for (auto &child : *node) {
    if (child->getKind() ==
          Demangle::Node::Kind::DependentGenericParamCount)
      continue;

    if (child->getNumChildren() != 2)
      return CanGenericSignature();
    auto subjectType = swift::Demangle::decodeMangledType(
        *this, child->getChild(0));
    auto constraintType = swift::Demangle::decodeMangledType(
        *this, child->getChild(1));
    if (!subjectType || !constraintType)
      return CanGenericSignature();

    auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();

    switch (child->getKind()) {
    case Demangle::Node::Kind::DependentGenericConformanceRequirement: {
      builder.addRequirement(
          Requirement(constraintType->isExistentialType()
                        ? RequirementKind::Conformance
                        : RequirementKind::Superclass,
                      subjectType, constraintType),
          source, nullptr);
      break;
    }
    case Demangle::Node::Kind::DependentGenericSameTypeRequirement: {
      builder.addRequirement(
          Requirement(RequirementKind::SameType,
                      subjectType, constraintType),
          source, nullptr);
      break;
    }
    default:
      return CanGenericSignature();
    }
  }

  return std::move(builder).computeGenericSignature(SourceLoc())
      ->getCanonicalSignature();
}

DeclContext *
ASTBuilder::findDeclContext(const Demangle::NodePointer &node) {
  switch (node->getKind()) {
  case Demangle::Node::Kind::DeclContext:
  case Demangle::Node::Kind::Type:
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
      name = declNameNode->getChild(0)->getText();
      relatedEntityKind = declNameNode->getText();

    // Ignore any other decl-name productions for now.
    } else {
      return nullptr;
    }

    DeclContext *dc = findDeclContext(node->getChild(0));
    if (!dc) {
      // Do some backup logic for foreign type declarations.
      if (privateDiscriminator.empty()) {
        if (auto foreignModuleKind = getForeignModuleKind(node->getChild(0))) {
          return findForeignNominalTypeDecl(name, relatedEntityKind,
                                            foreignModuleKind.getValue(),
                                            node->getKind());
        }
      }
      return nullptr;
    }

    return findNominalTypeDecl(dc, Ctx.getIdentifier(name),
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

NominalTypeDecl *
ASTBuilder::findNominalTypeDecl(DeclContext *dc,
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

  NominalTypeDecl *result = nullptr;
  for (auto decl : lookupResults) {
    // Ignore results that are not the right kind of nominal type declaration.
    NominalTypeDecl *candidate = getAcceptableNominalTypeCandidate(decl, kind);
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

NominalTypeDecl *
ASTBuilder::findForeignNominalTypeDecl(StringRef name,
                                       StringRef relatedEntityKind,
                                       ForeignModuleKind foreignKind,
                                       Demangle::Node::Kind kind) {
  // Check to see if we have an importer loaded.
  auto importer = static_cast<ClangImporter *>(Ctx.getClangModuleLoader());
  if (!importer) return nullptr;

  // Find the unique declaration that has the right kind.
  struct Consumer : VisibleDeclConsumer {
    Demangle::Node::Kind ExpectedKind;
    NominalTypeDecl *Result = nullptr;
    bool HadError = false;

    explicit Consumer(Demangle::Node::Kind kind) : ExpectedKind(kind) {}

    void foundDecl(ValueDecl *decl, DeclVisibilityKind reason) override {
      if (HadError) return;
      if (decl == Result) return;
      if (!Result) {
        // A synthesized type from the Clang importer may resolve to a
        // compatibility alias.
        if (auto resultAlias = dyn_cast<TypeAliasDecl>(decl)) {
          if (resultAlias->isCompatibilityAlias()) {
            Result = resultAlias->getUnderlyingTypeLoc().getType()
                                ->getAnyNominal();
          }
        } else {
          Result = dyn_cast<NominalTypeDecl>(decl);
        }
        HadError |= !Result;
      } else {
        HadError = true;
        Result = nullptr;
      }
    }
  } consumer(kind);

  switch (foreignKind) {
  case ForeignModuleKind::SynthesizedByImporter:
    if (!relatedEntityKind.empty()) {
      Optional<ClangTypeKind> lookupKind = getClangTypeKindForNodeKind(kind);
      if (!lookupKind)
        return nullptr;
      importer->lookupRelatedEntity(name, lookupKind.getValue(),
                                    relatedEntityKind, [&](TypeDecl *found) {
        consumer.foundDecl(found, DeclVisibilityKind::VisibleAtTopLevel);
      });
      break;
    }
    importer->lookupValue(Ctx.getIdentifier(name), consumer);
    if (consumer.Result)
      consumer.Result = getAcceptableNominalTypeCandidate(consumer.Result,kind);
    break;
  case ForeignModuleKind::Imported: {
    Optional<ClangTypeKind> lookupKind = getClangTypeKindForNodeKind(kind);
    if (!lookupKind)
      return nullptr;
    importer->lookupTypeDecl(name, lookupKind.getValue(),
                             [&](TypeDecl *found) {
      consumer.foundDecl(found, DeclVisibilityKind::VisibleAtTopLevel);
    });
  }
  }

  return consumer.Result;
}

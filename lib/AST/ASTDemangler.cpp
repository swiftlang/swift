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

#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
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
ASTBuilder::createBuiltinType(const std::string &mangledName) {
  // TODO
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

  // Validate the parent type.
  if (!validateNominalParent(decl, parent))
    return Type();

  return NominalType::get(decl, parent, Ctx);
}

Type ASTBuilder::createBoundGenericType(
       NominalTypeDecl *decl,
       ArrayRef<Type> args,
       ArrayRef<std::pair<unsigned, BuiltProtocolConformance>> retroactive) {
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

Type ASTBuilder::createBoundGenericType(
    NominalTypeDecl *decl,
    ArrayRef<Type> args,
    ArrayRef<std::pair<unsigned, BuiltProtocolConformance>> retroactive,
    Type parent) {
  // If the declaration isn't generic, fail.
  if (!decl->getGenericParams())
    return Type();

  // Validate the parent type.
  if (!validateNominalParent(decl, parent))
    return Type();

  // Make a generic type repr that's been resolved to this decl.
  TypeReprList genericArgReprs(args);
  auto genericRepr = GenericIdentTypeRepr::create(Ctx, SourceLoc(),
                                                  decl->getName(),
                                                  genericArgReprs.getList(),
                                                  SourceRange());
  // FIXME
  genericRepr->setValue(decl, nullptr);

  Type genericType;

  // If we have a parent type, we need to build a compound type repr.
  if (parent) {
    // Life would be much easier if we could just use a FixedTypeRepr for
    // the parent.  But we can't!  So we have to recursively expand
    // like this; and recursing with a lambda isn't impossible, so it gets
    // even worse.
    SmallVector<Type, 4> ancestry;
    for (auto p = parent; p; p = p->getNominalParent()) {
      ancestry.push_back(p);
    }

    struct GenericRepr {
      TypeReprList GenericArgs;
      GenericIdentTypeRepr *Ident;

      GenericRepr(const ASTContext &Ctx, BoundGenericType *type)
        : GenericArgs(type->getGenericArgs()),
          Ident(GenericIdentTypeRepr::create(Ctx, SourceLoc(),
                                             type->getDecl()->getName(),
                                             GenericArgs.getList(),
                                             SourceRange())) {
        // FIXME
        Ident->setValue(type->getDecl(), nullptr);
      }

      // SmallVector::emplace_back will never need to call this because
      // we reserve the right size, but it does try statically.
      GenericRepr(const GenericRepr &other) : GenericArgs({}), Ident(nullptr) {
        llvm_unreachable("should not be called dynamically");
      }
    };

    // Pre-allocate the component vectors so that we can form references
    // into them safely.
    SmallVector<SimpleIdentTypeRepr, 4> simpleComponents;
    SmallVector<GenericRepr, 4> genericComponents;
    simpleComponents.reserve(ancestry.size());
    genericComponents.reserve(ancestry.size());

    // Build the parent hierarchy.
    SmallVector<ComponentIdentTypeRepr*, 4> componentReprs;
    for (size_t i = ancestry.size(); i != 0; --i) {
      Type p = ancestry[i - 1];
      if (auto boundGeneric = p->getAs<BoundGenericType>()) {
        genericComponents.emplace_back(Ctx, boundGeneric);
        componentReprs.push_back(genericComponents.back().Ident);
      } else {
        auto nominal = p->castTo<NominalType>();
        simpleComponents.emplace_back(SourceLoc(),
                                      nominal->getDecl()->getName());
        // FIXME
        simpleComponents.back().setValue(nominal->getDecl(), nullptr);
        componentReprs.push_back(&simpleComponents.back());
      }
    }
    componentReprs.push_back(genericRepr);

    auto compoundRepr = CompoundIdentTypeRepr::create(Ctx, componentReprs);
    genericType = checkTypeRepr(compoundRepr);
  } else {
    genericType = checkTypeRepr(genericRepr);
  }

  // If type-checking failed, we've failed.
  if (!genericType) return Type();

  // Validate that we used the right decl.
  if (auto bgt = genericType->getAs<BoundGenericType>()) {
    if (bgt->getDecl() != decl)
      return Type();
  }

  return genericType;
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

Type ASTBuilder::checkTypeRepr(TypeRepr *repr) {
  DeclContext *dc = getNotionalDC();

  TypeLoc loc(repr);
  if (performTypeLocChecking(Ctx, loc, dc, /*diagnose*/ false))
    return Type();

  return loc.getType();
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
  if (node->getKind() == Demangle::Node::Kind::Module)
    return node;

  if (!node->hasChildren()) return nullptr;
  const auto &child = node->getFirstChild();
  if (child->getKind() != Demangle::Node::Kind::DeclContext)
    return nullptr;

  return findModuleNode(child->getFirstChild());
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

  // Bail out on other kinds of contexts.
  // TODO: extensions
  // TODO: local contexts
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

auto ASTBuilder::createProtocolConformanceRef(Type conformingType,
                                              ProtocolDecl *protocol,
                                              StringRef module)
    -> BuiltProtocolConformanceRef {
  // FIXME: Implement lookup.
  return nullptr;
}

auto ASTBuilder::createProtocolConformance(
                      Type conformingType,
                      NormalProtocolConformance *normal,
                      ArrayRef<BuiltProtocolConformance> conditionalReqs)
    -> BuiltProtocolConformance {
  // FIXME: Implement specialization.
  return nullptr;
}

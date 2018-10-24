//===--- RemoteAST.cpp ----------------------------------------------------===//
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
// This file implements the RemoteAST interface.
//
//===----------------------------------------------------------------------===//

#include "swift/RemoteAST/RemoteAST.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Mangler.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/StringSwitch.h"

// TODO: Develop a proper interface for this.
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/SILOptions.h"
#include "swift/SIL/SILModule.h"
#include "../IRGen/IRGenModule.h"
#include "../IRGen/FixedTypeInfo.h"
#include "../IRGen/GenClass.h"
#include "../IRGen/GenStruct.h"
#include "../IRGen/GenTuple.h"
#include "../IRGen/MemberAccessStrategy.h"

using namespace swift;
using namespace swift::remote;
using namespace swift::remoteAST;

using irgen::Alignment;
using irgen::Size;

static inline RemoteAddress operator+(RemoteAddress address, Size offset) {
  return RemoteAddress(address.getAddressData() + offset.getValue());
}

namespace {

/// A "minimal" class for querying IRGen.
struct IRGenContext {
  IRGenOptions IROpts;
  SILOptions SILOpts;
  std::unique_ptr<SILModule> SILMod;
  llvm::LLVMContext LLVMContext;
  irgen::IRGenerator IRGen;
  irgen::IRGenModule IGM;

private:
  IRGenContext(ASTContext &ctx, ModuleDecl *module)
    : IROpts(createIRGenOptions()),
      SILMod(SILModule::createEmptyModule(module, SILOpts)),
      IRGen(IROpts, *SILMod),
      IGM(IRGen, IRGen.createTargetMachine(), LLVMContext) {}

  static IRGenOptions createIRGenOptions() {
    IRGenOptions IROpts;
    IROpts.EnableResilienceBypass = true;
    return IROpts;
  }

public:
  static std::unique_ptr<IRGenContext>
  create(ASTContext &ctx, DeclContext *nominalDC) {
    auto module = nominalDC->getParentModule();
    return std::unique_ptr<IRGenContext>(new IRGenContext(ctx, module));
  }
};

/// An implementation of MetadataReader's BuilderType concept that
/// just finds and builds things in the AST.
class RemoteASTTypeBuilder {
  ASTContext &Ctx;
  Demangle::NodeFactory Factory;

  /// The notional context in which we're writing and type-checking code.
  /// Created lazily.
  DeclContext *NotionalDC = nullptr;

  Optional<Failure> CurFailure;

public:
  using BuiltType = swift::Type;
  using BuiltNominalTypeDecl = swift::NominalTypeDecl *;
  using BuiltProtocolDecl = swift::ProtocolDecl *;
  explicit RemoteASTTypeBuilder(ASTContext &ctx) : Ctx(ctx) {}

  std::unique_ptr<IRGenContext> createIRGenContext() {
    return IRGenContext::create(Ctx, getNotionalDC());
  }

  template <class Result, class FailureKindTy, class... FailureArgTys>
  Result fail(FailureKindTy kind, FailureArgTys &&...failureArgs) {
    if (!CurFailure) {
      CurFailure.emplace(kind, std::forward<FailureArgTys>(failureArgs)...);
    }
    return Result();
  }

  template <class T, class DefaultFailureKindTy, class... DefaultFailureArgTys>
  Result<T> getFailureAsResult(DefaultFailureKindTy defaultFailureKind,
                               DefaultFailureArgTys &&...defaultFailureArgs) {
    // If we already have a failure, use that.
    if (CurFailure) {
      Result<T> result = std::move(*CurFailure);
      CurFailure.reset();
      return result;
    }

    // Otherwise, use the default failure.
    return Result<T>::emplaceFailure(defaultFailureKind,
               std::forward<DefaultFailureArgTys>(defaultFailureArgs)...);
  }

  Demangle::NodeFactory &getNodeFactory() { return Factory; }

  Type createBuiltinType(const std::string &mangledName) {
    // TODO
    return Type();
  }

  NominalTypeDecl *createNominalTypeDecl(StringRef mangledName) {
    Demangle::Demangler Dem;
    Demangle::NodePointer node = Dem.demangleType(mangledName);
    if (!node) return nullptr;

    return createNominalTypeDecl(node);
  }
  
  NominalTypeDecl *createNominalTypeDecl(const Demangle::NodePointer &node);

  ProtocolDecl *createProtocolDecl(const Demangle::NodePointer &node) {
    return dyn_cast_or_null<ProtocolDecl>(createNominalTypeDecl(node));
  }

  Type createNominalType(NominalTypeDecl *decl) {
    // If the declaration is generic, fail.
    if (decl->isGenericContext())
      return Type();

    return decl->getDeclaredType();
  }

  Type createNominalType(NominalTypeDecl *decl, Type parent) {
    // If the declaration is generic, fail.
    if (decl->getGenericParams())
      return Type();

    // Validate the parent type.
    if (!validateNominalParent(decl, parent))
      return Type();

    return NominalType::get(decl, parent, Ctx);
  }

  Type createBoundGenericType(NominalTypeDecl *decl, ArrayRef<Type> args) {
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

  Type createBoundGenericType(NominalTypeDecl *decl, ArrayRef<Type> args,
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

  Type createTupleType(ArrayRef<Type> eltTypes, StringRef labels,
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

  Type createFunctionType(ArrayRef<remote::FunctionParam<Type>> params,
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

  Type createProtocolCompositionType(ArrayRef<ProtocolDecl *> protocols,
                                     Type superclass,
                                     bool isClassBound) {
    std::vector<Type> members;
    for (auto protocol : protocols)
      members.push_back(protocol->getDeclaredType());
    if (superclass && superclass->getClassOrBoundGenericClass())
      members.push_back(superclass);
    return ProtocolCompositionType::get(Ctx, members, isClassBound);
  }

  Type createExistentialMetatypeType(Type instance) {
    if (!instance->isAnyExistentialType())
      return Type();
    return ExistentialMetatypeType::get(instance);
  }

  Type createMetatypeType(Type instance, bool wasAbstract=false) {
    // FIXME: Plumb through metatype representation and generalize silly
    // 'wasAbstract' flag
    return MetatypeType::get(instance);
  }

  Type createGenericTypeParameterType(unsigned depth, unsigned index) {
    return GenericTypeParamType::get(depth, index, Ctx);
  }

  Type createDependentMemberType(StringRef member, Type base,
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
  Type create##Name##StorageType(Type base) { \
    if (!base->allowsOwnership()) \
      return Type(); \
    return Name##StorageType::get(base, Ctx); \
  }
#include "swift/AST/ReferenceStorage.def"

  Type createSILBoxType(Type base) {
    return SILBoxType::get(base->getCanonicalType());
  }

  Type createObjCClassType(StringRef name) {
    auto typeDecl =
        findForeignNominalTypeDecl(name, /*relatedEntityKind*/{},
                                   ForeignModuleKind::Imported,
                                   Demangle::Node::Kind::Class);
    if (!typeDecl) return Type();
    return createNominalType(typeDecl, /*parent*/ Type());
  }

  Type createForeignClassType(StringRef mangledName) {
    auto typeDecl = createNominalTypeDecl(mangledName);
    if (!typeDecl) return Type();

    return createNominalType(typeDecl, /*parent*/ Type());
  }

  Type getUnnamedForeignClassType() {
    return Type();
  }

  Type getOpaqueType() {
    return Type();
  }

private:
  bool validateNominalParent(NominalTypeDecl *decl, Type parent) {
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

  DeclContext *findDeclContext(const Demangle::NodePointer &node);
  ModuleDecl *findModule(const Demangle::NodePointer &node);
  Demangle::NodePointer findModuleNode(const Demangle::NodePointer &node);

  enum class ForeignModuleKind {
    Imported,
    SynthesizedByImporter
  };

  Optional<ForeignModuleKind>
  getForeignModuleKind(const Demangle::NodePointer &node);

  NominalTypeDecl *findNominalTypeDecl(DeclContext *dc,
                                       Identifier name,
                                       Identifier privateDiscriminator,
                                       Demangle::Node::Kind kind);
  NominalTypeDecl *findForeignNominalTypeDecl(StringRef name,
                                              StringRef relatedEntityKind,
                                              ForeignModuleKind lookupKind,
                                              Demangle::Node::Kind kind);

  Type checkTypeRepr(TypeRepr *repr) {
    DeclContext *dc = getNotionalDC();

    TypeLoc loc(repr);
    if (performTypeLocChecking(Ctx, loc, dc, /*diagnose*/ false))
      return Type();

    return loc.getType();
  }

  static NominalTypeDecl *getAcceptableNominalTypeCandidate(ValueDecl *decl, 
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

  DeclContext *getNotionalDC() {
    if (!NotionalDC) {
      NotionalDC = ModuleDecl::create(Ctx.getIdentifier(".RemoteAST"), Ctx);
      NotionalDC = new (Ctx) TopLevelCodeDecl(NotionalDC);
    }
    return NotionalDC;
  }

  class TypeReprList {
    SmallVector<FixedTypeRepr, 4> Reprs;
    SmallVector<TypeRepr*, 4> Refs;

  public:
    explicit TypeReprList(ArrayRef<Type> types) {
      Reprs.reserve(types.size());
      Refs.reserve(types.size());

      for (auto type : types) {
        Reprs.emplace_back(type, SourceLoc());
        Refs.push_back(&Reprs.back());
      }
    }

    ArrayRef<TypeRepr*> getList() const {
      return Refs;
    }
  };
};
} // end anonymous namespace

NominalTypeDecl *
RemoteASTTypeBuilder::createNominalTypeDecl(const Demangle::NodePointer &node) {
  auto DC = findDeclContext(node);
  if (!DC) {
    return fail<NominalTypeDecl*>(Failure::CouldNotResolveTypeDecl,
                                  Demangle::mangleNode(node));
  }

  auto decl = dyn_cast<NominalTypeDecl>(DC);
  if (!decl) return nullptr;

  return decl;
}

ModuleDecl *RemoteASTTypeBuilder::findModule(const Demangle::NodePointer &node){
  assert(node->getKind() == Demangle::Node::Kind::Module);
  const auto &moduleName = node->getText();
  return Ctx.getModuleByName(moduleName);
}

Demangle::NodePointer
RemoteASTTypeBuilder::findModuleNode(const Demangle::NodePointer &node) {
  if (node->getKind() == Demangle::Node::Kind::Module)
    return node;

  if (!node->hasChildren()) return nullptr;
  const auto &child = node->getFirstChild();
  if (child->getKind() != Demangle::Node::Kind::DeclContext)
    return nullptr;

  return findModuleNode(child->getFirstChild());
}

Optional<RemoteASTTypeBuilder::ForeignModuleKind>
RemoteASTTypeBuilder::getForeignModuleKind(const Demangle::NodePointer &node) {
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
RemoteASTTypeBuilder::findDeclContext(const Demangle::NodePointer &node) {
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
RemoteASTTypeBuilder::findNominalTypeDecl(DeclContext *dc,
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
RemoteASTTypeBuilder::findForeignNominalTypeDecl(StringRef name,
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

namespace {

/// The basic implementation of the RemoteASTContext interface.
/// The template subclasses do target-specific logic.
class RemoteASTContextImpl {
  std::unique_ptr<IRGenContext> IRGen;
public:
  RemoteASTContextImpl() = default;
  virtual ~RemoteASTContextImpl() = default;

  virtual Result<Type>
  getTypeForRemoteTypeMetadata(RemoteAddress metadata, bool skipArtificial) = 0;
  virtual Result<MetadataKind>
  getKindForRemoteTypeMetadata(RemoteAddress metadata) = 0;
  virtual Result<NominalTypeDecl*>
  getDeclForRemoteNominalTypeDescriptor(RemoteAddress descriptor) = 0;
  virtual Result<RemoteAddress>
  getHeapMetadataForObject(RemoteAddress object) = 0;
  virtual Result<std::pair<Type, RemoteAddress>>
  getDynamicTypeAndAddressForExistential(RemoteAddress object,
                                         Type staticType) = 0;

  Result<uint64_t>
  getOffsetOfMember(Type type, RemoteAddress optMetadata, StringRef memberName){
    // Sanity check: obviously invalid arguments.
    if (!type || memberName.empty())
      return Result<uint64_t>::emplaceFailure(Failure::BadArgument);

    // Sanity check: if the caller gave us a dependent type, there's no way
    // we can handle that.
    if (type->hasTypeParameter() || type->hasArchetype())
      return Result<uint64_t>::emplaceFailure(Failure::DependentArgument);

    // Split into cases.
    if (auto typeDecl = type->getNominalOrBoundGenericNominal()) {
      return getOffsetOfField(type, typeDecl, optMetadata, memberName);
    } else if (auto tupleType = type->getAs<TupleType>()) {
      return getOffsetOfTupleElement(tupleType, optMetadata, memberName);
    } else {
      return Result<uint64_t>::emplaceFailure(Failure::TypeHasNoSuchMember,
                                              memberName);
    }
  }

protected:
  template <class T>
  Result<T> getFailure() {
    return getBuilder().getFailureAsResult<T>(Failure::Unknown);
  }

  template <class T, class KindTy, class... ArgTys>
  Result<T> fail(KindTy kind, ArgTys &&...args) {
    return Result<T>::emplaceFailure(kind, std::forward<ArgTys>(args)...);
  }

private:
  virtual RemoteASTTypeBuilder &getBuilder() = 0;
  virtual MemoryReader &getReader() = 0;
  virtual bool readWordOffset(RemoteAddress address, int64_t *offset) = 0;
  virtual std::unique_ptr<IRGenContext> createIRGenContext() = 0;
  virtual Result<uint64_t>
  getOffsetOfTupleElementFromMetadata(RemoteAddress metadata,
                                      unsigned elementIndex) = 0;
  virtual Result<uint64_t>
  getOffsetOfFieldFromMetadata(RemoteAddress metadata,
                               StringRef memberName) = 0;

  IRGenContext *getIRGen() {
    if (!IRGen) IRGen = createIRGenContext();
    return IRGen.get();
  }

  Result<uint64_t>
  getOffsetOfField(Type type, NominalTypeDecl *typeDecl,
                   RemoteAddress optMetadata, StringRef memberName) {
    if (!isa<StructDecl>(typeDecl) && !isa<ClassDecl>(typeDecl))
      return fail<uint64_t>(Failure::Unimplemented,
                            "access members of this kind of type");

    // Try to find the member.
    VarDecl *member = findField(typeDecl, memberName);

    // If we found a member, try to find its offset statically.
    if (member) {
      if (auto irgen = getIRGen()) {
        return getOffsetOfFieldFromIRGen(irgen->IGM, type, typeDecl,
                                          optMetadata, member);
      }
    }

    // Try searching the metadata for a member with the given name.
    if (optMetadata) {
      return getOffsetOfFieldFromMetadata(optMetadata, memberName);
    }

    // Okay, that's everything we know how to try.

    // Use a specialized diagnostic if we couldn't find any such member.
    if (!member) {
      return fail<uint64_t>(Failure::TypeHasNoSuchMember, memberName);
    }

    return fail<uint64_t>(Failure::Unknown);
  }

  /// Look for an instance property of the given nominal type that's
  /// known to be stored.
  VarDecl *findField(NominalTypeDecl *typeDecl, StringRef memberName) {
    for (auto field : typeDecl->getStoredProperties()) {
      if (field->getName().str() == memberName)
        return field;
    }
    return nullptr;
  }

  using MemberAccessStrategy = irgen::MemberAccessStrategy;

  Result<uint64_t>
  getOffsetOfFieldFromIRGen(irgen::IRGenModule &IGM, Type type,
                            NominalTypeDecl *typeDecl,
                            RemoteAddress optMetadata, VarDecl *member) {
    SILType loweredTy = IGM.getSILTypes().getLoweredType(type);

    MemberAccessStrategy strategy =
      (isa<StructDecl>(typeDecl)
        ? getPhysicalStructMemberAccessStrategy(IGM, loweredTy, member)
        : getPhysicalClassMemberAccessStrategy(IGM, loweredTy, member));

    switch (strategy.getKind()) {
    case MemberAccessStrategy::Kind::Complex:
      return fail<uint64_t>(Failure::Unimplemented,
                            "access members with complex storage");

    case MemberAccessStrategy::Kind::DirectFixed:
      return uint64_t(strategy.getDirectOffset().getValue());

    case MemberAccessStrategy::Kind::DirectGlobal: {
      RemoteAddress directOffsetAddress =
        getReader().getSymbolAddress(strategy.getDirectGlobalSymbol());
      if (!directOffsetAddress)
        return getFailure<uint64_t>();

      return readDirectOffset(directOffsetAddress,
                              strategy.getDirectOffsetKind());
    }

    case MemberAccessStrategy::Kind::IndirectFixed: {
      // We can't apply indirect offsets without metadata.
      if (!optMetadata)
        return fail<uint64_t>(Failure::Unimplemented,
                              "access generically-offset members without "
                              "metadata");

      Size indirectOffset = strategy.getIndirectOffset();
      return readIndirectOffset(optMetadata, indirectOffset,
                                strategy.getDirectOffsetKind());
    }

    case MemberAccessStrategy::Kind::IndirectGlobal: {
      // We can't apply indirect offsets without metadata.
      if (!optMetadata)
        return fail<uint64_t>(Failure::Unimplemented,
                              "access generically-offset members without "
                              "metadata");

      RemoteAddress indirectOffsetAddress =
        getReader().getSymbolAddress(strategy.getIndirectGlobalSymbol());

      Size indirectOffset;
      if (!readOffset(indirectOffsetAddress,
                      strategy.getIndirectOffsetKind(),
                      indirectOffset))
        return getFailure<uint64_t>();

      return readIndirectOffset(optMetadata, indirectOffset,
                                strategy.getDirectOffsetKind());
    }
    }
    llvm_unreachable("bad member MemberAccessStrategy");
  }

  bool readOffset(RemoteAddress address,
                  MemberAccessStrategy::OffsetKind kind,
                  Size &offset) {
    switch (kind) {
    case MemberAccessStrategy::OffsetKind::Bytes_Word: {
      int64_t rawOffset;
      if (!readWordOffset(address, &rawOffset))
        return false;
      offset = Size(rawOffset);
      return true;
    }
    }
    llvm_unreachable("bad offset kind");
  }

  Result<uint64_t> readIndirectOffset(RemoteAddress metadata,
                                      Size indirectOffset,
                                      MemberAccessStrategy::OffsetKind kind) {
    RemoteAddress directOffsetAddress = metadata + indirectOffset;
    return readDirectOffset(directOffsetAddress, kind);
  }


  Result<uint64_t> readDirectOffset(RemoteAddress directOffsetAddress,
                                    MemberAccessStrategy::OffsetKind kind) {
    Size directOffset;
    if (!readOffset(directOffsetAddress, kind, directOffset))
      return getFailure<uint64_t>();

    return uint64_t(directOffset.getValue());
  }

  /// Read the
  Result<uint64_t>
  getOffsetOfTupleElement(TupleType *type, RemoteAddress optMetadata,
                          StringRef memberName) {
    // Check that the member "name" is a valid index into the tuple.
    unsigned targetIndex;
    if (memberName.getAsInteger(10, targetIndex) ||
        targetIndex >= type->getNumElements())
      return fail<uint64_t>(Failure::TypeHasNoSuchMember, memberName);

    // Fast path: element 0 is always at offset 0.
    if (targetIndex == 0) return uint64_t(0);

    // Create an IRGen instance.
    auto irgen = getIRGen();
    if (!irgen) return Result<uint64_t>::emplaceFailure(Failure::Unknown);
    auto &IGM = irgen->IGM;

    SILType loweredTy = IGM.getSILTypes().getLoweredType(type);

    // If the type has a statically fixed offset, return that.
    if (auto offset =
          irgen::getFixedTupleElementOffset(IGM, loweredTy, targetIndex))
      return offset->getValue();

    // If we have metadata, go load from that.
    if (optMetadata)
      return getOffsetOfTupleElementFromMetadata(optMetadata, targetIndex);

    // Okay, reproduce tuple layout.

    // Find the last element with a known offset.  Note that we don't
    // have to ask IRGen about element 0 because we know its size is zero.
    Size lastOffset = Size(0);
    unsigned lastIndex = targetIndex;
    for (--lastIndex; lastIndex != 0; --lastIndex) {
      if (auto offset =
            irgen::getFixedTupleElementOffset(IGM, loweredTy, lastIndex)) {
        lastOffset = *offset;
        break;
      }
    }

    // Okay, iteratively build up from there.
    for (; ; ++lastIndex) {
      // Try to get the size and alignment of this element.
      SILType eltTy = loweredTy.getTupleElementType(lastIndex);
      auto sizeAndAlignment = getTypeSizeAndAlignment(IGM, eltTy);
      if (!sizeAndAlignment) return getFailure<uint64_t>();

      // Round up to the alignment of the element.
      lastOffset = lastOffset.roundUpToAlignment(sizeAndAlignment->second);

      // If this is the target, we're done.
      if (lastIndex == targetIndex)
        return lastOffset.getValue();

      // Otherwise, skip forward by the size of the element.
      lastOffset += sizeAndAlignment->first;
    }

    llvm_unreachable("didn't reach target index");
  }

  /// Attempt to discover the size and alignment of the given type.
  Optional<std::pair<Size, Alignment>>
  getTypeSizeAndAlignment(irgen::IRGenModule &IGM, SILType eltTy) {
    auto &eltTI = IGM.getTypeInfo(eltTy);
    if (auto fixedTI = dyn_cast<irgen::FixedTypeInfo>(&eltTI)) {
      return std::make_pair(fixedTI->getFixedSize(),
                            fixedTI->getFixedAlignment());
    }

    // TODO: handle resilient types
    return None;
  }
};

/// A template for generating target-specific implementations of the
/// RemoteASTContext interface.
template <class Runtime>
class RemoteASTContextConcreteImpl final : public RemoteASTContextImpl {
  MetadataReader<Runtime, RemoteASTTypeBuilder> Reader;

  RemoteASTTypeBuilder &getBuilder() override {
    return Reader.Builder;
  }

  MemoryReader &getReader() override {
    return *Reader.Reader;
  }

  bool readWordOffset(RemoteAddress address, int64_t *extendedOffset) override {
    using unsigned_size_t = typename Runtime::StoredSize;
    using signed_size_t = typename std::make_signed<unsigned_size_t>::type;
    signed_size_t offset;
    if (!getReader().readInteger(address, &offset))
      return false;

    *extendedOffset = offset;
    return true;
  }

public:
  RemoteASTContextConcreteImpl(std::shared_ptr<MemoryReader> &&reader,
                               ASTContext &ctx)
    : Reader(std::move(reader), ctx) {}

  Result<Type> getTypeForRemoteTypeMetadata(RemoteAddress metadata,
                                            bool skipArtificial) override {
    if (auto result = Reader.readTypeFromMetadata(metadata.getAddressData(),
                                                  skipArtificial))
      return result;
    return getFailure<Type>();
  }

  Result<MetadataKind>
  getKindForRemoteTypeMetadata(RemoteAddress metadata) override {
    auto result = Reader.readKindFromMetadata(metadata.getAddressData());
    if (result)
      return *result;
    return getFailure<MetadataKind>();
  }

  Result<NominalTypeDecl*>
  getDeclForRemoteNominalTypeDescriptor(RemoteAddress descriptor) override {
    if (auto result =
          Reader.readNominalTypeFromDescriptor(descriptor.getAddressData()))
      return result;
    return getFailure<NominalTypeDecl*>();
  }

  std::unique_ptr<IRGenContext> createIRGenContext() override {
    return getBuilder().createIRGenContext();
  }

  Result<uint64_t>
  getOffsetOfTupleElementFromMetadata(RemoteAddress metadata,
                                      unsigned index) override {
    typename Runtime::StoredSize offset;
    if (Reader.readTupleElementOffset(metadata.getAddressData(),
                                      index, &offset))
      return uint64_t(offset);
    return getFailure<uint64_t>();
  }

  Result<uint64_t>
  getOffsetOfFieldFromMetadata(RemoteAddress metadata,
                               StringRef memberName) override {
    // TODO: this would be useful for resilience
    return fail<uint64_t>(Failure::Unimplemented,
                          "look up field offset by name");
  }

  Result<RemoteAddress>
  getHeapMetadataForObject(RemoteAddress object) override {
    auto result = Reader.readMetadataFromInstance(object.getAddressData());
    if (result) return RemoteAddress(*result);
    return getFailure<RemoteAddress>();
  }

  Result<std::pair<Type, RemoteAddress>>
  getDynamicTypeAndAddressClassExistential(RemoteAddress object) {
    auto pointerval = Reader.readPointerValue(object.getAddressData());
    if (!pointerval)
      return getFailure<std::pair<Type, RemoteAddress>>();
    auto result = Reader.readMetadataFromInstance(*pointerval);
    if (!result)
      return getFailure<std::pair<Type, RemoteAddress>>();
    auto typeResult = Reader.readTypeFromMetadata(result.getValue());
    if (!typeResult)
      return getFailure<std::pair<Type, RemoteAddress>>();
    return std::make_pair<Type, RemoteAddress>(std::move(typeResult),
                                               RemoteAddress(*pointerval));
  }

  Result<std::pair<Type, RemoteAddress>>
  getDynamicTypeAndAddressErrorExistential(RemoteAddress object) {
    auto pointerval = Reader.readPointerValue(object.getAddressData());
    if (!pointerval)
      return getFailure<std::pair<Type, RemoteAddress>>();
    auto result =
        Reader.readMetadataAndValueErrorExistential(RemoteAddress(*pointerval));
    if (!result)
      return getFailure<std::pair<Type, RemoteAddress>>();
    RemoteAddress metadataAddress = result->first;
    RemoteAddress valueAddress = result->second;

    auto typeResult =
        Reader.readTypeFromMetadata(metadataAddress.getAddressData());
    if (!typeResult)
      return getFailure<std::pair<Type, RemoteAddress>>();
    return std::make_pair<Type, RemoteAddress>(std::move(typeResult),
                                               std::move(valueAddress));
  }

  Result<std::pair<Type, RemoteAddress>>
  getDynamicTypeAndAddressOpaqueExistential(RemoteAddress object) {
    auto result = Reader.readMetadataAndValueOpaqueExistential(object);
    if (!result)
      return getFailure<std::pair<Type, RemoteAddress>>();
    RemoteAddress metadataAddress = result->first;
    RemoteAddress valueAddress = result->second;

    auto typeResult =
        Reader.readTypeFromMetadata(metadataAddress.getAddressData());
    if (!typeResult)
      return getFailure<std::pair<Type, RemoteAddress>>();
    return std::make_pair<Type, RemoteAddress>(std::move(typeResult),
                                               std::move(valueAddress));
  }

  Result<std::pair<Type, RemoteAddress>>
  getDynamicTypeAndAddressExistentialMetatype(RemoteAddress object) {
    // The value of the address is just the input address.
    // The type is obtained through the following sequence of steps:
    // 1) Loading a pointer from the input address
    // 2) Reading it as metadata and resolving the type
    // 3) Wrapping the resolved type in an existential metatype.
    auto pointerval = Reader.readPointerValue(object.getAddressData());
    if (!pointerval)
      return getFailure<std::pair<Type, RemoteAddress>>();
    auto typeResult = Reader.readTypeFromMetadata(*pointerval);
    if (!typeResult)
      return getFailure<std::pair<Type, RemoteAddress>>();
    auto wrappedType = ExistentialMetatypeType::get(typeResult);
    if (!wrappedType)
      return getFailure<std::pair<Type, RemoteAddress>>();
    return std::make_pair<Type, RemoteAddress>(std::move(wrappedType),
                                               std::move(object));
  }

  /// Resolve the dynamic type and the value address of an existential,
  /// given its address and its static type. For class and error existentials,
  /// this API takes a pointer to the instance reference rather than the
  /// instance reference itself.
  Result<std::pair<Type, RemoteAddress>>
  getDynamicTypeAndAddressForExistential(RemoteAddress object,
                                         Type staticType) override {
    // If this is not an existential, give up.
    if (!staticType->isAnyExistentialType())
      return getFailure<std::pair<Type, RemoteAddress>>();

    // Handle the case where this is an ExistentialMetatype.
    if (!staticType->isExistentialType())
      return getDynamicTypeAndAddressExistentialMetatype(object);

    // This should be an existential type at this point.
    auto layout = staticType->getExistentialLayout();
    switch (layout.getKind()) {
    case ExistentialLayout::Kind::Class:
      return getDynamicTypeAndAddressClassExistential(object);
    case ExistentialLayout::Kind::Error:
      return getDynamicTypeAndAddressErrorExistential(object);
    case ExistentialLayout::Kind::Opaque:
      return getDynamicTypeAndAddressOpaqueExistential(object);
    }
    llvm_unreachable("invalid type kind");
  }
};

} // end anonymous namespace

static RemoteASTContextImpl *createImpl(ASTContext &ctx,
                                      std::shared_ptr<MemoryReader> &&reader) {
  auto &target = ctx.LangOpts.Target;
  assert(target.isArch32Bit() || target.isArch64Bit());

  if (target.isArch32Bit()) {
    using Target = External<RuntimeTarget<4>>;
    return new RemoteASTContextConcreteImpl<Target>(std::move(reader), ctx);
  } else {
    using Target = External<RuntimeTarget<8>>;
    return new RemoteASTContextConcreteImpl<Target>(std::move(reader), ctx);
  }
}

static RemoteASTContextImpl *asImpl(void *impl) {
  return static_cast<RemoteASTContextImpl*>(impl);
}

RemoteASTContext::RemoteASTContext(ASTContext &ctx,
                                   std::shared_ptr<MemoryReader> reader)
  : Impl(createImpl(ctx, std::move(reader))) {
}

RemoteASTContext::~RemoteASTContext() {
  delete asImpl(Impl);
}

Result<Type>
RemoteASTContext::getTypeForRemoteTypeMetadata(RemoteAddress address,
                                               bool skipArtificial) {
  return asImpl(Impl)->getTypeForRemoteTypeMetadata(address, skipArtificial);
}

Result<MetadataKind>
RemoteASTContext::getKindForRemoteTypeMetadata(remote::RemoteAddress address) {
  return asImpl(Impl)->getKindForRemoteTypeMetadata(address);
}

Result<NominalTypeDecl *>
RemoteASTContext::getDeclForRemoteNominalTypeDescriptor(RemoteAddress address) {
  return asImpl(Impl)->getDeclForRemoteNominalTypeDescriptor(address);
}

Result<uint64_t>
RemoteASTContext::getOffsetOfMember(Type type, RemoteAddress optMetadata,
                                    StringRef memberName) {
  return asImpl(Impl)->getOffsetOfMember(type, optMetadata, memberName);
}

Result<remote::RemoteAddress>
RemoteASTContext::getHeapMetadataForObject(remote::RemoteAddress address) {
  return asImpl(Impl)->getHeapMetadataForObject(address);
}

Result<std::pair<Type, remote::RemoteAddress>>
RemoteASTContext::getDynamicTypeAndAddressForExistential(
    remote::RemoteAddress address, Type staticType) {
  return asImpl(Impl)->getDynamicTypeAndAddressForExistential(address,
                                                              staticType);
}

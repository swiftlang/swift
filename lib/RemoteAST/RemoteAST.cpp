//===--- RemoteAST.cpp ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the RemoteAST interface.
//
//===----------------------------------------------------------------------===//

#include "swift/RemoteAST/RemoteAST.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"

using namespace swift;
using namespace swift::remote;
using namespace swift::remoteAST;

namespace {

/// An implementation of MetadataReader's BuilderType concept that
/// just finds and builds things in the AST.
class RemoteASTTypeBuilder {
  ASTContext &Ctx;

public:
  using BuiltType = swift::Type;
  using BuiltNominalTypeDecl = swift::NominalTypeDecl*;
  explicit RemoteASTTypeBuilder(ASTContext &ctx) : Ctx(ctx) {}

  Type createBuiltinType(const std::string &mangledName) {
    // TODO
    return Type();
  }

  NominalTypeDecl *createNominalTypeDecl(std::string &&mangledName) {
    auto node = Demangle::demangleSymbolAsNode(mangledName);
    if (!node) return nullptr;

    return createNominalTypeDecl(node);
  }

  NominalTypeDecl *createNominalTypeDecl(const Demangle::NodePointer &node);

  Type createNominalType(NominalTypeDecl *decl, Type parent) {
    // If the declaration is generic, fail.
    if (decl->getGenericSignature())
      return Type();

    // Validate the parent type.
    if (!validateNominalParent(decl, parent))
      return Type();

    return NominalType::get(decl, parent, Ctx);
  }

  Type createBoundGenericType(NominalTypeDecl *decl, ArrayRef<Type> args,
                              Type parent) {
    // If the declaration isn't generic, fail.
    if (!decl->getGenericSignature())
      return Type();

    // Validate the parent type.
    if (!validateNominalParent(decl, parent))
      return Type();

    // Make a generic type repr that's been resolved to this decl.
    TypeReprList genericArgReprs(args);
    GenericIdentTypeRepr genericRepr(SourceLoc(), decl->getName(),
                                     genericArgReprs.getList(), SourceRange());
    genericRepr.setValue(decl);

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
        GenericIdentTypeRepr Ident;

        GenericRepr(BoundGenericType *type)
          : GenericArgs(type->getGenericArgs()),
            Ident(SourceLoc(), type->getDecl()->getName(),
                  GenericArgs.getList(), SourceRange()) {
          Ident.setValue(type->getDecl());
        }

        // SmallVector::emplace_back will never need to call this because
        // we reserve the right size, but it does try statically.
        GenericRepr(const GenericRepr &other)
          : GenericArgs({}),
            Ident(SourceLoc(), Identifier(), {}, SourceRange()) {
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
          genericComponents.emplace_back(boundGeneric);
          componentReprs.push_back(&genericComponents.back().Ident);
        } else {
          auto nominal = p->castTo<NominalType>();
          simpleComponents.emplace_back(SourceLoc(),
                                        nominal->getDecl()->getName());
          componentReprs.push_back(&simpleComponents.back());
        }
      }

      CompoundIdentTypeRepr compoundRepr(componentReprs);
      genericType = checkTypeRepr(&compoundRepr);
    } else {
      genericType = checkTypeRepr(&genericRepr);
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

  Type createTupleType(ArrayRef<Type> eltTypes, bool isVariadic) {
    // Just bail out on variadic tuples for now.
    if (isVariadic) return Type();

    SmallVector<TupleTypeElt, 4> elements;
    elements.reserve(eltTypes.size());
    for (auto eltType : eltTypes) {
      elements.push_back(eltType);
    }

    return TupleType::get(elements, Ctx);
  }

  Type createFunctionType(ArrayRef<Type> args,
                          const std::vector<bool> &inOutArgs,
                          Type output, FunctionTypeFlags flags) {
    assert(args.size() == inOutArgs.size());

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
                                          /*noreturn*/ false,
                                          /*throws*/ flags.throws());

    // The result type must be materializable.
    if (!output->isMaterializable()) return Type();

    // All the argument types must be materializable (before inout is applied).
    for (auto arg : args) {
      if (!arg->isMaterializable()) return Type();
    }

    Type input;
    if (args.size() == 1) {
      input = args[0];
    } else {
      SmallVector<TupleTypeElt, 4> elts;
      elts.reserve(args.size());
      for (auto i : indices(args)) {
        Type arg = args[i];
        if (inOutArgs[i]) arg = InOutType::get(arg);
        elts.push_back(arg);
      }
    }

    return FunctionType::get(input, output, einfo);
  }

  Type createProtocolType(StringRef moduleName, StringRef protocolName) {
    auto module = Ctx.getModuleByName(moduleName);
    if (!module) return Type();

    Identifier name = Ctx.getIdentifier(protocolName);
    auto decl = findNominalTypeDecl(module, name, Identifier(),
                                    Demangle::Node::Kind::Protocol);
    if (!decl) return Type();

    return decl->getDeclaredType();
  }

  Type createProtocolCompositionType(ArrayRef<Type> protocols) {
    for (auto protocol : protocols) {
      if (!protocol->is<ProtocolType>())
        return Type();
    }
    return ProtocolCompositionType::get(Ctx, protocols);
  }

  Type createExistentialMetatypeType(Type instance) {
    if (!instance->isAnyExistentialType())
      return Type();
    return ExistentialMetatypeType::get(instance);
  }

  Type createMetatypeType(Type instance) {
    return MetatypeType::get(instance);
  }

  Type createGenericTypeParameterType(unsigned depth, unsigned index) {
    return GenericTypeParamType::get(depth, index, Ctx);
  }

  Type createDependentMemberType(StringRef member, Type base, Type protocol) {
    if (!base->isTypeParameter())
      return Type();
    // TODO: look up protocol?
    return DependentMemberType::get(base, Ctx.getIdentifier(member), Ctx);
  }

  Type createUnownedStorageType(Type base) {
    if (!base->allowsOwnership())
      return Type();
    return UnownedStorageType::get(base, Ctx);
  }

  Type createUnmanagedStorageType(Type base) {
    if (!base->allowsOwnership())
      return Type();
    return UnmanagedStorageType::get(base, Ctx);
  }

  Type createWeakStorageType(Type base) {
    if (!base->allowsOwnership())
      return Type();
    return WeakStorageType::get(base, Ctx);
  }

  Type getUnnamedObjCClassType() {
    return Type();
  }

  Type getUnnamedForeignClassType() {
    return Type();
  }

  Type getOpaqueType() {
    return Type();
  }

private:
  bool validateNominalParent(NominalTypeDecl *decl, Type parent) {
    auto parentDecl =
      decl->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();

    // If we don't have a parent type, fast-path.
    if (!parent) {
      return parentDecl == nullptr;
    }

    // We do have a parent type.  If the nominal type doesn't, it's an error.
    if (parentDecl) {
      return false;
    }

    // FIXME: validate that the parent is a correct application of the
    // enclosing context?
    return true;
  }

  DeclContext *findDeclContext(const Demangle::NodePointer &node);
  ModuleDecl *findModule(const Demangle::NodePointer &node);
  Demangle::NodePointer findModuleNode(const Demangle::NodePointer &node);

  NominalTypeDecl *findNominalTypeDecl(DeclContext *dc,
                                       Identifier name,
                                       Identifier privateDiscriminator,
                                       Demangle::Node::Kind kind);

  Type checkTypeRepr(TypeRepr *repr) {
    // TODO: invoke the type-checker here
    return Type();
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
}

NominalTypeDecl *
RemoteASTTypeBuilder::createNominalTypeDecl(const Demangle::NodePointer &node) {
  auto DC = findDeclContext(node);
  if (!DC) return nullptr;

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

DeclContext *
RemoteASTTypeBuilder::findDeclContext(const Demangle::NodePointer &node) {
  switch (node->getKind()) {
  case Demangle::Node::Kind::DeclContext:
    return findDeclContext(node->getFirstChild());

  case Demangle::Node::Kind::Module:
    return findModule(node);

  case Demangle::Node::Kind::Class:
  case Demangle::Node::Kind::Enum:
  case Demangle::Node::Kind::Protocol:
  case Demangle::Node::Kind::Structure: {
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

    DeclContext *dc = findDeclContext(node->getChild(0));
    if (!dc) return nullptr;

    Identifier name;
    Identifier privateDiscriminator;
    if (declNameNode->getKind() == Demangle::Node::Kind::Identifier) {
      name = Ctx.getIdentifier(declNameNode->getText());
    } else if (declNameNode->getKind() ==
                 Demangle::Node::Kind::PrivateDeclName) {
      name = Ctx.getIdentifier(declNameNode->getChild(1)->getText());
      privateDiscriminator =
        Ctx.getIdentifier(declNameNode->getChild(0)->getText());

    // Ignore any other decl-name productions for now.
    } else {
      return nullptr;
    }

    return findNominalTypeDecl(dc, name, privateDiscriminator, node->getKind());
  }

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
    NominalTypeDecl *candidate;
    if (kind == Demangle::Node::Kind::Class) {
      candidate = dyn_cast<ClassDecl>(decl);
    } else if (kind == Demangle::Node::Kind::Enum) {
      candidate = dyn_cast<EnumDecl>(decl);
    } else if (kind == Demangle::Node::Kind::Protocol) {
      candidate = dyn_cast<ProtocolDecl>(decl);
    } else {
      assert(kind == Demangle::Node::Kind::Structure);
      candidate = dyn_cast<StructDecl>(decl);
    }
    if (!candidate) return nullptr;

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

namespace {
class RemoteASTContextImpl {
  using Reader32Ty = MetadataReader<External<RuntimeTarget<4>>,
                                    RemoteASTTypeBuilder>;
  using Reader64Ty = MetadataReader<External<RuntimeTarget<8>>,
                                    RemoteASTTypeBuilder>;
  union {
    Reader32Ty *Reader32;
    Reader64Ty *Reader64;
  };
  bool Is32;

public:
  RemoteASTContextImpl(ASTContext &ctx,
                       std::shared_ptr<MemoryReader> &&reader) {
    auto &target = ctx.LangOpts.Target;
    assert(target.isArch32Bit() || target.isArch64Bit());

    Is32 = target.isArch32Bit();
    if (Is32) {
      Reader32 = new Reader32Ty(std::move(reader), ctx);
    } else {
      Reader64 = new Reader64Ty(std::move(reader), ctx);
    }
  }

  ~RemoteASTContextImpl() {
    if (Is32) {
      delete Reader32;
    } else {
      delete Reader64;
    }
  }

  Result<Type> getTypeForRemoteTypeMetadata(RemoteAddress address) {
    Type result;
    if (Is32) {
      result = Reader32->readTypeFromMetadata(address.getAddressData());
    } else {
      result = Reader64->readTypeFromMetadata(address.getAddressData());
    }
    if (result) return result;
    return Result<Type>::emplaceFailure(Failure::Unknown);
  }

  Result<NominalTypeDecl *>
  getDeclForRemoteNominalTypeDescriptor(RemoteAddress address) {
    NominalTypeDecl *result;
    if (Is32) {
      result =Reader32->readNominalTypeFromDescriptor(address.getAddressData());
    } else {
      result =Reader64->readNominalTypeFromDescriptor(address.getAddressData());
    }

    if (result) return result;
    return Result<NominalTypeDecl *>::emplaceFailure(Failure::Unknown);
  }

  Result<uint64_t> getOffsetForProperty(Type type, StringRef propertyName) {
    // TODO
    return Result<uint64_t>::emplaceFailure(Failure::Unknown);
  }
};

} // end anonymous namespace

static RemoteASTContextImpl* asImpl(void *impl) {
  return static_cast<RemoteASTContextImpl*>(impl);
}

RemoteASTContext::RemoteASTContext(ASTContext &ctx,
                                   std::shared_ptr<MemoryReader> reader)
  : Impl(new RemoteASTContextImpl(ctx, std::move(reader))) {
}

RemoteASTContext::~RemoteASTContext() {
  delete asImpl(Impl);
}

Result<Type>
RemoteASTContext::getTypeForRemoteTypeMetadata(RemoteAddress address) {
  return asImpl(Impl)->getTypeForRemoteTypeMetadata(address);
}

Result<NominalTypeDecl *>
RemoteASTContext::getDeclForRemoteNominalTypeDescriptor(RemoteAddress address) {
  return asImpl(Impl)->getDeclForRemoteNominalTypeDescriptor(address);
}

Result<uint64_t>
RemoteASTContext::getOffsetForProperty(Type type, StringRef propertyName) {
  return asImpl(Impl)->getOffsetForProperty(type, propertyName);
}

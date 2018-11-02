//===--- ASTDemangler.h - Swift AST symbol demangling -----------*- C++ -*-===//
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

#ifndef __SWIFT_AST_ASTDEMANGLER_H__
#define __SWIFT_AST_ASTDEMANGLER_H__

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/TypeDecoder.h"

namespace swift {
 
namespace Demangle {

Type getTypeForMangling(ASTContext &ctx,
                        llvm::StringRef mangling);

/// An implementation of MetadataReader's BuilderType concept that
/// just finds and builds things in the AST.
class ASTBuilder {
  ASTContext &Ctx;
  Demangle::NodeFactory Factory;

  /// The notional context in which we're writing and type-checking code.
  /// Created lazily.
  DeclContext *NotionalDC = nullptr;

public:
  using BuiltType = swift::Type;
  using BuiltNominalTypeDecl = swift::NominalTypeDecl *;
  using BuiltProtocolDecl = swift::ProtocolDecl *;
  explicit ASTBuilder(ASTContext &ctx) : Ctx(ctx) {}

  ASTContext &getASTContext() { return Ctx; }
  DeclContext *getNotionalDC();

  Demangle::NodeFactory &getNodeFactory() { return Factory; }

  Type createBuiltinType(const std::string &mangledName);

  NominalTypeDecl *createNominalTypeDecl(StringRef mangledName);
  
  NominalTypeDecl *createNominalTypeDecl(const Demangle::NodePointer &node);

  ProtocolDecl *createProtocolDecl(const Demangle::NodePointer &node);

  Type createNominalType(NominalTypeDecl *decl);

  Type createNominalType(NominalTypeDecl *decl, Type parent);

  Type createBoundGenericType(NominalTypeDecl *decl, ArrayRef<Type> args);

  Type createBoundGenericType(NominalTypeDecl *decl, ArrayRef<Type> args,
                              Type parent);

  Type createTupleType(ArrayRef<Type> eltTypes, StringRef labels,
                       bool isVariadic);

  Type createFunctionType(ArrayRef<Demangle::FunctionParam<Type>> params,
                          Type output, FunctionTypeFlags flags);

  Type createProtocolCompositionType(ArrayRef<ProtocolDecl *> protocols,
                                     Type superclass,
                                     bool isClassBound);

  Type createExistentialMetatypeType(Type instance);

  Type createMetatypeType(Type instance, bool wasAbstract=false);

  Type createGenericTypeParameterType(unsigned depth, unsigned index);

  Type createDependentMemberType(StringRef member, Type base,
                                 ProtocolDecl *protocol);

#define REF_STORAGE(Name, ...) \
  Type create##Name##StorageType(Type base);
#include "swift/AST/ReferenceStorage.def"

  Type createSILBoxType(Type base);

  Type createObjCClassType(StringRef name);

  ProtocolDecl *createObjCProtocolDecl(StringRef name);

  Type createForeignClassType(StringRef mangledName);

  Type getUnnamedForeignClassType();

  Type getOpaqueType();

private:
  bool validateNominalParent(NominalTypeDecl *decl, Type parent);
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

  Type checkTypeRepr(TypeRepr *repr);

  static NominalTypeDecl *getAcceptableNominalTypeCandidate(ValueDecl *decl, 
                                                     Demangle::Node::Kind kind);

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

}  // namespace Demangle

}  // namespace swift

#endif  // __SWIFT_AST_ASTDEMANGLER_H__

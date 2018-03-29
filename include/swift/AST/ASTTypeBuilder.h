//===--- ASTTypeBuilder.h - Swift AST Type builder --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the MangledNameTypeBuilder class. It is used to construct
// types starting from mangled names.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Mangler.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/RemoteAST/RemoteAST.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

class MangledNameTypeBuilder {
  ASTContext &Ctx;

public:
  using BuiltType = swift::Type;
  using BuiltNominalTypeDecl = swift::NominalTypeDecl *;
  using BuildProtocolDecl = swift::ProtocolDecl *;

  explicit MangledNameTypeBuilder(ASTContext &ctx) : Ctx(ctx) {}

  Type createBuiltinType(const std::string &mangledName) {
    // TODO
    return Type();
  }

  Type createTupleType(ArrayRef<Type> eltTypes, StringRef labels,
                       bool isVariadic) {
    // Just bail out on variadic tuples for now.
    if (isVariadic)
      return Type();

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

  Type createExistentialMetatypeType(Type instance) {
    if (!instance->isAnyExistentialType())
      return Type();
    return ExistentialMetatypeType::get(instance);
  }

  Type createMetatypeType(Type instance, bool wasAbstract = false) {
    // FIXME: Plumb through metatype representation and generalize silly
    // 'wasAbstract' flag
    return MetatypeType::get(instance);
  }

  Type createGenericTypeParameterType(unsigned depth, unsigned index) {
    return GenericTypeParamType::get(depth, index, Ctx);
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

  Type createSILBoxType(Type base) {
    return SILBoxType::get(base->getCanonicalType());
  }

  Type getUnnamedForeignClassType() { return Type(); }

  Type getOpaqueType() { return Type(); }
};

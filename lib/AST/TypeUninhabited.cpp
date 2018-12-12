//===--- TypeUninhabited.cpp - Type inhabitant checking -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements a check for uninhabited types.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
using namespace swift;

namespace {
  
enum Inhabitedness : uint8_t {
  Unknown = 1 << 0,
  Inhabited = 1 << 1,
  Uninhabited = 1 << 2
};

/// A helper class for checking whether a type is inhabited.
class InhabitantChecker {
  /// The type being checked.
  Type originalType;
  
  llvm::DenseMap<CanType, bool> isInhabited;
  SmallVector<CanType, 4> stack;
  
public:
  InhabitantChecker(Type type) : originalType(type) {};
  
  Inhabitedness run();
  
private:
  Inhabitedness checkOrPush(Type type);
  Inhabitedness checkType(CanType type);
  Inhabitedness checkEnum(CanType type, EnumDecl *decl);
  Inhabitedness checkEnumCase(EnumElementDecl *decl);
  Inhabitedness checkClass(CanType type, ClassDecl *decl);
  Inhabitedness checkStruct(CanType type, StructDecl *decl);
  Inhabitedness checkTuple(TupleType *tuple);
};
  
} // end anonymous namespace

bool TypeBase::isStructurallyUninhabited() {
  return InhabitantChecker(Type(this)).run() == Uninhabited;
}

/// The main routine for checking inhabitedness.
Inhabitedness InhabitantChecker::run() {
  auto result = checkOrPush(originalType);
  if (result != Unknown) {
    return result;
  }
  
  while (!stack.empty()) {
    auto type = stack.back();
    auto result = checkType(type);
    if (result != Unknown) {
      stack.pop_back();
      isInhabited.insert({type, result == Inhabited});
    }
  }
  
  auto it = isInhabited.find(originalType->getCanonicalType());
  assert(it != isInhabited.end());
  return it->second ? Inhabited : Uninhabited;
}

Inhabitedness InhabitantChecker::checkOrPush(Type type) {
  auto canonical = type->getCanonicalType();
  
  if (canonical->isUninhabited())
    return Uninhabited;
  
  if (auto tuple = dyn_cast<TupleType>(canonical))
    return checkTuple(tuple);
  
  auto nominal = canonical.getAnyNominal();
  if (!nominal)
    return Inhabited;
  
  auto params = nominal->getGenericParams();
  // Only bother with types that become inhabited because of their generic parameters.
  // If the type is declared to be inhabited (but isn't an enum--that's detected above)
  // then it'll generate a warning anyway.
  if (!params)
    return Inhabited;
  
  // If the type has a cycle, bail and call in inhabited.
  if (std::find(stack.begin(), stack.end(), canonical) != stack.end())
    return Inhabited;
  
  auto it = isInhabited.find(canonical);
  if (it != isInhabited.end())
    return it->second ? Inhabited : Uninhabited;

  stack.push_back(canonical);
  return Unknown;
}

Inhabitedness InhabitantChecker::checkType(CanType type) {
  if (auto decl = type.getAnyNominal()) {
    if (auto c = dyn_cast<ClassDecl>(decl)) {
      return checkClass(type, c);
    } else if (auto e = dyn_cast<EnumDecl>(decl)) {
      return checkEnum(type, e);
    } else if (auto s = dyn_cast<StructDecl>(decl)) {
      return checkStruct(type, s);
    } else {
      return Inhabited;
    }
  } else if (auto tuple = dyn_cast<TupleType>(type)) {
    return checkTuple(tuple);
  } else {
    return Inhabited;
  }
}

Inhabitedness InhabitantChecker::checkEnum(CanType type, EnumDecl *decl) {
  for (auto elt : decl->getAllElements()) {
    switch (checkEnumCase(elt)) {
      case Unknown:
        return Unknown;
      case Inhabited:
        return Inhabited;
      case Uninhabited:
        break;
    }
  }
  return Uninhabited;
}

Inhabitedness InhabitantChecker::checkEnumCase(EnumElementDecl *decl) {
  auto params = decl->getParameterList();
  for (auto i = params->begin(); i != params->end(); ++i) {
    auto *param = *i;
    auto paramType = param->getType();
    switch (checkOrPush(paramType)) {
      case Unknown:
        return Unknown;
      case Inhabited:
        break;
      case Uninhabited:
        return Uninhabited;
    }
  }
  return Inhabited;
}

Inhabitedness InhabitantChecker::checkClass(CanType type, ClassDecl *decl) {
  for (auto field : decl->getStoredProperties()) {
    auto fieldType = type->getTypeOfMember(decl->getModuleContext(), field);
    switch (checkOrPush(fieldType)) {
      case Unknown:
        return Unknown;
      case Inhabited:
        break;
      case Uninhabited:
        return Uninhabited;
    }
  }
  return Inhabited;
}

Inhabitedness InhabitantChecker::checkStruct(CanType type, StructDecl *decl) {
  for (auto field : decl->getStoredProperties()) {
    auto fieldType = type->getTypeOfMember(decl->getModuleContext(), field);
    switch (checkOrPush(fieldType)) {
      case Unknown:
        return Unknown;
      case Inhabited:
        break;
      case Uninhabited:
        return Uninhabited;
    }
  }
  return Inhabited;
}

Inhabitedness InhabitantChecker::checkTuple(TupleType *tuple) {
  for (auto eltType : tuple->getElementTypes())
    switch (checkOrPush(eltType)) {
      case Unknown:
        return Unknown;
      case Inhabited:
        break;
      case Uninhabited:
        return Uninhabited;
    }
  return Inhabited;
}

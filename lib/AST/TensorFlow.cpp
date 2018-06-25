//===--- TensorFlow.cpp - AST Level TensorFlow Support Logic --------------===//
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
// This file implements the AST level TensorFlow support logic that is used
// across the Swift compiler.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/TensorFlow.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
using namespace swift;
using namespace tf;


/// If the specified type is the well-known TensorHandle<T> type, then return
/// "T".  If not, return a null type.
Type tf::getTensorHandleElementType(Type ty) {
  // TODO: Check that this type is declared in the TensorFlow module.
  if (auto *bgct = ty->getAs<BoundGenericClassType>()) {
    if (bgct->getDecl()->getNameStr() == "TensorHandle") {
      assert(bgct->getGenericArgs().size() == 1 && "Expected one generic arg");
      return bgct->getGenericArgs()[0];
    }
  }
  return Type();
}

/// Determine whether the specified type is one of our well-known types, and
/// if so, which one it is.
TFValueKind tf::classifyTensorFlowValue(Type ty) {
  // TODO: Check that these types are declared in the TensorFlow module.
  if (auto *ct = ty->getAs<ClassType>()) {
    auto name = ct->getDecl()->getNameStr();
    if (name == "ResourceHandle")
      return TFValueKind::ResourceHandle;
    if (name == "VariantHandle")
      return TFValueKind::VariantHandle;
  }

  if (getTensorHandleElementType(ty))
    return TFValueKind::TensorHandle;
  return TFValueKind::Nope;
}

/// Return true if the specified type is a TensorHandle<T>.
bool tf::isTensorHandle(Type ty) {
  return classifyTensorFlowValue(ty) == TFValueKind::TensorHandle;
}

/// Return true if the specified type is TensorHandle<T>, ResourceHandle, or
/// VariantHandle.
bool tf::isTensorFlowValue(Type ty) {
  return classifyTensorFlowValue(ty) != TFValueKind::Nope;
}

/// Returns true if the specified type is a TensorFlow value or an tuple or
/// struct of such.
bool tf::isTensorFlowValueOrAggregate(Type ty) {
  if (isTensorFlowValue(ty))
    return true;
  if (auto *tupleTy = ty->getAs<TupleType>())
    return llvm::all_of(tupleTy->getElementTypes(),
      [](Type eltTy) {
        return isTensorFlowValueOrAggregate(eltTy);
      });
  if (auto *structTy = ty->getAs<StructType>())
    return llvm::all_of(structTy->getDecl()->getStoredProperties(),
      [](VarDecl *member) {
        return isTensorFlowValueOrAggregate(member->getType());
      });
  if (auto *genericStructTy = ty->getAs<BoundGenericStructType>())
    return llvm::all_of(genericStructTy->getDecl()->getStoredProperties(),
      [](VarDecl *member) {
        return isTensorFlowValueOrAggregate(member->getType());
      });
  return false;
}

/// Return true if the specified type contains a TensorFlow value type that
/// will be exposed after deabstraction.
bool TypeContainsTensorFlowValue::containsTensorFlowValue(Type ty) {
  // If this type literally is a value type, then yep, we contain it.  This is
  // the base case.
  if (isTensorFlowValue(ty))
    return true;

  // Deabstraction flattens tuples, so if a tuple contains any tensor values,
  // then the tuple itself does.
  if (auto *tuple = ty->getAs<TupleType>()) {
    for (auto &elt : tuple->getElements())
      if (containsTensorFlowValue(elt.getType()))
        return true;
    return false;
  }

  // Deabstraction scalarizes structs.
  if (auto *st = ty->getAs<StructType>())
    return structContainsTensorFlowValue(st->getDecl());

  // Deabstractions binds specialized generic structs.  Check if either the
  // struct itself or one of the generic arguments contains a tensor value.
  if (auto *bgst = ty->getAs<BoundGenericStructType>()) {
    // Check the generic arguments.
    for (auto arg : bgst->getGenericArgs())
      if (containsTensorFlowValue(arg))
        return true;

    return structContainsTensorFlowValue(bgst->getDecl());
  }

  // Handle still-generic types that may contain a tensor value.
  if (auto *ugst = ty->getAs<UnboundGenericType>())
    if (auto *decl = dyn_cast<StructDecl>(ugst->getDecl()))
      return structContainsTensorFlowValue(decl);

  // Otherwise we have a class or some other type that is opaque to
  // deabstraction.
  return false;
}

/// Determine whether the given struct contains a TensorFlow value type, caching
/// the result.
bool TypeContainsTensorFlowValue::
structContainsTensorFlowValue(StructDecl *decl) {
  auto it = declContainsTensorFlowValue.find(decl);
  if (it != declContainsTensorFlowValue.end())
    return it->second;

  bool hasTensorFlowValue = false;
  for (auto p : decl->getStoredProperties())
    if (containsTensorFlowValue(p->getType())) {
      hasTensorFlowValue = true;
      break;
    }

  return declContainsTensorFlowValue[decl] = hasTensorFlowValue;
}

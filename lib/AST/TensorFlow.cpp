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
Type tf::isTensorHandle(Type ty) {
  if (auto *bgct = ty->getAs<BoundGenericClassType>()) {
    if (bgct->getDecl()->getNameStr() == "TensorHandle") {
      assert(bgct->getGenericArgs().size() == 1 && "Expected one generic arg");
      return bgct->getGenericArgs()[0];
    }
  }
  return Type();
}

/// Return true if the specified type contains a TensorHandle that will be
/// exposed after deabstraction.
bool TypeContainsTensorHandle::containsTensorHandle(Type ty) {
  // If this type literally is TensorHandle, then yep, we contain it.  This is
  // the base case.
  if (isTensorHandle(ty))
    return true;

  // Deabstraction flattens tuples, so if a tuple contains any tensor handles,
  // then the tuple itself does.
  if (auto *tuple = ty->getAs<TupleType>()) {
    for (auto &elt : tuple->getElements())
      if (containsTensorHandle(elt.getType()))
        return true;
    return false;
  }

  // Deabstraction scalarizes structs.
  if (auto *st = ty->getAs<StructType>())
    return structContainsTensorHandle(st->getDecl());

  // Deabstractions binds specialized generic structs.  Check if either the
  // struct itself or one of the generic arguments contains a TensorHandle.
  if (auto *bgst = ty->getAs<BoundGenericStructType>()) {
    // Check the generic arguments.
    for (auto arg : bgst->getGenericArgs())
      if (containsTensorHandle(arg))
        return true;

    return structContainsTensorHandle(bgst->getDecl());
  }

  // Handle still-generic types that may contain a TensorHandle.
  if (auto *ugst = ty->getAs<UnboundGenericType>())
    if (auto *decl = dyn_cast<StructDecl>(ugst->getDecl()))
      return structContainsTensorHandle(decl);

  // Otherwise we have a class or some other type that is opaque to
  // deabstraction.
  return false;
}

/// Determine whether the given struct contains a TensorHandle, caching the
/// result.
bool TypeContainsTensorHandle::structContainsTensorHandle(StructDecl *decl) {
  auto it = declContainsTensorHandle.find(decl);
  if (it != declContainsTensorHandle.end())
    return it->second;

  bool hasTensorHandle = false;
  for (auto p : decl->getStoredProperties())
    if (containsTensorHandle(p->getType())) {
      hasTensorHandle = true;
      break;
    }

  return declContainsTensorHandle[decl] = hasTensorHandle;
}

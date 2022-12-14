//===--- GenPack.cpp - Swift IR Generation For Variadic Generics ----------===//
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
//  This file implements IR generation for type and value packs in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "llvm/IR/DerivedTypes.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

llvm::Value *IRGenFunction::emitPackShapeExpression(CanType type) {

  type = type->getReducedShape()->getCanonicalType();

  auto kind = LocalTypeDataKind::forPackShapeExpression();

  llvm::Value *result = tryGetLocalTypeData(type, kind);
  if (result != nullptr)
    return result;

  // If shape(T) == t and shape(U) == u, the shape expression for a pack
  // {T..., Int, T..., U..., String} becomes 't + t + u + 2'.
  unsigned scalarElements = 0;

  auto accumulate = [&](llvm::Value *value) {
    if (result == nullptr) {
      result = value;
      return;
    }

    result = Builder.CreateAdd(result, value);
  };

  auto packType = cast<PackType>(type);
  for (auto elt : packType.getElementTypes()) {
    if (auto expansionType = dyn_cast<PackExpansionType>(elt)) {
      auto reducedShape = expansionType.getCountType();
      accumulate(emitPackShapeExpression(reducedShape));
      continue;
    }

    ++scalarElements;
  }

  if (scalarElements > 0) {
    auto *constant = llvm::ConstantInt::get(IGM.SizeTy, scalarElements);
    accumulate(constant);
  }

  setScopedLocalTypeData(type, kind, result);
  return result;
}
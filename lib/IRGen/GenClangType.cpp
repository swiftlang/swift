//===--- GenClangType.cpp - Swift IR Generation For Types -----------------===//
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
// Wrapper functions for creating Clang types from Swift types.
//
//===----------------------------------------------------------------------===//

#include "IRGenModule.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/CanonicalType.h"
#include "clang/AST/Type.h"

using namespace swift;
using namespace irgen;

clang::CanQualType IRGenModule::getClangType(CanType type, bool allowBridging) {
  auto *ty = type->getASTContext().getClangTypeForIRGen(type, allowBridging);
  return ty ? ty->getCanonicalTypeUnqualified() : clang::CanQualType();
}

clang::CanQualType IRGenModule::getClangType(SILType type, bool allowBridging) {
  if (type.isForeignReferenceType())
    return getClangType(type.getASTType()
                            ->wrapInPointer(PTK_UnsafePointer)
                            ->getCanonicalType(),
                        allowBridging);
  return getClangType(type.getASTType(), allowBridging);
}

clang::CanQualType IRGenModule::getClangType(SILParameterInfo params,
                                             CanSILFunctionType funcTy,
                                             bool allowBridging) {
  auto paramTy = params.getSILStorageType(getSILModule(), funcTy,
                                          getMaximalTypeExpansionContext());
  auto clangType = getClangType(paramTy, allowBridging);
  // @block_storage types must be @inout_aliasable and have
  // special lowering
  if (!paramTy.is<SILBlockStorageType>()) {
    if (params.isIndirectMutating()) {
      return getClangASTContext().getPointerType(clangType);
    }
    if (params.isFormalIndirect() &&
        // Sensitive return types are represented as indirect return value in SIL,
        // but are returned as values (if small) in LLVM IR.
        !paramTy.isSensitive()) {
      auto constTy =
        getClangASTContext().getCanonicalType(clangType.withConst());
      return getClangASTContext().getPointerType(constTy);
    }
  }
  return clangType;
}

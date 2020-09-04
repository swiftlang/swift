//===--- ExtInfo.cpp - Extended information for function types ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements ASTExtInfo, SILExtInfo and related classes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ExtInfo.h"

#include "clang/AST/Type.h"

static void assertIsFunctionType(const clang::Type *type) {
#ifndef NDEBUG
  if (!(type->isFunctionPointerType() || type->isBlockPointerType() ||
        type->isFunctionReferenceType())) {
    llvm::errs() << "Expected a Clang function type wrapped in a pointer type "
                 << "or a block pointer type but found:\n";
    type->dump();
    llvm_unreachable("\nUnexpected Clang type when creating ExtInfo!");
  }
#endif
}

namespace swift {

// MARK: - ClangTypeInfo

bool operator==(ClangTypeInfo lhs, ClangTypeInfo rhs) {
  if (lhs.type == rhs.type)
    return true;
  if (lhs.type && rhs.type)
    return lhs.type->getCanonicalTypeInternal() ==
           rhs.type->getCanonicalTypeInternal();
  return false;
}

ClangTypeInfo ClangTypeInfo::getCanonical() const {
  if (!type)
    return ClangTypeInfo();
  return ClangTypeInfo(type->getCanonicalTypeInternal().getTypePtr());
}

void ClangTypeInfo::printType(ClangModuleLoader *cml,
                              llvm::raw_ostream &os) const {
  cml->printClangType(type, os);
}

void ClangTypeInfo::dump(llvm::raw_ostream &os,
                         const clang::ASTContext &ctx) const {
  if (type) {
    type->dump(os, ctx);
  } else {
    os << "<nullptr>";
  }
}

// MARK: - ASTExtInfoBuilder

void ASTExtInfoBuilder::checkInvariants() const {
  // TODO: [clang-function-type-serialization] Once we start serializing
  // the Clang type, we should also assert that the pointer is non-null.
  auto Rep = Representation(bits & RepresentationMask);
  if ((Rep == Representation::CFunctionPointer) && clangTypeInfo.type)
    assertIsFunctionType(clangTypeInfo.type);
}

// MARK: - ASTExtInfo

ASTExtInfo ASTExtInfoBuilder::build() const {
  checkInvariants();
  return ASTExtInfo(*this);
}

// MARK: - SILExtInfoBuilder

void SILExtInfoBuilder::checkInvariants() const {
  // TODO: Add validation checks here while making sure things don't blow up.
}

SILExtInfo SILExtInfoBuilder::build() const {
  checkInvariants();
  return SILExtInfo(*this);
}

} // end namespace swift

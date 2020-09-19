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
#include "swift/AST/Types.h"

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

// MARK: - ThrowsInfo
ThrowsInfo ThrowsInfo::get(bool throws, Type type) {
  if (!throws) return Kind::Nonthrowing;

  if (!type) return Kind::Untyped;

  if (type->isStructurallyUninhabited()) return Kind::Nonthrowing;

  return ThrowsInfo(Kind::Typed, type);
}

Type ThrowsInfo::getThrowsType() const {
  assert(kind == Kind::Typed && "Can only get type of typed throws");
  return throwsType;
}

// MARK: - ASTExtInfoBuilder
ASTExtInfoBuilder ASTExtInfoBuilder::get(Representation rep, bool isNoEscape,
                                         bool throws, Type throwsType,
                                         DifferentiabilityKind diffKind,
                                         const clang::Type *type) {
  ThrowsInfo throwsInfo = ThrowsInfo::get(throws, throwsType);

  return
      ASTExtInfoBuilder(((unsigned)rep) | (isNoEscape ? NoEscapeMask : 0) |
                        (((unsigned)throwsInfo.kind << ThrowsKindMaskOffset) &
                         ThrowsKindMask) |
                        (((unsigned)diffKind << DifferentiabilityMaskOffset) &
                         DifferentiabilityMask),
                        throwsInfo.throwsType, ClangTypeInfo(type));
}

void ASTExtInfoBuilder::checkInvariants() const {
  // TODO: [clang-function-type-serialization] Once we start serializing
  // the Clang type, we should also assert that the pointer is non-null.
  auto Rep = Representation(bits & RepresentationMask);
  if ((Rep == Representation::CFunctionPointer) && clangTypeInfo.type)
    assertIsFunctionType(clangTypeInfo.type);
}

bool ASTExtInfoBuilder::isEqualTo(ASTExtInfoBuilder other,
                                  bool useClangTypes,
                                  bool considerThrowsType) const {

  bool throwsTypeEquivalent = (throwsType && other.throwsType) ?
    (throwsType->getCanonicalType() == other.throwsType->getCanonicalType()) :
      false;

  return bits == other.bits &&
    (considerThrowsType ? throwsTypeEquivalent : true) &&
    (useClangTypes ? (clangTypeInfo == other.clangTypeInfo) : true);
}

std::tuple<unsigned, const void *, const void *>
ASTExtInfoBuilder::getFuncAttrKey() const {
  return std::make_tuple(bits,
                         throwsType ?
                            throwsType->getCanonicalType().getPointer() :
                            nullptr,
                         clangTypeInfo.getType());
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

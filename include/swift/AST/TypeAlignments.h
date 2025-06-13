//===--- TypeAlignments.h - Alignments of various Swift types ---*- C++ -*-===//
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
/// \file This file defines the alignment of various Swift AST classes.
///
/// It's useful to do this in a dedicated place to avoid recursive header
/// problems. To make sure we don't have any ODR violations, this header
/// should be included in every header that defines one of the forward-
/// declared types listed here.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPEALIGNMENTS_H
#define SWIFT_TYPEALIGNMENTS_H

#include <cstddef>

namespace swift {
  class AbstractClosureExpr;
  class AbstractSpecializeAttr;
  class AbstractStorageDecl;
  class ArchetypeType;
  class AssociatedTypeDecl;
  class ASTContext;
  class AttributeBase;
  class BraceStmt;
  class CaptureListExpr;
  class Decl;
  class DeclContext;
  class DifferentiableAttr;
  class Expr;
  class ExtensionDecl;
  class FileUnit;
  class GenericEnvironment;
  class GenericParamList;
  class GenericTypeParamDecl;
  class NominalTypeDecl;
  class NormalProtocolConformance;
  class OpaqueValueExpr;
  class OperatorDecl;
  class PackConformance;
  class Pattern;
  class ProtocolDecl;
  class ProtocolConformance;
  class RequirementRepr;
  class SILFunction;
  class SILFunctionType;
  class SpecializeAttr;
  class SpecializedAttr;
  class Stmt;
  class TrailingWhereClause;
  class TypeVariableType;
  class TypeBase;
  class TypeDecl;
  class TypeRepr;
  class ValueDecl;
  class CaseLabelItem;
  class StmtConditionElement;

  /// We frequently use three tag bits on all of these types.
  constexpr size_t AttrAlignInBits = 3;
  constexpr size_t DeclAlignInBits = 3;
  constexpr size_t DeclContextAlignInBits = 3;
  constexpr size_t ExprAlignInBits = 3;
  constexpr size_t StmtAlignInBits = 3;
  constexpr size_t TypeAlignInBits = 3;
  constexpr size_t PatternAlignInBits = 3;
  constexpr size_t TypeReprAlignInBits = 3;

  constexpr size_t SILFunctionAlignInBits = 2;
  constexpr size_t ASTContextAlignInBits = 2;
  constexpr size_t TypeVariableAlignInBits = 4;
  constexpr size_t StoredDefaultArgumentAlignInBits = 3;

  // Well, this is the *minimum* pointer alignment; it's going to be 3 on
  // 64-bit targets, but that doesn't matter.
  constexpr size_t PointerAlignInBits = 2;
}

namespace llvm {
  /// Helper class for declaring the expected alignment of a pointer.
  /// TODO: LLVM should provide this.
  template <class T, size_t AlignInBits> struct MoreAlignedPointerTraits {
    enum { NumLowBitsAvailable = AlignInBits };
    static inline void *getAsVoidPointer(T *ptr) { return ptr; }
    static inline T *getFromVoidPointer(void *ptr) {
      return static_cast<T*>(ptr);
    }
  };

  template <class T> struct PointerLikeTypeTraits;
}

/// Declare the expected alignment of pointers to the given type.
/// This macro should be invoked from a top-level file context.
#define LLVM_DECLARE_TYPE_ALIGNMENT(CLASS, ALIGNMENT)     \
namespace llvm {                                          \
template <> struct PointerLikeTypeTraits<CLASS*>          \
  : public MoreAlignedPointerTraits<CLASS, ALIGNMENT> {}; \
}

LLVM_DECLARE_TYPE_ALIGNMENT(swift::Decl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::AbstractStorageDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::AssociatedTypeDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::GenericTypeParamDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::OperatorDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::ProtocolDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::TypeDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::ValueDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::NominalTypeDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::ExtensionDecl, swift::DeclAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(swift::TypeBase, swift::TypeAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::ArchetypeType, swift::TypeAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::TypeVariableType, swift::TypeVariableAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::SILFunctionType,
                            swift::TypeVariableAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(swift::Stmt, swift::StmtAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::BraceStmt, swift::StmtAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(swift::ASTContext, swift::ASTContextAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::DeclContext, swift::DeclContextAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::FileUnit, swift::DeclContextAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::DifferentiableAttr, swift::PointerAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::Expr, swift::ExprAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::CaptureListExpr, swift::ExprAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::AbstractClosureExpr, swift::ExprAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::OpaqueValueExpr, swift::ExprAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::PackConformance, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::ProtocolConformance, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::NormalProtocolConformance,
                            swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::GenericEnvironment,
                            swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::GenericParamList,
                            swift::PointerAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::Pattern,
                            swift::PatternAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::RequirementRepr,
                            swift::PointerAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::SILFunction,
                            swift::SILFunctionAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::SpecializeAttr, swift::PointerAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::SpecializedAttr, swift::PointerAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::AbstractSpecializeAttr, swift::PointerAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::TrailingWhereClause,
                            swift::PointerAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(swift::AttributeBase, swift::AttrAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(swift::TypeRepr, swift::TypeReprAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(swift::CaseLabelItem, swift::PatternAlignInBits)

LLVM_DECLARE_TYPE_ALIGNMENT(swift::StmtConditionElement,
                            swift::PatternAlignInBits)

static_assert(alignof(void*) >= 2, "pointer alignment is too small");

#endif

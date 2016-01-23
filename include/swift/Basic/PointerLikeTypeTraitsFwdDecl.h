//===--- PointerLikeTypeTraitsFwdDecl.h -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file contains explicit PointerLikeTypeTraits specialization for objects
/// that need to have a PointerLikeTypeTraits specialization while they are
/// forward declared.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_POINTERLIKETYPETRAITSFWDDECL_H
#define SWIFT_BASIC_POINTERLIKETYPETRAITSFWDDECL_H

#include "llvm/Support/PointerLikeTypeTraits.h"

namespace swift {

template <typename T> struct FwdDeclPointerLikeTypeTraits {
  static inline void *getAsVoidPointer(T P) { return P; }
  static inline T getFromVoidPointer(void *P) { return static_cast<T>(P); }

  /// Note, we assume here that void* is related to raw malloc'ed memory and
  /// that malloc returns objects at least 4-byte aligned. However, this may be
  /// wrong, or pointers may be from something other than malloc. In this case,
  /// you should specify a real typed pointer or avoid this template.
  ///
  /// All clients should use assertions to do a run-time check to ensure that
  /// this is actually true.
  enum { NumLowBitsAvailable = 2 };
};

} // end swift namespace

namespace swift {
class ASTContext;
class AssociatedTypeDecl;
class BraceStmt;
class Expr;
class ExprHandle;
class GenericTypeParamDecl;
class Pattern;
class SILTypeList;
class Stmt;
class ValueDecl;
namespace driver {
class JobAction;
} // end driver namespace
} // end swift namespace

#ifdef DEFINE_FWDDECLTRAITS
#error "Can not redefine DEFINE_FWDDECLTRAITS"
#endif

#define DEFINE_FWDDECLTRAITS(Cls)                                              \
  namespace llvm {                                                             \
  template <>                                                                  \
  struct PointerLikeTypeTraits<Cls *>                                          \
      : swift::FwdDeclPointerLikeTypeTraits<Cls *> {};                         \
  }

DEFINE_FWDDECLTRAITS(swift::ASTContext);
DEFINE_FWDDECLTRAITS(swift::AssociatedTypeDecl);
DEFINE_FWDDECLTRAITS(swift::BraceStmt);
DEFINE_FWDDECLTRAITS(swift::Expr);
DEFINE_FWDDECLTRAITS(swift::ExprHandle);
DEFINE_FWDDECLTRAITS(swift::GenericTypeParamDecl);
DEFINE_FWDDECLTRAITS(swift::Pattern);
DEFINE_FWDDECLTRAITS(swift::SILTypeList);
DEFINE_FWDDECLTRAITS(swift::Stmt);
DEFINE_FWDDECLTRAITS(swift::ValueDecl);
DEFINE_FWDDECLTRAITS(swift::driver::JobAction);

#undef DEFINE_FWDDECLTRAITS

#endif

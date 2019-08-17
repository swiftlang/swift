//===--- CodeSynthesis.h - Typechecker code synthesis -----------*- C++ -*-===//
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
//  This file defines a typechecker-internal interface to a bunch of
//  routines for synthesizing various declarations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPECHECKING_CODESYNTHESIS_H
#define SWIFT_TYPECHECKING_CODESYNTHESIS_H

#include "swift/AST/ForeignErrorConvention.h"
#include "swift/Basic/ExternalUnion.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"

namespace swift {

class AbstractFunctionDecl;
class AbstractStorageDecl;
class ASTContext;
class ClassDecl;
class ConstructorDecl;
class FuncDecl;
class GenericParamList;
class NominalTypeDecl;
class ParamDecl;
class Type;
class ValueDecl;
class VarDecl;

class TypeChecker;

class ObjCReason;

enum class SelfAccessorKind {
  /// We're building a derived accessor on top of whatever this
  /// class provides.
  Peer,

  /// We're building a setter or something around an underlying
  /// implementation, which might be storage or inherited from a
  /// superclass.
  Super,
};

Expr *buildSelfReference(VarDecl *selfDecl,
                         SelfAccessorKind selfAccessorKind,
                         bool isLValue,
                         ASTContext &ctx);

/// Build an expression that evaluates the specified parameter list as a tuple
/// or paren expr, suitable for use in an apply expr.
Expr *buildArgumentForwardingExpr(ArrayRef<ParamDecl*> params,
                                  ASTContext &ctx);

} // end namespace swift

#endif

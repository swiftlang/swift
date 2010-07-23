//===--- Sema.h - Swift Language Semantic Analysis --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Sema interface which implement hooks invoked by the 
// parser to build the AST.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_SEMA_H
#define SWIFT_SEMA_SEMA_H

#include "swift/Sema/SemaType.h"
#include "swift/Sema/SemaExpr.h"
#include "swift/Sema/SemaDecl.h"

namespace swift {
  class VarDecl;
  class Expr;
  class ASTContext;

/// Sema - This is the common semantic analysis module, which has the high-level
/// state that is shared by subclasses.
class Sema {
  Sema(const Sema&);           // DO NOT IMPLEMENT
  void operator=(const Sema&); // DO NOT IMPLEMENT
public:
  ASTContext &Context;
  
  // Sema subobjects for handling different subsystems.  These are public for
  // direct access by clients.  They are lower case so they don't shadow the
  // type names they correspond to.
  SemaType type;
  SemaExpr expr;
  SemaDecl decl;
  
  explicit Sema(ASTContext &Context);
};

} // end namespace swift

#endif

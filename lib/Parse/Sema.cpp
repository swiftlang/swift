//===--- Sema.cpp - Swift Language Semantic Analysis ----------------------===//
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
//  This file implements semantic analysis for Swift.
//
//===----------------------------------------------------------------------===//

#include "Sema.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
using namespace swift;

Sema::Sema(ASTContext &context)
  : Context(context), expr(*this), decl(*this) {
}



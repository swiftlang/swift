//===--- Builtin.cpp - Builtin Declarations -------------------------------===//
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
// This file implements declaration construction for various intrinsic
// types and functions.
//
//===----------------------------------------------------------------------===//

#include "Builtin.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"

using namespace swift;

Type swift::getBuiltinType(ASTContext &Context, Identifier Name) {
  return Type();
}

ValueDecl *swift::getBuiltinValue(ASTContext &Context, Identifier Name) {
  return nullptr;
}

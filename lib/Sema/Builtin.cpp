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
#include "swift/AST/AST.h"
using namespace swift;

Type swift::getBuiltinType(ASTContext &Context, Identifier Id) {
  llvm::StringRef Name = Id.str();
  if (Name == "int1") {
    return Context.TheInt1Type;
  } else if (Name == "int8") {
    return Context.TheInt8Type;
  } else if (Name == "int16") {
    return Context.TheInt16Type;
  } else if (Name == "int32") {
    return Context.TheInt32Type;
  } else if (Name == "int64") {
    return Context.TheInt64Type;
  } else if (Name == "float32") {
    return Context.TheFloat32Type;
  } else if (Name == "float64") {
    return Context.TheFloat64Type;
  }
  return Type();
}

ValueDecl *swift::getBuiltinValue(ASTContext &Context, Identifier Name) {
  return nullptr;
}

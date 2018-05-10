//===--- DefaultArgumentKind.cpp - Default Argument Implementation --------===//
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
//  This file implements utilities associated with default arguments.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/DefaultArgumentKind.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
using namespace swift;

StringRef swift::getDefaultArgumentSpelling(DefaultArgumentKind kind) {
  switch (kind) {
  case DefaultArgumentKind::None:
  case DefaultArgumentKind::Normal:
  case DefaultArgumentKind::Inherited:
    return StringRef();
  case DefaultArgumentKind::File:      return "#file";
  case DefaultArgumentKind::Line:      return "#line";
  case DefaultArgumentKind::Column:    return "#column";
  case DefaultArgumentKind::Function:  return "#function";
  case DefaultArgumentKind::DSOHandle: return "#dsohandle";
  case DefaultArgumentKind::NilLiteral: return "nil";
  case DefaultArgumentKind::EmptyArray: return "[]";
  case DefaultArgumentKind::EmptyDictionary: return "[:]";
  }

  llvm_unreachable("Unhandled DefaultArgumentKind in switch.");
}

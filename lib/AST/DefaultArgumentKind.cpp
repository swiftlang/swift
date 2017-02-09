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
  case DefaultArgumentKind::Nil:       return "nil";
  case DefaultArgumentKind::EmptyArray: return "[]";
  case DefaultArgumentKind::EmptyDictionary: return "[:]";
  }

  llvm_unreachable("Unhandled DefaultArgumentKind in switch.");
}

DefaultArgumentKind swift::inferDefaultArgumentKind(Expr *expr) {
  if (auto call = dyn_cast<CallExpr>(expr)) {
    if (auto ctorRefCall = dyn_cast<ConstructorRefCallExpr>(call->getFn())) {
      if (auto ctorRef = dyn_cast<DeclRefExpr>(ctorRefCall->getFn())) {
        if (auto ctor = dyn_cast<ConstructorDecl>(ctorRef->getDecl())) {
          auto ctorArg = call->getArg()->getSemanticsProvidingExpr();

          // #file, #line, #column, #function, #dsohandle.
          if (auto magic = dyn_cast<MagicIdentifierLiteralExpr>(ctorArg)) {
            switch (magic->getKind()) {
            case MagicIdentifierLiteralExpr::File:
              return DefaultArgumentKind::File;
            case MagicIdentifierLiteralExpr::Line:
              return DefaultArgumentKind::Line;
            case MagicIdentifierLiteralExpr::Column:
              return DefaultArgumentKind::Column;
            case MagicIdentifierLiteralExpr::Function:
              return DefaultArgumentKind::Function;
            case MagicIdentifierLiteralExpr::DSOHandle:
              return DefaultArgumentKind::DSOHandle;
            }
          }

          // nil.
          if (ctor->getFullName().getArgumentNames().size() == 1 &&
              ctor->getFullName().getArgumentNames()[0]
                == ctor->getASTContext().Id_nilLiteral)
            return DefaultArgumentKind::Nil;
        }
      }
    }
  }

  // Empty array literals, [].
  if (auto arrayExpr = dyn_cast<ArrayExpr>(expr)) {
    if (arrayExpr->getElements().empty())
      return DefaultArgumentKind::EmptyArray;

    return DefaultArgumentKind::None;
  }

  // Empty dictionary literals, [:].
  if (auto dictionaryExpr = dyn_cast<DictionaryExpr>(expr)) {
    if (dictionaryExpr->getElements().empty())
      return DefaultArgumentKind::EmptyDictionary;

    return DefaultArgumentKind::None;
  }

  return DefaultArgumentKind::None;
}


//===--- IRGenModule.cpp - Swift Global LLVM IR Generation ----------------===//
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
//  This file implements IR generation for global declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"

#include "swift/IRGen/IRGen.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

void IRGenModule::emitTranslationUnit(TranslationUnitDecl *TU) {
  // TODO
}

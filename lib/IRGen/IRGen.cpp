//===--- IRGen.cpp - Swift LLVM IR Generation -----------------------------===//
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
//  This file implements the entrypoints into IR generation.
//
//===----------------------------------------------------------------------===//

#include "swift/IRGen/IRGen.h"
#include "swift/IRGen/Options.h"
#include "swift/AST/Decl.h"

using namespace swift;
using namespace irgen;

void swift::performIRGeneration(TranslationUnitDecl *TU, ASTContext &Context,
                                Options &Opts) {
  // TODO :)
}

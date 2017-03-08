//===--- PrettyStackTrace.cpp - Defines SIL crash prettifiers -------------===//
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

#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

llvm::cl::opt<bool>
SILPrintOnError("sil-print-on-error", llvm::cl::init(false),
                llvm::cl::desc("Printing SIL function bodies in crash diagnostics."));

void swift::printSILLocationDescription(llvm::raw_ostream &out,
                                        SILLocation loc,
                                        ASTContext &Context) {
  if (loc.isNull()) {
    out << "<<invalid location>>";
  } else if (loc.isSILFile()) {
    printSourceLocDescription(out, loc.getSourceLoc(), Context);
  } else if (auto decl = loc.getAsASTNode<Decl>()) {
    printDeclDescription(out, decl, Context);
  } else if (auto expr = loc.getAsASTNode<Expr>()) {
    printExprDescription(out, expr, Context);
  } else if (auto stmt = loc.getAsASTNode<Stmt>()) {
    printStmtDescription(out, stmt, Context);
  } else if (auto pattern = loc.castToASTNode<Pattern>()) {
    printPatternDescription(out, pattern, Context);
  }
}

void PrettyStackTraceSILLocation::print(llvm::raw_ostream &out) const {
  out << "While " << Action << " at ";
  printSILLocationDescription(out, Loc, Context);
}

void PrettyStackTraceSILFunction::print(llvm::raw_ostream &out) const {
  out << "While " << Action << " SIL function ";
  if (!TheFn) {
    out << " <<null>>";
    return;
  }

  printFunctionInfo(out);
}

void PrettyStackTraceSILFunction::printFunctionInfo(llvm::raw_ostream &out) const {  
  out << "\"";
  TheFn->printName(out);
  out << "\".\n";

  if (!TheFn->getLocation().isNull()) {
    out << " for ";
    printSILLocationDescription(out, TheFn->getLocation(),
                                TheFn->getModule().getASTContext());
  }
  if (SILPrintOnError)
    TheFn->print(out);
}

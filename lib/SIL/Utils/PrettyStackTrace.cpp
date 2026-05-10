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

#include "swift/Basic/QuotedString.h"
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

llvm::cl::opt<bool> SILPrintModuleOnError(
    "sil-print-module-on-error", llvm::cl::init(false),
    llvm::cl::desc("Printing SIL module in crash diagnostics."));

static void printLocationDescription(llvm::raw_ostream &out,
                                         SILLocation::FilenameAndLocation loc,
                                         ASTContext &Context) {
  out << "<<debugloc at " << QuotedString(loc.filename)
      << ":" << loc.line << ":" << loc.column << ">>";
}

void swift::printSILLocationDescription(llvm::raw_ostream &out,
                                        SILLocation loc,
                                        ASTContext &Context) {
  if (loc.isASTNode()) {
    if (auto decl = loc.getAsASTNode<Decl>()) {
      printDeclDescription(out, decl);
    } else if (auto expr = loc.getAsASTNode<Expr>()) {
      printExprDescription(out, expr, Context);
    } else if (auto stmt = loc.getAsASTNode<Stmt>()) {
      printStmtDescription(out, stmt, Context);
    } else if (auto pattern = loc.castToASTNode<Pattern>()) {
      printPatternDescription(out, pattern, Context);
    } else {
      out << "<<unknown AST node>>";
    }
    return;
  }
  if (loc.isFilenameAndLocation()) {
    printLocationDescription(out, *loc.getFilenameAndLocation(), Context);
    return;
  }
  if (loc.isSILFile()) {
    printSourceLocDescription(out, loc.getSourceLoc(), Context);
    return;
  }
  out << "<<invalid location>>";
  return;
}

void PrettyStackTraceSILLocation::print(llvm::raw_ostream &out) const {
  out << "While " << Action << " at ";
  printSILLocationDescription(out, Loc, Context);
}

void PrettyStackTraceSILFunction::print(llvm::raw_ostream &out) const {
  out << "While " << action << " SIL function ";
  if (!func) {
    out << " <<null>>";
    return;
  }

  printFunctionInfo(out);
}

void PrettyStackTraceSILFunction::printFunctionInfo(llvm::raw_ostream &out) const {  
  out << "\"";
  func->printName(out);
  out << "\".\n";

  if (!func->getLocation().isNull()) {
    out << " for ";
    printSILLocationDescription(out, func->getLocation(),
                                func->getModule().getASTContext());
  }
  if (SILPrintOnError)
    func->print(out);
  if (SILPrintModuleOnError)
    func->getModule().print(out, func->getModule().getSwiftModule());
}

void PrettyStackTraceSILNode::print(llvm::raw_ostream &out) const {
  out << "While " << Action << " SIL node ";
  if (Node)
    out << *Node;
}

void PrettyStackTraceSILDeclRef::print(llvm::raw_ostream &out) const {
  out << "While " << action << " SIL decl '";
  declRef.print(out);
  out << "'\n";
}

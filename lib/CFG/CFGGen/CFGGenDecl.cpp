//===--- CFGGenDecl.cpp - Implements Lowering of ASTs -> CFGs for Decls ---===//
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

#include "CFGGen.h"
#include "Scope.h"
#include "swift/AST/AST.h"
using namespace swift;
using namespace Lowering;


namespace {
/// InitPatternWithExpr - A visitor for traversing a pattern and generating CFG
/// code to allocate the declared variables and generate initialization
/// sequences for their values.  If an initial value is specified by the "Init"
/// expression, then it is used to initialize the corresponding decls.  If not,
/// the decls are default initialized.  This does not occur within the
/// initializer's full-expression;  that should be pushed at the appropriate
/// moment.
struct InitPatternWithExpr : public PatternVisitor<InitPatternWithExpr> {
  CFGGen &Gen;
  Value *Init;
  InitPatternWithExpr(CFGGen &Gen, Value *Init) : Gen(Gen), Init(Init) {}
  
  // Paren & Typed patterns are noops, just look through them.
  void visitParenPattern(ParenPattern *P) {return visit(P->getSubPattern());}
  void visitTypedPattern(TypedPattern *P) {return visit(P->getSubPattern());}
  
  // AnyPatterns (i.e, _) don't require any further codegen beyond emitting the
  // initializer.
  void visitAnyPattern(AnyPattern *P) {}
  
  // Bind to a named pattern by creating a memory location and initializing it
  // with the initial value.
  void visitNamedPattern(NamedPattern *P) {
    VarDecl *VD = P->getDecl();
    Value *AllocVar = Gen.B.createAllocVar(VD);
    
    /// Remember that this is the memory location that we're emitting the
    /// decl to.
    Gen.VarLocs[VD] = AllocVar;

    // NOTE: "Default zero initialization" is a dubious concept.  When we get
    // something like typestate or another concept that allows us to model
    // definitive assignment, then we can consider removing it.
    auto InitVal = Init ? Init : Gen.B.createZeroValue(VD);
    Gen.B.createInitialization(VD, InitVal, AllocVar);
  }
  
  // Bind to a tuple pattern by first trying to see if we can emit
  // the initializers independently.
  void visitTuplePattern(TuplePattern *P) {
    // If we have no initializer, just emit the subpatterns using
    // the missing initializer.
    if (Init == nullptr) {
      for (auto &elt : P->getFields())
        visit(elt.getPattern());
      return;
    }

    // Otherwise, iterate through the fields of the tuple that we're
    // initializing and extract the interesting bits of Init out for each tuple
    // element.
    unsigned FieldNo = 0;
    Value *TupleInit = Init;
    for (auto &elt : P->getFields()) {
      Init = Gen.B.createTupleElement(elt.getPattern()->getType(), TupleInit,
                                      FieldNo++);
      visit(elt.getPattern());
    }
  }
};
} // end anonymous namespace


void CFGGen::visitPatternBindingDecl(PatternBindingDecl *D) {
  // FIXME: Implement support for cleanups.
  //Initialization I;
  
  // Register any cleanups with the Initialization object.
  //RegisterPattern(*this, I).visit(D->getPattern());
  
  // Actually emit the code to allocate space for each declared variable, and
  // then initialize them.

  // If an initial value was specified by the decl, use it to produce the
  // initial values, otherwise use the default value for the type.
  Value *Initializer = nullptr;
  if (D->getInit()) {
    FullExpr Scope(Cleanups);
    Initializer = visit(D->getInit());
  }
  InitPatternWithExpr(*this, Initializer).visit(D->getPattern());
}

void CFGGen::emitProlog(FuncExpr *FE) {
  // Emit the argument variables.
  for (auto &ParamPattern : FE->getParamPatterns())
    InitPatternWithExpr(*this, nullptr).visit(ParamPattern);

  // FIXME: The initializers should come from basic block arguments.
}


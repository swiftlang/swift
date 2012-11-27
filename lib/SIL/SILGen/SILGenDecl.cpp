//===--- SILGenDecl.cpp - Implements Lowering of ASTs -> SIL for Decls ----===//
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

#include "SILGen.h"
#include "Scope.h"
#include "swift/SIL/BBArgument.h"
#include "swift/AST/AST.h"
using namespace swift;
using namespace Lowering;

#include "llvm/Support/raw_ostream.h"

namespace {

class CleanupVar : public Cleanup {
  AllocBoxInst *box;
public:
  CleanupVar(AllocBoxInst *box) : box(box) {}
  void emit(SILGen &gen) override {
    gen.B.createRelease(SILLocation(), Value(box, 0));
  }
};

/// InitPatternWithExpr - A visitor for traversing a pattern and generating SIL
/// code to allocate the declared variables and generate initialization
/// sequences for their values.  If an initial value is specified by the "Init"
/// expression, then it is used to initialize the corresponding decls.  If not,
/// the decls are default initialized.  This does not occur within the
/// initializer's full-expression;  that should be pushed at the appropriate
/// moment.
struct InitPatternWithExpr : public PatternVisitor<InitPatternWithExpr> {
  SILGen &Gen;
  Value Init;
  InitPatternWithExpr(SILGen &Gen, Value Init) : Gen(Gen), Init(Init) {}
  
  // Paren & Typed patterns are noops, just look through them.
  void visitParenPattern(ParenPattern *P) { visit(P->getSubPattern()); }
  void visitTypedPattern(TypedPattern *P) { visit(P->getSubPattern()); }
  
  // AnyPatterns (i.e, _) don't require any further codegen beyond emitting the
  // initializer.
  void visitAnyPattern(AnyPattern *P) {}
  
  // Bind to a named pattern by creating a memory location and initializing it
  // with the initial value.
  void visitNamedPattern(NamedPattern *P) {
    VarDecl *vd = P->getDecl();

    // If this is a [byref] argument, just use the argument lvalue as our
    // address.
    if (vd->getType()->is<LValueType>()) {
      Gen.VarLocs[vd] = Init;
      return;
    }

    auto allocBox = Gen.B.createAllocBox(vd, vd->getType());
    auto addr = Value(allocBox, 1);

    /// Remember that this is the memory location that we're emitting the
    /// decl to.
    Gen.VarLocs[vd] = Value(allocBox, 1);

    // NOTE: "Default zero initialization" is a dubious concept.  When we get
    // something like typestate or another concept that allows us to model
    // definitive assignment, then we can consider removing it.
    auto initVal = Init ? Init : Gen.B.createZeroValue(vd, vd->getType());
    Gen.B.createStore(vd, initVal, addr);
    
    Gen.Cleanups.pushCleanup<CleanupVar>(allocBox);
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
    Value TupleInit = Init;
    for (auto &elt : P->getFields()) {
      Init = Gen.B.createTupleElement(SILLocation(), TupleInit, FieldNo++,
                                      elt.getPattern()->getType());
      visit(elt.getPattern());
    }
  }
};
} // end anonymous namespace


void SILGen::visitPatternBindingDecl(PatternBindingDecl *D) {
  // FIXME: Implement support for cleanups.
  //Initialization I;
  
  // Register any cleanups with the Initialization object.
  //RegisterPattern(*this, I).visit(D->getPattern());
  
  // Actually emit the code to allocate space for each declared variable, and
  // then initialize them.

  // If an initial value was specified by the decl, use it to produce the
  // initial values, otherwise use the default value for the type.
  Value Initializer = nullptr;
  if (D->getInit()) {
    FullExpr Scope(Cleanups);
    Initializer = visit(D->getInit());
  }
  InitPatternWithExpr(*this, Initializer).visit(D->getPattern());
}


namespace {
/// ArgumentCreatorVisitor - A visitor for traversing a pattern and creating
/// BBArgument's for each pattern variable.  This is used to create function
/// arguments.
struct ArgumentCreatorVisitor :
  public PatternVisitor<ArgumentCreatorVisitor, Value> {
  Function &F;
  ArgumentCreatorVisitor(Function &F) : F(F) {}

  // Paren & Typed patterns are noops, just look through them.
  Value visitParenPattern(ParenPattern *P) {return visit(P->getSubPattern());}
  Value visitTypedPattern(TypedPattern *P) {return visit(P->getSubPattern());}

  // Bind to a tuple pattern by first trying to see if we can emit
  // the initializers independently.
  Value visitTuplePattern(TuplePattern *P) {
    SmallVector<Value, 4> Elements;
    for (auto &elt : P->getFields())
      Elements.push_back(visit(elt.getPattern()));

    SILBuilder B(F.begin(), F);
    return B.createTuple(SILLocation(), P->getType(), Elements);
  }

  Value visitAnyPattern(AnyPattern *P) {
    return new (F) BBArgument(P->getType(), F.begin());
  }

  Value visitNamedPattern(NamedPattern *P) {
    return new (F) BBArgument(P->getType(), F.begin());
  }
};
} // end anonymous namespace


void SILGen::emitProlog(FuncExpr *FE) {
  // Emit the argument variables.
  for (auto &ParamPattern : FE->getBodyParamPatterns()) {
    // Add the BBArgument's and collect them as a Value.
    Value ArgInit = ArgumentCreatorVisitor(F).visit(ParamPattern);
    // Use the value to initialize a (mutable) variable allocation.
    InitPatternWithExpr(*this, ArgInit).visit(ParamPattern);
  }
}


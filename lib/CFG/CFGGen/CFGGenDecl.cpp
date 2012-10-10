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
struct InitPatternWithExpr
  : public PatternVisitor<InitPatternWithExpr> {
    CFGGen &Gen;
    //Initialization &I;
    Expr *Init;
    InitPatternWithExpr(CFGGen &Gen, /*Initialization &I,*/ Expr *Init)
      : Gen(Gen), /*I(I),*/ Init(Init) {}
    
    // Paren & Typed patterns are noops, just look through them.
    void visitParenPattern(ParenPattern *P) {return visit(P->getSubPattern());}
    void visitTypedPattern(TypedPattern *P) {return visit(P->getSubPattern());}


    // Bind to a wildcard pattern ("_") by emitting and ignoring the
    // initializer.
    void visitAnyPattern(AnyPattern *P) {
      if (Init) {
        FullExpr Scope(Gen.Cleanups);
        Gen.visit(Init);
      }
    }
    
    // Bind to a named pattern by emitting the initializer into place.
    void visitNamedPattern(NamedPattern *P) {
      assert(0 && "Unimp");
#if 0
      VarDecl *var = P->getDecl();
      //const TypeInfo &type = IGF.getFragileTypeInfo(var->getType());
      
      FullExpr Scope(Gen.Cleanups);
      //Address addr = I.emitVariable(IGF, var, type);
      
      //if (Init) {
      //  I.emitInit(IGF, I.getObjectForDecl(var), addr, Init, type);
      //} else {
      //  I.emitZeroInit(IGF, I.getObjectForDecl(var), addr, type);
      //}
#endif
    }
    
    
#if 0 // Tuple patterns.
    /// Try to initialize the distinct elements of a tuple pattern
    /// independently.
    bool tryInitTupleElementsIndependently(TuplePattern *P) {
      // Skip sugar.
      Expr *E = Init->getSemanticsProvidingExpr();
      
      // If we can break the initializer down into a literal, that's great.
      if (TupleExpr *literal = dyn_cast<TupleExpr>(E)) {
        assert(literal->getNumElements() == P->getNumFields());
        
        for (unsigned i = 0, e = literal->getNumElements(); i != e; ++i) {
          Init = literal->getElement(i);
          assert(Init && "no expression for tuple element!");
          visit(P->getFields()[i].getPattern());
        }
        return true;
      }
      
      // TODO: there are other possibilities here, e.g. with shuffles
      // around tuple literals.
      return false;
    }
#endif
    // Bind to a tuple pattern by first trying to see if we can emit
    // the initializers independently.
    void visitTuplePattern(TuplePattern *P) {
      assert(0 && "Unimp");
#if 0
      // If we have no initializer, just emit the subpatterns using
      // the missing initializer.
      if (!Init) {
        for (auto &elt : P->getFields())
          visit(elt.getPattern());
        return;
      }
      
      // Otherwise, try to initialize the tuple elements independently.
      if (tryInitTupleElementsIndependently(P))
        return;
      
      // Otherwise, a single expression will initialize multiple
      // tuple elements.
      FullExpr Scope(Gen.Cleanups);
      
      const TypeInfo &TI = IGF.getFragileTypeInfo(P->getType());
      
      // If we can emit the expression as an address, we can copy from
      // there into the tuple.
      if (Optional<Address> addr = IGF.tryEmitAsAddress(Init, TI)) {
        emitTuplePatternInitFromAddress(IGF, I, addr.getValue(), P, TI);
        return;
      }
      
      // Otherwise, we have to explode.
      Explosion explosion(ExplosionKind::Maximal);
      IGF.emitRValue(Init, explosion);
      InitPatternWithRValue(IGF, I, explosion).visitTuplePattern(P);
#endif
    }
};
} // end anonymous namespace


void CFGGen::visitPatternBindingDecl(PatternBindingDecl *D) {
  // FIXME: Implement support for cleanups.
  //Initialization I;
  
  // Register any cleanups with the Initialization object.
  //RegisterPattern(*this, I).visit(D->getPattern());
  
  // Actually emit the code to allocate space for each declared variable, and
  // then initialize them.  If an initial value was specified by the decl, use
  // it to produce the initial values, otherwise use the default value for the
  // type.
  InitPatternWithExpr(*this, /*I,*/ D->getInit()).visit(D->getPattern());
}

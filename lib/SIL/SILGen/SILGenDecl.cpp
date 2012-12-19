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
#include "ManagedValue.h"
#include "Scope.h"
#include "TypeInfo.h"
#include "swift/SIL/BBArgument.h"
#include "swift/AST/AST.h"
#include <iterator>
using namespace swift;
using namespace Lowering;

#include "llvm/Support/raw_ostream.h"

namespace {
  class CleanupClosureConstant : public Cleanup {
    Value closure;
  public:
    CleanupClosureConstant(Value closure) : closure(closure) {}
    void emit(SILGenFunction &gen) override {
      gen.B.createRelease(SILLocation(), closure);
    }
  };
}

void SILGenFunction::visitFuncDecl(FuncDecl *fd) {
  // Generate the local function body.
  SGM.emitFunction(fd, fd->getBody());
  
  // If there are captures, build the local closure value for the function and
  // store it as a local constant.
  if (!fd->getBody()->getCaptures().empty()) {
    Value closure = emitClosureForCapturingExpr(fd, SILConstant(fd),
                                                fd->getBody())
      .forward(*this);
    Cleanups.pushCleanup<CleanupClosureConstant>(closure);
    LocalConstants[SILConstant(fd)] = closure;
  }
}

namespace {

class CleanupVar : public Cleanup {
  AllocBoxInst *box;
public:
  CleanupVar(AllocBoxInst *box) : box(box) {}
  void emit(SILGenFunction &gen) override {
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
  SILGenFunction &Gen;
  Value Init;
  InitPatternWithExpr(SILGenFunction &Gen, Value Init) : Gen(Gen), Init(Init) {}
  
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
    
    // If this is a property, we don't need to do anything here. We'll generate
    // the getter and setter when we see their FuncDecls.
    if (vd->isProperty())
      return;

    // If this is a [byref] argument, just use the argument lvalue as our
    // address.
    // FIXME: capturing byrefs?
    if (vd->getType()->is<LValueType>()) {
      Gen.VarLocs[vd] = {Value(), Init};
      return;
    }

    // FIXME: Use escape analysis info to generate "alloc_var"/"dealloc_var"
    // stack allocations instead of "alloc_box"/"release" for values that don't
    // escape and thus don't need boxes.
    TypeInfo const &ti = Gen.getTypeInfo(vd->getType());
    auto allocBox = Gen.B.createAllocBox(vd, ti.getLoweredType());
    auto box = Value(allocBox, 0);
    auto addr = Value(allocBox, 1);

    /// Remember that this is the memory location that we're emitting the
    /// decl to.
    Gen.VarLocs[vd] = {box, addr};

    // FIXME: "Default zero initialization" is a dubious concept.  When we get
    // something like typestate or another concept that allows us to model
    // definitive assignment, then we can consider removing it.
    // FIXME: Indirect returns (such as for address-only types) could be
    // emplaced directly into the storage for the variable.
    if (ti.isAddressOnly()) {
      if (Init) {
        Gen.B.createCopyAddr(vd, Init, addr,
                             /*isTake=*/ false,
                             /*isInitialize=*/ true);
      } else {
        Gen.B.createZeroAddr(vd, addr);
      }
    } else {
      auto initVal = Init ? Init : Gen.B.createZeroValue(vd,
                                                         ti.getLoweredType());
      Gen.B.createStore(vd, initVal, addr);
    }

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
      TypeInfo const &eltTI = Gen.getTypeInfo(elt.getPattern()->getType());
      // FIXME address-only tuple
      Init = Gen.B.createExtract(SILLocation(), TupleInit, FieldNo++,
                                 eltTI.getLoweredType());
      visit(elt.getPattern());
    }
  }
};
} // end anonymous namespace


void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *D) {
  // FIXME: Implement cleanups in a way that stands up to unwinding and handles
  // cleanup of partial initializations.
  
  // Actually emit the code to allocate space for each declared variable, and
  // then initialize them.

  // If an initial value was specified by the decl, use it to produce the
  // initial values, otherwise use a "zero" placeholder value as the
  // initializer.
  Value initializer = nullptr;
  if (D->getInit()) {
    FullExpr Scope(Cleanups);
    initializer = visit(D->getInit()).forward(*this);
  }
  InitPatternWithExpr(*this, initializer).visit(D->getPattern());
}


namespace {

/// ArgumentCreatorVisitor - A visitor for traversing a pattern and creating
/// BBArguments for each pattern variable.  This is used to create function
/// arguments and to set up cleanups to release the arguments on function exit.
struct ArgumentCreatorVisitor :
  public PatternVisitor<ArgumentCreatorVisitor, Value> {
  SILGenFunction &gen;
  Function &f;
  ArgumentCreatorVisitor(SILGenFunction &gen, Function &f) : gen(gen), f(f) {}

  Value makeArgument(Type ty, BasicBlock *parent) {
    TypeInfo const &ti = gen.getTypeInfo(ty);
    return new (f.getModule()) BBArgument(ti.getLoweredType(), parent);
  }
    
  // Paren & Typed patterns are noops, just look through them.
  Value visitParenPattern(ParenPattern *P) {return visit(P->getSubPattern());}
  Value visitTypedPattern(TypedPattern *P) {
    // FIXME: work around a bug in visiting the "this" argument of methods
    if (isa<NamedPattern>(P->getSubPattern()))
      return makeArgument(P->getType(), f.begin());
    else
      return visit(P->getSubPattern());
  }

  // Bind to a tuple pattern by first trying to see if we can emit
  // the initializers independently.
  Value visitTuplePattern(TuplePattern *P) {
    SmallVector<Value, 4> Elements;
    for (auto &elt : P->getFields())
      Elements.push_back(visit(elt.getPattern()));

    SILBuilder B(f.begin(), f);
    
    // FIXME bypass SIL type lowering and make a loadable tuple even if that's
    // nonsense until we figure out what to do with address-only tuples that
    // doesn't make argument passing horrendous
    SILType loweredType = SILType::getPreLoweredType(
                            P->getType()->getCanonicalType());
    return B.createTuple(SILLocation(), loweredType, Elements);
  }

  Value visitAnyPattern(AnyPattern *P) {
    return makeArgument(P->getType(), f.begin());
  }

  Value visitNamedPattern(NamedPattern *P) {
    return makeArgument(P->getType(), f.begin());
  }
};

class CleanupCaptureBox : public Cleanup {
  Value box;
public:
  CleanupCaptureBox(Value box) : box(box) {}
  void emit(SILGenFunction &gen) override {
    gen.B.createRelease(SILLocation(), box);
  }
};
  
class CleanupCaptureValue : public Cleanup {
  Value v;
public:
  CleanupCaptureValue(Value v) : v(v) {}
  void emit(SILGenFunction &gen) override {
    gen.emitReleaseRValue(SILLocation(), v);
  }
};
  
static void makeCaptureBBArguments(SILGenFunction &gen, ValueDecl *capture) {
  // FIXME: capture local properties
  ASTContext &c = capture->getASTContext();
  switch (gen.getDeclCaptureKind(capture)) {
  case CaptureKind::LValue: {
    // LValues are captured as two arguments: a retained ObjectPointer that owns
    // the captured value, and the address of the value itself.
    SILType ty = gen.getLoweredType(capture->getTypeOfReference());
    Value box = new (gen.SGM.M) BBArgument(SILType::getObjectPointerType(c),
                                           gen.F.begin());
    Value addr = new (gen.SGM.M) BBArgument(ty,
                                            gen.F.begin());
    gen.VarLocs[capture] = {box, addr};
    gen.Cleanups.pushCleanup<CleanupCaptureBox>(box);
    break;
  }
  case CaptureKind::Byref: {
    // Byref captures are non-escaping, so it's sufficient to capture only the
    // address.
    SILType ty = gen.getLoweredType(capture->getTypeOfReference());
    Value addr = new (gen.SGM.M) BBArgument(ty, gen.F.begin());
    gen.VarLocs[capture] = {Value(), addr};
    break;
  }
  case CaptureKind::Constant: {
    // Constants are captured by value.
    assert(!capture->getType()->is<LValueType>() &&
           "capturing byref by value?!");
    TypeInfo const &ti = gen.getTypeInfo(capture->getType());
    Value value = new (gen.SGM.M) BBArgument(ti.getLoweredType(),
                                             gen.F.begin());
    gen.LocalConstants[SILConstant(capture)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    break;
  }
  case CaptureKind::GetterSetter: {
    // Capture the setter and getter closures by value.
    Type setTy = gen.SGM.Types.getPropertyType(SILConstant::Setter,
                                               capture->getType());
    SILType lSetTy = gen.getLoweredType(setTy);
    Value value = new (gen.SGM.M) BBArgument(lSetTy, gen.F.begin());
    gen.LocalConstants[SILConstant(capture, SILConstant::Setter)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    /* FALLTHROUGH */
  }
  case CaptureKind::Getter: {
    // Capture the getter closure by value.
    Type getTy = gen.SGM.Types.getPropertyType(SILConstant::Getter,
                                               capture->getType());
    SILType lGetTy = gen.getLoweredType(getTy);
    Value value = new (gen.SGM.M) BBArgument(lGetTy, gen.F.begin());
    gen.LocalConstants[SILConstant(capture, SILConstant::Getter)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    break;
  }
  }
}
  
} // end anonymous namespace

void SILGenFunction::emitProlog(CapturingExpr *ce,
                                ArrayRef<Pattern*> paramPatterns) {
  // Emit the capture argument variables.
  for (auto capture : ce->getCaptures()) {
    makeCaptureBBArguments(*this, capture);
  }
  
  // Emit the argument variables.
  for (auto &paramPattern : paramPatterns) {
    // Add the BBArguments and collect them as a Value.
    Value ArgInit = ArgumentCreatorVisitor(*this, F).visit(paramPattern);
    // Use the value to initialize a (mutable) variable allocation.
    InitPatternWithExpr(*this, ArgInit).visit(paramPattern);
  }
  
  // If the return type is address-only, emit the indirect return argument.
  Type resultType = ce->getType()->castTo<AnyFunctionType>()->getResult();
  TypeInfo const &returnTI = getTypeInfo(resultType);
  if (returnTI.isAddressOnly()) {
    IndirectReturnAddress = new (SGM.M) BBArgument(returnTI.getLoweredType(),
                                                   F.begin());
  }
}

static void rrLoadableValueElement(SILGenFunction &gen, SILLocation loc,
                                   Value v,
                                   void (SILBuilder::*createRR)(SILLocation,
                                                                Value),
                                   ReferenceTypeElement const &elt) {
  for (FragileElement comp : elt.path) {
    TypeInfo const &ti = gen.getTypeInfo(comp.type);
    assert(ti.isLoadable() && "fragile element is address-only?!");
    v = gen.B.createExtract(loc, v, comp.index, ti.getLoweredType());
  }
  (gen.B.*createRR)(loc, v);
}

static void rrLoadableValue(SILGenFunction &gen, SILLocation loc, Value v,
                            void (SILBuilder::*createRR)(SILLocation, Value),
                            ArrayRef<ReferenceTypeElement> elts) {
  for (auto &elt : elts)
    rrLoadableValueElement(gen, loc, v, createRR, elt);
}

void SILGenFunction::emitRetainRValue(SILLocation loc, Value v) {
  if (v.getType()->is<LValueType>()) {
    // v is an address-only type.
    // FIXME: should allocate, copy_addr, and return a temporary
    llvm_unreachable("FIXME: address-only types not implemented yet");
  } else {
    // v is a loadable type; retain it if necessary.
    rrLoadableValue(*this, loc, v, &SILBuilder::createRetain,
                    getTypeInfo(v.getType()).getReferenceTypeElements());
  }
}

void SILGenFunction::emitReleaseRValue(SILLocation loc, Value v) {
  if (v.getType()->is<LValueType>()) {
    // v is an address-only type; destroy it indirectly with destroy_addr.
    assert(getTypeInfo(v.getType()).isAddressOnly() &&
           "released address is not of an address-only type");
    B.createDestroyAddr(loc, v);
  } else {
    // v is a loadable type; release it if necessary.
    rrLoadableValue(*this, loc, v, &SILBuilder::createRelease,
                    getTypeInfo(v.getType()).getReferenceTypeElements());
  }
}

void SILGenModule::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(*this).visit(ntd);
}

void SILGenFunction::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(SGM).visit(ntd);
}

void SILGenType::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  for (Decl *member : ntd->getMembers())
    visit(member);
}

void SILGenType::visitFuncDecl(FuncDecl *fd) {
  SGM.emitFunction(fd, fd->getBody());
}

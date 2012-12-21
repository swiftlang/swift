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
#include "Initialization.h"
#include "ManagedValue.h"
#include "Scope.h"
#include "TypeInfo.h"
#include "llvm/ADT/OwningPtr.h"
#include "swift/SIL/BBArgument.h"
#include "swift/SIL/SILType.h"
#include "swift/AST/AST.h"
#include <iterator>
using namespace swift;
using namespace Lowering;

#include "llvm/Support/raw_ostream.h"

void Initialization::_anchor() {}

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

void SILGenFunction::visitFuncDecl(FuncDecl *fd, SGFContext C) {
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

/// An Initialization of a tuple pattern, such as "var (a,b)".
class TupleInitialization : public Initialization {
public:
  /// The sub-Initializations aggregated by this tuple initialization.
  /// The TupleInitialization object takes ownership of Initializations pushed
  /// here.
  SmallVector<Initialization*, 4>  subInitializations;

  TupleInitialization() {}
  
  ~TupleInitialization() override {
    for (auto sub : subInitializations) {
      delete sub;
    }
  }
  
  Value getAddressOrNull() override {
    if (subInitializations.size() == 1)
      return subInitializations[0]->getAddressOrNull();
    else
      return Value();
  }
  
  ArrayRef<Initialization*> getSubInitializations() override {
    return subInitializations;
  }
  
  void zeroInitialize(SILGenFunction &gen) override {
    for (auto sub : subInitializations)
      sub->zeroInitialize(gen);
  }
};

/// Cleanup for a boxed variable. Before the box contents are initialized, only
/// the allocation can be cleaned up using dealloc_ref. After initialization the
/// box and contained value are both governed by its retain count, so it
/// cleans up by a release.
class CleanupBox : public Cleanup {
  AllocBoxInst *box;
public:
  CleanupBox(AllocBoxInst *box)
    : box(box) {}
  
  void emit(SILGenFunction &gen) override {
    // FIXME: provide a way to mark the cleanup initialized.
    //if (initialized)
      gen.B.createRelease(SILLocation(), Value(box, 0));
    //else
    //  gen.B.createDeallocRef(SILLocation(), Value(box, 0));
  }
};

/// An initialization of a box allocated by alloc_box.
class BoxInitialization : public SingleInitializationBase {
  /// The box being initialized.
  AllocBoxInst *box;
  /// The cleanup for the allocated but uninitialized box. Once
  /// this box has been initialized, it can be replaced by a 'release'
  /// cleanup.
  CleanupsDepth cleanup;
  
  bool didFinish;
public:
  /// Sets up an initialization for the allocated box. This pushes a
  /// CleanupUninitializedBox cleanup that will be replaced when
  /// initialization is completed.
  BoxInitialization(AllocBoxInst *box, SILGenFunction &gen)
    : box(box),
      didFinish(false)
  {
    gen.Cleanups.pushCleanup<CleanupBox>(box);
    cleanup = gen.getCleanupsDepth();
  }
  
  ~BoxInitialization() override {
    assert(didFinish && "did not call BoxInit::finishInitialization!");
  }
  
  Value getAddressOrNull() override {
    return Value(box, 1);
  }
  
  void finishInitialization(SILGenFunction &gen) override {
    assert(!didFinish && "called BoxInit::finishInitialization twice!");
    // FIXME: mark the cleanup "initialized" somehow.
    // cleanup.markInitialized();
    didFinish = true;
  }
};
  
/// An initialization for a byref argument.
class ByrefArgumentInitialization : public Initialization {
  /// The VarDecl for the byref symbol.
  VarDecl *vd;
public:
  ByrefArgumentInitialization(VarDecl *vd) : vd(vd) {}
  
  Value getAddressOrNull() override {
    llvm_unreachable("byref argument does not have an address to store to");
  }
  ArrayRef<Initialization*> getSubInitializations() override { return {}; }

  void bindAddress(Value address, SILGenFunction &gen) override {
    // Use the input address as the var's address.
    assert(address.getType().isAddress() &&
           "binding a non-address to a byref argument?!");
    gen.VarLocs[vd] = {Value(), address};
  }

  void zeroInitialize(SILGenFunction &gen) override {}
};

/// A "null" initialization that indicates that any value being initialized into
/// this initialization should be discarded. This represents AnyPatterns
/// (that is, 'var (_)') that bind to values without storing them.
class BlackHoleInitialization : public Initialization {
public:
  BlackHoleInitialization() {}
  
  Value getAddressOrNull() override { return Value(); }
  ArrayRef<Initialization*> getSubInitializations() override {
    return {};
  }
  
  void zeroInitialize(SILGenFunction &gen) override {}
};

/// InitializationForPattern - A visitor for traversing a pattern, generating
/// SIL code to allocate the declared variables, and generating an
/// Initialization representing the needed initializations. 
struct InitializationForPattern
  : public PatternVisitor<InitializationForPattern, Initialization *>
{
  SILGenFunction &Gen;
  InitializationForPattern(SILGenFunction &Gen) : Gen(Gen) {}
  
  // Paren & Typed patterns are noops, just look through them.
  Initialization *visitParenPattern(ParenPattern *P) {
    return visit(P->getSubPattern());
  }
  Initialization *visitTypedPattern(TypedPattern *P) {
    return visit(P->getSubPattern());
  }
  
  // AnyPatterns (i.e, _) don't require any storage. Any value bound here will
  // just be dropped.
  Initialization *visitAnyPattern(AnyPattern *P) {
    return new BlackHoleInitialization();
  }
  
  // Bind to a named pattern by creating a memory location and initializing it
  // with the initial value.
  Initialization *visitNamedPattern(NamedPattern *P) {
    VarDecl *vd = P->getDecl();
    
    // If this is a property, we don't need to do anything here. We'll generate
    // the getter and setter when we see their FuncDecls.
    if (vd->isProperty())
      return new BlackHoleInitialization();

    // If this is a [byref] argument, bind the argument lvalue as our
    // address.
    if (vd->getType()->is<LValueType>())
      return new ByrefArgumentInitialization(vd);

    // FIXME: Use escape analysis info to generate "alloc_var"/"dealloc_var"
    // stack allocations instead of "alloc_box"/"release" for values that don't
    // escape and thus don't need boxes.
    SILType lType = Gen.getLoweredType(vd->getType());
    AllocBoxInst *allocBox = Gen.B.createAllocBox(vd, lType);
    auto box = Value(allocBox, 0);
    auto addr = Value(allocBox, 1);

    /// Remember that this is the memory location that we're emitting the
    /// decl to.
    Gen.VarLocs[vd] = {box, addr};

    /// Create a BoxInitialization for the uninitialized box.
    return new BoxInitialization(allocBox, Gen);
  }
  
  // Bind a tuple pattern by aggregating the component variables into a
  // TupleInitialization.
  Initialization *visitTuplePattern(TuplePattern *P) {
    TupleInitialization *init = new TupleInitialization();
    for (auto &elt : P->getFields())
      init->subInitializations.push_back(visit(elt.getPattern()));
    return init;
  }
};

} // end anonymous namespace


void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *D,
                                             SGFContext C) {
  // Allocate the variables and build up an Initialization over their
  // allocated storage.
  llvm::OwningPtr<Initialization> initialization(
    InitializationForPattern(*this).visit(D->getPattern()));
  
  // If an initial value expression was specified by the decl, emit it into
  // the initialization. Otherwise, use "zero" placeholder initialization.
  if (D->getInit()) {
    FullExpr Scope(Cleanups);
    emitExprInto(D->getInit(), initialization.get());
  } else {
    initialization->zeroInitialize(*this);
  }
}

namespace {

/// ArgumentInitVisitor - A visitor for traversing a pattern, creating
/// BBArguments, and initializing the local value for each pattern variable
/// in a function argument list.
struct ArgumentInitVisitor :
  public PatternVisitor<ArgumentInitVisitor, /*RetTy=*/ Value,
                        /*Args...=*/ Initialization*>
{
  SILGenFunction &gen;
  Function &f;
  SILBuilder initB;
  ArgumentInitVisitor(SILGenFunction &gen, Function &f)
    : gen(gen), f(f), initB(f.begin(), f) {}

  Value makeArgument(Type ty, BasicBlock *parent) {
    return new (f.getModule()) BBArgument(gen.getLoweredType(ty), parent);
  }
  
  void storeArgumentInto(Type ty, Value arg, SILLocation loc, Initialization *I)
  {
    if (I) {
      if (ty->is<LValueType>()) {
        I->bindAddress(arg, gen);
      } else if (arg.getType().isAddressOnly()) {
        initB.createCopyAddr(loc, arg, I->getAddress(),
                             /*isTake=*/false,
                             /*isInitialize=*/true);
      } else {
        initB.createStore(loc, arg, I->getAddress());
      }
      I->finishInitialization(gen);
    }
  }

  /// Create a BBArgument and store its value into the given Initialization,
  /// if not null.
  Value makeArgumentInto(Type ty, BasicBlock *parent,
                        SILLocation loc, Initialization *I) {
    Value arg = makeArgument(ty, parent);
    storeArgumentInto(ty, arg, loc, I);
    return arg;
  }
    
  // Paren & Typed patterns are noops, just look through them.
  Value visitParenPattern(ParenPattern *P, Initialization *I) {
    return visit(P->getSubPattern(), I);
  }
  Value visitTypedPattern(TypedPattern *P, Initialization *I) {
    // FIXME: work around a bug in visiting the "this" argument of methods
    if (NamedPattern *np = dyn_cast<NamedPattern>(P->getSubPattern()))
      return makeArgumentInto(P->getType(), f.begin(),
                              np->getDecl(), I);
    else
      return visit(P->getSubPattern(), I);
  }

  Value visitTuplePattern(TuplePattern *P, Initialization *I) {
    // If the tuple is empty, so should be our initialization. Just pass an
    // empty tuple upwards.
    if (P->getFields().empty()) {
      assert(I->getSubInitializations().empty() &&
             (!I->hasAddress() ||
              I->getAddress().getType().getSwiftRValueType() == P->getType()) &&
             "empty tuple pattern with non-empty-tuple initializer?!");
      return initB.createTuple(SILLocation(), gen.getLoweredType(P->getType()),
                               {});
    }
    
    // First see if we can emit the initializers independently by destructuring
    // a TupleInitialization.
    ArrayRef<Initialization *> subInits = {};
    if (I)
      subInits = I->getSubInitializations();
    if (!subInits.empty()) {
      assert(P->getFields().size() == subInits.size() &&
             "TupleInitialization size does not match tuple pattern size!");
      for (size_t i = 0; i < P->getFields().size(); ++i)
        visit(P->getFields()[i].getPattern(), subInits[i]);
      return Value();
    }
    
    // Otherwise, build a tuple of the subarguments and store it to the
    // FIXME: Doesn't work for address-only tuples. We should implement an
    // Initialization::breakIntoTupleInitialization method and use that to
    // initialize a tuple in memory piecewise.
    
    SmallVector<Value, 4> Elements;
    for (auto &elt : P->getFields())
      Elements.push_back(visit(elt.getPattern(), nullptr));
    
    SILType loweredType = gen.getLoweredType(P->getType()->getCanonicalType());
    assert(loweredType.isLoadable() && "address-only tuples not yet supported");
    Value tup = initB.createTuple(SILLocation(), loweredType, Elements);
    storeArgumentInto(P->getType(), tup, SILLocation(), I);
    return tup;
  }

  Value visitAnyPattern(AnyPattern *P, Initialization *I) {
    // A value bound to _ is unused and can be immediately released.
    assert(!I->hasAddress() && I->getSubInitializations().empty() &&
           "any pattern should match a black-hole Initialization");
    Value arg = makeArgument(P->getType(), f.begin());
    if (arg.getType().isLoadable())
      gen.emitReleaseRValue(SILLocation(), arg);
    return arg;
  }

  Value visitNamedPattern(NamedPattern *P, Initialization *I) {
    return makeArgumentInto(P->getType(), f.begin(),
                            P->getDecl(), I);
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
    // Allocate the local mutable argument storage and set up an Initialization.
    llvm::OwningPtr<Initialization> argInit(
      InitializationForPattern(*this).visit(paramPattern));
    
    // Add the BBArguments and use them to initialize the local argument values.
    ArgumentInitVisitor(*this, F).visit(paramPattern, argInit.get());
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
  assert(!v.getType().isAddress() &&
         "emitRetainRValue cannot retain an address");

  TypeInfo const &ti = getTypeInfo(v.getType().getSwiftRValueType());
  rrLoadableValue(*this, loc, v, &SILBuilder::createRetain,
                  ti.getReferenceTypeElements());
}

void SILGenFunction::emitReleaseRValue(SILLocation loc, Value v) {
  assert(!v.getType().isAddress() &&
         "emitReleaseRValue cannot release an address");

  TypeInfo const &ti = getTypeInfo(v.getType().getSwiftRValueType());
  rrLoadableValue(*this, loc, v, &SILBuilder::createRelease,
                  ti.getReferenceTypeElements());
}

void SILGenModule::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(*this).visit(ntd);
}

void SILGenFunction::visitNominalTypeDecl(NominalTypeDecl *ntd, SGFContext C) {
  SILGenType(SGM).visit(ntd);
}

void SILGenType::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  for (Decl *member : ntd->getMembers())
    visit(member);
}

void SILGenType::visitFuncDecl(FuncDecl *fd) {
  SGM.emitFunction(fd, fd->getBody());
}

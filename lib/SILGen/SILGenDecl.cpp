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
#include "RValue.h"
#include "Scope.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/AST/AST.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/Fallthrough.h"
#include <iterator>
using namespace swift;
using namespace Mangle;
using namespace Lowering;

void Initialization::_anchor() {}

namespace {
  /// A "null" initialization that indicates that any value being initialized
  /// into this initialization should be discarded. This represents AnyPatterns
  /// (that is, 'var (_)') that bind to values without storing them.
  class BlackHoleInitialization : public Initialization {
  public:
    BlackHoleInitialization()
      : Initialization(Initialization::Kind::Ignored)
    {}
    
    SILValue getAddressOrNull() override { return SILValue(); }
    ArrayRef<InitializationPtr> getSubInitializations() override {
      return {};
    }
  };
  

  /// An Initialization subclass used to destructure tuple initializations.
  class TupleElementInitialization : public SingleBufferInitialization {
  public:
    SILValue ElementAddr;
    
    TupleElementInitialization(SILValue addr)
      : ElementAddr(addr)
    {}
    
    SILValue getAddressOrNull() override { return ElementAddr; }

    void finishInitialization(SILGenFunction &gen) override {}
  };
}

ArrayRef<InitializationPtr>
Initialization::getSubInitializationsForTuple(SILGenFunction &gen, CanType type,
                                      SmallVectorImpl<InitializationPtr> &buf,
                                      SILLocation Loc) {
  auto tupleTy = cast<TupleType>(type);
  switch (kind) {
  case Kind::Tuple:
    return getSubInitializations();
  case Kind::Ignored: {
    // "Destructure" an ignored binding into multiple ignored bindings.
    for (auto fieldType : tupleTy->getElementTypes()) {
      (void) fieldType;
      buf.push_back(InitializationPtr(new BlackHoleInitialization()));
    }
    return buf;
  }
  case Kind::SingleBuffer: {
    // Destructure the buffer into per-element buffers.
    SILValue baseAddr = getAddress();
    for (unsigned i = 0, size = tupleTy->getNumElements(); i < size; ++i) {
      auto fieldType = tupleTy.getElementType(i);
      SILType fieldTy = gen.getLoweredType(fieldType).getAddressType();
      SILValue fieldAddr = gen.B.createTupleElementAddr(Loc,
                                                        baseAddr, i,
                                                        fieldTy);
                          
      buf.push_back(InitializationPtr(new
                                        TupleElementInitialization(fieldAddr)));
    }
    finishInitialization(gen);
    return buf;
  }
  case Kind::Translating:
    // This could actually be done by collecting translated values, if
    // we introduce new needs for translating initializations.
    llvm_unreachable("cannot destructure a translating initialization");
  case Kind::AddressBinding:
    llvm_unreachable("cannot destructure an address binding initialization");
  }
  llvm_unreachable("bad initialization kind");
}

namespace {
  class CleanupClosureConstant : public Cleanup {
    SILValue closure;
  public:
    CleanupClosureConstant(SILValue closure) : closure(closure) {}
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.emitStrongRelease(l, closure);
    }
  };
}

ArrayRef<Substitution> SILGenFunction::getForwardingSubstitutions() {
  auto outerFTy = F.getLoweredFunctionType();
  if (outerFTy->isPolymorphic()) {
    return buildForwardingSubstitutions(
                            outerFTy->getGenericParams()->getAllArchetypes());
  }
  return {};
}

void SILGenFunction::visitFuncDecl(FuncDecl *fd) {
  // Generate the local function body.
  SGM.emitFunction(fd);
  
  // If there are captures, build the local closure value for the function and
  // store it as a local constant.
  if (fd->getCaptureInfo().hasLocalCaptures()) {
    SILValue closure = emitClosureValue(fd, SILDeclRef(fd),
                                        getForwardingSubstitutions(), fd)
      .forward(*this);
    Cleanups.pushCleanup<CleanupClosureConstant>(closure);
    LocalFunctions[SILDeclRef(fd)] = closure;
  }
}

ArrayRef<InitializationPtr>
SingleBufferInitialization::getSubInitializations() {
  return {};
}

void TemporaryInitialization::finishInitialization(SILGenFunction &gen) {
  if (Cleanup.isValid())
    gen.Cleanups.setCleanupState(Cleanup, CleanupState::Active);
};

namespace {

/// An Initialization of a tuple pattern, such as "var (a,b)".
class TupleInitialization : public Initialization {
public:
  /// The sub-Initializations aggregated by this tuple initialization.
  /// The TupleInitialization object takes ownership of Initializations pushed
  /// here.
  SmallVector<InitializationPtr, 4> subInitializations;

  TupleInitialization() : Initialization(Initialization::Kind::Tuple) {}
  
  SILValue getAddressOrNull() override {
    if (subInitializations.size() == 1)
      return subInitializations[0]->getAddressOrNull();
    else
      return SILValue();
  }
  
  ArrayRef<InitializationPtr> getSubInitializations() override {
    return subInitializations;
  }
  
  void finishInitialization(SILGenFunction &gen) override {
    for (auto &sub : subInitializations)
      sub->finishInitialization(gen);
  }
};

/// Cleanup to destroy an initialized variable.
class DeallocStack : public Cleanup {
  SILLocation Loc;
  SILValue Addr;
public:
  DeallocStack(SILLocation loc, SILValue addr) : Loc(loc), Addr(addr) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.createDeallocStack(l, Addr);
  }
};

/// Cleanup to destroy an initialized variable.
class DestroyLocalVariable : public Cleanup {
  VarDecl *Var;
public:
  DestroyLocalVariable(VarDecl *var) : Var(var) {}
  
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.destroyLocalVariable(l, Var);
  }
};

/// Cleanup to destroy an uninitialized local variable.
class DeallocateUninitializedLocalVariable : public Cleanup {
  VarDecl *Var;
public:
  DeallocateUninitializedLocalVariable(VarDecl *var) : Var(var) {}
  
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.deallocateUninitializedLocalVariable(l, Var);
  }
};
  
/// Cleanup to perform a destroy address.
class DestroyAddr : public Cleanup {
  SILValue Addr;
public:
  DestroyAddr(SILValue addr) : Addr(addr) {}
  
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.emitDestroyAddr(l, Addr);
  }
};

/// An initialization of a local variable.
class LocalVariableInitialization : public SingleBufferInitialization {
  /// The local variable decl being initialized.
  VarDecl *Var;
  SILGenFunction &Gen;

  /// The cleanup we pushed to deallocate the local variable before it
  /// gets initialized.
  CleanupHandle DeallocCleanup;

  /// The cleanup we pushed to destroy and deallocate the local variable.
  CleanupHandle ReleaseCleanup;
  
  bool DidFinish = false;
public:
  /// Sets up an initialization for the allocated box. This pushes a
  /// CleanupUninitializedBox cleanup that will be replaced when
  /// initialization is completed.
  LocalVariableInitialization(VarDecl *var, SILGenFunction &gen)
    : Var(var), Gen(gen) {
    // Push a cleanup to destroy the local variable.  This has to be
    // inactive until the variable is initialized.
    gen.Cleanups.pushCleanupInState<DestroyLocalVariable>(CleanupState::Dormant,
                                                          var);
    ReleaseCleanup = gen.Cleanups.getTopCleanup();

    // Push a cleanup to deallocate the local variable.
    gen.Cleanups.pushCleanup<DeallocateUninitializedLocalVariable>(var);
    DeallocCleanup = gen.Cleanups.getTopCleanup();
  }
  
  ~LocalVariableInitialization() override {
    assert(DidFinish && "did not call VarInit::finishInitialization!");
  }
  
  SILValue getAddressOrNull() override {
    assert(Gen.VarLocs.count(Var) && "did not emit var?!");
    return Gen.VarLocs[Var].getAddress();
  }

  void finishInitialization(SILGenFunction &gen) override {
    assert(!DidFinish &&
           "called LocalVariableInitialization::finishInitialization twice!");
    Gen.Cleanups.setCleanupState(DeallocCleanup, CleanupState::Dead);
    Gen.Cleanups.setCleanupState(ReleaseCleanup, CleanupState::Active);
    DidFinish = true;
  }
};
  
/// An initialization for a global variable.
class GlobalInitialization : public SingleBufferInitialization {
  /// The physical address of the global.
  SILValue address;
  
public:
  GlobalInitialization(SILValue address) : address(address)
  {}
  
  SILValue getAddressOrNull() override {
    return address;
  }
  
  void finishInitialization(SILGenFunction &gen) override {
    // Globals don't need to be cleaned up.
  }
};
  
/// Cleanup that writes back to a inout argument on function exit.
class CleanupWriteBackToInOut : public Cleanup {
  VarDecl *var;
  SILValue inoutAddr;

public:
  CleanupWriteBackToInOut(VarDecl *var, SILValue inoutAddr)
    : var(var), inoutAddr(inoutAddr) {}
  
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    // Assign from the local variable to the inout address with an
    // 'autogenerated' copyaddr.
    l.markAutoGenerated();
    gen.B.createCopyAddr(l, gen.VarLocs[var].getAddress(), inoutAddr,
                         IsNotTake, IsNotInitialization);
  }
};
  
/// Initialize a writeback buffer that receives the "in" value of a inout
/// argument on function entry and writes the "out" value back to the inout
/// address on function exit.
class InOutInitialization : public Initialization {
  /// The VarDecl for the inout symbol.
  VarDecl *vd;
public:
  InOutInitialization(VarDecl *vd)
    : Initialization(Initialization::Kind::AddressBinding), vd(vd) {}
  
  SILValue getAddressOrNull() override {
    llvm_unreachable("inout argument should be bound by bindAddress");
  }
  ArrayRef<InitializationPtr> getSubInitializations() override {
    return {};
  }

  void bindAddress(SILValue address, SILGenFunction &gen,
                   SILLocation loc) override {
    // Allocate the local variable for the inout.
    auto initVar = gen.emitLocalVariableWithCleanup(vd);
    
    // Initialize with the value from the inout with an "autogenerated"
    // copyaddr.
    loc.markAsPrologue();
    loc.markAutoGenerated();
    gen.B.createCopyAddr(loc, address, initVar->getAddress(),
                         IsNotTake, IsInitialization);
    initVar->finishInitialization(gen);
    
    // Set up a cleanup to write back to the inout.
    gen.Cleanups.pushCleanup<CleanupWriteBackToInOut>(vd, address);
  }
};

/// Initialize a variable of reference-storage type.
class ReferenceStorageInitialization : public Initialization {
  InitializationPtr VarInit;
public:
  ReferenceStorageInitialization(InitializationPtr &&subInit)
    : Initialization(Initialization::Kind::Translating),
      VarInit(std::move(subInit)) {}

  ArrayRef<InitializationPtr> getSubInitializations() override { return {}; }
  SILValue getAddressOrNull() override { return SILValue(); }

  void translateValue(SILGenFunction &gen, SILLocation loc,
                      ManagedValue value) override {
    value.forwardInto(gen, loc, VarInit->getAddress());
  }

  void finishInitialization(SILGenFunction &gen) override {
    VarInit->finishInitialization(gen);
  }
};
  
/// InitializationForPattern - A visitor for traversing a pattern, generating
/// SIL code to allocate the declared variables, and generating an
/// Initialization representing the needed initializations. 
struct InitializationForPattern
  : public PatternVisitor<InitializationForPattern, InitializationPtr>
{
  SILGenFunction &Gen;
  enum ArgumentOrVar_t { Argument, Var } ArgumentOrVar;
  InitializationForPattern(SILGenFunction &Gen, ArgumentOrVar_t ArgumentOrVar)
    : Gen(Gen), ArgumentOrVar(ArgumentOrVar) {}
  
  // Paren & Typed patterns are noops, just look through them.
  InitializationPtr visitParenPattern(ParenPattern *P) {
    return visit(P->getSubPattern());
  }
  InitializationPtr visitTypedPattern(TypedPattern *P) {
    return visit(P->getSubPattern());
  }
  
  // AnyPatterns (i.e, _) don't require any storage. Any value bound here will
  // just be dropped.
  InitializationPtr visitAnyPattern(AnyPattern *P) {
    return InitializationPtr(new BlackHoleInitialization());
  }
  
  // Bind to a named pattern by creating a memory location and initializing it
  // with the initial value.
  InitializationPtr visitNamedPattern(NamedPattern *P) {
    VarDecl *vd = P->getDecl();
    
    // If this is a computed variable, we don't need to do anything here.
    // We'll generate the getter and setter when we see their FuncDecls.
    if (vd->isComputed())
      return InitializationPtr(new BlackHoleInitialization());

    // If this is a global variable, initialize it without allocations or
    // cleanups.
    if (!vd->getDeclContext()->isLocalContext()) {
      SILValue addr = Gen.B.createGlobalAddr(vd, vd,
                          Gen.getLoweredType(vd->getType()).getAddressType());
      
      // In a top level context, all global variables must be initialized.
      addr = Gen.B.createMarkUninitializedGlobalVar(vd, addr);
      
      Gen.VarLocs[vd] = SILGenFunction::VarLoc::getAddress(addr);
      return InitializationPtr(new GlobalInitialization(addr));
    }

    CanType varType = vd->getType()->getCanonicalType();

    // If this is an @inout parameter, set up the writeback variable.
    if (isa<LValueType>(varType))
      return InitializationPtr(new InOutInitialization(vd));

    // If this is a 'self' argument for a class method (not struct method), emit
    // it as a constant value.  Since it is a constant value, there is no memory
    // location associated with it, and thus we don't need an initialization -
    // the store will provide the value directly into the VarLocs map.
    if (vd->isImplicit() && vd->getName().str() == "self" &&
        vd->getType()->hasReferenceSemantics())
      return InitializationPtr(new BlackHoleInitialization());
    
    // Otherwise, we have a normal local-variable initialization.
    auto varInit = Gen.emitLocalVariableWithCleanup(vd);

    // Initializing a @weak or @unowned variable requires a change in type.
    if (isa<ReferenceStorageType>(varType))
      return InitializationPtr(new ReferenceStorageInitialization(
                                                         std::move(varInit)));

    // Otherwise, the pattern type should match the type of the variable.
    // FIXME: why do we ever get patterns without types here?
    assert(!P->hasType() || varType == P->getType()->getCanonicalType());
    return varInit;
  }
  
  // Bind a tuple pattern by aggregating the component variables into a
  // TupleInitialization.
  InitializationPtr visitTuplePattern(TuplePattern *P) {
    TupleInitialization *init = new TupleInitialization();
    for (auto &elt : P->getFields())
      init->subInitializations.push_back(visit(elt.getPattern()));
    return InitializationPtr(init);
  }
  
  // TODO: Handle bindings from 'case' labels and match expressions.
#define INVALID_PATTERN(Id, Parent) \
  InitializationPtr visit##Id##Pattern(Id##Pattern *) { \
    llvm_unreachable("pattern not valid in argument or var binding"); \
  }
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) INVALID_PATTERN(Id, Parent)
#include "swift/AST/PatternNodes.def"
#undef INVALID_PATTERN
};

} // end anonymous namespace


void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *D) {
  // Allocate the variables and build up an Initialization over their
  // allocated storage.
  InitializationPtr initialization =
    InitializationForPattern(*this, InitializationForPattern::Var)
      .visit(D->getPattern());
  
  // If an initial value expression was specified by the decl, emit it into
  // the initialization. Otherwise, leave it uninitialized for DI to resolve.
  if (D->getInit()) {
    FullExpr Scope(Cleanups, CleanupLocation(D->getInit()));
    emitExprInto(D->getInit(), initialization.get());
  } else {
    initialization->finishInitialization(*this);
  }
}

InitializationPtr
SILGenFunction::emitPatternBindingInitialization(Pattern *P) {
  return InitializationForPattern(*this, InitializationForPattern::Var)
      .visit(P);
}

/// Enter a cleanup to deallocate the given location.
CleanupHandle SILGenFunction::enterDeallocStackCleanup(SILLocation loc,
                                                       SILValue temp) {
  assert(temp.getType().isLocalStorage() &&
         "must deallocate container operand, not address operand!");
  Cleanups.pushCleanup<DeallocStack>(loc, temp);
  return Cleanups.getTopCleanup();
}


class CleanupCaptureBox : public Cleanup {
  SILValue box;
public:
  CleanupCaptureBox(SILValue box) : box(box) {}
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.emitStrongRelease(l, box);
  }
};

class CleanupCaptureValue : public Cleanup {
  SILValue v;
public:
  CleanupCaptureValue(SILValue v) : v(v) {}
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.emitDestroyValueOperation(l, v);
  }
};


namespace {

/// A visitor for traversing a pattern, creating
/// SILArguments, and initializing the local value for each pattern variable
/// in a function argument list.
struct ArgumentInitVisitor :
  public PatternVisitor<ArgumentInitVisitor, /*RetTy=*/ SILValue,
                        /*Args...=*/ Initialization*>
{
  SILGenFunction &gen;
  SILFunction &f;
  SILBuilder &initB;
  ArgumentInitVisitor(SILGenFunction &gen, SILFunction &f)
    : gen(gen), f(f), initB(gen.B) {}

  SILValue makeArgument(Type ty, SILBasicBlock *parent, SILLocation l) {
    assert(ty && "no type?!");
    return RValue::emitBBArguments(ty->getCanonicalType(),
                                   gen, parent, l).forwardAsSingleValue(gen, l);
  }
  
  void storeArgumentInto(Type ty, SILValue arg, SILLocation loc,
                         Initialization *I)
  {
    assert(ty && "no type?!");
    if (!I) return;
    switch (I->kind) {
    case Initialization::Kind::AddressBinding:
      I->bindAddress(arg, gen, loc);
      // If this is an address-only non-inout argument, we take ownership
      // of the referenced value.
      if (!ty->is<LValueType>())
        gen.Cleanups.pushCleanup<DestroyAddr>(arg);
      break;

    case Initialization::Kind::Translating:
      I->translateValue(gen, loc, gen.emitManagedRValueWithCleanup(arg));
      break;

    case Initialization::Kind::SingleBuffer:
      gen.emitSemanticStore(loc, arg, I->getAddress(),
                            gen.getTypeLowering(ty), IsInitialization);
      break;

    case Initialization::Kind::Ignored:
      break;

    case Initialization::Kind::Tuple:
      llvm_unreachable("tuple initializations should be destructured before "
                       "reaching here");
    }

    I->finishInitialization(gen);
  }

  /// Create a SILArgument and store its value into the given Initialization,
  /// if not null.
  SILValue makeArgumentInto(Type ty, SILBasicBlock *parent,
                        SILLocation loc, Initialization *I) {
    assert(ty && "no type?!");
    loc.markAsPrologue();
    SILValue arg = makeArgument(ty, parent, loc);
    storeArgumentInto(ty, arg, loc, I);
    return arg;
  }
    
  // Paren & Typed patterns are no-ops. Just look through them.
  SILValue visitParenPattern(ParenPattern *P, Initialization *I) {
    return visit(P->getSubPattern(), I);
  }
  SILValue visitTypedPattern(TypedPattern *P, Initialization *I) {
    return visit(P->getSubPattern(), I);
  }

  SILValue visitTuplePattern(TuplePattern *P, Initialization *I) {
    // If the tuple is empty, so should be our initialization. Just pass an
    // empty tuple upwards.
    if (P->getFields().empty()) {
      switch (I->kind) {
      case Initialization::Kind::Ignored:
        break;
      case Initialization::Kind::Tuple:
        assert(I->getSubInitializations().empty() &&
               "empty tuple pattern with non-empty-tuple initializer?!");
        break;
      case Initialization::Kind::AddressBinding:
        llvm_unreachable("empty tuple pattern with inout initializer?!");
      case Initialization::Kind::Translating:
        llvm_unreachable("empty tuple pattern with translating initializer?!");
        
      case Initialization::Kind::SingleBuffer:
        assert(I->getAddress().getType().getSwiftRValueType()
                 == P->getType()->getCanonicalType()
               && "empty tuple pattern with non-empty-tuple initializer?!");
        break;
      }

      return initB.createTuple(P, gen.getLoweredType(P->getType()), {});
    }
    
    // Destructure the initialization into per-element Initializations.
    SmallVector<InitializationPtr, 2> buf;
    ArrayRef<InitializationPtr> subInits =
      I->getSubInitializationsForTuple(gen, P->getType()->getCanonicalType(),
                                       buf, RegularLocation(P));

    assert(P->getFields().size() == subInits.size() &&
           "TupleInitialization size does not match tuple pattern size!");
    for (size_t i = 0, size = P->getFields().size(); i < size; ++i)
      visit(P->getFields()[i].getPattern(), subInits[i].get());
    return SILValue();
  }

  SILValue visitAnyPattern(AnyPattern *P, Initialization *I) {
    // A value bound to _ is unused and can be immediately released.
    assert(I->kind == Initialization::Kind::Ignored &&
           "any pattern should match a black-hole Initialization");
    auto &lowering = gen.getTypeLowering(P->getType());

    auto &AC = gen.getASTContext();
    auto VD = new (AC) VarDecl(/*static*/ false, /*IsLet*/ true, SourceLoc(),
                               // FIXME: we should probably number them.
                               AC.getIdentifier("_"), P->getType(),
                               f.getDeclContext());

    SILValue arg = makeArgument(P->getType(), f.begin(), VD);
    lowering.emitDestroyRValue(gen.B, P, arg);
    return arg;
  }

  SILValue visitNamedPattern(NamedPattern *P, Initialization *I) {
    VarDecl *vd = P->getDecl();
    
    // If this is a 'self' argument for a class method (not struct method), emit
    // it as a constant value.  Since it is a constant value, there is no memory
    // location associated with it, and thus we don't need an initialization -
    // the store will provide the value directly into the VarLocs map.
    if (vd->isImplicit() && vd->getName().str() == "self" &&
        vd->getType()->hasReferenceSemantics()) {
      SILLocation loc = vd;
      loc.markAsPrologue();
      SILValue arg = makeArgument(P->getType(), f.begin(), loc);
      assert(!gen.VarLocs.count(vd) && "Already emitted a this vardecl?");
      gen.VarLocs[vd] = SILGenFunction::VarLoc::getConstant(arg);
      gen.Cleanups.pushCleanup<CleanupCaptureValue>(arg);
      I->finishInitialization(gen);
      return arg;
    }
    
    return makeArgumentInto(P->getType(), f.begin(), vd, I);
  }
  
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) \
  SILValue visit##Id##Pattern(Id##Pattern *, Initialization *) { \
    llvm_unreachable("pattern not valid in argument binding"); \
  }
#include "swift/AST/PatternNodes.def"

};

static void emitCaptureArguments(SILGenFunction & gen, ValueDecl *capture) {
  ASTContext &c = capture->getASTContext();
  switch (getDeclCaptureKind(capture)) {
  case CaptureKind::None:
    break;

  case CaptureKind::Box: {
    // LValues are captured as two arguments: a retained ObjectPointer that owns
    // the captured value, and the address of the value itself.
    SILType ty = gen.getLoweredType(capture->getType()).getAddressType();
    SILValue box = new (gen.SGM.M) SILArgument(SILType::getObjectPointerType(c),
                                               gen.F.begin(), capture);
    SILValue addr = new (gen.SGM.M) SILArgument(ty, gen.F.begin(), capture);
    gen.VarLocs[capture] = SILGenFunction::VarLoc::getAddress(addr, box);
    gen.Cleanups.pushCleanup<CleanupCaptureBox>(box);
    break;
  }
  case CaptureKind::Constant: {
    // Constant decls are captured by value.
    SILType ty = gen.getLoweredType(capture->getType());
    SILValue val = new (gen.SGM.M) SILArgument(ty, gen.F.begin(), capture);
    gen.VarLocs[capture] = SILGenFunction::VarLoc::getConstant(val);
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(val);
    break;
  }
  case CaptureKind::LocalFunction: {
    // Local functions are captured by value.
    assert(!capture->getType()->is<LValueType>() &&
           "capturing inout by value?!");
    const TypeLowering &ti = gen.getTypeLowering(capture->getType());
    SILValue value = new (gen.SGM.M) SILArgument(ti.getLoweredType(),
                                                 gen.F.begin(), capture);
    gen.LocalFunctions[SILDeclRef(capture)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    break;
  }
  case CaptureKind::GetterSetter: {
    // Capture the setter and getter closures by value.
    Type setTy;
    if (auto subscript = dyn_cast<SubscriptDecl>(capture))
      setTy = subscript->getSetterType();
    else
      setTy = cast<VarDecl>(capture)->getSetterType();
    SILType lSetTy = gen.getLoweredType(setTy);
    SILValue value = new (gen.SGM.M) SILArgument(lSetTy, gen.F.begin(),capture);
    gen.LocalFunctions[SILDeclRef(capture, SILDeclRef::Kind::Setter)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    SWIFT_FALLTHROUGH;
  }
  case CaptureKind::Getter: {
    // Capture the getter closure by value.
    Type getTy;
    if (auto subscript = dyn_cast<SubscriptDecl>(capture))
      getTy = subscript->getGetterType();
    else
      getTy = cast<VarDecl>(capture)->getGetterType();
    SILType lGetTy = gen.getLoweredType(getTy);
    SILValue value = new (gen.SGM.M) SILArgument(lGetTy, gen.F.begin(),capture);
    gen.LocalFunctions[SILDeclRef(capture, SILDeclRef::Kind::Getter)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    break;
  }
  }
}
  
} // end anonymous namespace

void SILGenFunction::emitProlog(AnyFunctionRef TheClosure,
                                ArrayRef<Pattern *> paramPatterns,
                                Type resultType) {
  emitProlog(paramPatterns, resultType, TheClosure.getAsDeclContext());

  // Emit the capture argument variables. These are placed last because they
  // become the first curry level of the SIL function.
  SmallVector<ValueDecl*, 4> LocalCaptures;
  TheClosure.getCaptureInfo().getLocalCaptures(LocalCaptures);
  for (auto capture : LocalCaptures)
    emitCaptureArguments(*this, capture);
}

void SILGenFunction::emitProlog(ArrayRef<Pattern *> paramPatterns,
                                Type resultType, DeclContext *DeclCtx) {
  // If the return type is address-only, emit the indirect return argument.
  const TypeLowering &returnTI = getTypeLowering(resultType);
  if (returnTI.isReturnedIndirectly()) {
    auto &AC = getASTContext();
    auto VD = new (AC) VarDecl(/*static*/ false, /*IsLet*/ false, SourceLoc(),
                               AC.getIdentifier("$return_value"), resultType,
                               DeclCtx);
    IndirectReturnAddress = new (SGM.M)
      SILArgument(returnTI.getLoweredType(), F.begin(), VD);
  }
  
  // Emit the argument variables in calling convention order.
  for (Pattern *p : reversed(paramPatterns)) {
    // Allocate the local mutable argument storage and set up an Initialization.
    InitializationPtr argInit
      = InitializationForPattern(*this, InitializationForPattern::Argument)
         .visit(p);
    // Add the SILArguments and use them to initialize the local argument
    // values.
    ArgumentInitVisitor(*this, F).visit(p, argInit.get());
  }
}

SILValue SILGenFunction::emitDestructorProlog(ClassDecl *CD,
                                              DestructorDecl *DD) {
  // Emit the implicit 'self' argument.
  VarDecl *selfDecl = DD ? DD->getImplicitSelfDecl() : nullptr;
  assert((!selfDecl || selfDecl->getType()->hasReferenceSemantics()) &&
         "destructor's implicit this is a value type?!");

  SILType selfType = getLoweredLoadableType(CD->getDeclaredTypeInContext());
  assert((!selfDecl || getLoweredLoadableType(selfDecl->getType()) == selfType)
         && "decl type doesn't match destructor's implicit this type");
  
  SILValue selfValue = new (SGM.M) SILArgument(selfType, F.begin(), selfDecl);
  VarLocs[selfDecl] = VarLoc::getConstant(selfValue);
  return selfValue;
}

void SILGenFunction::prepareEpilog(Type resultType, CleanupLocation CleanupL) {
  auto *epilogBB = createBasicBlock();
  
  // If we have a non-null, non-void, non-address-only return type, receive the
  // return value via a BB argument.
  NeedsReturn = resultType && !resultType->isVoid();
  if (NeedsReturn) {
    auto &resultTI = getTypeLowering(resultType);
    if (!resultTI.isAddressOnly())
      new (F.getModule()) SILArgument(resultTI.getLoweredType(), epilogBB);
  }
  ReturnDest = JumpDest(epilogBB, getCleanupsDepth(), CleanupL);
}

/// Determine whether this is a generic function that isn't generic simply
/// because it's in a protocol.
///
/// FIXME: This whole thing is a hack. Sema should be properly
/// annotating as [objc] only those functions that can have Objective-C
/// entry points.
static bool isNonProtocolGeneric(AbstractFunctionDecl *func) {
  // If the function type is polymorphic and the context isn't a protocol
  // (where everything has an implicit single level of genericity),
  // it's generic.
  if (func->getType()->is<PolymorphicFunctionType>() &&
      !isa<ProtocolDecl>(func->getDeclContext()))
    return true;

  // Is this a polymorphic function within a non-generic type?
  if (func->getType()->castTo<AnyFunctionType>()->getResult()
        ->is<PolymorphicFunctionType>())
    return true;

  return false;
}

bool SILGenModule::requiresObjCMethodEntryPoint(FuncDecl *method) {
  // Property accessors should be generated alongside the property.
  if (method->isGetterOrSetter())
    return false;
    
  // We don't export generic methods or subclasses to Objective-C yet.
  if (isNonProtocolGeneric(method))
    return false;
    
  if (method->isObjC() || method->getAttrs().isIBAction())
    return true;
  if (auto override = method->getOverriddenDecl())
    return requiresObjCMethodEntryPoint(override);
  return false;
}

bool SILGenModule::requiresObjCMethodEntryPoint(ConstructorDecl *constructor) {
  // We don't export generic methods or subclasses to Objective-C yet.
  if (isNonProtocolGeneric(constructor))
    return false;

  return constructor->isObjC();
}

bool SILGenModule::requiresObjCPropertyEntryPoints(VarDecl *property) {
  // We don't export generic methods or subclasses to IRGen yet.
  if (property->getDeclContext()->getDeclaredTypeInContext() &&
      property->getDeclContext()->getDeclaredTypeInContext()
        ->is<BoundGenericType>() &&
      !isa<ProtocolDecl>(property->getDeclContext()))
    return false;
  
  if (auto override = property->getOverriddenDecl())
    return requiresObjCPropertyEntryPoints(override);

  if (!property->isObjC())
    return false;
  
  // Don't expose objc properties for function types. We can't autorelease them,
  // and eventually we want to map them back to blocks.
  if (property->getType()->is<AnyFunctionType>())
    return false;
  
  return true;
}

bool SILGenModule::requiresObjCSubscriptEntryPoints(SubscriptDecl *subscript) {
  // We don't export generic methods or subclasses to IRGen yet.
  if (subscript->getDeclContext()->getDeclaredTypeInContext()
        ->is<BoundGenericType>() &&
      !isa<ProtocolDecl>(subscript->getDeclContext()))
    return false;
  
  if (auto override = subscript->getOverriddenDecl())
    return requiresObjCSubscriptEntryPoints(override);

  return subscript->isObjC();
}

bool SILGenModule::requiresObjCDispatch(ValueDecl *vd) {
  if (auto *fd = dyn_cast<FuncDecl>(vd)) {
    // If a function has an associated Clang node, it's foreign.
    if (vd->hasClangNode())
      return true;

    return requiresObjCMethodEntryPoint(fd);
  }
  if (auto *cd = dyn_cast<ConstructorDecl>(vd))
    return requiresObjCMethodEntryPoint(cd);
  if (auto *pd = dyn_cast<VarDecl>(vd))
    return requiresObjCPropertyEntryPoints(pd);
  if (auto *sd = dyn_cast<SubscriptDecl>(vd))
    return requiresObjCSubscriptEntryPoints(sd);
  return vd->isObjC();
}

bool SILGenModule::requiresObjCSuperDispatch(ValueDecl *vd) {
  if (auto *cd = dyn_cast<ConstructorDecl>(vd)) {
    DeclContext *ctorDC = cd->getDeclContext();
    if (auto *cls = dyn_cast<ClassDecl>(ctorDC)) {
      return cls->isObjC();
    }
  }
  return requiresObjCDispatch(vd);
}

/// An ASTVisitor for populating SILVTable entries from ClassDecl members.
class SILGenVTable : public Lowering::ASTVisitor<SILGenVTable> {
public:
  SILGenModule &SGM;
  ClassDecl *theClass;
  std::vector<SILVTable::Pair> vtableEntries;
  
  SILGenVTable(SILGenModule &SGM, ClassDecl *theClass)
    : SGM(SGM), theClass(theClass)
  {
    // Populate the superclass members, if any.
    Type super = theClass->getSuperclass();
    if (super && super->getClassOrBoundGenericClass())
      visitAncestor(super->getClassOrBoundGenericClass());
  }

  ~SILGenVTable() {
    // Create the vtable.
    SILVTable::create(SGM.M, theClass, vtableEntries);
  }
  
  void visitAncestor(ClassDecl *ancestor) {
    // Recursively visit all our ancestors.
    Type super = ancestor->getSuperclass();
    if (super && super->getClassOrBoundGenericClass())
      visitAncestor(super->getClassOrBoundGenericClass());
    
    for (auto member : ancestor->getMembers())
      visit(member);
  }
  
  // Add an entry to the vtable.
  void addEntry(SILDeclRef member) {
    // Try to find an overridden entry.
    // NB: Mutates vtableEntries in-place
    // FIXME: O(n^2)
    if (auto overridden = member.getOverridden()) {
      // If we overrode an ObjC decl, or a decl from an extension, it won't be
      // in a vtable; create a new entry.
      if (overridden.getDecl()->hasClangNode())
        goto not_overridden;
      // If we overrode a decl from an extension, it won't be in a vtable
      // either. This can occur for extensions to ObjC classes.
      if (isa<ExtensionDecl>(overridden.getDecl()->getDeclContext()))
        goto not_overridden;

      for (SILVTable::Pair &entry : vtableEntries) {
        SILDeclRef ref = overridden;
        
        do {
          // Replace the overridden member.
          if (entry.first == ref) {
            entry = {member, SGM.getFunction(member)};
            return;
          }
        } while ((ref = ref.getOverridden()));
      }
      llvm_unreachable("no overridden vtable entry?!");
    }
    
  not_overridden:
    // Otherwise, introduce a new vtable entry.
    vtableEntries.emplace_back(member, SGM.getFunction(member));
  }
  
  // Default for members that don't require vtable entries.
  void visitDecl(Decl*) {}
  
  void visitFuncDecl(FuncDecl *fd) {
    // ObjC decls don't go in vtables.
    if (fd->hasClangNode())
      return;
    
    addEntry(SILDeclRef(fd));
  }
  
  void visitConstructorDecl(ConstructorDecl *cd) {
    // FIXME: If this is a dynamically-dispatched constructor, add its
    // initializing entry point to the vtable.
  }
  
  void visitVarDecl(VarDecl *vd) {
    // FIXME: If this is a dynamically-dispatched property, add its getter and
    // setter to the vtable.
  }
  
  void visitSubscriptDecl(SubscriptDecl *sd) {
    // FIXME: If this is a dynamically-dispatched property, add its getter and
    // setter to the vtable.
  }
};

/// An ASTVisitor for generating SIL from method declarations
/// inside nominal types.
class SILGenType : public Lowering::ASTVisitor<SILGenType> {
public:
  SILGenModule &SGM;
  NominalTypeDecl *theType;
  DestructorDecl *explicitDestructor;
  Optional<SILGenVTable> genVTable;
  
  SILGenType(SILGenModule &SGM, NominalTypeDecl *theType)
    : SGM(SGM), theType(theType), explicitDestructor(nullptr) {}

  ~SILGenType() {
    // Emit the destructor for a class type.
    if (ClassDecl *theClass = dyn_cast<ClassDecl>(theType)) {
      SGM.emitDestructor(theClass, explicitDestructor);
    } else {
      assert(!explicitDestructor && "destructor in non-class type?!");
    }
  }
  
  /// Emit SIL functions for all the members of the type.
  void emitType() {
    // Start building a vtable if this is a class.
    if (auto theClass = dyn_cast<ClassDecl>(theType))
      genVTable.emplace(SGM, theClass);
    
    for (Decl *member : theType->getMembers()) {
      if (genVTable)
        genVTable->visit(member);
      
      visit(member);
    }
    
    // Emit witness tables for conformances.
    for (auto *conformance : theType->getConformances())
      SGM.emitProtocolConformance(conformance);
  }
  
  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    SGM.emitFunction(fd);
    // FIXME: Default implementations in protocols.
    if (SGM.requiresObjCMethodEntryPoint(fd) &&
        !isa<ProtocolDecl>(fd->getDeclContext()))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    SGM.emitConstructor(cd);

    if (SGM.requiresObjCMethodEntryPoint(cd) &&
        !isa<ProtocolDecl>(cd->getDeclContext()))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    // Save the destructor decl so we can use it to generate the destructor
    // later.
    assert(!explicitDestructor && "more than one destructor decl in type?!");
    explicitDestructor = dd;
  }
  
  void visitEnumElementDecl(EnumElementDecl *ued) {
    assert(isa<EnumDecl>(theType));
    SGM.emitEnumConstructor(ued);
  }
  
  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers for static variables.
    if (pd->isStatic() && pd->hasInit()) {
      SGM.emitGlobalInitialization(pd);
    }
  }
  
  void visitVarDecl(VarDecl *vd) {
    // Collect global variables for static properties.
    // FIXME: We can't statically emit a global variable for generic properties.
    if (vd->isStatic() && !vd->isComputed()) {
      assert(!theType->getGenericParams()
             && "generic static properties not implemented");
      assert((isa<StructDecl>(theType) || isa<EnumDecl>(theType))
             && "only value type static properties are implemented");
      
      SGM.addGlobalVariable(vd);
      
      return;
    }
    
    // FIXME: Default implementations in protocols.
    if (SGM.requiresObjCPropertyEntryPoints(vd) &&
        !isa<ProtocolDecl>(vd->getDeclContext()))
      SGM.emitObjCPropertyMethodThunks(vd);
  }

  void visitSubscriptDecl(SubscriptDecl *sd) {
    // FIXME: Default implementations in protocols.
    if (SGM.requiresObjCSubscriptEntryPoints(sd) &&
        !isa<ProtocolDecl>(sd->getDeclContext()))
      SGM.emitObjCSubscriptMethodThunks(sd);
  }
};

void SILGenModule::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(*this, ntd).emitType();
}

void SILGenFunction::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(SGM, ntd).emitType();
}

void SILGenModule::emitExternalDefinition(Decl *d) {
  switch (d->getKind()) {
  case DeclKind::Func: {
    emitFunction(cast<FuncDecl>(d));
    break;
  }
  case DeclKind::Constructor: {
    emitConstructor(cast<ConstructorDecl>(d));
    break;
  }
  case DeclKind::Enum: {
    // Emit the enum cases and RawRepresentable methods.
    for (auto member : cast<EnumDecl>(d)->getMembers()) {
      if (auto elt = dyn_cast<EnumElementDecl>(member))
        emitEnumConstructor(elt);
      else if (auto func = dyn_cast<FuncDecl>(member))
        emitFunction(func);
    }
    break;
  }
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
    // Nothing to do in SILGen for other external types.
    break;
      
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::EnumElement:
  case DeclKind::TopLevelCode:
  case DeclKind::TypeAlias:
  case DeclKind::AssociatedType:
  case DeclKind::GenericTypeParam:
  case DeclKind::Var:
  case DeclKind::Import:
  case DeclKind::Subscript:
  case DeclKind::Destructor:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("Not a valid external definition for SILGen");
  }
}

/// SILGenExtension - an ASTVisitor for generating SIL from method declarations
/// and protocol conformances inside type extensions.
class SILGenExtension : public Lowering::ASTVisitor<SILGenExtension> {
public:
  SILGenModule &SGM;

  SILGenExtension(SILGenModule &SGM)
    : SGM(SGM) {}
  
  /// Emit ObjC thunks necessary for an ObjC protocol conformance.
  void emitObjCConformanceThunks(ProtocolDecl *protocol,
                                 ProtocolConformance *conformance) {
    assert(conformance);
    if (protocol->isObjC())
      for (auto &mapping : conformance->getWitnesses()) {
        if (!mapping.second)
          continue;
        
        ValueDecl *vd = mapping.second.getDecl();
        if (auto *method = cast<FuncDecl>(vd))
          SGM.emitObjCMethodThunk(method);
        else if (auto *prop = cast<VarDecl>(vd))
          SGM.emitObjCPropertyMethodThunks(prop);
        else
          llvm_unreachable("unexpected conformance mapping");
      }
    for (auto &inherited : conformance->getInheritedConformances())
      emitObjCConformanceThunks(inherited.first, inherited.second);
  }
  
  /// Emit SIL functions for all the members of the type.
  void emitExtension(ExtensionDecl *e) {
    for (Decl *member : e->getMembers())
      visit(member);

    // Emit witness tables for protocol conformances.
    for (auto *conformance : e->getConformances())
      SGM.emitProtocolConformance(conformance);
    
    // ObjC protocol conformances may require ObjC thunks to be introduced for
    // definitions from other contexts.
    for (unsigned i = 0, size = e->getProtocols().size(); i < size; ++i)
      emitObjCConformanceThunks(e->getProtocols()[i],
                                e->getConformances()[i]);
  }
  
  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    SGM.emitFunction(fd);
    if (SGM.requiresObjCMethodEntryPoint(fd))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    SGM.emitConstructor(cd);
    if (SGM.requiresObjCMethodEntryPoint(cd))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    llvm_unreachable("destructor in extension?!");
  }
  
  // no-ops. We don't deal with the layout of types here.
  void visitPatternBindingDecl(PatternBindingDecl *) {}
  
  void visitVarDecl(VarDecl *vd) {
    if (SGM.requiresObjCPropertyEntryPoints(vd))
      SGM.emitObjCPropertyMethodThunks(vd);
  }

  void visitSubscriptDecl(SubscriptDecl *sd) {
    if (SGM.requiresObjCSubscriptEntryPoints(sd))
      SGM.emitObjCSubscriptMethodThunks(sd);
  }
};

void SILGenModule::visitExtensionDecl(ExtensionDecl *ed) {
  SILGenExtension(*this).emitExtension(ed);
}

void SILGenFunction::emitLocalVariable(VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(!vd->isComputed() &&
         "can't emit storage for a computed variable");
  assert(!VarLocs.count(vd) && "Already have an entry for this decl?");

  SILType lType = getLoweredType(vd->getType()->getRValueType());

  // The variable may have its lifetime extended by a closure, heap-allocate it
  // using a box.
  AllocBoxInst *allocBox = B.createAllocBox(vd, lType);
  auto box = SILValue(allocBox, 0);
  auto addr = SILValue(allocBox, 1);
  
  /// Remember that this is the memory location that we're emitting the
  /// decl to.
  VarLocs[vd] = SILGenFunction::VarLoc::getAddress(addr, box);
}

/// Create a LocalVariableInitialization for the uninitialized var.
InitializationPtr SILGenFunction::emitLocalVariableWithCleanup(VarDecl *vd) {
  emitLocalVariable(vd);
  return InitializationPtr(new LocalVariableInitialization(vd, *this));
}

/// Create an Initialization for an uninitialized temporary.
std::unique_ptr<TemporaryInitialization>
SILGenFunction::emitTemporary(SILLocation loc, const TypeLowering &tempTL) {
  SILValue addr = emitTemporaryAllocation(loc, tempTL.getLoweredType());
  CleanupHandle cleanup = enterDormantTemporaryCleanup(addr, tempTL);
  return std::unique_ptr<TemporaryInitialization>(
                                   new TemporaryInitialization(addr, cleanup));
}

CleanupHandle
SILGenFunction::enterDormantTemporaryCleanup(SILValue addr,
                                             const TypeLowering &tempTL) {
  if (tempTL.isTrivial()) {
    return CleanupHandle::invalid();
  }

  Cleanups.pushCleanupInState<DestroyAddr>(CleanupState::Dormant, addr);
  return Cleanups.getCleanupsDepth();
}

void SILGenFunction::destroyLocalVariable(SILLocation silLoc, VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(!vd->isComputed() &&
         "can't emit storage for a computed variable");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");
  
  auto &loc = VarLocs[vd];
  
  // For a heap variable, the box is responsible for the value. We just need
  // to give up our retain count on it.
  assert(loc.box && "captured var should have been given a box");
  B.emitStrongRelease(silLoc, loc.box);
}

void SILGenFunction::deallocateUninitializedLocalVariable(SILLocation silLoc,
                                                          VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(!vd->isComputed() &&
         "can't emit storage for a computed variable");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");

  auto &loc = VarLocs[vd];
  if (loc.isConstant()) return;

  assert(loc.box && "captured var should have been given a box");
  B.createDeallocBox(silLoc, loc.getAddress().getType().getObjectType(),
                     loc.box);
}

//===----------------------------------------------------------------------===//
// ObjC method thunks
//===----------------------------------------------------------------------===//

static SILValue emitBridgeObjCReturnValue(SILGenFunction &gen,
                                          SILLocation loc,
                                          SILValue result,
                                          CanType origNativeTy,
                                          CanType substNativeTy,
                                          CanType bridgedTy) {
  Scope scope(gen.Cleanups, CleanupLocation::getCleanupLocation(loc));
  
  ManagedValue native = gen.emitManagedRValueWithCleanup(result);
  ManagedValue bridged = gen.emitNativeToBridgedValue(loc, native,
                                                      AbstractCC::ObjCMethod,
                                                      origNativeTy,
                                                      substNativeTy,
                                                      bridgedTy);
  return bridged.forward(gen);
}

/// Take a return value at +1 and adjust it to the retain count expected by
/// the given ownership conventions.
static void emitObjCReturnValue(SILGenFunction &gen,
                                SILLocation loc,
                                SILValue result,
                                CanType nativeTy,
                                SILResultInfo resultInfo) {
  // Bridge the result.
  result = emitBridgeObjCReturnValue(gen, loc, result, nativeTy, nativeTy,
                                     resultInfo.getType());
  
  // Autorelease the bridged result if necessary.
  switch (resultInfo.getConvention()) {
  case ResultConvention::Autoreleased:
    gen.B.createAutoreleaseReturn(loc, result);
    return;
  case ResultConvention::Unowned:
    gen.B.emitDestroyValueOperation(loc, result);
    SWIFT_FALLTHROUGH;
  case ResultConvention::Owned:
    gen.B.createReturn(loc, result);
    return;
  }
}

/// Take an argument at +0 and bring it to +1.
static SILValue emitObjCUnconsumedArgument(SILGenFunction &gen,
                                           SILLocation loc,
                                           SILValue arg) {
  auto &lowering = gen.getTypeLowering(arg.getType());
  // If address-only, make a +1 copy and operate on that.
  if (lowering.isAddressOnly()) {
    auto tmp = gen.emitTemporaryAllocation(loc, arg.getType().getObjectType());
    gen.B.createCopyAddr(loc, arg, tmp, IsNotTake, IsInitialization);
    return tmp;
  }
  
  return lowering.emitCopyValue(gen.B, loc, arg);
}

/// Bridge argument types and adjust retain count conventions for an ObjC thunk.
static SILFunctionType *emitObjCThunkArguments(SILGenFunction &gen,
                                               SILDeclRef thunk,
                                               SmallVectorImpl<SILValue> &args){
  auto objcInfo = gen.SGM.Types.getConstantFunctionType(thunk);
  auto swiftInfo = gen.SGM.Types.getConstantFunctionType(thunk.asForeign(false));

  RegularLocation Loc(thunk.getDecl());
  Loc.markAutoGenerated();

  SmallVector<ManagedValue, 8> bridgedArgs;
  bridgedArgs.reserve(objcInfo->getParameters().size());
  
  // Emit the indirect return argument, if any.
  if (objcInfo->hasIndirectResult()) {
    auto arg = new (gen.F.getModule())
      SILArgument(objcInfo->getIndirectResult().getSILType(), gen.F.begin());
    bridgedArgs.push_back(ManagedValue(arg, ManagedValue::Unmanaged));
  }
  
  // Emit the other arguments, taking ownership of arguments if necessary.
  auto inputs = objcInfo->getParametersWithoutIndirectResult();
  assert(!inputs.empty());
  for (unsigned i = 0, e = inputs.size(); i < e; ++i) {
    SILValue arg = new(gen.F.getModule())
                     SILArgument(inputs[i].getSILType(), gen.F.begin());

    // Convert the argument to +1 if necessary.
    if (!inputs[i].isConsumed()) {
      arg = emitObjCUnconsumedArgument(gen, Loc, arg);
    }

    auto managedArg = gen.emitManagedRValueWithCleanup(arg);

    bridgedArgs.push_back(managedArg);
  }

  assert(bridgedArgs.size() == objcInfo->getParameters().size() &&
         "objc inputs don't match number of arguments?!");
  assert(bridgedArgs.size() == swiftInfo->getParameters().size() &&
         "swift inputs don't match number of arguments?!");

  // Bridge the input types.
  Scope scope(gen.Cleanups, CleanupLocation::getCleanupLocation(Loc));
  for (unsigned i = 0, size = bridgedArgs.size(); i < size; ++i) {
    ManagedValue native =
      gen.emitBridgedToNativeValue(Loc,
                                   bridgedArgs[i],
                                   AbstractCC::ObjCMethod,
                     swiftInfo->getParameters()[i].getSILType().getSwiftType());
    args.push_back(native.forward(gen));
  }
  
  return objcInfo;
}

void SILGenFunction::emitObjCMethodThunk(SILDeclRef thunk) {
  SILDeclRef native = thunk.asForeign(false);
  
  SmallVector<SILValue, 4> args;  
  auto objcFnTy = emitObjCThunkArguments(*this, thunk, args);
  auto nativeInfo = getConstantInfo(native);
  auto swiftResultTy = nativeInfo.SILFnType->getResult();
  auto objcResultTy = objcFnTy->getResult();
  
  // Call the native entry point.
  RegularLocation loc(thunk.getDecl());
  loc.markAutoGenerated();

  SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);
  SILValue result = B.createApply(loc, nativeFn, nativeFn.getType(),
                                  swiftResultTy.getSILType(), {}, args,
                                  thunk.isTransparent());
  emitObjCReturnValue(*this, loc, result, nativeInfo.LoweredType.getResult(),
                      objcResultTy);
}

void SILGenFunction::emitObjCPropertyGetter(SILDeclRef getter) {
  SmallVector<SILValue, 2> args;
  auto objcFnTy = emitObjCThunkArguments(*this, getter, args);
  SILDeclRef native = getter.asForeign(false);
  auto nativeInfo = getConstantInfo(native);
  SILResultInfo swiftResultTy = nativeInfo.SILFnType->getResult();
  SILResultInfo objcResultTy = objcFnTy->getResult();

  RegularLocation loc(getter.getDecl());
  loc.markAutoGenerated();

  // If the property is computed, forward to the native getter.
  auto *var = cast<VarDecl>(getter.getDecl());
  if (var->isComputed()) {
    SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);
    SILValue result = B.createApply(loc, nativeFn, nativeFn.getType(),
                                    swiftResultTy.getSILType(), {}, args,
                                    getter.isTransparent());
    emitObjCReturnValue(*this, loc, result, nativeInfo.LoweredType.getResult(),
                        objcResultTy);
    return;
  }

  // If the native property has storage, load it.
  SILValue indirectReturn, selfValue;
  if (objcFnTy->hasIndirectResult()) {
    assert(args.size() == 2 && "wrong number of arguments for getter");
    indirectReturn = args[0];
    selfValue = args[1];
  } else {
    assert(args.size() == 1 && "wrong number of arguments for getter");
    selfValue = args[0];
  }

  auto fieldType = var->getType()->getCanonicalType();
  auto &fieldLowering = getTypeLowering(fieldType);
  auto &resultLowering =
    (fieldType == swiftResultTy.getType()
       ? fieldLowering : getTypeLowering(swiftResultTy.getSILType()));

  SILValue fieldAddr = B.createRefElementAddr(loc, selfValue, var,
                             fieldLowering.getLoweredType().getAddressType());
  if (indirectReturn) {
    // This is basically returning +1, but there's no obvious
    // alternative, and there really isn't an ObjC convention for
    // transferring ownership in aggregates.
    emitSemanticLoadInto(loc, fieldAddr, fieldLowering,
                         indirectReturn, resultLowering,
                         IsNotTake, IsInitialization);
    B.emitStrongRelease(loc, selfValue);
    B.createReturn(loc, emitEmptyTuple(loc));
    return;
  }

  // Bridge the result.
  SILValue result = emitSemanticLoad(loc, fieldAddr, fieldLowering,
                                     resultLowering, IsNotTake);

  // FIXME: This should have artificial location.
  B.emitStrongRelease(loc, selfValue);
  return emitObjCReturnValue(*this, loc, result,
                             nativeInfo.LoweredType.getResult(),
                             objcResultTy);
}

void SILGenFunction::emitObjCPropertySetter(SILDeclRef setter) {
  SmallVector<SILValue, 2> args;
  emitObjCThunkArguments(*this, setter, args);
  SILDeclRef native = setter.asForeign(false);
  auto nativeInfo = getConstantInfo(native);

  RegularLocation loc(setter.getDecl());
  loc.markAutoGenerated();

  // If the native property is computed, store to the native setter.
  auto *var = cast<VarDecl>(setter.getDecl());
  if (var->isComputed()) {
    SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);
    SILValue result = B.createApply(loc, nativeFn, nativeFn.getType(),
                                    SGM.Types.getEmptyTupleType(),
                                    {}, args,
                                    setter.isTransparent());
    // Result should be void.
    B.createReturn(loc, result);
    return;
  }

  assert(args.size() == 2 && "wrong number of args for setter");
  SILValue selfValue = args[1];
  SILValue setValue = args[0];
  
  // If the native property has storage, store to it.
  auto &varTI = getTypeLowering(var->getType());
  SILValue addr = B.createRefElementAddr(loc, selfValue, var,
                                 varTI.getLoweredType().getAddressType());
  emitSemanticStore(loc, setValue, addr, varTI, IsNotInitialization);
  
  // FIXME: This should have artificial location.
  B.emitStrongRelease(loc, selfValue);
  B.createReturn(loc, emitEmptyTuple(loc));
}

void SILGenFunction::emitObjCSubscriptGetter(SILDeclRef getter) {
  SmallVector<SILValue, 2> args;
  auto objcFnTy = emitObjCThunkArguments(*this, getter, args);
  SILDeclRef native = getter.asForeign(false);
  auto nativeInfo = getConstantInfo(native);
  auto swiftResultTy = nativeInfo.SILFnType->getResult();
  auto objcResultTy = objcFnTy->getResult();

  RegularLocation loc(getter.getDecl());
  loc.markAutoGenerated();

  SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);
  SILValue result = B.createApply(loc, nativeFn, nativeFn.getType(),
                                  swiftResultTy.getSILType(), {}, args,
                                  getter.isTransparent());
  emitObjCReturnValue(*this, loc, result, nativeInfo.LoweredType.getResult(),
                      objcResultTy);
}

void SILGenFunction::emitObjCSubscriptSetter(SILDeclRef setter) {
  SmallVector<SILValue, 2> args;
  emitObjCThunkArguments(*this, setter, args);
  SILDeclRef native = setter.asForeign(false);
  auto nativeInfo = getConstantInfo(native);

  RegularLocation loc(setter.getDecl());
  loc.markAutoGenerated();

  // Store to the native setter.
  SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);
  SILValue result = B.createApply(loc, nativeFn, nativeFn.getType(),
                                  SGM.Types.getEmptyTupleType(), {}, args,
                                  setter.isTransparent());
  // Result should be void.
  B.createReturn(loc, result);
}

//===----------------------------------------------------------------------===//
// Global initialization
//===----------------------------------------------------------------------===//

namespace {
  
/// A visitor for traversing a pattern, creating
/// global accessor functions for all of the global variables declared inside.
struct GenGlobalAccessors : public PatternVisitor<GenGlobalAccessors>
{
  /// The module generator.
  SILGenModule &SGM;
  /// The Builtin.once token guarding the global initialization.
  SILGlobalVariable *OnceToken;
  /// The function containing the initialization code.
  SILFunction *OnceFunc;
  
  /// A reference to the Builtin.once declaration.
  FuncDecl *BuiltinOnceDecl;
  
  GenGlobalAccessors(SILGenModule &SGM,
                     SILGlobalVariable *OnceToken,
                     SILFunction *OnceFunc)
    : SGM(SGM), OnceToken(OnceToken), OnceFunc(OnceFunc)
  {
    // Find Builtin.once.
    auto &C = SGM.M.getASTContext();
    SmallVector<ValueDecl*, 2> found;
    C.TheBuiltinModule
      ->lookupValue({}, C.getIdentifier("once"),
                    NLKind::QualifiedLookup, found);
    
    assert(found.size() == 1 && "didn't find Builtin.once?!");
    
    BuiltinOnceDecl = cast<FuncDecl>(found[0]);
  }
  
  // Walk through non-binding patterns.
  void visitParenPattern(ParenPattern *P) {
    return visit(P->getSubPattern());
  }
  void visitTypedPattern(TypedPattern *P) {
    return visit(P->getSubPattern());
  }
  void visitTuplePattern(TuplePattern *P) {
    for (auto &elt : P->getFields())
      visit(elt.getPattern());
  }
  void visitAnyPattern(AnyPattern *P) {}
  
  // When we see a variable binding, emit its global accessor.
  void visitNamedPattern(NamedPattern *P) {
    SGM.emitGlobalAccessor(P->getDecl(), BuiltinOnceDecl, OnceToken, OnceFunc);
  }
  
#define INVALID_PATTERN(Id, Parent) \
  void visit##Id##Pattern(Id##Pattern *) { \
    llvm_unreachable("pattern not valid in argument or var binding"); \
  }
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) INVALID_PATTERN(Id, Parent)
#include "swift/AST/PatternNodes.def"
#undef INVALID_PATTERN
};
  
} // end anonymous namespace

/// Emit a global initialization.
void SILGenModule::emitGlobalInitialization(PatternBindingDecl *pd) {
  // Generic and dynamic static properties require lazy initialization, which
  // isn't implemented yet.
  if (pd->isStatic()) {
    auto theType = pd->getDeclContext()->getDeclaredTypeInContext();
    assert(!theType->is<BoundGenericType>()
           && "generic static properties not implemented");
    assert((theType->getStructOrBoundGenericStruct()
            || theType->getEnumOrBoundGenericEnum())
           && "only value type static properties are implemented");
    (void)theType;
  }
  
  // Emit the lazy initialization token for the initialization expression.
  auto counter = anonymousSymbolCounter++;
  
  llvm::SmallString<20> onceTokenName;
  {
    llvm::raw_svector_ostream os(onceTokenName);
    os << "globalinit_token" << counter;
    os.flush();
  }
  
  auto onceTy = BuiltinIntegerType::getWordType(M.getASTContext());
  auto onceSILTy
    = SILType::getPrimitiveObjectType(onceTy->getCanonicalType());
  
  auto onceToken = new (M) SILGlobalVariable(M, SILLinkage::Internal,
                                           onceTokenName,
                                           onceSILTy, /*isDefinition*/ true);
  
  // Emit the initialization code into a function.
  llvm::SmallString<20> onceFuncName;
  {
    llvm::raw_svector_ostream os(onceFuncName);
    os << "globalinit_func" << counter;
    os.flush();
  }
  
  SILFunction *onceFunc = emitLazyGlobalInitializer(onceFuncName, pd);
  
  // Generate accessor functions for all of the declared variables, which
  // Builtin.once the lazy global initializer we just generated then return
  // the address of the individual variable.
  GenGlobalAccessors(*this, onceToken, onceFunc)
    .visit(pd->getPattern());
}

namespace {
  
// Is this a free function witness satisfying a static method requirement?
static IsFreeFunctionWitness_t isFreeFunctionWitness(ValueDecl *requirement,
                                                     ValueDecl *witness) {
  if (!witness->getDeclContext()->isTypeContext()) {
    assert(!requirement->isInstanceMember()
           && "free function satisfying instance method requirement?!");
    return IsFreeFunctionWitness;
  }
  
  return IsNotFreeFunctionWitness;
}

/// Emit a witness table for a protocol conformance.
class SILGenConformance : public Lowering::ASTVisitor<SILGenConformance> {
public:
  SILGenModule &SGM;
  NormalProtocolConformance *Conformance;
  std::vector<SILWitnessTable::Entry> Entries;

  SILGenConformance(SILGenModule &SGM, ProtocolConformance *C)
    // We only need to emit witness tables for base NormalProtocolConformances.
    : SGM(SGM), Conformance(dyn_cast<NormalProtocolConformance>(C))
  {
    // ObjC protocols do not use witness tables.
    if (Conformance->getProtocol()->isObjC())
      Conformance = nullptr;
  }
  
  SILWitnessTable *emit() {
    // Nothing to do if this wasn't a normal conformance.
    if (!Conformance)
      return nullptr;
    
    // TODO: Reference conformances for inherited protocols.
    
    // Emit witnesses in protocol declaration order.
    for (auto reqt : Conformance->getProtocol()->getMembers())
      visit(reqt);
    
    // Create the witness table.
    return SILWitnessTable::create(SGM.M, Conformance, Entries);
  }
  
  /// Fallback for unexpected protocol requirements.
  void visitDecl(Decl *d) {
    d->print(llvm::errs());
    llvm_unreachable("unhandled protocol requirement");
  }
  
  void visitFuncDecl(FuncDecl *fd) {
    // Find the witness in the conformance.
    auto foundWitness = Conformance->getWitnesses().find(fd);
    assert(foundWitness != Conformance->getWitnesses().end()
           && "no witness for protocol requirement");
    
    // Emit the witness thunk and add it to the table.
    SILDeclRef requirementRef(fd, SILDeclRef::Kind::Func);
    ConcreteDeclRef witness = foundWitness->second;
    // Free function witnesses have an implicit uncurry layer imposed on them by
    // the inserted metatype argument.
    auto isFree = isFreeFunctionWitness(fd, witness.getDecl());
    unsigned witnessUncurryLevel = isFree ? requirementRef.uncurryLevel - 1
                                          : requirementRef.uncurryLevel;
  
    SILDeclRef witnessRef(witness.getDecl(), SILDeclRef::Kind::Func,
                          witnessUncurryLevel);

    SILFunction *witnessFn = SGM.emitProtocolWitness(Conformance,
                                                   requirementRef, witnessRef,
                                                   isFree,
                                                   witness.getSubstitutions());
    Entries.push_back(
                    SILWitnessTable::MethodWitness{requirementRef, witnessFn});
  }
  
  void visitAssociatedTypeDecl(AssociatedTypeDecl *td) {
    // Find the substitution info for the witness type.
    auto foundWitness = Conformance->getTypeWitnesses().find(td);
    assert(foundWitness != Conformance->getTypeWitnesses().end()
           && "no witness for associated type requirement");
    
    Substitution witness = foundWitness->second;
    
    // Emit the record for the type itself.
    Entries.push_back(SILWitnessTable::AssociatedTypeWitness{td,
                                      witness.Replacement->getCanonicalType()});
    
    // Emit records for the protocol requirements on the type.
    for (auto conformance : witness.Conformance) {
      Entries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
                          td, conformance->getProtocol(),
                          conformance});
    }
  }

  void visitVarDecl(VarDecl *vd) {
    // FIXME: Emit getter and setter (if settable) witnesses.
    assert(false && "property requirements in protocols not implemented");
  }
};
  
} // end anonymous namespace

void SILGenModule::emitProtocolConformance(ProtocolConformance *conformance) {
  if (!getASTContext().LangOpts.EmitSILProtocolWitnessTables)
    return;
  
  SILGenConformance(*this, conformance).emit();
}

SILFunction *
SILGenModule::emitProtocolWitness(ProtocolConformance *conformance,
                                  SILDeclRef requirement,
                                  SILDeclRef witness,
                                  IsFreeFunctionWitness_t isFree,
                                  ArrayRef<Substitution> witnessSubs) {
  ASTContext &C = M.getASTContext();
  
  // Get the type of the witness with its witnessing substitutions applied.
  // FIXME: Rework for interface types.
  auto witnessTy = Types.getConstantFormalType(witness);
  auto requirementTy = Types.getConstantFormalType(requirement);
  unsigned witnessUncurryLevel = witness.uncurryLevel;

  // If the witness is a free function, add the artificial self metatype
  // argument and uncurry level to its signature.
  if (isFree) {
    auto metaty = CanMetaTypeType::get(
                                conformance->getType()->getCanonicalType(), C);
    witnessTy = CanFunctionType::get(metaty, witnessTy, C);
    ++witnessUncurryLevel;
  }
  
  // Substitute generic arguments from the conformance.
  if (auto pft = dyn_cast<PolymorphicFunctionType>(witnessTy.getResult())) {
    CanAnyFunctionType witnessMethodTy = cast<FunctionType>(
      pft->substGenericArgs(M.getSwiftModule(), witnessSubs)
        ->getCanonicalType());
    
    // If the requirement is generic, apply its generic parameters to the
    // witness type.
    if (auto reqtPFT
               = dyn_cast<PolymorphicFunctionType>(requirementTy.getResult())) {
      witnessMethodTy = CanPolymorphicFunctionType::get(witnessMethodTy.getInput(),
                                                witnessMethodTy.getResult(),
                                                &reqtPFT->getGenericParams(),
                                                witnessMethodTy->getExtInfo(),
                                                C);
    }
    
    witnessTy = CanFunctionType::get(witnessTy.getInput(), witnessMethodTy,
                                     witnessTy->getExtInfo(), C);
  } else {
    assert(witnessSubs.empty() &&
           "substitutions for nongeneric witness?!");
  }
  
  // The witness SIL function has the type of the AST-level witness, at the
  // abstraction level of the original protocol requirement.
  // FIXME: Should have @cc(witness) convention.
  assert(requirement.uncurryLevel == witnessUncurryLevel
         && "uncurry level of requirement and witness do not match");
      
  // In addition to the usual bevy of abstraction differences, protocol
  // witnesses have potential differences in @inout-ness of self. Value type
  // methods may reassign 'self' as an @inout parameter, but class methods may
  // not. Handle this special case in the witness type before applying the
  // abstraction change.
  auto inOutSelf = DoesNotHaveInOutSelfAbstractionDifference;
  if (!isa<LValueType>(witnessTy.getInput())
      && isa<LValueType>(requirementTy.getInput())) {
    inOutSelf = HasInOutSelfAbstractionDifference;
    witnessTy = CanFunctionType::get(
                   CanLValueType::get(witnessTy.getInput(),
                                      LValueType::Qual::DefaultForInOutSelf, C),
                   witnessTy.getResult(), witnessTy->getExtInfo(), C);
  }
      
  // Lower the witness type with the requirement's abstraction level.
  SILType witnessSILType = Types.getLoweredType(
                                              AbstractionPattern(requirementTy),
                                              witnessTy,
                                              requirement.uncurryLevel);
  
  // TODO: Should have internal linkage when we actually implement thunk
  // generation. The verifier rejects undefined internal symbols.
  auto *f = new (M) SILFunction(M, SILLinkage::External, "",
                                witnessSILType.castTo<SILFunctionType>(),
                                SILLocation(witness.getDecl()),
                                IsNotBare,
                                IsNotTransparent);
  
  // Mangle the name of the witness thunk.
  llvm::raw_string_ostream buffer(f->getMutableName());
  buffer << "_TTW";
  Mangler mangler(buffer);
  
  mangler.mangleProtocolConformance(conformance);
  assert(isa<FuncDecl>(requirement.getDecl())
         && "need to handle mangling of non-Func SILDeclRefs here");
  mangler.mangleEntity(requirement.getDecl(), ExplosionKind::Minimal,
                       requirement.uncurryLevel);
  buffer.flush();
  
  /*TODO
  // Create the witness.
  SILGenFunction(*this, *f)
    .emitProtocolWitness(conformance, requirement, witness, witnessSubs,
                         isFree, inOutSelf);
   */
  
  return f;
}

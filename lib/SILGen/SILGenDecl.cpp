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
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebuggerClient.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/AST/AST.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/ADT/SmallString.h"
#include <iterator>
using namespace swift;
using namespace Mangle;
using namespace Lowering;

void Initialization::_anchor() {}
void SILDebuggerClient::anchor() {}

namespace {
  /// A "null" initialization that indicates that any value being initialized
  /// into this initialization should be discarded. This represents AnyPatterns
  /// (that is, 'var (_)') that bind to values without storing them.
  class BlackHoleInitialization : public Initialization {
  public:
    BlackHoleInitialization()
      : Initialization(Initialization::Kind::Ignored)
    {}

    SILValue getAddressOrNull() const override { return SILValue(); }

    void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                             SILLocation loc, SILGenFunction &gen) override {
      /// This just ignores the provided value.
    }

  };


  /// An Initialization subclass used to destructure tuple initializations.
  class TupleElementInitialization : public SingleBufferInitialization {
  public:
    SILValue ElementAddr;

    TupleElementInitialization(SILValue addr)
      : ElementAddr(addr)
    {}

    SILValue getAddressOrNull() const override { return ElementAddr; }
  };
  
  /// An Initialization of a tuple pattern, such as "var (a,b)".
  class TupleInitialization : public Initialization {
  public:
    /// The sub-Initializations aggregated by this tuple initialization.
    /// The TupleInitialization object takes ownership of Initializations pushed
    /// here.
    SmallVector<InitializationPtr, 4> subInitializations;
    
    TupleInitialization() : Initialization(Initialization::Kind::Tuple) {}
    
    SILValue getAddressOrNull() const override {
      if (subInitializations.size() == 1)
        return subInitializations[0]->getAddressOrNull();
      else
        return SILValue();
    }

    ArrayRef<InitializationPtr> getSubInitializations() const {
      return subInitializations;
    }
    
    void finishInitialization(SILGenFunction &gen) override {
      for (auto &sub : subInitializations)
        sub->finishInitialization(gen);
    }
    
    void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                             SILLocation loc, SILGenFunction &gen) override {
      llvm_unreachable("tuple initialization not destructured?!");
    }
    
  };
}

bool Initialization::canForwardInBranch() const {
  switch (kind) {
  case Kind::Ignored:
  case Kind::SingleBuffer:
    return true;

  // These initializations expect to be activated exactly once.
  case Kind::LetValue:
  case Kind::Translating:
  case Kind::Refutable:
    return false;

  case Kind::Tuple:
    for (auto &subinit : ((TupleInitialization*)this)->getSubInitializations()){
      if (!subinit->canForwardInBranch())
        return false;
    }
    return true;
  }
  llvm_unreachable("bad initialization kind!");
}

ArrayRef<InitializationPtr>
Initialization::getSubInitializationsForTuple(SILGenFunction &gen, CanType type,
                                      SmallVectorImpl<InitializationPtr> &buf,
                                      SILLocation Loc) {
  assert(canSplitIntoSubelementAddresses() && "Client shouldn't call this");
  switch (kind) {
  case Kind::Tuple:
    return ((TupleInitialization*)this)->getSubInitializations();
      
  case Kind::Ignored:
    // "Destructure" an ignored binding into multiple ignored bindings.
    for (auto fieldType : cast<TupleType>(type)->getElementTypes()) {
      (void) fieldType;
      buf.push_back(InitializationPtr(new BlackHoleInitialization()));
    }
    return buf;
  case Kind::LetValue:
  case Kind::SingleBuffer: {
    // Destructure the buffer into per-element buffers.
    auto tupleTy = cast<TupleType>(type);
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
  case Kind::Refutable:
    llvm_unreachable("cannot destructure a refutable initialization");
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
  return F.getForwardingSubstitutions();
}

void SILGenFunction::visitFuncDecl(FuncDecl *fd) {
  // Generate the local function body.
  SGM.emitFunction(fd);

  // If there are captures or we are in a generic context, build the local
  // closure value for the function and store it as a local constant.
  if (fd->getCaptureInfo().hasLocalCaptures()
      || F.getContextGenericParams()) {
    SILValue closure =
        emitClosureValue(fd, SILDeclRef(fd), F.getForwardingSubstitutions(), fd)
            .forward(*this);
    Cleanups.pushCleanup<CleanupClosureConstant>(closure);
    LocalFunctions[SILDeclRef(fd)] = closure;
  }
}

SingleBufferInitialization::~SingleBufferInitialization() {
  // vtable anchor.
}

void SingleBufferInitialization::
copyOrInitValueIntoSingleBuffer(ManagedValue explodedElement, bool isInit,
                                SILValue BufferAddress,
                                SILLocation loc, SILGenFunction &gen) {
  if (!isInit) {
    assert(explodedElement.getValue() != BufferAddress && "copying in place?!");
    explodedElement.copyInto(gen, BufferAddress, loc);
    return;
  }
  
  // If we didn't evaluate into the initialization buffer, do so now.
  if (explodedElement.getValue() != BufferAddress) {
    explodedElement.forwardInto(gen, loc, BufferAddress);
  } else {
    // If we did evaluate into the initialization buffer, disable the
    // cleanup.
    explodedElement.forwardCleanup(gen);
  }
}


void TemporaryInitialization::finishInitialization(SILGenFunction &gen) {
  if (Cleanup.isValid())
    gen.Cleanups.setCleanupState(Cleanup, CleanupState::Active);
};

namespace {

class ReleaseValueCleanup : public Cleanup {
  SILValue v;
public:
  ReleaseValueCleanup(SILValue v) : v(v) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    if (v.getType().isAddress())
      gen.B.emitDestroyAddr(l, v);
    else
      gen.B.emitReleaseValueOperation(l, v);
  }
};

/// Cleanup to destroy an initialized variable.
class DeallocStackCleanup : public Cleanup {
  SILValue Addr;
public:
  DeallocStackCleanup(SILValue addr) : Addr(addr) {}

  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.createDeallocStack(l, Addr);
  }
};

/// Cleanup to destroy an initialized 'var' variable.
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

/// An initialization of a local 'var'.
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

  SILValue getAddressOrNull() const override {
    assert(Gen.VarLocs.count(Var) && "did not emit var?!");
    return Gen.VarLocs[Var].value;
  }

  void finishInitialization(SILGenFunction &gen) override {
    assert(!DidFinish &&
           "called LocalVariableInitialization::finishInitialization twice!");
    Gen.Cleanups.setCleanupState(DeallocCleanup, CleanupState::Dead);
    Gen.Cleanups.setCleanupState(ReleaseCleanup, CleanupState::Active);
    DidFinish = true;
  }
};

/// Initialize a writeback buffer that receives the value of a 'let'
/// declaration.
class LetValueInitialization : public Initialization {
  /// The VarDecl for the let decl.
  VarDecl *vd;

  /// The address of the buffer used for the binding, if this is an address-only
  /// let.
  SILValue address;

  /// The cleanup we pushed to destroy the local variable.
  CleanupHandle DestroyCleanup;

  bool DidFinish = false;

public:
  LetValueInitialization(VarDecl *vd, SILGenFunction &gen)
    : Initialization(Initialization::Kind::LetValue), vd(vd)
  {
    auto &lowering = gen.getTypeLowering(vd->getType());
    
    // Decide whether we need a temporary stack buffer to evaluate this 'let'.
    // There are three cases we need to handle here: parameters, initialized (or
    // bound) decls, and uninitialized ones.
    bool needsTemporaryBuffer;
    bool isUninitialized = false;

    assert(!isa<ParamDecl>(vd)
           && "should not bind function params on this path");
    if (vd->getParentPatternBinding() && !vd->getParentInitializer()) {
      // This value is uninitialized (and unbound) if it has a pattern binding
      // decl, with no initializer value.
      assert(!vd->hasNonPatternBindingInit() && "Bound values aren't uninit!");
      
      // If this is a let-value without an initializer, then we need a temporary
      // buffer.  DI will make sure it is only assigned to once.
      needsTemporaryBuffer = true;
      isUninitialized = true;
    } else {
      // If this is a let with an initializer or bound value, we only need a
      // buffer if the type is address only.
      needsTemporaryBuffer = lowering.isAddressOnly();
    }
   
    if (needsTemporaryBuffer) {
      address = gen.emitTemporaryAllocation(vd, lowering.getLoweredType());
      if (isUninitialized)
        address = gen.B.createMarkUninitializedVar(vd, address);
      DestroyCleanup = gen.enterDormantTemporaryCleanup(address, lowering);
      gen.VarLocs[vd] = SILGenFunction::VarLoc::get(address);
    } else if (!lowering.isTrivial()) {
      // Push a cleanup to destroy the let declaration.  This has to be
      // inactive until the variable is initialized: if control flow exits the
      // before the value is bound, we don't want to destroy the value.
      gen.Cleanups.pushCleanupInState<DestroyLocalVariable>(
                                                    CleanupState::Dormant, vd);
      DestroyCleanup = gen.Cleanups.getTopCleanup();
    } else {
      DestroyCleanup = CleanupHandle::invalid();
    }
  }

  ~LetValueInitialization() override {
    assert(DidFinish && "did not call LetValueInit::finishInitialization!");
  }

  void emitDebugValue(SILValue v, SILGenFunction &gen) {
    // Emit a debug_value[_addr] instruction to record the start of this value's
    // lifetime.
    SILLocation PrologueLoc(vd);
    PrologueLoc.markAsPrologue();
    if (address.isValid())
      gen.B.createDebugValueAddr(PrologueLoc, v);
    else
      gen.B.createDebugValue(PrologueLoc, v);
  }

  SILValue getAddressOrNull() const override {
    return address;
  }

  void bindValue(SILValue value, SILGenFunction &gen) {
    assert(!gen.VarLocs.count(vd) && "Already emitted this vardecl?");
    // If we're binding an address to this let value, then we can use it as an
    // address later.  This happens when binding an address only parameter to
    // an argument, for example.
    if (value.getType().isAddress())
      address = value;
    gen.VarLocs[vd] = SILGenFunction::VarLoc::get(value);

    emitDebugValue(value, gen);
  }
  
  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &gen) override {
    // If this let value has an address, we can handle it just like a single
    // buffer value.
    if (hasAddress())
      return SingleBufferInitialization::
           copyOrInitValueIntoSingleBuffer(explodedElement, isInit,
                                           getAddress(), loc, gen);
    
    // Otherwise, we bind the value.
    if (isInit) {
      // Disable the rvalue expression cleanup, since the let value
      // initialization has a cleanup that lives for the entire scope of the
      // let declaration.
      bindValue(explodedElement.forward(gen), gen);
    } else {
      // Disable the expression cleanup of the copy, since the let value
      // initialization has a cleanup that lives for the entire scope of the
      // let declaration.
      bindValue(explodedElement.copyUnmanaged(gen, loc).forward(gen), gen);
    }
  }

  void finishInitialization(SILGenFunction &gen) override {
    assert(!DidFinish &&
           "called LetValueInit::finishInitialization twice!");
    assert(gen.VarLocs.count(vd) && "Didn't bind a value to this let!");
    if (DestroyCleanup != CleanupHandle::invalid())
      gen.Cleanups.setCleanupState(DestroyCleanup, CleanupState::Active);
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

  SILValue getAddressOrNull() const override {
    return address;
  }
};

class DebuggerInitialization : public GlobalInitialization {
public:
  DebuggerInitialization(SILValue address) : GlobalInitialization(address) {
  }
};

/// Initialize a variable of reference-storage type.
class ReferenceStorageInitialization : public Initialization {
  InitializationPtr VarInit;
public:
  ReferenceStorageInitialization(InitializationPtr &&subInit)
    : Initialization(Initialization::Kind::Translating),
      VarInit(std::move(subInit)) {}

  SILValue getAddressOrNull() const override { return SILValue(); }


  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &gen) override {
    // If this is not an initialization, copy the value before we translateIt,
    // translation expects a +1 value.
    if (isInit)
      explodedElement.forwardInto(gen, loc, VarInit->getAddress());
    else
      explodedElement.copyInto(gen, VarInit->getAddress(), loc);
  }
  
  void finishInitialization(SILGenFunction &gen) override {
    VarInit->finishInitialization(gen);
  }
};

/// Abstract base class for refutable pattern initializations.
class RefutablePatternInitialization : public Initialization {
  /// This is the label to jump to if the pattern fails to match.
  JumpDest failureDest;
public:
  RefutablePatternInitialization(JumpDest failureDest)
  : Initialization(Initialization::Kind::Refutable), failureDest(failureDest) {
    assert(failureDest.isValid() &&
           "Refutable patterns can only exist in failable conditions");
  }

  JumpDest getFailureDest() const { return failureDest; }

  SILValue getAddressOrNull() const override { return SILValue(); }

  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &SGF) override = 0;


  void bindVariable(SILLocation loc, VarDecl *var, ManagedValue value,
                    CanType formalValueType, SILGenFunction &SGF) {
    // Initialize the variable value.
    InitializationPtr init = SGF.emitInitializationForVarDecl(var, Type());
    RValue(SGF, loc, formalValueType, value).forwardInto(SGF, init.get(), loc);
  }

};

class ExprPatternInitialization : public RefutablePatternInitialization {
  ExprPattern *P;
public:
  ExprPatternInitialization(ExprPattern *P, JumpDest patternFailDest)
    : RefutablePatternInitialization(patternFailDest), P(P) {}

  void copyOrInitValueInto(ManagedValue explodedElement, bool isInit,
                           SILLocation loc, SILGenFunction &SGF) override;
};

void ExprPatternInitialization::
copyOrInitValueInto(ManagedValue value, bool isInit,
                    SILLocation loc, SILGenFunction &SGF) {
  FullExpr scope(SGF.Cleanups, CleanupLocation(P));
  bindVariable(P, P->getMatchVar(), value,
               P->getType()->getCanonicalType(), SGF);

  // Emit the match test.
  SILValue testBool;
  {
    FullExpr scope(SGF.Cleanups, CleanupLocation(P->getMatchExpr()));
    testBool = SGF.emitRValueAsSingleValue(P->getMatchExpr()).
       getUnmanagedValue();
  }

  SILBasicBlock *contBB = SGF.B.splitBlockForFallthrough();
  auto falseBB = SGF.Cleanups.emitBlockForCleanups(getFailureDest(), loc);
  SGF.B.createCondBranch(loc, testBool, contBB, falseBB);

  SGF.B.setInsertionPoint(contBB);
}


/// InitializationForPattern - A visitor for traversing a pattern, generating
/// SIL code to allocate the declared variables, and generating an
/// Initialization representing the needed initializations.
struct InitializationForPattern
  : public PatternVisitor<InitializationForPattern, InitializationPtr>
{
  SILGenFunction &SGF;

  /// This is the place that should be jumped to if the pattern fails to match.
  /// This is invalid for irrefutable pattern initializations.
  JumpDest patternFailDest;

  InitializationForPattern(SILGenFunction &SGF, JumpDest patternFailDest)
    : SGF(SGF), patternFailDest(patternFailDest) {}

  // Paren, Typed, and Var patterns are noops, just look through them.
  InitializationPtr visitParenPattern(ParenPattern *P) {
    return visit(P->getSubPattern());
  }
  InitializationPtr visitTypedPattern(TypedPattern *P) {
    return visit(P->getSubPattern());
  }
  InitializationPtr visitVarPattern(VarPattern *P) {
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
    if (!P->getDecl()->hasName()) {
      // Unnamed parameters don't require any storage. Any value bound here will
      // just be dropped.
      return InitializationPtr(new BlackHoleInitialization());
    }

    auto Ty = P->hasType() ? P->getType() : Type();
    return SGF.emitInitializationForVarDecl(P->getDecl(), Ty);
  }

  // Bind a tuple pattern by aggregating the component variables into a
  // TupleInitialization.
  InitializationPtr visitTuplePattern(TuplePattern *P) {
    TupleInitialization *init = new TupleInitialization();
    for (auto &elt : P->getElements())
      init->subInitializations.push_back(visit(elt.getPattern()));
    return InitializationPtr(init);
  }

  InitializationPtr visitEnumElementPattern(EnumElementPattern *P) {
    llvm_unreachable("enum element pattern not supported in let/else yet");
  }
  InitializationPtr visitOptionalSomePattern(OptionalSomePattern *P) {
    llvm_unreachable("x? pattern not supported in let/else yet");
  }
  InitializationPtr visitIsPattern(IsPattern *P) {
    llvm_unreachable("'as' and 'is' pattern not supported in let/else yet");
  }
  InitializationPtr visitBoolPattern(BoolPattern *P) {
    llvm_unreachable("bools not supported in let/else yet");
  }
  InitializationPtr visitExprPattern(ExprPattern *P) {
    return InitializationPtr(new ExprPatternInitialization(P, patternFailDest));
  }
  InitializationPtr visitNominalTypePattern(NominalTypePattern *P) {
    P->dump();
    llvm_unreachable("pattern not supported in let/else yet");
  }
};

} // end anonymous namespace

InitializationPtr
SILGenFunction::emitInitializationForVarDecl(VarDecl *vd, Type patternType) {
  // If this is a computed variable, we don't need to do anything here.
  // We'll generate the getter and setter when we see their FuncDecls.
  if (!vd->hasStorage())
    return InitializationPtr(new BlackHoleInitialization());

  if (vd->isDebuggerVar()) {
    DebuggerClient *DebugClient = SGM.SwiftModule->getDebugClient();
    assert(DebugClient && "Debugger variables with no debugger client");
    SILDebuggerClient *SILDebugClient = DebugClient->getAsSILDebuggerClient();
    assert(SILDebugClient && "Debugger client doesn't support SIL");
    SILValue SV = SILDebugClient->emitLValueForVariable(vd, B);

    VarLocs[vd] = SILGenFunction::VarLoc::get(SV);
    return InitializationPtr(new DebuggerInitialization(SV));
  }

  CanType varType = vd->getType()->getCanonicalType();

  assert(!isa<InOutType>(varType) && "local variables should never be inout");

  // If this is a 'let' initialization for a non-global, set up a
  // let binding, which stores the initialization value into VarLocs directly.
  if (vd->isLet() && vd->getDeclContext()->isLocalContext() &&
      !isa<ReferenceStorageType>(varType))
    return InitializationPtr(new LetValueInitialization(vd, *this));

  // If the variable has no initial value, emit a mark_uninitialized instruction
  // so that DI tracks and enforces validity of it.
  bool isUninitialized =
    vd->getParentPatternBinding() && !vd->getParentInitializer();
  
  // If this is a global variable, initialize it without allocations or
  // cleanups.
  InitializationPtr Result;
  if (!vd->getDeclContext()->isLocalContext()) {
    auto *silG = SGM.getSILGlobalVariable(vd, NotForDefinition);
    SILValue addr = B.createGlobalAddr(vd, silG);
    if (isUninitialized)
      addr = B.createMarkUninitializedVar(vd, addr);

    VarLocs[vd] = SILGenFunction::VarLoc::get(addr);
    Result = InitializationPtr(new GlobalInitialization(addr));
  } else {
    Result = emitLocalVariableWithCleanup(vd, isUninitialized);
  }

  // If we're initializing a weak or unowned variable, this requires a change in
  // type.
  if (isa<ReferenceStorageType>(varType))
    Result = InitializationPtr(new
                           ReferenceStorageInitialization(std::move(Result)));
  return Result;
}

void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *PBD) {
  // If this is a conditional PBD, it will have an 'else' block.  Prepare for
  // its emission.
  JumpDest elseBB = JumpDest::invalid();
  if (PBD->isRefutable()) {
    assert(PBD->getElse().isExplicit() &&
           "Refutable PatternBinding must have an else block");

    // Create a basic block for the else block we'll jump to.
    elseBB = JumpDest(createBasicBlock(),
                      getCleanupsDepth(), CleanupLocation(PBD));
  }

  // Allocate the variables and build up an Initialization over their
  // allocated storage.
  for (auto entry : PBD->getPatternList()) {
    InitializationPtr initialization =
      InitializationForPattern(*this, elseBB).visit(entry.ThePattern);

    // If an initial value expression was specified by the decl, emit it into
    // the initialization. Otherwise, mark it uninitialized for DI to resolve.
    if (auto *Init = entry.Init) {
      FullExpr Scope(Cleanups, CleanupLocation(Init));
      emitExprInto(Init, initialization.get());
    } else {
      initialization->finishInitialization(*this);
    }
  }


  // For our conditional PBD, emit the else block logic.
  if (elseBB.getBlock()) {  // Emit the else block logic.
    // Move the insertion point to the throw destination.
    SavedInsertionPoint savedIP(*this, elseBB.getBlock());

    emitStmt(PBD->getElse().getExplicitBody());
    if (B.hasValidInsertionPoint()) {
      // The else block must end in a noreturn call, return, break etc.  It
      // isn't valid to fall off into the normal flow.  To model this, we emit
      // an unreachable instruction and then have SIL diagnostic check this.
      B.createUnreachable(PBD);
    }
  }
}

InitializationPtr
SILGenFunction::emitPatternBindingInitialization(Pattern *P) {
  return InitializationForPattern(*this, JumpDest::invalid()).visit(P);
}

/// Enter a cleanup to deallocate the given location.
CleanupHandle SILGenFunction::enterDeallocStackCleanup(SILValue temp) {
  assert(temp.getType().isLocalStorage() &&
         "must deallocate container operand, not address operand!");
  Cleanups.pushCleanup<DeallocStackCleanup>(temp);
  return Cleanups.getTopCleanup();
}

CleanupHandle SILGenFunction::enterDestroyCleanup(SILValue valueOrAddr) {
  Cleanups.pushCleanup<ReleaseValueCleanup>(valueOrAddr);
  return Cleanups.getTopCleanup();
}

void SILGenModule::emitExternalDefinition(Decl *d) {
  switch (d->getKind()) {
  case DeclKind::Func: {
    // We'll emit all the members of an enum when we visit the enum.
    if (isa<EnumDecl>(d->getDeclContext()))
      break;
    emitFunction(cast<FuncDecl>(d));
    break;
  }
  case DeclKind::Constructor: {
    auto C = cast<ConstructorDecl>(d);
    // We'll emit all the members of an enum when we visit the enum.
    if (isa<EnumDecl>(d->getDeclContext()))
      break;
    // For factories, we don't need to emit a special thunk; the normal
    // foreign-to-native thunk is sufficient.
    if (C->isFactoryInit())
      break;

    emitConstructor(C);
    break;
  }
  case DeclKind::Enum: {
    auto ed = cast<EnumDecl>(d);
    // Emit the enum cases and derived conformance methods for the type.
    for (auto member : ed->getMembers()) {
      if (auto elt = dyn_cast<EnumElementDecl>(member))
        emitEnumConstructor(elt);
      else if (auto func = dyn_cast<FuncDecl>(member))
        emitFunction(func);
      else if (auto ctor = dyn_cast<ConstructorDecl>(member))
        emitConstructor(ctor);
    }
    // Emit derived global decls.
    for (auto derived : ed->getDerivedGlobalDecls()) {
      emitFunction(cast<FuncDecl>(derived));
    }
    SWIFT_FALLTHROUGH;
  }
  case DeclKind::Struct:
  case DeclKind::Class: {
    // Emit witness tables.
    for (auto c : cast<NominalTypeDecl>(d)->getLocalConformances()) {
      if (Types.protocolRequiresWitnessTable(c->getProtocol()) &&
          c->isComplete() && isa<NormalProtocolConformance>(c))
        getWitnessTable(c);
    }
    break;
  }

  case DeclKind::Protocol:
    // Nothing to do in SILGen for other external types.
    break;

  case DeclKind::IfConfig:
    // Any active decls have been added to their parent, so there's nothing
    // else to emit.
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
  case DeclKind::Param:
  case DeclKind::Import:
  case DeclKind::Subscript:
  case DeclKind::Destructor:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("Not a valid external definition for SILGen");
  }
}

void SILGenFunction::emitLocalVariable(VarDecl *vd,
                               Optional<MarkUninitializedInst::Kind> MUIKind) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(vd->hasStorage() && "can't emit storage for a computed variable");
  assert(!VarLocs.count(vd) && "Already have an entry for this decl?");

  SILType lType = getLoweredType(vd->getType()->getRValueType());

  // The variable may have its lifetime extended by a closure, heap-allocate it
  // using a box.
  AllocBoxInst *allocBox = B.createAllocBox(vd, lType);
  auto box = SILValue(allocBox, 0);
  auto addr = SILValue(allocBox, 1);

  // Mark the memory as uninitialized, so DI will track it for us.
  if (MUIKind.hasValue())
    addr = B.createMarkUninitialized(vd, addr, MUIKind.getValue());

  /// Remember that this is the memory location that we're emitting the
  /// decl to.
  VarLocs[vd] = SILGenFunction::VarLoc::get(addr, box);
}

/// Create a LocalVariableInitialization for the uninitialized var.
InitializationPtr SILGenFunction::
emitLocalVariableWithCleanup(VarDecl *vd, bool NeedsMarkUninit) {
  Optional<MarkUninitializedInst::Kind> MUIKind;
  if (NeedsMarkUninit) MUIKind = MarkUninitializedInst::Var;
  emitLocalVariable(vd, MUIKind);
  return InitializationPtr(new LocalVariableInitialization(vd, *this));
}

/// Create an Initialization for an uninitialized temporary.
std::unique_ptr<TemporaryInitialization>
SILGenFunction::emitTemporary(SILLocation loc, const TypeLowering &tempTL) {
  SILValue addr = emitTemporaryAllocation(loc, tempTL.getLoweredType());
  return useBufferAsTemporary(loc, addr, tempTL);
}

/// Create an Initialization for an uninitialized buffer.
std::unique_ptr<TemporaryInitialization>
SILGenFunction::useBufferAsTemporary(SILLocation loc,
                                     SILValue addr,
                                     const TypeLowering &tempTL) {
  CleanupHandle cleanup = enterDormantTemporaryCleanup(addr, tempTL);
  return std::unique_ptr<TemporaryInitialization>(
                                    new TemporaryInitialization(addr, cleanup));
}

CleanupHandle
SILGenFunction::enterDormantTemporaryCleanup(SILValue addr,
                                             const TypeLowering &tempTL) {
  if (tempTL.isTrivial())
    return CleanupHandle::invalid();

  Cleanups.pushCleanupInState<ReleaseValueCleanup>(CleanupState::Dormant, addr);
  return Cleanups.getCleanupsDepth();
}

void SILGenFunction::destroyLocalVariable(SILLocation silLoc, VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(vd->hasStorage() && "can't emit storage for a computed variable");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");

  auto loc = VarLocs[vd];

  // For a heap variable, the box is responsible for the value. We just need
  // to give up our retain count on it.
  if (loc.box) {
    B.emitStrongRelease(silLoc, loc.box);
    return;
  }

  // For 'let' bindings, we emit a release_value or destroy_addr, depending on
  // whether we have an address or not.
  SILValue Val = loc.value;
  if (!Val.getType().isAddress())
    B.emitReleaseValueOperation(silLoc, Val);
  else
    B.emitDestroyAddr(silLoc, Val);
}

void SILGenFunction::deallocateUninitializedLocalVariable(SILLocation silLoc,
                                                          VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(vd->hasStorage() && "can't emit storage for a computed variable");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");

  auto loc = VarLocs[vd];

  // Ignore let values captured without a memory location.
  if (!loc.value.getType().isAddress()) return;

  assert(loc.box && "captured var should have been given a box");
  B.createDeallocBox(silLoc, loc.value.getType().getObjectType(),
                     loc.box);
}

//===----------------------------------------------------------------------===//
// ObjC method thunks
//===----------------------------------------------------------------------===//

static SILValue emitBridgeObjCReturnValue(SILGenFunction &gen,
                                          SILLocation loc,
                                          SILValue result,
                                          AbstractionPattern origNativeTy,
                                          CanType substNativeTy,
                                          CanType bridgedTy) {
  Scope scope(gen.Cleanups, CleanupLocation::get(loc));

  ManagedValue native = gen.emitManagedRValueWithCleanup(result);
  ManagedValue bridged = gen.emitNativeToBridgedValue(loc, native,
                                      SILFunctionTypeRepresentation::ObjCMethod,
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
  result = emitBridgeObjCReturnValue(gen, loc, result,
                                     AbstractionPattern(nativeTy),
                                     nativeTy,
                                     resultInfo.getType());

  // Autorelease the bridged result if necessary.
  switch (resultInfo.getConvention()) {
  case ResultConvention::Autoreleased:
    gen.B.createAutoreleaseReturn(loc, result);
    return;
  case ResultConvention::UnownedInnerPointer:
  case ResultConvention::Unowned:
    assert(gen.getTypeLowering(result.getType()).isTrivial()
           && "nontrivial result is returned unowned?!");
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

  lowering.emitRetainValue(gen.B, loc, arg);
  return arg;
}

/// Bridge argument types and adjust retain count conventions for an ObjC thunk.
static SILFunctionType *emitObjCThunkArguments(SILGenFunction &gen,
                                               SILLocation loc,
                                               SILDeclRef thunk,
                                               SmallVectorImpl<SILValue> &args){
  SILDeclRef native = thunk.asForeign(false);
  auto objcInfo = gen.SGM.Types.getConstantFunctionType(thunk);
  auto swiftInfo = gen.SGM.Types.getConstantFunctionType(native);

  // Borrow the context archetypes from the unthunked function.
  SILFunction *orig = gen.SGM.getFunction(native, NotForDefinition);
  gen.F.setContextGenericParams(orig->getContextGenericParams());

  SmallVector<ManagedValue, 8> bridgedArgs;
  bridgedArgs.reserve(objcInfo->getParameters().size());

  // Emit the indirect return argument, if any.
  if (objcInfo->hasIndirectResult()) {
    SILType argTy = gen.F.mapTypeIntoContext(
                           objcInfo->getIndirectResult().getSILType());
    auto arg = new (gen.F.getModule()) SILArgument(gen.F.begin(), argTy);
    bridgedArgs.push_back(ManagedValue::forUnmanaged(arg));
  }

  // Emit the other arguments, taking ownership of arguments if necessary.
  auto inputs = objcInfo->getParametersWithoutIndirectResult();
  auto nativeInputs = swiftInfo->getParametersWithoutIndirectResult();
  assert(!inputs.empty());
  assert(inputs.size() == nativeInputs.size());
  for (unsigned i = 0, e = inputs.size(); i < e; ++i) {
    SILType argTy = gen.F.mapTypeIntoContext(inputs[i].getSILType());
    SILValue arg = new(gen.F.getModule()) SILArgument(gen.F.begin(), argTy);

    // If this parameter is deallocating, emit an unmanged rvalue and
    // continue. The object has the deallocating bit set so retain, release is
    // irrelevent.
    if (inputs[i].isDeallocating()) {
      bridgedArgs.push_back(ManagedValue::forUnmanaged(arg));
      continue;
    }

    // If the argument is a block, copy it.
    if (argTy.isBlockPointerCompatible()) {
      auto copy = gen.B.createCopyBlock(loc, arg);
      // If the argument is consumed, we're still responsible for releasing the
      // original.
      if (inputs[i].isConsumed())
        gen.emitManagedRValueWithCleanup(arg);
      arg = copy;
    }
    // Convert the argument to +1 if necessary.
    else if (!inputs[i].isConsumed()) {
      arg = emitObjCUnconsumedArgument(gen, loc, arg);
    }

    auto managedArg = gen.emitManagedRValueWithCleanup(arg);

    bridgedArgs.push_back(managedArg);
  }

  assert(bridgedArgs.size() == objcInfo->getParameters().size() &&
         "objc inputs don't match number of arguments?!");
  assert(bridgedArgs.size() == swiftInfo->getParameters().size() &&
         "swift inputs don't match number of arguments?!");

  // Bridge the input types.
  Scope scope(gen.Cleanups, CleanupLocation::get(loc));
  assert(bridgedArgs.size() == nativeInputs.size());
  for (unsigned i = 0, size = bridgedArgs.size(); i < size; ++i) {
    SILType argTy = gen.F.mapTypeIntoContext(
                           swiftInfo->getParameters()[i].getSILType());
    ManagedValue native =
      gen.emitBridgedToNativeValue(loc,
                                   bridgedArgs[i],
                                   SILFunctionTypeRepresentation::ObjCMethod,
                                   argTy.getSwiftType());
    SILValue argValue;

    if (nativeInputs[i].isConsumed())
      argValue = native.forward(gen);
    else
      argValue = native.getValue();
    
    args.push_back(argValue);
  }

  return objcInfo;
}

void SILGenFunction::emitNativeToForeignThunk(SILDeclRef thunk) {
  assert(thunk.isForeign);
  SILDeclRef native = thunk.asForeign(false);

  auto loc = thunk.getAsRegularLocation();
  loc.markAutoGenerated();
  Scope scope(Cleanups, CleanupLocation::get(loc));

  // Bridge the arguments.
  SmallVector<SILValue, 4> args;
  auto objcFnTy = emitObjCThunkArguments(*this, loc, thunk, args);
  auto nativeInfo = getConstantInfo(native);
  auto swiftResultTy = nativeInfo.SILFnType->getResult()
    .map([&](CanType t) { return F.mapTypeIntoContext(t)->getCanonicalType(); });
  auto objcResultTy = objcFnTy->getResult()
    .map([&](CanType t) { return F.mapTypeIntoContext(t)->getCanonicalType(); });

  // Call the native entry point.
  SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);
  auto subs = F.getForwardingSubstitutions();
  auto substTy = nativeFn.getType().castTo<SILFunctionType>()
    ->substGenericArgs(SGM.M, SGM.M.getSwiftModule(), subs);
  SILValue result = B.createApply(loc, nativeFn,
                                  SILType::getPrimitiveObjectType(substTy),
                                  swiftResultTy.getSILType(), subs, args);

  scope.pop();
  
  emitObjCReturnValue(*this, loc, result, nativeInfo.LoweredType.getResult(),
                      objcResultTy);
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
  SILLinkage Linkage;

  SILGenConformance(SILGenModule &SGM, NormalProtocolConformance *C)
    // We only need to emit witness tables for base NormalProtocolConformances.
    : SGM(SGM), Conformance(C->getRootNormalConformance()),
      Linkage(SGM.Types.getLinkageForProtocolConformance(Conformance,
                                                         ForDefinition))
  {
    // Not all protocols use witness tables.
    if (!SGM.Types.protocolRequiresWitnessTable(Conformance->getProtocol()))
      Conformance = nullptr;
  }

  SILWitnessTable *emit() {
    // Nothing to do if this wasn't a normal conformance.
    if (!Conformance)
      return nullptr;

    // Reference conformances for refined protocols.
    auto protocol = Conformance->getProtocol();
    for (auto base : protocol->getInheritedProtocols(nullptr))
      emitBaseProtocolWitness(base);

    // Emit witnesses in protocol declaration order.
    for (auto reqt : protocol->getMembers())
      visit(reqt);

    // Check if we already have a declaration or definition for this witness
    // table.
    if (auto *wt = SGM.M.lookUpWitnessTable(Conformance, false).first) {
      // If we have a definition already, just return it.
      //
      // FIXME: I am not sure if this is possible, if it is not change this to an
      // assert.
      if (wt->isDefinition())
        return wt;

      // If we have a declaration, convert the witness table to a definition.
      if (wt->isDeclaration()) {
        wt->convertToDefinition(Entries, SGM.makeModuleFragile);

        // Since we had a declaration before, its linkage should be external,
        // ensure that we have a compatible linkage for sanity. *NOTE* we are ok
        // with both being shared since we do not have a shared_external
        // linkage.
        assert(stripExternalFromLinkage(wt->getLinkage()) == Linkage &&
               "Witness table declaration has inconsistent linkage with"
               " silgen definition.");

        // And then override the linkage with the new linkage.
        wt->setLinkage(Linkage);
        return wt;
      }
    }

    // Otherwise if we have no witness table yet, create it.
    return SILWitnessTable::create(SGM.M, Linkage, SGM.makeModuleFragile,
                                 Conformance, Entries);
  }

  void emitBaseProtocolWitness(ProtocolDecl *baseProtocol) {
    // Only include the witness if the base protocol requires it.
    if (!SGM.Types.protocolRequiresWitnessTable(baseProtocol))
      return;

    auto foundBaseConformance
      = Conformance->getInheritedConformances().find(baseProtocol);
    assert(foundBaseConformance != Conformance->getInheritedConformances().end()
           && "no inherited conformance for base protocol");

    auto conformance = foundBaseConformance->second;

    Entries.push_back(SILWitnessTable::BaseProtocolWitness{
      baseProtocol,
      conformance,
    });

    // Emit the witness table for the base conformance if it belongs to this
    // module or is shared.
    if (conformance->getDeclContext()->getParentModule()
          == SGM.SwiftModule
        || SGM.Types.getLinkageForProtocolConformance(
                                        conformance->getRootNormalConformance(),
                                        NotForDefinition)
          == SILLinkage::Shared)
      SGM.getWitnessTable(conformance->getRootNormalConformance());
  }

  /// Fallback for unexpected protocol requirements.
  void visitDecl(Decl *d) {
    d->print(llvm::errs());
    llvm_unreachable("unhandled protocol requirement");
  }

  void visitFuncDecl(FuncDecl *fd) {
    // FIXME: Emit getter and setter (if settable) witnesses.
    // For now we ignore them, like the IRGen witness table builder did.
    if (fd->isAccessor())
      return;

    // Find the witness in the conformance.
    ConcreteDeclRef witness = Conformance->getWitness(fd, nullptr);
    emitFuncEntry(fd, witness.getDecl(), witness.getSubstitutions());
  }

  void emitFuncEntry(FuncDecl *fd, ValueDecl *witnessDecl,
                     ArrayRef<Substitution> WitnessSubstitutions) {
    // Emit the witness thunk and add it to the table.

    // If this is a non-present optional requirement, emit a MissingOptional.
    if (!witnessDecl) {
      assert(fd->getAttrs().hasAttribute<OptionalAttr>() &&
             "Non-optional protocol requirement lacks a witness?");
      Entries.push_back(SILWitnessTable::MissingOptionalWitness{ fd });
      return;
    }


    // TODO: multiple resilience expansions?
    // TODO: multiple uncurry levels?
    SILDeclRef requirementRef(fd, SILDeclRef::Kind::Func,
                              ResilienceExpansion::Minimal);
    // Free function witnesses have an implicit uncurry layer imposed on them by
    // the inserted metatype argument.
    auto isFree = isFreeFunctionWitness(fd, witnessDecl);
    unsigned witnessUncurryLevel = isFree ? requirementRef.uncurryLevel - 1
                                          : requirementRef.uncurryLevel;

    SILDeclRef witnessRef(witnessDecl, SILDeclRef::Kind::Func,
                          SILDeclRef::ConstructAtBestResilienceExpansion,
                          witnessUncurryLevel);

    SILFunction *witnessFn =
      SGM.emitProtocolWitness(Conformance, Linkage, requirementRef, witnessRef,
                              isFree, WitnessSubstitutions);
    Entries.push_back(
                    SILWitnessTable::MethodWitness{requirementRef, witnessFn});
  }

  void visitConstructorDecl(ConstructorDecl *cd) {
    SILDeclRef requirementRef(cd, SILDeclRef::Kind::Allocator,
                              ResilienceExpansion::Minimal);

    ConcreteDeclRef witness = Conformance->getWitness(cd, nullptr);
    SILDeclRef witnessRef(witness.getDecl(), SILDeclRef::Kind::Allocator,
                          SILDeclRef::ConstructAtBestResilienceExpansion,
                          requirementRef.uncurryLevel);
    SILFunction *witnessFn =
      SGM.emitProtocolWitness(Conformance, Linkage, requirementRef, witnessRef,
                              IsNotFreeFunctionWitness,
                              witness.getSubstitutions());
    Entries.push_back(
      SILWitnessTable::MethodWitness{requirementRef, witnessFn});
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *d) {
    // Find the witness in the conformance.
    ConcreteDeclRef witness = Conformance->getWitness(d, nullptr);
    auto *witnessSD = cast<AbstractStorageDecl>(witness.getDecl());
    emitFuncEntry(d->getGetter(), witnessSD->getGetter(),
                  witness.getSubstitutions());
    if (d->isSettable(d->getDeclContext()))
      emitFuncEntry(d->getSetter(), witnessSD->getSetter(),
                    witness.getSubstitutions());
    if (auto materializeForSet = d->getMaterializeForSetFunc())
      emitFuncEntry(materializeForSet, witnessSD->getMaterializeForSetFunc(),
                    witness.getSubstitutions());
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *td) {
    // Find the substitution info for the witness type.
    const auto &witness = Conformance->getTypeWitness(td, /*resolver=*/nullptr);

    // Emit the record for the type itself.
    Entries.push_back(SILWitnessTable::AssociatedTypeWitness{td,
                                witness.getReplacement()->getCanonicalType()});

    // Emit records for the protocol requirements on the type.
    assert(td->getConformingProtocols(nullptr).size()
             == witness.getConformances().size()
           && "number of conformances in assoc type substitution do not match "
              "number of requirements on assoc type");
    // The conformances should be all null or all nonnull.
    assert(witness.getConformances().empty()
           || (witness.getConformances()[0]
                 ? std::all_of(witness.getConformances().begin(),
                               witness.getConformances().end(),
                               [&](const ProtocolConformance *C) -> bool {
                                 return C;
                               })
                 : std::all_of(witness.getConformances().begin(),
                               witness.getConformances().end(),
                               [&](const ProtocolConformance *C) -> bool {
                                 return !C;
                               })));

    for (unsigned i = 0, e = td->getConformingProtocols(nullptr).size(); i < e;
         ++i) {
      auto protocol = td->getConformingProtocols(nullptr)[i];

      // Only reference the witness if the protocol requires it.
      if (!SGM.Types.protocolRequiresWitnessTable(protocol))
        continue;

      ProtocolConformance *conformance = nullptr;
      // If the associated type requirement is satisfied by an associated type,
      // these will all be null.
      if (witness.getConformances()[0]) {
        auto foundConformance = std::find_if(witness.getConformances().begin(),
                                        witness.getConformances().end(),
                                        [&](ProtocolConformance *c) {
                                          return c->getProtocol() == protocol;
                                        });
        assert(foundConformance != witness.getConformances().end());
        conformance = *foundConformance;
      }

      Entries.push_back(SILWitnessTable::AssociatedTypeProtocolWitness{
        td, protocol, conformance
      });
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *pbd) {
    // We only care about the contained VarDecls.
  }

  void visitIfConfigDecl(IfConfigDecl *icd) {
    // We only care about the active members, which were already subsumed by the
    // enclosing type.
  }
};

} // end anonymous namespace

SILWitnessTable *
SILGenModule::getWitnessTable(ProtocolConformance *conformance) {
  auto normal = conformance->getRootNormalConformance();

  // If we've already emitted this witness table, return it.
  auto found = emittedWitnessTables.find(normal);
  if (found != emittedWitnessTables.end())
    return found->second;

  SILWitnessTable *table = SILGenConformance(*this, normal).emit();
  emittedWitnessTables.insert({normal, table});
  return table;
}

/// FIXME: This should just be a call down to Types.getLoweredType(), but I
/// really don't want to thread an old-type/interface-type pair through all
/// of TypeLowering.
static SILType
getWitnessFunctionType(SILModule &M,
                       AbstractionPattern origRequirementTy,
                       CanAnyFunctionType witnessSubstTy,
                       CanAnyFunctionType witnessSubstIfaceTy,
                       unsigned uncurryLevel) {
  // Lower the types to uncurry and get ExtInfo.
  AbstractionPattern origLoweredTy = origRequirementTy;
  if (auto origFTy = origRequirementTy.getAs<AnyFunctionType>())
    origLoweredTy =
      AbstractionPattern(M.Types.getLoweredASTFunctionType(origFTy,
                                                           uncurryLevel,
                                                           None));
  auto witnessLoweredTy
    = M.Types.getLoweredASTFunctionType(witnessSubstTy, uncurryLevel, None);
  auto witnessLoweredIfaceTy
    = M.Types.getLoweredASTFunctionType(witnessSubstIfaceTy, uncurryLevel, None);

  // Convert to SILFunctionType.
  auto fnTy = getNativeSILFunctionType(M, origLoweredTy,
                                       witnessLoweredTy,
                                       witnessLoweredIfaceTy);
  return SILType::getPrimitiveObjectType(fnTy);
}

SILFunction *
SILGenModule::emitProtocolWitness(ProtocolConformance *conformance,
                                  SILLinkage linkage,
                                  SILDeclRef requirement,
                                  SILDeclRef witness,
                                  IsFreeFunctionWitness_t isFree,
                                  ArrayRef<Substitution> witnessSubs) {
  // Get the type of the protocol requirement and the original type of the
  // witness.
  // FIXME: Rework for interface types.
  auto requirementInfo = Types.getConstantInfo(requirement);
  auto requirementTy
    = cast<PolymorphicFunctionType>(requirementInfo.FormalType);
  unsigned witnessUncurryLevel = witness.uncurryLevel;

  // Substitute the 'self' type into the requirement to get the concrete
  // witness type.
  auto witnessSubstTy = cast<AnyFunctionType>(
    requirementTy
      ->substGenericArgs(conformance->getDeclContext()->getParentModule(),
                         conformance->getType())
      ->getCanonicalType());

  GenericParamList *conformanceParams = conformance->getGenericParams();

  // If the requirement is generic, reparent its generic parameter list to
  // the generic parameters of the conformance.
  CanType methodTy = witnessSubstTy.getResult();
  if (auto pft = dyn_cast<PolymorphicFunctionType>(methodTy)) {
    auto &reqtParams = pft->getGenericParams();
    // Preserve the depth of generic arguments by adding an empty outer generic
    // param list if the conformance is concrete.
    GenericParamList *outerParams = conformanceParams;
    if (!outerParams)
      outerParams = GenericParamList::getEmpty(getASTContext());
    auto methodParams
      = reqtParams.cloneWithOuterParameters(getASTContext(), outerParams);
    methodTy = CanPolymorphicFunctionType::get(pft.getInput(), pft.getResult(),
                                               methodParams,
                                               pft->getExtInfo());
  }

  // If the conformance is generic, its generic parameters apply to
  // the witness as its outer generic param list.
  if (conformanceParams) {
    witnessSubstTy = CanPolymorphicFunctionType::get(witnessSubstTy.getInput(),
                                                   methodTy,
                                                   conformanceParams,
                                                   witnessSubstTy->getExtInfo());
  } else {
    witnessSubstTy = CanFunctionType::get(witnessSubstTy.getInput(),
                                          methodTy,
                                          witnessSubstTy->getExtInfo());
  }

  // If the witness is a free function, consider the self argument
  // uncurry level.
  if (isFree)
    ++witnessUncurryLevel;

  // The witness SIL function has the type of the AST-level witness, at the
  // abstraction level of the original protocol requirement.
  assert(requirement.uncurryLevel == witnessUncurryLevel &&
         "uncurry level of requirement and witness do not match");

  // Work out the interface type for the witness.
  auto reqtIfaceTy
    = cast<GenericFunctionType>(requirementInfo.FormalInterfaceType);
  // Substitute the 'self' type into the requirement to get the concrete witness
  // type, leaving the other generic parameters open.
  CanAnyFunctionType witnessSubstIfaceTy = cast<AnyFunctionType>(
    reqtIfaceTy->partialSubstGenericArgs(conformance->getDeclContext()->getParentModule(),
                                         conformance->getInterfaceType())
               ->getCanonicalType());

  // If the conformance is generic, its generic parameters apply to the witness.
  GenericSignature *sig
    = conformance->getGenericSignature();
  if (sig) {
    if (auto gft = dyn_cast<GenericFunctionType>(witnessSubstIfaceTy)) {
      SmallVector<GenericTypeParamType*, 4> allParams(sig->getGenericParams().begin(),
                                                      sig->getGenericParams().end());
      allParams.append(gft->getGenericParams().begin(),
                       gft->getGenericParams().end());
      SmallVector<Requirement, 4> allReqts(sig->getRequirements().begin(),
                                           sig->getRequirements().end());
      allReqts.append(gft->getRequirements().begin(),
                      gft->getRequirements().end());
      GenericSignature *witnessSig = GenericSignature::get(allParams, allReqts);

      witnessSubstIfaceTy = cast<GenericFunctionType>(
        GenericFunctionType::get(witnessSig,
                                 gft.getInput(), gft.getResult(),
                                 gft->getExtInfo())
          ->getCanonicalType());
    } else {
      assert(isa<FunctionType>(witnessSubstIfaceTy));
      witnessSubstIfaceTy = cast<GenericFunctionType>(
        GenericFunctionType::get(sig,
                                 witnessSubstIfaceTy.getInput(),
                                 witnessSubstIfaceTy.getResult(),
                                 witnessSubstIfaceTy->getExtInfo())
          ->getCanonicalType());
    }
  }
  // Lower the witness type with the requirement's abstraction level.
  // FIXME: We should go through TypeConverter::getLoweredType once we settle
  // on interface types.
  /*
  SILType witnessSILType = Types.getLoweredType(
                                              AbstractionPattern(requirementTy),
                                              witnessSubstTy,
                                              requirement.uncurryLevel);
   */
  SILType witnessSILType = getWitnessFunctionType(M,
                                              AbstractionPattern(requirementTy),
                                              witnessSubstTy,
                                              witnessSubstIfaceTy,
                                              requirement.uncurryLevel);

  // Mangle the name of the witness thunk.
  llvm::SmallString<128> nameBuffer;
  {
    llvm::raw_svector_ostream nameStream(nameBuffer);
    nameStream << "_TTW";
    Mangler mangler(nameStream);
    mangler.mangleProtocolConformance(conformance);

    if (auto ctor = dyn_cast<ConstructorDecl>(requirement.getDecl())) {
      mangler.mangleConstructorEntity(ctor, /*isAllocating=*/true,
                                      ResilienceExpansion::Minimal,
                                      requirement.uncurryLevel);
    } else {
      assert(isa<FuncDecl>(requirement.getDecl())
             && "need to handle mangling of non-Func SILDeclRefs here");
      auto requiredDecl = cast<FuncDecl>(requirement.getDecl());
      auto accessorKind = requiredDecl->getAccessorKind();
      if (accessorKind != AccessorKind::NotAccessor) {
        mangler.mangleAccessorEntity(accessorKind,
                                     requiredDecl->getAddressorKind(),
                                     requiredDecl->getAccessorStorageDecl(),
                                     ResilienceExpansion::Minimal);
      } else {
        mangler.mangleEntity(requiredDecl, ResilienceExpansion::Minimal,
                             requirement.uncurryLevel);
      }
    }
  }

  // Collect the context generic parameters for the witness.
  GenericParamList *witnessContextParams = conformanceParams;
  // If the requirement is generic, reparent its parameters to the conformance
  // parameters.
  if (auto reqtParams = requirementInfo.InnerGenericParams) {
    // Preserve the depth of generic arguments by adding an empty outer generic
    // param list if the conformance is concrete.
    GenericParamList *outerParams = conformanceParams;
    if (!outerParams)
      outerParams = GenericParamList::getEmpty(getASTContext());

    witnessContextParams
      = reqtParams->cloneWithOuterParameters(getASTContext(), outerParams);
  }

  auto *f = SILFunction::create(M, linkage, nameBuffer,
                witnessSILType.castTo<SILFunctionType>(),
                witnessContextParams,
                SILLocation(witness.getDecl()),
                IsNotBare,
                IsTransparent,
                makeModuleFragile ? IsFragile : IsNotFragile,
                IsThunk);

  f->setDebugScope(new (M)
                   SILDebugScope(RegularLocation(witness.getDecl()), *f));

  // Create the witness.
  SILGenFunction(*this, *f)
    .emitProtocolWitness(conformance, requirement, witness, witnessSubs,isFree);

  f->verify();

  return f;
}

SILFunction * SILGenModule::
getOrCreateReabstractionThunk(GenericParamList *thunkContextParams,
                              CanSILFunctionType thunkType,
                              CanSILFunctionType fromType,
                              CanSILFunctionType toType,
                              IsFragile_t Fragile) {
  // Mangle the reabstraction thunk.
  llvm::SmallString<256> buffer;
  {
    llvm::raw_svector_ostream stream(buffer);
    Mangler mangler(stream);

    // This is actually the SIL helper function.  For now, IR-gen
    // makes the actual thunk.
    stream << "_TTR";
    if (auto generics = thunkType->getGenericSignature()) {
      stream << 'G';
      mangler.mangleGenericSignature(generics,
                                     ResilienceExpansion::Minimal);
    }

    // Substitute context parameters out of the "from" and "to" types.
    auto fromInterfaceType
      = Types.getInterfaceTypeOutOfContext(fromType, thunkContextParams);
    auto toInterfaceType
      = Types.getInterfaceTypeOutOfContext(toType, thunkContextParams);

    mangler.mangleType(fromInterfaceType,
                       ResilienceExpansion::Minimal, /*uncurry*/ 0);
    mangler.mangleType(toInterfaceType,
                       ResilienceExpansion::Minimal, /*uncurry*/ 0);
  }

  auto loc = RegularLocation::getAutoGeneratedLocation();
  return M.getOrCreateSharedFunction(loc,
                                     buffer.str(),
                                     thunkType,
                                     IsBare, IsTransparent,
                                     Fragile, IsThunk);
}

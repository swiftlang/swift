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
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
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
    ArrayRef<InitializationPtr> getSubInitializations() const override {
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

    SILValue getAddressOrNull() const override { return ElementAddr; }

    void finishInitialization(SILGenFunction &gen) override {}
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
    return false;

  case Kind::Tuple:
    for (auto &subinit : getSubInitializations()) {
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
    return getSubInitializations();
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

ArrayRef<InitializationPtr>
SingleBufferInitialization::getSubInitializations() const {
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

  SILValue getAddressOrNull() const override {
    if (subInitializations.size() == 1)
      return subInitializations[0]->getAddressOrNull();
    else
      return SILValue();
  }

  ArrayRef<InitializationPtr> getSubInitializations() const override {
    return subInitializations;
  }

  void finishInitialization(SILGenFunction &gen) override {
    for (auto &sub : subInitializations)
      sub->finishInitialization(gen);
  }
};

class StrongReleaseCleanup : public Cleanup {
  SILValue box;
public:
  StrongReleaseCleanup(SILValue box) : box(box) {}
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.emitStrongRelease(l, box);
  }
};

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
    } else {
      // Push a cleanup to destroy the let declaration.  This has to be
      // inactive until the variable is initialized: if control flow exits the
      // before the value is bound, we don't want to destroy the value.
      gen.Cleanups.pushCleanupInState<DestroyLocalVariable>(
                                                    CleanupState::Dormant, vd);
      DestroyCleanup = gen.Cleanups.getTopCleanup();
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
  ArrayRef<InitializationPtr> getSubInitializations() const override {
    return {};
  }

  void bindValue(SILValue value, SILGenFunction &gen) override {
    assert(!gen.VarLocs.count(vd) && "Already emitted this vardecl?");
    // If we're binding an address to this let value, then we can use it as an
    // address later.  This happens when binding an address only parameter to
    // an argument, for example.
    if (value.getType().isAddress())
      address = value;
    gen.VarLocs[vd] = SILGenFunction::VarLoc::get(value);

    emitDebugValue(value, gen);
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

  void finishInitialization(SILGenFunction &gen) override {
    // Globals don't need to be cleaned up.
  }
};

class DebuggerInitialization : public GlobalInitialization {
public:
  DebuggerInitialization(SILValue address) : GlobalInitialization(address) {
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
    gen.B.createCopyAddr(l, gen.VarLocs[var].value, inoutAddr,
                         IsNotTake, IsNotInitialization);
  }
};

/// Initialize a variable of reference-storage type.
class ReferenceStorageInitialization : public Initialization {
  InitializationPtr VarInit;
public:
  ReferenceStorageInitialization(InitializationPtr &&subInit)
    : Initialization(Initialization::Kind::Translating),
      VarInit(std::move(subInit)) {}

  ArrayRef<InitializationPtr> getSubInitializations() const override { return {}; }
  SILValue getAddressOrNull() const override { return SILValue(); }

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

  InitializationForPattern(SILGenFunction &Gen) : Gen(Gen) {}

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
    return Gen.emitInitializationForVarDecl(P->getDecl(), Ty);
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

/// Get or create SILGlobalVariable for a given global VarDecl.
SILGlobalVariable *SILGenModule::getSILGlobalVariable(VarDecl *gDecl,
                                                      ForDefinition_t forDef) {
  // First mangle the global VarDecl.
  llvm::SmallString<32> mangledName;
  {
    llvm::raw_svector_ostream buffer(mangledName);

    // As a special case, Clang functions and globals don't get mangled at all.
    // FIXME: When we can import C++, use Clang's mangler.
    bool specialCase = false;
    if (auto clangDecl = gDecl->getClangDecl()) {
      if (auto namedClangDecl = dyn_cast<clang::DeclaratorDecl>(clangDecl)) {
        if (auto asmLabel = namedClangDecl->getAttr<clang::AsmLabelAttr>()) {
          buffer << '\01' << asmLabel->getLabel();
        } else {
          buffer << namedClangDecl->getName();
        }
        specialCase = true;
      }
    }

    if (!specialCase) {
      buffer << "_T";
      Mangler mangler(buffer);
      mangler.mangleEntity(gDecl, ResilienceExpansion(0), 0);
    }
  }

  // Check if it is already created, and update linkage if necessary.
  for (SILGlobalVariable &v : M.getSILGlobals()) {
    if (v.getName() == mangledName) {
      // Update the SILLinkage here if this is a definition.
      if (forDef == ForDefinition) {
        v.setLinkage(getSILLinkage(getDeclLinkage(gDecl), ForDefinition));
        v.setDeclaration(false);
      }
      return &v;
    }
  }

  // Get the linkage for SILGlobalVariable.
  SILLinkage link = getSILLinkage(getDeclLinkage(gDecl), forDef);

  Type ty = gDecl->getType();
  // If a NSString * global was imported as a String, emit a SIL global of type
  // NSString.
  if (gDecl->getClangDecl() && ty->isEqual(M.Types.getStringType())) {
    ty = M.Types.getNSStringType();
  }

  auto silTy = M.Types.getLoweredType(AbstractionPattern(ty),
                   ty->getCanonicalType()).getObjectType();

  auto *silGlobal = SILGlobalVariable::create(M, link,
                                              makeModuleFragile ? IsFragile : IsNotFragile,
                                              mangledName, silTy,
                                              None, gDecl);
  silGlobal->setDeclaration(!forDef);

  return silGlobal;
}

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

void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *D) {
  // Allocate the variables and build up an Initialization over their
  // allocated storage.
  for (auto entry : D->getPatternList()) {
    InitializationPtr initialization =
      InitializationForPattern(*this).visit(entry.ThePattern);

    // If an initial value expression was specified by the decl, emit it into
    // the initialization. Otherwise, mark it uninitialized for DI to resolve.
    if (auto *Init = entry.Init) {
      FullExpr Scope(Cleanups, CleanupLocation(Init));
      emitExprInto(Init, initialization.get());
    } else {
      initialization->finishInitialization(*this);
    }
  }
}

InitializationPtr
SILGenFunction::emitPatternBindingInitialization(Pattern *P) {
  return InitializationForPattern(*this).visit(P);
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

namespace {

class EmitBBArguments : public CanTypeVisitor<EmitBBArguments,
                                              /*RetTy*/ ManagedValue>
{
public:
  SILGenFunction &gen;
  SILBasicBlock *parent;
  SILLocation loc;
  bool functionArgs;
  ArrayRef<SILParameterInfo> &parameters;

  EmitBBArguments(SILGenFunction &gen, SILBasicBlock *parent,
                  SILLocation l, bool functionArgs,
                  ArrayRef<SILParameterInfo> &parameters)
    : gen(gen), parent(parent), loc(l), functionArgs(functionArgs),
      parameters(parameters) {}

  ManagedValue getManagedValue(SILValue arg, CanType t,
                                 SILParameterInfo parameterInfo) const {
    switch (parameterInfo.getConvention()) {
    case ParameterConvention::Direct_Deallocating:
      // If we have a deallocating parameter, it is passed in at +0 and will not
      // be deallocated since we do not allow for resurrection.
      return ManagedValue::forUnmanaged(arg);

    case ParameterConvention::Direct_Guaranteed:
    case ParameterConvention::Indirect_In_Guaranteed:
      // If we have a guaranteed parameter, it is passed in at +0, and its
      // lifetime is guaranteed. We can potentially use the argument as-is
      // if the parameter is bound as a 'let' without cleaning up.
      return ManagedValue::forUnmanaged(arg);
      
    case ParameterConvention::Direct_Unowned:
      // An unowned parameter is passed at +0, like guaranteed, but it isn't
      // kept alive by the caller, so we need to retain and manage it
      // regardless.
      return std::move(gen.emitManagedRetain(loc, arg));
    
    case ParameterConvention::Indirect_Inout:
      // An inout parameter is +0 and guaranteed, but represents an lvalue.
      return ManagedValue::forLValue(arg);
      
    case ParameterConvention::Direct_Owned:
    case ParameterConvention::Indirect_In:
      // An owned or 'in' parameter is passed in at +1. We can claim ownership
      // of the parameter and clean it up when it goes out of scope.
      return gen.emitManagedRValueWithCleanup(arg);
      
    case ParameterConvention::Indirect_Out:
      llvm_unreachable("should not emit @out parameters here");
    }
  }

  ManagedValue visitType(CanType t) {
    auto argType = gen.getLoweredType(t);
    // Pop the next parameter info.
    auto parameterInfo = parameters.front();
    parameters = parameters.slice(1);
    assert(argType == parent->getParent()
                            ->mapTypeIntoContext(parameterInfo.getSILType()) &&
           "argument does not have same type as specified by parameter info");
    
    SILValue arg = new (gen.SGM.M)
      SILArgument(parent, argType, loc.getAsASTNode<ValueDecl>());
    ManagedValue mv = getManagedValue(arg, t, parameterInfo);

    // If the value is a (possibly optional) ObjC block passed into the entry
    // point of the function, then copy it so we can treat the value reliably
    // as a heap object. Escape analysis can eliminate this copy if it's
    // unneeded during optimization.
    CanType objectType = t;
    if (auto theObjTy = t.getAnyOptionalObjectType())
      objectType = theObjTy;
    if (functionArgs
        && isa<FunctionType>(objectType)
        && cast<FunctionType>(objectType)->getRepresentation()
              == FunctionType::Representation::Block) {
      SILValue blockCopy = gen.B.createCopyBlock(loc, mv.getValue());
      mv = gen.emitManagedRValueWithCleanup(blockCopy);
    }
    return mv;
  }

  ManagedValue visitTupleType(CanTupleType t) {
    SmallVector<ManagedValue, 4> elements;
    
    auto &tl = gen.getTypeLowering(t);
    bool canBeGuaranteed = tl.isLoadable();

    // Collect the exploded elements.
    for (auto fieldType : t.getElementTypes()) {
      auto elt = visit(fieldType);
      // If we can't borrow one of the elements as a guaranteed parameter, then
      // we have to +1 the tuple.
      if (elt.hasCleanup())
        canBeGuaranteed = false;
      elements.push_back(elt);
    }
    
    if (tl.isLoadable()) {
      SmallVector<SILValue, 4> elementValues;
      if (canBeGuaranteed) {
        // If all of the elements were guaranteed, we can form a guaranteed tuple.
        for (auto element : elements)
          elementValues.push_back(element.getUnmanagedValue());
      } else {
        // Otherwise, we need to move or copy values into a +1 tuple.
        for (auto element : elements) {
          SILValue value = element.hasCleanup()
            ? element.forward(gen)
            : element.copyUnmanaged(gen, loc).forward(gen);
          elementValues.push_back(value);
        }
      }
      auto tupleValue = gen.B.createTuple(loc, tl.getLoweredType(),
                                          elementValues);
      return canBeGuaranteed
        ? ManagedValue::forUnmanaged(tupleValue)
        : gen.emitManagedRValueWithCleanup(tupleValue);
    } else {
      // If the type is address-only, we need to move or copy the elements into
      // a tuple in memory.
      // TODO: It would be a bit more efficient to use a preallocated buffer
      // in this case.
      auto buffer = gen.emitTemporaryAllocation(loc, tl.getLoweredType());
      for (auto i : indices(elements)) {
        auto element = elements[i];
        auto elementBuffer = gen.B.createTupleElementAddr(loc, buffer,
                                        i, element.getType().getAddressType());
        if (element.hasCleanup())
          element.forwardInto(gen, loc, elementBuffer);
        else
          element.copyInto(gen, elementBuffer, loc);
      }
      return gen.emitManagedRValueWithCleanup(buffer);
    }
  }
};

/// A visitor for traversing a pattern, creating
/// SILArguments, and binding variables to the argument names.
struct ArgumentInitVisitor :
  public PatternVisitor<ArgumentInitVisitor, /*RetTy=*/ void>
{
  SILGenFunction &gen;
  SILFunction &f;
  SILBuilder &initB;

  /// An ArrayRef that we use in our SILParameterList queue. Parameters are
  /// sliced off of the front as they're emitted.
  ArrayRef<SILParameterInfo> parameters;

  ArgumentInitVisitor(SILGenFunction &gen, SILFunction &f)
    : gen(gen), f(f), initB(gen.B),
      parameters(f.getLoweredFunctionType()->getParameters()) {
    // If we have an out parameter, skip it.
    if (parameters.size() && parameters[0].isIndirectResult())
      parameters = parameters.slice(1);
  }
  
  ManagedValue makeArgument(Type ty, SILBasicBlock *parent, SILLocation l) {
    assert(ty && "no type?!");

    // Create an RValue by emitting destructured arguments into a basic block.
    CanType canTy = ty->getCanonicalType();
    
    return EmitBBArguments(gen, parent, l, /*functionArgs*/ true,
                           parameters).visit(canTy);
  }

  /// Create a SILArgument and store its value into the given Initialization,
  /// if not null.
  void makeArgumentIntoBinding(Type ty, SILBasicBlock *parent, VarDecl *vd) {
    SILLocation loc(vd);
    loc.markAsPrologue();

    ManagedValue argrv = makeArgument(ty, parent, loc);

    // Create a shadow copy of inout parameters so they can be captured
    // by closures. The InOutDeshadowing guaranteed optimization will
    // eliminate the variable if it is not needed.
    if (auto inOutTy = vd->getType()->getAs<InOutType>()) {
      
      SILValue address = argrv.getUnmanagedValue();
      
      CanType objectType = inOutTy->getObjectType()->getCanonicalType();

      // As a special case, don't introduce a local variable for
      // Builtin.UnsafeValueBuffer, which is not copyable.
      if (isa<BuiltinUnsafeValueBufferType>(objectType)) {
        // FIXME: mark a debug location?
        gen.VarLocs[vd] = SILGenFunction::VarLoc::get(address);
        return;
      }

      // Allocate the local variable for the inout.
      auto initVar = gen.emitLocalVariableWithCleanup(vd, false);

      // Initialize with the value from the inout with an "autogenerated"
      // copyaddr.
      loc.markAutoGenerated();
      gen.B.createCopyAddr(loc, address, initVar->getAddress(),
                           IsNotTake, IsInitialization);
      initVar->finishInitialization(gen);

      // Set up a cleanup to write back to the inout.
      gen.Cleanups.pushCleanup<CleanupWriteBackToInOut>(vd, address);
    } else if (vd->isLet()) {
      // If the variable is immutable, we can bind the value as is.
      // Leave the cleanup on the argument, if any, in place to consume the
      // argument if we're responsible for it.
      gen.VarLocs[vd] = SILGenFunction::VarLoc::get(argrv.getValue());
      if (argrv.getType().isAddress())
        gen.B.createDebugValueAddr(loc, argrv.getValue());
      else
        gen.B.createDebugValue(loc, argrv.getValue());
    } else {
      // If the variable is mutable, we need to copy or move the argument
      // value to local mutable memory.

      auto initVar = gen.emitLocalVariableWithCleanup(vd, false);

      // If we have a cleanup on the value, we can move it into the variable.
      if (argrv.hasCleanup())
        argrv.forwardInto(gen, loc, initVar->getAddress());
      // Otherwise, we need an independently-owned copy.
      else
        argrv.copyInto(gen, initVar->getAddress(), loc);
      
      initVar->finishInitialization(gen);
      
    }
  }

  // Paren, Typed, and Var patterns are no-ops. Just look through them.
  void visitParenPattern(ParenPattern *P) {
    visit(P->getSubPattern());
  }
  void visitTypedPattern(TypedPattern *P) {
    visit(P->getSubPattern());
  }
  void visitVarPattern(VarPattern *P) {
    visit(P->getSubPattern());
  }

  void visitTuplePattern(TuplePattern *P) {
    // Destructure tuples into their elements.
    for (size_t i = 0, size = P->getFields().size(); i < size; ++i)
      visit(P->getFields()[i].getPattern());
  }

  void visitAnyPattern(AnyPattern *P) {
    llvm_unreachable("unnamed parameters should have a ParamDecl");
  }

  void visitNamedPattern(NamedPattern *P) {
    auto PD = P->getDecl();
    if (!PD->hasName()) {
      // A value bound to _ is unused and can be immediately released.
      Scope discardScope(gen.Cleanups, CleanupLocation(P));
      makeArgument(P->getType(), f.begin(), PD);
      // Popping the scope destroys the value.
    } else {
      makeArgumentIntoBinding(P->getType(), f.begin(), PD);
    }
  }

#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) \
  void visit##Id##Pattern(Id##Pattern *) { \
    llvm_unreachable("pattern not valid in argument binding"); \
  }
#include "swift/AST/PatternNodes.def"

};

/// Tuple values captured by a closure are passed as individual arguments to the
/// SILFunction since SILFunctionType canonicalizes away tuple types.
static SILValue
emitReconstitutedConstantCaptureArguments(SILType ty,
                                          ValueDecl *capture,
                                          SILGenFunction &gen) {
  auto TT = ty.getAs<TupleType>();
  if (!TT)
    return new (gen.SGM.M) SILArgument(gen.F.begin(), ty, capture);

  SmallVector<SILValue, 4> Elts;
  for (unsigned i = 0, e = TT->getNumElements(); i != e; ++i) {
    auto EltTy = ty.getTupleElementType(i);
    auto EV =
      emitReconstitutedConstantCaptureArguments(EltTy, capture, gen);
    Elts.push_back(EV);
  }

  return gen.B.createTuple(capture, ty, Elts);
}

static void emitCaptureArguments(SILGenFunction &gen, CapturedValue capture) {
  ASTContext &c = gen.getASTContext();

  auto *VD = capture.getDecl();
  auto type = VD->getType();
  switch (gen.SGM.Types.getDeclCaptureKind(capture)) {
  case CaptureKind::None:
    break;

  case CaptureKind::Constant: {
    auto &lowering = gen.getTypeLowering(VD->getType());
    // Constant decls are captured by value.  If the captured value is a tuple
    // value, we need to reconstitute it before sticking it in VarLocs.
    SILType ty = lowering.getLoweredType();
    SILValue val = emitReconstitutedConstantCaptureArguments(ty, VD, gen);

    // If the original variable was settable, then Sema will have treated the
    // VarDecl as an lvalue, even in the closure's use.  As such, we need to
    // allow formation of the address for this captured value.  Create a
    // temporary within the closure to provide this address.
    if (VD->isSettable(VD->getDeclContext())) {
      auto addr = gen.emitTemporaryAllocation(VD, ty);
      gen.B.createStore(VD, val, addr);
      val = addr;
    }

    gen.VarLocs[VD] = SILGenFunction::VarLoc::get(val);
    if (!lowering.isTrivial())
      gen.enterDestroyCleanup(val);
    break;
  }

  case CaptureKind::Box: {
    // LValues are captured as two arguments: a retained NativeObject that owns
    // the captured value, and the address of the value itself.
    SILType ty = gen.getLoweredType(type).getAddressType();
    SILValue box = new (gen.SGM.M) SILArgument(gen.F.begin(),
                                               SILType::getNativeObjectType(c),
                                               VD);
    SILValue addr = new (gen.SGM.M) SILArgument(gen.F.begin(), ty, VD);
    gen.VarLocs[VD] = SILGenFunction::VarLoc::get(addr, box);
    gen.Cleanups.pushCleanup<StrongReleaseCleanup>(box);
    break;
  }
  case CaptureKind::StorageAddress: {
    // Non-escaping stored decls are captured as the address of the value.
    SILType ty = gen.getLoweredType(type).getAddressType();
    SILValue addr = new (gen.SGM.M) SILArgument(gen.F.begin(), ty, VD);
    gen.VarLocs[VD] = SILGenFunction::VarLoc::get(addr);
    break;
  }
  case CaptureKind::LocalFunction: {
    // Local functions are captured by value.
    assert(!type->is<LValueType>() && !type->is<InOutType>() &&
           "capturing inout by value?!");
    const TypeLowering &ti = gen.getTypeLowering(type);
    SILValue value = new (gen.SGM.M) SILArgument(gen.F.begin(),
                                                 ti.getLoweredType(),
                                                 VD);
    gen.LocalFunctions[SILDeclRef(VD)] = value;
    gen.enterDestroyCleanup(value);
    break;
  }
  case CaptureKind::GetterSetter: {
    // Capture the setter and getter closures by value.
    Type setTy = cast<AbstractStorageDecl>(VD)->getSetter()->getType();
    SILType lSetTy = gen.getLoweredType(setTy);
    SILValue value = new (gen.SGM.M) SILArgument(gen.F.begin(), lSetTy, VD);
    gen.LocalFunctions[SILDeclRef(cast<AbstractStorageDecl>(VD)->getSetter(),
                                  SILDeclRef::Kind::Func)] = value;
    gen.enterDestroyCleanup(value);
    SWIFT_FALLTHROUGH;
  }
  case CaptureKind::Getter: {
    // Capture the getter closure by value.
    Type getTy = cast<AbstractStorageDecl>(VD)->getGetter()->getType();
    SILType lGetTy = gen.getLoweredType(getTy);
    SILValue value = new (gen.SGM.M) SILArgument(gen.F.begin(), lGetTy, VD);
    gen.LocalFunctions[SILDeclRef(cast<AbstractStorageDecl>(VD)->getGetter(),
                                  SILDeclRef::Kind::Func)] = value;
    gen.enterDestroyCleanup(value);
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
  SmallVector<CapturedValue, 4> LocalCaptures;
  TheClosure.getLocalCaptures(LocalCaptures);
  for (auto capture : LocalCaptures)
    emitCaptureArguments(*this, capture);
}

void SILGenFunction::emitProlog(ArrayRef<Pattern *> paramPatterns,
                                Type resultType, DeclContext *DeclCtx) {
  // If the return type is address-only, emit the indirect return argument.
  const TypeLowering &returnTI = getTypeLowering(resultType);
  if (returnTI.isReturnedIndirectly()) {
    auto &AC = getASTContext();
    auto VD = new (AC) ParamDecl(/*IsLet*/ false, SourceLoc(),
                                 AC.getIdentifier("$return_value"), SourceLoc(),
                                 AC.getIdentifier("$return_value"), resultType,
                                 DeclCtx);
    IndirectReturnAddress = new (SGM.M)
      SILArgument(F.begin(), returnTI.getLoweredType(), VD);
  }

  // Emit the argument variables in calling convention order.
  ArgumentInitVisitor argVisitor(*this, F);
  for (Pattern *p : reversed(paramPatterns)) {
    // Add the SILArguments and use them to initialize the local argument
    // values.
    argVisitor.visit(p);
  }
}

SILValue SILGenFunction::emitSelfDecl(VarDecl *selfDecl) {
  // Emit the implicit 'self' argument.
  SILType selfType = getLoweredLoadableType(selfDecl->getType());
  SILValue selfValue = new (SGM.M) SILArgument(F.begin(), selfType, selfDecl);
  VarLocs[selfDecl] = VarLoc::get(selfValue);
  SILLocation PrologueLoc(selfDecl);
  PrologueLoc.markAsPrologue();
  B.createDebugValue(PrologueLoc, selfValue);
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
      new (F.getModule()) SILArgument(epilogBB, resultTI.getLoweredType());
  }
  ReturnDest = JumpDest(epilogBB, getCleanupsDepth(), CleanupL);
}

bool SILGenModule::requiresObjCMethodEntryPoint(FuncDecl *method) {
  // Property accessors should be generated alongside the property unless
  // the @NSManagedAttr attribute is present.
  if (method->isGetterOrSetter()) {
    auto asd = method->getAccessorStorageDecl();
    return asd->hasObjCGetterAndSetter() &&
           !asd->getAttrs().hasAttribute<NSManagedAttr>();
  }

  return method->isObjC() || method->getAttrs().hasAttribute<IBActionAttr>();
}

bool SILGenModule::requiresObjCMethodEntryPoint(ConstructorDecl *constructor) {
  return constructor->isObjC();
}

bool SILGenModule::requiresObjCDispatch(ValueDecl *vd) {
  // Final functions never require ObjC dispatch.
  if (vd->isFinal())
    return false;

  if (auto *fd = dyn_cast<FuncDecl>(vd)) {
    // If a function has an associated Clang node, it's foreign and only has
    // an ObjC entry point.
    if (vd->hasClangNode())
      return true;

    // Property accessors should be generated alongside the property.
    if (fd->isGetterOrSetter())
      return requiresObjCDispatch(fd->getAccessorStorageDecl());

    return fd->getAttrs().hasAttribute<DynamicAttr>();
  }
  if (auto *cd = dyn_cast<ConstructorDecl>(vd)) {
    // If a function has an associated Clang node, it's foreign and only has
    // an ObjC entry point.
    if (vd->hasClangNode())
      return true;

    return cd->getAttrs().hasAttribute<DynamicAttr>();
  }
  if (auto *asd = dyn_cast<AbstractStorageDecl>(vd))
    return asd->requiresObjCGetterAndSetter();

  return vd->getAttrs().hasAttribute<DynamicAttr>();
}

bool SILGenModule::requiresObjCSuperDispatch(ValueDecl *vd) {
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

    // Only visit the members for a class defined natively.
    if (!ancestor->hasClangNode()) {
      for (auto member : ancestor->getMembers())
        visit(member);
    }
  }

  // Add an entry to the vtable.
  void addEntry(SILDeclRef member) {
    /// Get the function to reference from the vtable.
    auto getVtableEntryFn = [&](SILDeclRef entry) -> SILFunction* {
      // If the member is dynamic, reference its dynamic dispatch thunk so that
      // it will be redispatched, funneling the method call through the runtime
      // hook point.
      // TODO: Dynamic thunks could conceivably require reabstraction too.
      if (member.getDecl()->getAttrs().hasAttribute<DynamicAttr>())
        return SGM.getDynamicThunk(member, SGM.Types.getConstantInfo(member));
      
      // The derived method may require thunking to match up to the ABI of the
      // base method.
      return SGM.emitVTableMethod(member, entry);
    };

    // Try to find an overridden entry.
    // NB: Mutates vtableEntries in-place
    // FIXME: O(n^2)
    if (auto overridden = member.getOverriddenVTableEntry()) {
      for (SILVTable::Pair &entry : vtableEntries) {
        SILDeclRef ref = overridden;

        do {
          // Replace the overridden member.
          if (entry.first == ref) {
            // The entry is keyed by the least derived method.
            entry = {ref, getVtableEntryFn(ref)};
            return;
          }
        } while ((ref = ref.getOverridden()));
      }
      llvm_unreachable("no overridden vtable entry?!");
    }

    // If this is a final member and isn't overriding something, we don't need
    // to add it to the vtable.
    if (member.getDecl()->isFinal())
      return;
    // If this is dynamic and isn't overriding a non-dynamic method, it'll
    // always be accessed by objc_msgSend, so we don't need to add it to the
    // vtable.
    if (member.getDecl()->getAttrs().hasAttribute<DynamicAttr>())
      return;

    // Otherwise, introduce a new vtable entry.
    vtableEntries.emplace_back(member, getVtableEntryFn(member));
  }

  // Default for members that don't require vtable entries.
  void visitDecl(Decl*) {}

  void visitFuncDecl(FuncDecl *fd) {
    // ObjC decls don't go in vtables.
    if (fd->hasClangNode())
      return;
    
    // Observers don't get separate vtable entries.
    if (fd->isObservingAccessor())
      return;

    addEntry(SILDeclRef(fd));
  }

  void visitConstructorDecl(ConstructorDecl *cd) {
    // Stub constructors don't get an entry.
    if (cd->hasStubImplementation())
      return;

    // Required constructors (or overrides thereof) have their allocating entry
    // point in the vtable.
    bool isRequired = false;
    auto override = cd;
    while (override) {
      if (override->isRequired()) {
        isRequired = true;
        break;
      }
      override = override->getOverriddenDecl();
    }
    if (isRequired) {
      addEntry(SILDeclRef(cd, SILDeclRef::Kind::Allocator));
    }

    // All constructors have their initializing constructor in the
    // vtable, which can be used by a convenience initializer.
    addEntry(SILDeclRef(cd, SILDeclRef::Kind::Initializer));
  }

  void visitVarDecl(VarDecl *vd) {
    // Note: dynamically-dispatched properties have their getter and setter
    // added to the vtable when they are visited.
  }

  void visitDestructorDecl(DestructorDecl *dd) {
    if (dd->getParent()->isClassOrClassExtensionContext() == theClass) {
      // Add the deallocating destructor to the vtable just for the purpose
      // that it is referenced and cannot be eliminated by dead function removal.
      addEntry(SILDeclRef(dd, SILDeclRef::Kind::Deallocator));
    }
  }

  void visitSubscriptDecl(SubscriptDecl *sd) {
    // Note: dynamically-dispatched properties have their getter and setter
    // added to the vtable when they are visited.
  }
};

static void emitTypeMemberGlobalVariable(SILGenModule &SGM,
                                         GenericParamList *generics,
                                         NominalTypeDecl *theType,
                                         VarDecl *var) {
  assert(!generics && "generic static properties not implemented");
  if (var->getDeclContext()->isClassOrClassExtensionContext()) {
    assert(var->isFinal() && "only 'static' ('class final') stored properties are implemented in classes");
  }

  SGM.addGlobalVariable(var);
}


/// An ASTVisitor for generating SIL from method declarations
/// inside nominal types.
class SILGenType : public TypeMemberVisitor<SILGenType> {
public:
  SILGenModule &SGM;
  NominalTypeDecl *theType;
  Optional<SILGenVTable> genVTable;

  SILGenType(SILGenModule &SGM, NominalTypeDecl *theType)
    : SGM(SGM), theType(theType) {}

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

    for (Decl *member : theType->getDerivedGlobalDecls()) {
      SGM.visit(member);
    }

    // Emit witness tables for conformances of concrete types. Protocol types
    // are existential and do not have witness tables.
    if (isa<ProtocolDecl>(theType))
      return;

    for (auto *conformance : theType->getAllConformances(nullptr,
                                                         /*sorted=*/true)) {
      if (conformance->isComplete() &&
          isa<NormalProtocolConformance>(conformance))
        SGM.getWitnessTable(conformance);
    }
  }

  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitTypeAliasDecl(TypeAliasDecl *tad) {}
  void visitAbstractTypeParamDecl(AbstractTypeParamDecl *tpd) {}
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    ProfilerRAII Profiler(SGM, fd);
    SGM.emitFunction(fd);
    // FIXME: Default implementations in protocols.
    if (SGM.requiresObjCMethodEntryPoint(fd) &&
        !isa<ProtocolDecl>(fd->getDeclContext()))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    ProfilerRAII Profiler(SGM, cd);
    SGM.emitConstructor(cd);

    if (SGM.requiresObjCMethodEntryPoint(cd) &&
        !isa<ProtocolDecl>(cd->getDeclContext()))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    assert(isa<ClassDecl>(theType) && "destructor in a non-class type");
    ProfilerRAII Profiler(SGM, dd);
    SGM.emitDestructor(cast<ClassDecl>(theType), dd);
  }

  void visitEnumCaseDecl(EnumCaseDecl *ecd) {}
  void visitEnumElementDecl(EnumElementDecl *ued) {
    assert(isa<EnumDecl>(theType));
    SGM.emitEnumConstructor(ued);
  }

  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers for static variables.
    if (!pd->isStatic()) return;
    
    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i)
      if (pd->getInit(i))
        SGM.emitGlobalInitialization(pd, i);
  }

  void visitVarDecl(VarDecl *vd) {
    // Collect global variables for static properties.
    // FIXME: We can't statically emit a global variable for generic properties.
    if (vd->isStatic() && vd->hasStorage()) {
      return emitTypeMemberGlobalVariable(SGM, theType->getGenericParams(),
                                          theType, vd);
    }

    visitAbstractStorageDecl(vd);
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *asd) {
    // FIXME: Default implementations in protocols.
    if (asd->hasObjCGetterAndSetter() &&
        !isa<ProtocolDecl>(asd->getDeclContext()))
      SGM.emitObjCPropertyMethodThunks(asd);
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
    for (auto c : cast<NominalTypeDecl>(d)->getLocalConformances(nullptr)) {
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

/// SILGenExtension - an ASTVisitor for generating SIL from method declarations
/// and protocol conformances inside type extensions.
class SILGenExtension : public TypeMemberVisitor<SILGenExtension> {
public:
  SILGenModule &SGM;

  SILGenExtension(SILGenModule &SGM)
    : SGM(SGM) {}

  /// Emit SIL functions for all the members of the extension.
  void emitExtension(ExtensionDecl *e) {
    for (Decl *member : e->getMembers())
      visit(member);

    if (!e->getExtendedType()->isExistentialType()) {
      // Emit witness tables for protocol conformances introduced by the
      // extension.
      for (auto *conformance : e->getLocalConformances(nullptr)) {
        if (conformance->isComplete() &&
            isa<NormalProtocolConformance>(conformance))
          SGM.getWitnessTable(conformance);
      }
    }
  }

  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitTypeAliasDecl(TypeAliasDecl *tad) {}
  void visitAbstractTypeParamDecl(AbstractTypeParamDecl *tpd) {}
  void visitNominalTypeDecl(NominalTypeDecl *ntd) {
    SILGenType(SGM, ntd).emitType();
  }
  void visitFuncDecl(FuncDecl *fd) {
    ProfilerRAII Profiler(SGM, fd);
    SGM.emitFunction(fd);
    if (SGM.requiresObjCMethodEntryPoint(fd))
      SGM.emitObjCMethodThunk(fd);
  }
  void visitConstructorDecl(ConstructorDecl *cd) {
    ProfilerRAII Profiler(SGM, cd);
    SGM.emitConstructor(cd);
    if (SGM.requiresObjCMethodEntryPoint(cd))
      SGM.emitObjCConstructorThunk(cd);
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    llvm_unreachable("destructor in extension?!");
  }

  void visitPatternBindingDecl(PatternBindingDecl *pd) {
    // Emit initializers for static variables.
    if (!pd->isStatic()) return;
    
    for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i)
      if (pd->getInit(i))
        SGM.emitGlobalInitialization(pd, i);
  }

  void visitVarDecl(VarDecl *vd) {
    if (vd->isStatic() && vd->hasStorage()) {
      ExtensionDecl *ext = cast<ExtensionDecl>(vd->getDeclContext());
      NominalTypeDecl *theType = ext->getExtendedType()->getAnyNominal();
      return emitTypeMemberGlobalVariable(SGM, ext->getGenericParams(),
                                          theType, vd);
    }
    visitAbstractStorageDecl(vd);
  }

  void visitEnumCaseDecl(EnumCaseDecl *ecd) {}
  void visitEnumElementDecl(EnumElementDecl *ed) {
    llvm_unreachable("enum elements aren't allowed in extensions");
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *vd) {
    if (vd->hasObjCGetterAndSetter())
      SGM.emitObjCPropertyMethodThunks(vd);
  }
};

void SILGenModule::visitExtensionDecl(ExtensionDecl *ed) {
  SILGenExtension(*this).emitExtension(ed);
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
  Scope scope(gen.Cleanups, CleanupLocation::getCleanupLocation(loc));
  assert(bridgedArgs.size() == nativeInputs.size());
  for (unsigned i = 0, size = bridgedArgs.size(); i < size; ++i) {
    SILType argTy = gen.F.mapTypeIntoContext(
                           swiftInfo->getParameters()[i].getSILType());
    ManagedValue native =
      gen.emitBridgedToNativeValue(loc,
                                   bridgedArgs[i],
                                   AbstractCC::ObjCMethod,
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
  Scope scope(Cleanups, CleanupLocation::getCleanupLocation(loc));

  // Bridge the arguments.
  SmallVector<SILValue, 4> args;
  auto objcFnTy = emitObjCThunkArguments(*this, loc, thunk, args);
  auto nativeInfo = getConstantInfo(native);
  auto swiftResultTy = nativeInfo.SILFnType->getResult()
    .transform([&](Type t) { return F.mapTypeIntoContext(t); });
  auto objcResultTy = objcFnTy->getResult()
    .transform([&](Type t) { return F.mapTypeIntoContext(t); });

  // Call the native entry point.
  SILValue nativeFn = emitGlobalFunctionRef(loc, native, nativeInfo);
  auto subs = F.getForwardingSubstitutions();
  auto substTy = nativeFn.getType().castTo<SILFunctionType>()
    ->substGenericArgs(SGM.M, SGM.M.getSwiftModule(), subs);
  SILValue result = B.createApply(loc, nativeFn,
                                  SILType::getPrimitiveObjectType(substTy),
                                  swiftResultTy.getSILType(), subs, args,
                                  thunk.isTransparent());

  scope.pop();
  
  emitObjCReturnValue(*this, loc, result, nativeInfo.LoweredType.getResult(),
                      objcResultTy);
}

void SILGenFunction::emitObjCDestructor(SILDeclRef dtor) {
  auto dd = cast<DestructorDecl>(dtor.getDecl());
  auto cd = cast<ClassDecl>(dd->getDeclContext());
  MagicFunctionName = DeclName(SGM.M.getASTContext().getIdentifier("deinit"));

  RegularLocation loc(dd);
  if (dd->isImplicit())
    loc.markAutoGenerated();

  SILValue selfValue = emitSelfDecl(dd->getImplicitSelfDecl());

  // Create a basic block to jump to for the implicit destruction behavior
  // of releasing the elements and calling the superclass destructor.
  // We won't actually emit the block until we finish with the destructor body.
  prepareEpilog(Type(), CleanupLocation::getCleanupLocation(loc));

  // Emit the destructor body.
  emitStmt(dd->getBody());

  Optional<SILValue> maybeReturnValue;
  SILLocation returnLoc(loc);
  std::tie(maybeReturnValue, returnLoc) = emitEpilogBB(loc);

  if (!maybeReturnValue)
    return;

  auto cleanupLoc = CleanupLocation::getCleanupLocation(loc);

  // Note: the ivar destroyer is responsible for destroying the
  // instance variables before the object is actually deallocated.

  // Form a reference to the superclass -dealloc.
  Type superclassTy = ArchetypeBuilder::mapTypeIntoContext(dd,
                                                           cd->getSuperclass());
  assert(superclassTy && "Emitting Objective-C -dealloc without superclass?");
  ClassDecl *superclass = superclassTy->getClassOrBoundGenericClass();
  auto superclassDtorDecl = superclass->getDestructor();
  SILDeclRef superclassDtor(superclassDtorDecl,
                            SILDeclRef::Kind::Deallocator,
                            SILDeclRef::ConstructAtBestResilienceExpansion,
                            SILDeclRef::ConstructAtNaturalUncurryLevel,
                            /*isForeign=*/true);
  auto superclassDtorType = SGM.getConstantType(superclassDtor);
  SILValue superclassDtorValue = B.createSuperMethod(
                                   cleanupLoc, selfValue, superclassDtor,
                                   superclassDtorType);

  // Call the superclass's -dealloc.
  SILType superclassSILTy = getLoweredLoadableType(superclassTy);
  SILValue superSelf = B.createUpcast(cleanupLoc, selfValue, superclassSILTy);
  ArrayRef<Substitution> subs
    = superclassTy->gatherAllSubstitutions(SGM.M.getSwiftModule(), nullptr);
  auto substDtorType = superclassDtorType.castTo<SILFunctionType>()
    ->substGenericArgs(SGM.M, SGM.M.getSwiftModule(), subs);
  B.createApply(cleanupLoc, superclassDtorValue,
                SILType::getPrimitiveObjectType(substDtorType),
                substDtorType->getResult().getSILType(),
                subs, superSelf);

  // Return.
  B.createReturn(returnLoc, emitEmptyTuple(cleanupLoc));
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
  void visitVarPattern(VarPattern *P) {
    return visit(P->getSubPattern());
  }
  void visitTuplePattern(TuplePattern *P) {
    for (auto &elt : P->getFields())
      visit(elt.getPattern());
  }
  void visitAnyPattern(AnyPattern *P) {}

  // When we see a variable binding, emit its global accessor.
  void visitNamedPattern(NamedPattern *P) {
    SGM.emitGlobalAccessor(P->getDecl(), OnceToken, OnceFunc);
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
void SILGenModule::emitGlobalInitialization(PatternBindingDecl *pd,
                                            unsigned pbdEntry) {
  // Generic and dynamic static properties require lazy initialization, which
  // isn't implemented yet.
  if (pd->isStatic()) {
    auto theType = pd->getDeclContext()->getDeclaredTypeInContext();
    assert(!theType->is<BoundGenericType>()
           && "generic static properties not implemented");
    (void)theType;
  }

  // Emit the lazy initialization token for the initialization expression.
  auto counter = anonymousSymbolCounter++;

  // Pick one variable of the pattern. Usually it's only one variable, but it
  // can also be something like: var (a, b) = ...
  Pattern *pattern = pd->getPattern(pbdEntry);
  VarDecl *varDecl = nullptr;
  pattern->forEachVariable([&](VarDecl *D) {
    varDecl = D;
  });
  assert(varDecl);

  llvm::SmallString<20> onceTokenBuffer;
  llvm::raw_svector_ostream onceTokenStream(onceTokenBuffer);
  Mangler tokenMangler(onceTokenStream);
  tokenMangler.mangleGlobalInit(varDecl, counter, false);

  auto onceTy = BuiltinIntegerType::getWordType(M.getASTContext());
  auto onceSILTy
    = SILType::getPrimitiveObjectType(onceTy->getCanonicalType());

  // TODO: include the module in the onceToken's name mangling.
  // Then we can make it fragile.
  auto onceToken = SILGlobalVariable::create(M, SILLinkage::Private,
                                             makeModuleFragile,
                                             onceTokenStream.str(), onceSILTy);
  onceToken->setDeclaration(false);

  // Emit the initialization code into a function.
  llvm::SmallString<20> onceFuncBuffer;
  llvm::raw_svector_ostream onceFuncStream(onceFuncBuffer);
  Mangler funcMangler(onceFuncStream);
  funcMangler.mangleGlobalInit(varDecl, counter, true);

  SILFunction *onceFunc = emitLazyGlobalInitializer(onceFuncStream.str(), pd,
                                                    pbdEntry);

  // Generate accessor functions for all of the declared variables, which
  // Builtin.once the lazy global initializer we just generated then return
  // the address of the individual variable.
  GenGlobalAccessors(*this, onceToken, onceFunc)
    .visit(pd->getPattern(pbdEntry));
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

  SILGenConformance(SILGenModule &SGM, ProtocolConformance *C)
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
      SGM.getWitnessTable(conformance);
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
  conformance = conformance->getRootNormalConformance();

  // If we've already emitted this witness table, return it.
  auto found = emittedWitnessTables.find(conformance);
  if (found != emittedWitnessTables.end())
    return found->second;

  SILWitnessTable *table = SILGenConformance(*this, conformance).emit();
  emittedWitnessTables.insert({conformance, table});
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
  CanType origLoweredTy;
  if (auto origFTy = dyn_cast<AnyFunctionType>(origRequirementTy.getAsType()))
    origLoweredTy = M.Types.getLoweredASTFunctionType(origFTy,
                                                      uncurryLevel,
                                                      None);
  else
    origLoweredTy = origRequirementTy.getAsType();
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

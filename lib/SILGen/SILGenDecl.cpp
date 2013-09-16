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
#include "OwnershipConventions.h"
#include "RValue.h"
#include "Scope.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/AST/AST.h"
#include "swift/Basic/Fallthrough.h"
#include <iterator>
using namespace swift;
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
  class TupleElementInitialization : public SingleInitializationBase {
  public:
    SILValue elementAddr;
    
    TupleElementInitialization(SILValue addr)
      : elementAddr(addr)
    {}
    
    SILValue getAddressOrNull() override { return elementAddr; }
    
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
  case Kind::AddressBinding:
    llvm_unreachable("cannot destructure an address binding initialization");
  }
}

namespace {
  class CleanupClosureConstant : public Cleanup {
    SILValue closure;
  public:
    CleanupClosureConstant(SILValue closure) : closure(closure) {}
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      gen.B.createStrongRelease(l, closure);
    }
  };
}

ArrayRef<Substitution> SILGenFunction::getForwardingSubstitutions() {
  if (auto outerPFT = F.getLoweredType().getAs<PolymorphicFunctionType>()) {
    return buildForwardingSubstitutions(outerPFT->getAllArchetypes());
  }
  return {};
}

void SILGenFunction::visitFuncDecl(FuncDecl *fd) {
  // Generate the local function body.
  SGM.emitFunction(fd);
  
  // If there are captures, build the local closure value for the function and
  // store it as a local constant.
  if (fd->getCaptureInfo().hasLocalCaptures()) {
    SILValue closure = emitClosureForCapturingExpr(fd, SILDeclRef(fd),
                                                   getForwardingSubstitutions(),
                                                   fd)
      .forward(*this);
    Cleanups.pushCleanup<CleanupClosureConstant>(closure);
    LocalConstants[SILDeclRef(fd)] = closure;
  }
}

namespace {

/// An Initialization of a tuple pattern, such as "var (a,b)".
class TupleInitialization : public Initialization {
public:
  /// The sub-Initializations aggregated by this tuple initialization.
  /// The TupleInitialization object takes ownership of Initializations pushed
  /// here.
  SmallVector<InitializationPtr, 4> subInitializations;

  TupleInitialization() : Initialization(Initialization::Kind::Tuple)
  {}
  
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
class CleanupLocalVariable : public Cleanup {
  VarDecl *var;
public:
  CleanupLocalVariable(VarDecl *var)
    : var(var) {}
  
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.destroyLocalVariable(l, var);
  }
};
  
/// Cleanup to destroy an address-only argument. We destroy the value without
/// deallocating the storage.
class CleanupAddressOnlyArgument : public Cleanup {
  SILValue addr;
public:
  CleanupAddressOnlyArgument(SILValue addr)
    : addr(addr) {}
  
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.createDestroyAddr(l, addr);
  }
};

/// An initialization of a local variable.
class LocalVariableInitialization : public SingleInitializationBase {
  // FIXME: We should install a deallocation cleanup then deactivate it and
  // activate a destroying cleanup when the value is initialized.
  
  /// The local variable decl being initialized.
  VarDecl *var;
  SILGenFunction &gen;
  
  bool didFinish;
public:
  /// Sets up an initialization for the allocated box. This pushes a
  /// CleanupUninitializedBox cleanup that will be replaced when
  /// initialization is completed.
  LocalVariableInitialization(VarDecl *var, SILGenFunction &gen)
    : var(var), gen(gen), didFinish(false)
  {
    gen.Cleanups.pushCleanup<CleanupLocalVariable>(var);
  }
  
  ~LocalVariableInitialization() override {
    assert(didFinish && "did not call VarInit::finishInitialization!");
  }
  
  SILValue getAddressOrNull() override {
    assert(gen.VarLocs.count(var) && "did not emit var?!");
    return gen.VarLocs[var].address;
  }
  
  void finishInitialization(SILGenFunction &gen) override {
    assert(!didFinish &&
           "called LocalVariableInitialization::finishInitialization twice!");
    // FIXME: deactivate the deallocating cleanup and activate the
    // destroying one.
    didFinish = true;
  }
};
  
/// An initialization for a global variable.
class GlobalInitialization : public SingleInitializationBase {
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
  
/// Initialize the value by binding the variable address to the
/// incoming address.
class AddressBindingInitialization : public Initialization {
  /// The VarDecl for the byref symbol.
  VarDecl *vd;
public:
  AddressBindingInitialization(VarDecl *vd)
    : Initialization(Initialization::Kind::AddressBinding),
      vd(vd)
  {}
  
  SILValue getAddressOrNull() override {
    llvm_unreachable("byref argument does not have an address to store to");
  }
  ArrayRef<InitializationPtr> getSubInitializations() override {
    return {};
  }

  void bindAddress(SILValue address, SILGenFunction &gen) override {
    // Use the input address as the var's address.
    assert(address.getType().isAddress() &&
           "binding a variable to a non-address argument?!");
    gen.VarLocs[vd] = {SILValue(), address};
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
    
    // If this is a property, we don't need to do anything here. We'll generate
    // the getter and setter when we see their FuncDecls.
    if (vd->isProperty())
      return InitializationPtr(new BlackHoleInitialization());

    // If this is a [byref] argument, bind the argument lvalue as our
    // address.
    if (vd->getType()->is<LValueType>())
      return InitializationPtr(new AddressBindingInitialization(vd));

    // If this is a global variable, initialize it without allocations or
    // cleanups.
    if (!vd->getDeclContext()->isLocalContext()) {
      SILValue addr = Gen.B.createGlobalAddr(vd, vd,
                          Gen.getLoweredType(vd->getType()).getAddressType());
      
      // In a top level context, all global variables must be initialized.
      addr = Gen.B.createMarkUninitialized(vd, addr);
      
      Gen.VarLocs[vd] = {SILValue(), addr};
      return InitializationPtr(new GlobalInitialization(addr));
    }

    // Otherwise, this is a normal variable initialization.
    return Gen.emitLocalVariableWithCleanup(vd);
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
  // the initialization. Otherwise, emit 'initialize_var' placeholder
  // instructions.
  if (D->getInit()) {
    FullExpr Scope(Cleanups, CleanupLocation(D->getInit()));
    emitExprInto(D->getInit(), initialization.get());
  } else {
    initialization->finishInitialization(*this);
  }
}

/// Enter a cleanup to deallocate the given location.
CleanupsDepth SILGenFunction::enterDeallocStackCleanup(SILLocation loc,
                                                       SILValue temp) {
  assert(temp.getType().isLocalStorage() &&
         "must deallocate container operand, not address operand!");
  Cleanups.pushCleanup<DeallocStack>(loc, temp);
  return Cleanups.getCleanupsDepth();
}

namespace {

/// ArgumentInitVisitor - A visitor for traversing a pattern, creating
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
      I->bindAddress(arg, gen);
      // If this is an address-only non-byref argument, we take ownership
      // of the referenced value.
      if (!ty->is<LValueType>())
        gen.Cleanups.pushCleanup<CleanupAddressOnlyArgument>(arg);
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
    SILValue arg = makeArgument(ty, parent, loc);
    storeArgumentInto(ty, arg, loc, I);
    return arg;
  }
    
  // Paren & Typed patterns are no-ops. Just look through them.
  SILValue visitParenPattern(ParenPattern *P, Initialization *I) {
    return visit(P->getSubPattern(), I);
  }
  SILValue visitTypedPattern(TypedPattern *P, Initialization *I) {
    // FIXME: work around a bug in visiting the "self" argument of methods
    if (NamedPattern *np = dyn_cast<NamedPattern>(P->getSubPattern()))
      return makeArgumentInto(P->getType(), f.begin(),
                              np->getDecl(), I);
    else
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
        llvm_unreachable("empty tuple pattern with byref initializer?!");
        
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
    SILValue arg = makeArgument(P->getType(), f.begin(), P);
    lowering.emitDestroyRValue(gen.B, P, arg);
    return arg;
  }

  SILValue visitNamedPattern(NamedPattern *P, Initialization *I) {
    return makeArgumentInto(P->getType(), f.begin(),
                            P->getDecl(), I);
  }
  
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) \
  SILValue visit##Id##Pattern(Id##Pattern *, Initialization *) { \
    llvm_unreachable("pattern not valid in argument binding"); \
  }
#include "swift/AST/PatternNodes.def"

};

class CleanupCaptureBox : public Cleanup {
  SILValue box;
public:
  CleanupCaptureBox(SILValue box) : box(box) {}
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.createStrongRelease(l, box);
  }
};
  
class CleanupCaptureValue : public Cleanup {
  SILValue v;
public:
  CleanupCaptureValue(SILValue v) : v(v) {}
  void emit(SILGenFunction &gen, CleanupLocation l) override {
    gen.B.emitReleaseValue(l, v);
  }
};
  
static void emitCaptureArguments(SILGenFunction &gen, ValueDecl *capture) {
  ASTContext &c = capture->getASTContext();
  switch (getDeclCaptureKind(capture)) {
  case CaptureKind::Box: {
    // LValues are captured as two arguments: a retained ObjectPointer that owns
    // the captured value, and the address of the value itself.
    SILType ty = gen.getLoweredType(capture->getType()).getAddressType();
    SILValue box = new (gen.SGM.M) SILArgument(SILType::getObjectPointerType(c),
                                               gen.F.begin());
    SILValue addr = new (gen.SGM.M) SILArgument(ty, gen.F.begin());
    gen.VarLocs[capture] = {box, addr};
    gen.Cleanups.pushCleanup<CleanupCaptureBox>(box);
    break;
  }
  case CaptureKind::Byref: {
    // Byref captures are non-escaping, so it's sufficient to capture only the
    // address.
    SILType ty = gen.getLoweredType(capture->getType()).getAddressType();
    SILValue addr = new (gen.SGM.M) SILArgument(ty, gen.F.begin());
    gen.VarLocs[capture] = {SILValue(), addr};
    break;
  }
  case CaptureKind::Constant: {
    // Constants are captured by value.
    assert(!capture->getType()->is<LValueType>() &&
           "capturing byref by value?!");
    const TypeLowering &ti = gen.getTypeLowering(capture->getType());
    SILValue value = new (gen.SGM.M) SILArgument(ti.getLoweredType(),
                                             gen.F.begin());
    gen.LocalConstants[SILDeclRef(capture)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    break;
  }
  case CaptureKind::GetterSetter: {
    // Capture the setter and getter closures by value.
    Type setTy = gen.SGM.Types.getPropertyType(SILDeclRef::Kind::Setter,
                                               capture->getType());
    SILType lSetTy = gen.getLoweredType(setTy);
    SILValue value = new (gen.SGM.M) SILArgument(lSetTy, gen.F.begin());
    gen.LocalConstants[SILDeclRef(capture, SILDeclRef::Kind::Setter)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    SWIFT_FALLTHROUGH;
  }
  case CaptureKind::Getter: {
    // Capture the getter closure by value.
    Type getTy = gen.SGM.Types.getPropertyType(SILDeclRef::Kind::Getter,
                                               capture->getType());
    SILType lGetTy = gen.getLoweredType(getTy);
    SILValue value = new (gen.SGM.M) SILArgument(lGetTy, gen.F.begin());
    gen.LocalConstants[SILDeclRef(capture, SILDeclRef::Kind::Getter)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    break;
  }
  }
}
  
} // end anonymous namespace

void SILGenFunction::emitProlog(AnyFunctionRef TheClosure,
                                ArrayRef<Pattern *> paramPatterns,
                                Type resultType) {
  emitProlog(paramPatterns, resultType);

  // Emit the capture argument variables. These are placed last because they
  // become the first curry level of the SIL function.
  SmallVector<ValueDecl*, 4> LocalCaptures;
  TheClosure.getCaptureInfo().getLocalCaptures(LocalCaptures);
  for (auto capture : LocalCaptures)
    emitCaptureArguments(*this, capture);
}

void SILGenFunction::emitProlog(ArrayRef<Pattern *> paramPatterns,
                                Type resultType) {
  // If the return type is address-only, emit the indirect return argument.
  const TypeLowering &returnTI = getTypeLowering(resultType);
  if (returnTI.isReturnedIndirectly()) {
    IndirectReturnAddress = new (SGM.M) SILArgument(returnTI.getLoweredType(),
                                                    F.begin());
  }
  
  auto emitPattern = [&](Pattern *p) {
    // Allocate the local mutable argument storage and set up an Initialization.
    InitializationPtr argInit
      = InitializationForPattern(*this, InitializationForPattern::Argument)
        .visit(p);
    // Add the SILArguments and use them to initialize the local argument
    // values.
    ArgumentInitVisitor(*this, F).visit(p, argInit.get());
    
  };
  
  // Emit the argument variables in calling convention order.
  for (Pattern *p : reversed(paramPatterns))
    emitPattern(p);
}

namespace {
  class CleanupDestructorSelf : public Cleanup {
    VarDecl *selfDecl;
  public:
    CleanupDestructorSelf(VarDecl *selfDecl) : selfDecl(selfDecl) {
    }
    
    void emit(SILGenFunction &gen, CleanupLocation l) override {
      // 'self' is passed in at +0 (and will be deallocated when we return),
      // so don't release the value, only deallocate the variable.
      gen.deallocateUninitializedLocalVariable(l, selfDecl);
    }
  };
} // end anonymous namespace

SILValue SILGenFunction::emitDestructorProlog(ClassDecl *CD,
                                              DestructorDecl *DD) {
  // Emit the implicit 'self' argument.
  VarDecl *selfDecl = DD ? DD->getImplicitSelfDecl() : nullptr;
  assert((!selfDecl || selfDecl->getType()->hasReferenceSemantics()) &&
         "destructor's implicit this is a value type?!");

  SILType selfType = getLoweredLoadableType(CD->getDeclaredTypeInContext());
  assert((!selfDecl || getLoweredLoadableType(selfDecl->getType()) == selfType)
         && "decl type doesn't match destructor's implicit this type");
  
  SILValue selfValue = new (SGM.M) SILArgument(selfType, F.begin());
  
  if (DD) {
    // Make a local variable for 'self'.
    emitLocalVariable(selfDecl);
    SILValue selfAddr = VarLocs[selfDecl].address;
    B.createStore(DD, selfValue, selfAddr);
    Cleanups.pushCleanup<CleanupDestructorSelf>(selfDecl);
  }
  return selfValue;
}

void SILGenFunction::prepareEpilog(Type resultType, CleanupLocation CleanupL) {
  auto *epilogBB = new (F.getModule()) SILBasicBlock(&F);
  
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

bool SILGenModule::requiresObjCMethodEntryPoint(FuncDecl *method) {
  // Property accessors should be generated alongside the property.
  if (method->isGetterOrSetter())
    return false;
    
  // We don't export generic methods or subclasses to Objective-C yet.
  if (method->getType()->is<PolymorphicFunctionType>()
      || method->getType()->castTo<AnyFunctionType>()
          ->getResult()->is<PolymorphicFunctionType>()
      || (method->getDeclContext()->getDeclaredTypeInContext()
          && method->getDeclContext()->getDeclaredTypeInContext()
            ->is<BoundGenericType>()))
    return false;
    
  if (method->isObjC() || method->getAttrs().isIBAction())
    return true;
  if (auto override = method->getOverriddenDecl())
    return requiresObjCMethodEntryPoint(override);
  return false;
}

bool SILGenModule::requiresObjCPropertyEntryPoints(VarDecl *property) {
  // We don't export generic methods or subclasses to IRGen yet.
  if (property->getDeclContext()->getDeclaredTypeInContext()
          ->is<BoundGenericType>())
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

bool SILGenModule::requiresObjCDispatch(ValueDecl *vd) {
  if (vd->hasClangNode())
    return true;
  if (auto *fd = dyn_cast<FuncDecl>(vd))
    return requiresObjCMethodEntryPoint(fd);
  if (auto *pd = dyn_cast<VarDecl>(vd))
    return requiresObjCPropertyEntryPoints(pd);
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

/// An ASTVisitor for generating SIL from method declarations
/// inside nominal types.
class SILGenType : public Lowering::ASTVisitor<SILGenType> {
public:
  SILGenModule &SGM;
  NominalTypeDecl *theType;
  DestructorDecl *explicitDestructor;
  
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
    for (Decl *member : theType->getMembers())
      visit(member);
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
  }
  void visitDestructorDecl(DestructorDecl *dd) {
    // Save the destructor decl so we can use it to generate the destructor later.
    assert(!explicitDestructor && "more than one destructor decl in type?!");
    explicitDestructor = dd;
  }
  
  void visitUnionElementDecl(UnionElementDecl *ued) {
    assert(isa<UnionDecl>(theType));
    SGM.emitUnionConstructor(ued);
  }
  
  // no-op. We don't deal with the layout of types here.
  void visitPatternBindingDecl(PatternBindingDecl *) {}
  
  void visitVarDecl(VarDecl *vd) {
    // FIXME: Default implementations in protocols.
    if (SGM.requiresObjCPropertyEntryPoints(vd) &&
        !isa<ProtocolDecl>(vd->getDeclContext()))
      SGM.emitObjCPropertyMethodThunks(vd);
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
  case DeclKind::Struct:
  case DeclKind::Protocol: {
    // Nothing to do in SILGen for external types.
    break;
  }
      
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::UnionElement:
  case DeclKind::Union:
  case DeclKind::Class:
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
/// inside type extensions.
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
};

void SILGenModule::visitExtensionDecl(ExtensionDecl *ed) {
  SILGenExtension(*this).emitExtension(ed);
}

void SILGenFunction::emitLocalVariable(VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(!vd->isProperty() &&
         "can't emit a physical local var for a property");

  SILType lType = getLoweredType(vd->getType());

  // The variable may have its lifetime extended by a closure, heap-allocate it
  // using a box.

  AllocBoxInst *allocBox = B.createAllocBox(vd, lType);
  auto box = SILValue(allocBox, 0);
  auto addr = SILValue(allocBox, 1);
  
  /// Remember that this is the memory location that we're emitting the
  /// decl to.
  VarLocs[vd] = {box, addr};
}

/// Create a LocalVariableInitialization for the uninitialized var.
InitializationPtr SILGenFunction::emitLocalVariableWithCleanup(VarDecl *vd) {
  emitLocalVariable(vd);
  return InitializationPtr(new LocalVariableInitialization(vd, *this));
}

void SILGenFunction::destroyLocalVariable(SILLocation silLoc, VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(!vd->isProperty() &&
         "can't emit a physical local var for a property");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");
  
  auto &loc = VarLocs[vd];
  
  // For a heap variable, the box is responsible for the value. We just need
  // to give up our retain count on it.
  assert(loc.box && "captured var should have been given a box");
  B.createStrongRelease(silLoc, loc.box);
}

void SILGenFunction::deallocateUninitializedLocalVariable(SILLocation silLoc,
                                                          VarDecl *vd) {
  assert(vd->getDeclContext()->isLocalContext() &&
         "can't emit a local var for a non-local var decl");
  assert(!vd->isProperty() &&
         "can't emit a physical local var for a property");

  assert(VarLocs.count(vd) && "var decl wasn't emitted?!");

  auto &loc = VarLocs[vd];
  assert(loc.box && "captured var should have been given a box");
  B.createDeallocBox(silLoc, loc.address.getType().getObjectType(), loc.box);
}

//===----------------------------------------------------------------------===//
// ObjC method thunks
//===----------------------------------------------------------------------===//

static SILValue emitBridgeObjCReturnValue(SILGenFunction &gen,
                                          SILLocation loc,
                                          SILValue result,
                                          SILType resultTy) {
  Scope scope(gen.Cleanups, CleanupLocation::getCleanupLocation(loc));
  
  ManagedValue native = gen.emitManagedRValueWithCleanup(result);
  ManagedValue bridged = gen.emitNativeToBridgedValue(loc, native,
                                                      AbstractCC::ObjCMethod,
                                                      resultTy.getSwiftType());
  return bridged.forward(gen);
}

/// Take a return value at +1 and adjust it to the retain count expected by
/// the given ownership conventions.
static void emitObjCReturnValue(SILGenFunction &gen,
                                SILLocation loc,
                                SILValue result,
                                SILType resultTy,
                                OwnershipConventions const &ownership) {
  // Bridge the result.
  result = emitBridgeObjCReturnValue(gen, loc, result, resultTy);
  
  // Autorelease the bridged result if necessary.
  switch (ownership.getReturn()) {
  case OwnershipConventions::Return::Autoreleased:
    gen.B.createAutoreleaseReturn(loc, result);
    return;
  case OwnershipConventions::Return::Unretained:
    gen.B.emitReleaseValue(loc, result);
    SWIFT_FALLTHROUGH;
  case OwnershipConventions::Return::Retained:
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
  
  lowering.emitRetain(gen.B, loc, arg);
  return arg;
}

/// Bridge argument types and adjust retain count conventions for an ObjC thunk.
static OwnershipConventions emitObjCThunkArguments(SILGenFunction &gen,
                                             SILDeclRef thunk,
                                             SmallVectorImpl<SILValue> &args) {
  SILType objcTy = gen.SGM.getConstantType(thunk),
          swiftTy = gen.SGM.getConstantType(thunk.asObjC(false));
  
  SILFunctionTypeInfo
    *objcInfo = objcTy.getFunctionTypeInfo(gen.SGM.M),
    *swiftInfo = swiftTy.getFunctionTypeInfo(gen.SGM.M);

  RegularLocation Loc(thunk.getDecl());
  Loc.markAutoGenerated();

  SmallVector<ManagedValue, 8> bridgedArgs;
  bridgedArgs.reserve(objcInfo->getInputTypes().size());
  
  // Emit the indirect return argument, if any.
  if (objcInfo->hasIndirectReturn()) {
    auto arg = new (gen.F.getModule())
                 SILArgument(objcInfo->getIndirectReturnType(), gen.F.begin());
    bridgedArgs.push_back(ManagedValue(arg, ManagedValue::Unmanaged));
  }
  
  // Emit the other arguments, taking ownership of arguments if necessary.
  auto ownership = OwnershipConventions::get(gen, thunk, objcTy);
  ArrayRef<SILType> inputs
    = objcInfo->getInputTypesWithoutIndirectReturnType();
  assert(!inputs.empty());
  for (unsigned i = 0, e = inputs.size(); i < e; ++i) {
    SILValue arg = new(gen.F.getModule()) SILArgument(inputs[i], gen.F.begin());

    // Convert the argument to +1 if necessary.
    if (!ownership.isArgumentConsumed(i)) {
      arg = emitObjCUnconsumedArgument(gen, Loc, arg);
    }

    auto managedArg = gen.emitManagedRValueWithCleanup(arg);

    bridgedArgs.push_back(managedArg);
  }

  assert(bridgedArgs.size() == objcInfo->getInputTypes().size() &&
         "objc inputs don't match number of arguments?!");
  assert(bridgedArgs.size() == swiftInfo->getInputTypes().size() &&
         "swift inputs don't match number of arguments?!");

  // Bridge the input types.
  Scope scope(gen.Cleanups, CleanupLocation::getCleanupLocation(Loc));
  for (unsigned i = 0, size = bridgedArgs.size(); i < size; ++i) {
    ManagedValue native = gen.emitBridgedToNativeValue(Loc,
                               bridgedArgs[i],
                               AbstractCC::ObjCMethod,
                               swiftInfo->getInputTypes()[i].getSwiftType());
    args.push_back(native.forward(gen));
  }
  
  return ownership;
}

void SILGenFunction::emitObjCMethodThunk(SILDeclRef thunk) {
  SILDeclRef native = thunk.asObjC(false);
  
  SmallVector<SILValue, 4> args;  
  auto ownership = emitObjCThunkArguments(*this, thunk, args);
  SILType swiftResultTy
    = SGM.getConstantType(native).getFunctionTypeInfo(SGM.M)->getResultType();
  SILType objcResultTy
    = SGM.getConstantType(thunk).getFunctionTypeInfo(SGM.M)->getResultType();
  
  // Call the native entry point.
  RegularLocation loc(thunk.getDecl());
  loc.markAutoGenerated();

  SILValue nativeFn = emitGlobalFunctionRef(loc, native);
  SILValue result = B.createApply(loc, nativeFn, swiftResultTy, args);
  emitObjCReturnValue(*this, loc, result, objcResultTy, ownership);
}

void SILGenFunction::emitObjCPropertyGetter(SILDeclRef getter) {
  SmallVector<SILValue, 2> args;
  auto ownership = emitObjCThunkArguments(*this, getter, args);
  SILDeclRef native = getter.asObjC(false);
  SILType swiftResultTy
    = SGM.getConstantType(native).getFunctionTypeInfo(SGM.M)->getResultType();
  SILType objcResultTy
    = SGM.getConstantType(getter).getFunctionTypeInfo(SGM.M)->getResultType();

  RegularLocation loc(getter.getDecl());
  loc.markAutoGenerated();

  // If the property is logical, forward to the native getter.
  auto *var = cast<VarDecl>(getter.getDecl());
  if (var->isProperty()) {
    SILValue nativeFn = emitGlobalFunctionRef(loc, native);
    SILValue result = B.createApply(loc, nativeFn, swiftResultTy, args);
    emitObjCReturnValue(*this, loc, result, objcResultTy, ownership);
    return;
  }

  // If the native property is physical, load it.
  SILFunctionTypeInfo *info
    = SGM.getConstantType(getter).getFunctionTypeInfo(SGM.M);
  SILValue indirectReturn, selfValue;
  if (info->hasIndirectReturn()) {
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
    (fieldType == swiftResultTy.getSwiftRValueType()
       ? fieldLowering : getTypeLowering(swiftResultTy));

  SILValue fieldAddr = B.createRefElementAddr(loc, selfValue, var,
                             fieldLowering.getLoweredType().getAddressType());
  if (indirectReturn) {
    assert(ownership.getReturn() == OwnershipConventions::Return::Unretained
           && "any address-only type should appear Unretained to ObjC");
    // This is basically returning +1, but there's no obvious
    // alternative, and there really isn't an ObjC convention for
    // transferring ownership in aggregates.
    emitSemanticLoadInto(loc, fieldAddr, fieldLowering,
                         indirectReturn, resultLowering,
                         IsNotTake, IsInitialization);
    B.createStrongRelease(loc, selfValue);
    B.createReturn(loc, emitEmptyTuple(loc));
    return;
  }

  // Bridge the result.
  SILValue result = emitSemanticLoad(loc, fieldAddr, fieldLowering,
                                     resultLowering, IsNotTake);

  // FIXME: This should have artificial location.
  B.createStrongRelease(loc, selfValue);
  return emitObjCReturnValue(*this, loc, result, objcResultTy,
                             ownership);
}

void SILGenFunction::emitObjCPropertySetter(SILDeclRef setter) {
  SmallVector<SILValue, 2> args;
  auto ownership = emitObjCThunkArguments(*this, setter, args);
  SILDeclRef native = setter.asObjC(false);

  RegularLocation loc(setter.getDecl());
  loc.markAutoGenerated();

  // If the native property is logical, store to the native setter.
  auto *var = cast<VarDecl>(setter.getDecl());
  if (var->isProperty()) {
    SILValue nativeFn = emitGlobalFunctionRef(loc, native);
    SILValue result = B.createApply(loc, nativeFn,
                                    SGM.Types.getEmptyTupleType(),
                                    args);
    // Result should be void.
    B.createReturn(loc, result);
    return;
  }

  assert(args.size() == 2 && "wrong number of args for setter");
  SILValue selfValue = args[1];
  SILValue setValue = args[0];
  
  // If the native property is physical, store to it.
  auto &varTI = getTypeLowering(var->getType());
  SILValue addr = B.createRefElementAddr(loc, selfValue, var,
                                 varTI.getLoweredType().getAddressType());
  emitSemanticStore(loc, setValue, addr, varTI, IsNotInitialization);
  
  // FIXME: This should have artificial location.
  B.createStrongRelease(loc, selfValue);
  B.createReturn(loc, emitEmptyTuple(loc));
}

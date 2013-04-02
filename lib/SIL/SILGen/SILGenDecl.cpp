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
#include "llvm/ADT/OwningPtr.h"
#include "swift/SIL/BBArgument.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/AST/AST.h"
#include <iterator>
using namespace swift;
using namespace Lowering;

#include "llvm/Support/raw_ostream.h"

void Initialization::_anchor() {}

namespace {
  /// A "null" initialization that indicates that any value being initialized into
  /// this initialization should be discarded. This represents AnyPatterns
  /// (that is, 'var (_)') that bind to values without storing them.
  class BlackHoleInitialization : public Initialization {
  public:
    BlackHoleInitialization(Type type)
      : Initialization(Initialization::Kind::Ignored, type)
    {}
    
    Value getAddressOrNull() override { return Value(); }
    ArrayRef<InitializationPtr> getSubInitializations() override {
      return {};
    }
    
    void defaultInitialize(SILGenFunction &gen) override {}
  };
  

  /// An Initialization subclass used to destructure tuple initializations.
  class TupleElementInitialization : public SingleInitializationBase {
  public:
    Value elementAddr;
    
    TupleElementInitialization(Value addr)
      : SingleInitializationBase(addr.getType().getSwiftRValueType()),
        elementAddr(addr)
    {}
    
    Value getAddressOrNull() override { return elementAddr; }
    
    void finishInitialization(SILGenFunction &gen) override {}
  };
}

ArrayRef<InitializationPtr> Initialization::getSubInitializations(
                                     SILGenFunction &gen,
                                     SmallVectorImpl<InitializationPtr> &buf) {
  TupleType *tupleTy = type->castTo<TupleType>();
  switch (kind) {
  case Kind::Tuple:
    return getSubInitializations();
  case Kind::Ignored: {
    // "Destructure" an ignored binding into multiple ignored bindings.
    for (auto &field : tupleTy->getFields()) {
      buf.push_back(InitializationPtr(
                                new BlackHoleInitialization(field.getType())));
    }
    return buf;
  }
  case Kind::SingleBuffer: {
    // Destructure the buffer into per-element buffers.
    Value baseAddr = getAddress();
    for (unsigned i = 0; i < tupleTy->getFields().size(); ++i) {
      auto &field = tupleTy->getFields()[i];
      Value fieldAddr = gen.B.createElementAddr(SILLocation(),
                                          baseAddr, i,
                                          gen.getLoweredType(field.getType()));
      buf.push_back(InitializationPtr(new TupleElementInitialization(fieldAddr)));
    }
  }
  case Kind::AddressBinding:
    llvm_unreachable("cannot destructure an address binding initialization");
  }
}

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
  SmallVector<InitializationPtr, 4> subInitializations;

  TupleInitialization(Type type)
    : Initialization(Initialization::Kind::Tuple, type)
  {}
  
  Value getAddressOrNull() override {
    if (subInitializations.size() == 1)
      return subInitializations[0]->getAddressOrNull();
    else
      return Value();
  }
  
  ArrayRef<InitializationPtr> getSubInitializations() override {
    return subInitializations;
  }
  
  void defaultInitialize(SILGenFunction &gen) override {
    for (auto &sub : subInitializations)
      sub->defaultInitialize(gen);
  }
};

/// Cleanup for an initialized boxed variable using a release instruction.
class CleanupBox : public Cleanup {
  AllocBoxInst *box;
public:
  CleanupBox(AllocBoxInst *box)
    : box(box) {}
  
  void emit(SILGenFunction &gen) override {
    gen.B.createRelease(SILLocation(), Value(box, 0));
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
    : SingleInitializationBase(box->getType(1).getSwiftRValueType()),
      box(box),
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
    // FIXME: deactivate the dealloc_ref cleanup and activate the release one.
    didFinish = true;
  }
};
  
/// An initialization for a global variable.
class GlobalInitialization : public SingleInitializationBase {
  /// The physical address of the global.
  Value address;
  
public:
  GlobalInitialization(Value address)
    : SingleInitializationBase(address.getType().getSwiftRValueType()),
      address(address)
  {}
  
  Value getAddressOrNull() override {
    return address;
  }
  
  void finishInitialization(SILGenFunction &) override {
    // Globals don't need to be cleaned up.
  }
};
  
/// An initialization for a byref argument.
class ByrefArgumentInitialization : public Initialization {
  /// The VarDecl for the byref symbol.
  VarDecl *vd;
public:
  ByrefArgumentInitialization(VarDecl *vd)
    : Initialization(Initialization::Kind::AddressBinding,
                     vd->getTypeOfReference()),
      vd(vd)
  {}
  
  Value getAddressOrNull() override {
    llvm_unreachable("byref argument does not have an address to store to");
  }
  ArrayRef<InitializationPtr> getSubInitializations() override { return {}; }

  void bindAddress(Value address, SILGenFunction &gen) override {
    // Use the input address as the var's address.
    assert(address.getType().isAddress() &&
           "binding a non-address to a byref argument?!");
    gen.VarLocs[vd] = {Value(), address};
  }

  void defaultInitialize(SILGenFunction &gen) override {}
};

/// InitializationForPattern - A visitor for traversing a pattern, generating
/// SIL code to allocate the declared variables, and generating an
/// Initialization representing the needed initializations. 
struct InitializationForPattern
  : public PatternVisitor<InitializationForPattern, InitializationPtr>
{
  SILGenFunction &Gen;
  InitializationForPattern(SILGenFunction &Gen) : Gen(Gen) {}
  
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
    return InitializationPtr(new BlackHoleInitialization(P->getType()));
  }
  
  // Bind to a named pattern by creating a memory location and initializing it
  // with the initial value.
  InitializationPtr visitNamedPattern(NamedPattern *P) {
    VarDecl *vd = P->getDecl();
    
    // If this is a property, we don't need to do anything here. We'll generate
    // the getter and setter when we see their FuncDecls.
    if (vd->isProperty())
      return InitializationPtr(new BlackHoleInitialization(vd->getType()));

    // If this is a [byref] argument, bind the argument lvalue as our
    // address.
    if (vd->getType()->is<LValueType>())
      return InitializationPtr(new ByrefArgumentInitialization(vd));

    // If this is a global variable, initialize it without allocations or
    // cleanups.
    if (!vd->getDeclContext()->isLocalContext()) {
      Gen.SGM.addGlobalVariable(vd);
      Value addr = Gen.emitGlobalConstantRef(vd,
                             SILConstant(vd, SILConstant::Kind::GlobalAddress));
      return InitializationPtr(new GlobalInitialization(addr));
    }
    
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
    return InitializationPtr(new BoxInitialization(allocBox, Gen));
  }
  
  // Bind a tuple pattern by aggregating the component variables into a
  // TupleInitialization.
  InitializationPtr visitTuplePattern(TuplePattern *P) {
    TupleInitialization *init = new TupleInitialization(P->getType());
    for (auto &elt : P->getFields())
      init->subInitializations.push_back(visit(elt.getPattern()));
    return InitializationPtr(init);
  }
};

} // end anonymous namespace


void SILGenFunction::visitPatternBindingDecl(PatternBindingDecl *D,
                                             SGFContext C) {
  // Allocate the variables and build up an Initialization over their
  // allocated storage.
  InitializationPtr initialization =
    InitializationForPattern(*this).visit(D->getPattern());
  
  // If an initial value expression was specified by the decl, emit it into
  // the initialization. Otherwise, emit 'initialize_var' placeholder
  // instructions.
  if (D->getInit()) {
    FullExpr Scope(Cleanups);
    emitExprInto(D->getInit(), initialization.get());
  } else {
    initialization->defaultInitialize(*this);
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
    assert(ty && "no type?!");
    // Destructure tuple arguments.
    if (TupleType *tupleTy = ty->getAs<TupleType>()) {
      SmallVector<Value, 4> tupleArgs;
      for (auto &field : tupleTy->getFields()) {
        tupleArgs.push_back(makeArgument(field.getType(), parent));
      }
      // FIXME: address-only tuples
      return initB.createTuple(SILLocation(), gen.getLoweredType(ty),
                               tupleArgs);
    }
    return new (f.getModule()) BBArgument(gen.getLoweredType(ty), parent);
  }
  
  void storeArgumentInto(Type ty, Value arg, SILLocation loc, Initialization *I)
  {
    assert(ty && "no type?!");
    if (I) {
      switch (I->kind) {
      case Initialization::Kind::AddressBinding:
        assert(ty->is<LValueType>() && "binding address to non-lvalue?!");
        I->bindAddress(arg, gen);
        break;

      case Initialization::Kind::SingleBuffer:
        if (arg.getType().isAddressOnly()) {
          initB.createCopyAddr(loc, arg, I->getAddress(),
                               /*isTake=*/ true,
                               /*isInitialize=*/ true);
        } else {
          initB.createStore(loc, arg, I->getAddress());
        }
        break;
      
      case Initialization::Kind::Ignored:
        break;
        
      case Initialization::Kind::Tuple:
        llvm_unreachable("tuple initializations should be destructured before "
                         "reaching here");
      }

      I->finishInitialization(gen);
    }
  }

  /// Create a BBArgument and store its value into the given Initialization,
  /// if not null.
  Value makeArgumentInto(Type ty, BasicBlock *parent,
                        SILLocation loc, Initialization *I) {
    assert(ty && "no type?!");
    Value arg = makeArgument(ty, parent);
    storeArgumentInto(ty, arg, loc, I);
    return arg;
  }
    
  // Paren & Typed patterns are no-ops. Just look through them.
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
        assert(I->getAddress().getType().getSwiftRValueType() == P->getType()
               && "empty tuple pattern with non-empty-tuple initializer?!");
        break;
      }

      return initB.createTuple(SILLocation(), gen.getLoweredType(P->getType()),
                               {});
    }
    
    // Destructure the initialization into per-element Initializations.
    SmallVector<InitializationPtr, 2> buf;
    ArrayRef<InitializationPtr> subInits = I->getSubInitializations(gen, buf);

    assert(P->getFields().size() == subInits.size() &&
           "TupleInitialization size does not match tuple pattern size!");
    for (size_t i = 0; i < P->getFields().size(); ++i)
      visit(P->getFields()[i].getPattern(), subInits[i].get());
    return Value();
  }

  Value visitAnyPattern(AnyPattern *P, Initialization *I) {
    // A value bound to _ is unused and can be immediately released.
    assert(I->kind == Initialization::Kind::Ignored &&
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
  switch (getDeclCaptureKind(capture)) {
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
    TypeLoweringInfo const &ti = gen.getTypeLoweringInfo(capture->getType());
    Value value = new (gen.SGM.M) BBArgument(ti.getLoweredType(),
                                             gen.F.begin());
    gen.LocalConstants[SILConstant(capture)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    break;
  }
  case CaptureKind::GetterSetter: {
    // Capture the setter and getter closures by value.
    Type setTy = gen.SGM.Types.getPropertyType(SILConstant::Kind::Setter,
                                               capture->getType());
    SILType lSetTy = gen.getLoweredType(setTy);
    Value value = new (gen.SGM.M) BBArgument(lSetTy, gen.F.begin());
    gen.LocalConstants[SILConstant(capture, SILConstant::Kind::Setter)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    [[clang::fallthrough]];
  }
  case CaptureKind::Getter: {
    // Capture the getter closure by value.
    Type getTy = gen.SGM.Types.getPropertyType(SILConstant::Kind::Getter,
                                               capture->getType());
    SILType lGetTy = gen.getLoweredType(getTy);
    Value value = new (gen.SGM.M) BBArgument(lGetTy, gen.F.begin());
    gen.LocalConstants[SILConstant(capture, SILConstant::Kind::Getter)] = value;
    gen.Cleanups.pushCleanup<CleanupCaptureValue>(value);
    break;
  }
  }
}
  
} // end anonymous namespace

void SILGenFunction::emitProlog(CapturingExpr *ce,
                                ArrayRef<Pattern*> paramPatterns,
                                Type resultType) {
  // Emit the capture argument variables. These are placed first because they
  // become the first curry level of the SIL function.
  for (auto capture : ce->getCaptures()) {
    makeCaptureBBArguments(*this, capture);
  }

  emitProlog(paramPatterns, resultType);
}

void SILGenFunction::emitProlog(ArrayRef<Pattern *> paramPatterns,
                                Type resultType) {
  // Emit the argument variables.
  for (size_t i = 0; i < paramPatterns.size(); ++i) {
    // Allocate the local mutable argument storage and set up an Initialization.
    InitializationPtr argInit =
                       InitializationForPattern(*this).visit(paramPatterns[i]);
    // Add the BBArguments and use them to initialize the local argument values.
    ArgumentInitVisitor(*this, F).visit(paramPatterns[i], argInit.get());
  }

  // If the return type is address-only, emit the indirect return argument.
  TypeLoweringInfo const &returnTI = getTypeLoweringInfo(resultType);
  if (returnTI.isAddressOnly()) {
    IndirectReturnAddress = new (SGM.M) BBArgument(returnTI.getLoweredType(),
                                                   F.begin());
  }
}

namespace {
  class CleanupDestructorThis : public Cleanup {
    Value thisAddr;
  public:
    CleanupDestructorThis(Value thisAddr) : thisAddr(thisAddr) {
    }
    
    void emit(SILGenFunction &gen) override {
      gen.B.createDeallocVar(SILLocation(), AllocKind::Stack, thisAddr);
    }
  };
} // end anonymous namespace

Value SILGenFunction::emitDestructorProlog(ClassDecl *CD,
                                          DestructorDecl *DD) {
  // Emit the implicit 'this' argument.
  VarDecl *thisDecl = DD ? DD->getImplicitThisDecl() : nullptr;
  assert((!thisDecl || thisDecl->getType()->hasReferenceSemantics()) &&
         "destructor's implicit this is a value type?!");
  
  SILType thisType = getLoweredLoadableType(CD->getDeclaredTypeInContext());
  assert((!thisDecl || getLoweredLoadableType(thisDecl->getType()) == thisType)
         && "decl type doesn't match destructor's implicit this type");
  
  Value thisValue = new (SGM.M) BBArgument(thisType, F.begin());
  
  if (DD) {
    // FIXME: Bump the retain count so that destruction doesn't fire
    // recursively while passing 'this' around in the destructor body.
    B.createRetain(DD, thisValue);
  
    // Materialize an lvalue for 'this' in the body's scope. It doesn't need a
    // full box because 'this' shouldn't be capturable out of a destructor
    // scope.
    Value thisAddr = B.createAllocVar(DD, AllocKind::Stack, thisType);
    Cleanups.pushCleanup<CleanupDestructorThis>(thisAddr);
    emitStore(DD, ManagedValue(thisValue, ManagedValue::Unmanaged), thisAddr);
    VarLocs[thisDecl] = {Value(), thisAddr};
  }
  return thisValue;
}

static void rrLoadableValueElement(SILGenFunction &gen, SILLocation loc,
                                   Value v,
                                   void (SILBuilder::*createRR)(SILLocation,
                                                                Value),
                                   ReferenceTypePath const &elt) {
  for (auto &comp : elt.path) {
    TypeLoweringInfo const &ti = gen.getTypeLoweringInfo(comp.type);
    assert(ti.isLoadable() && "fragile element is address-only?!");
    v = gen.B.createExtract(loc, v, comp.index, ti.getLoweredType());
  }
  (gen.B.*createRR)(loc, v);
}

static void rrLoadableValue(SILGenFunction &gen, SILLocation loc, Value v,
                            void (SILBuilder::*createRR)(SILLocation, Value),
                            ArrayRef<ReferenceTypePath> elts) {
  for (auto &elt : elts)
    rrLoadableValueElement(gen, loc, v, createRR, elt);
}

void SILGenFunction::emitRetainRValue(SILLocation loc, Value v) {
  assert(!v.getType().isAddress() &&
         "emitRetainRValue cannot retain an address");

  TypeLoweringInfo const &ti = getTypeLoweringInfo(v.getType().getSwiftRValueType());
  rrLoadableValue(*this, loc, v, &SILBuilder::createRetain,
                  ti.getReferenceTypeElements());
}

void SILGenFunction::emitReleaseRValue(SILLocation loc, Value v) {
  assert(!v.getType().isAddress() &&
         "emitReleaseRValue cannot release an address");

  TypeLoweringInfo const &ti = getTypeLoweringInfo(v.getType().getSwiftRValueType());
  rrLoadableValue(*this, loc, v, &SILBuilder::createRelease,
                  ti.getReferenceTypeElements());
}

void SILGenModule::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(*this, ntd).emitType();
}

void SILGenFunction::visitNominalTypeDecl(NominalTypeDecl *ntd, SGFContext C) {
  SILGenType(SGM, ntd).emitType();
}

void SILGenType::emitType() {
  for (Decl *member : theType->getMembers())
    visit(member);
}

void SILGenType::visitNominalTypeDecl(NominalTypeDecl *ntd) {
  SILGenType(SGM, ntd).emitType();
}

void SILGenType::visitFuncDecl(FuncDecl *fd) {
  SGM.emitFunction(fd, fd->getBody());
}

void SILGenType::visitDestructorDecl(DestructorDecl *dd) {
  // Save the destructor decl so we can use it to generate the destructor later.
  assert(!explicitDestructor && "more than one destructor decl in type?!");
  explicitDestructor = dd;
}

void SILGenType::visitConstructorDecl(ConstructorDecl *cd) {
  SGM.emitConstructor(cd);
}

void SILGenModule::emitExternalDefinition(Decl *d) {
  switch (d->getKind()) {
  case DeclKind::Func: {
    auto *fd = cast<FuncDecl>(d);
    emitFunction(fd, fd->getBody());
    break;
  }
  case DeclKind::Constructor: {
    emitConstructor(cast<ConstructorDecl>(d));
    break;
  }
  case DeclKind::Struct: {
    // Nothing to do in SILGen for external structs.
    break;
  }
      
  case DeclKind::Extension:
  case DeclKind::Protocol:
  case DeclKind::PatternBinding:
  case DeclKind::OneOfElement:
  case DeclKind::OneOf:
  case DeclKind::Class:
  case DeclKind::TopLevelCode:
  case DeclKind::TypeAlias:
  case DeclKind::Var:
  case DeclKind::Import:
  case DeclKind::Subscript:
  case DeclKind::Destructor:
    llvm_unreachable("Not a valid external definition for SILGen");
  }
}

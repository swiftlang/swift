//===--- GenFunc.cpp - Swift IR Generation for Function Types -------------===//
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
//
//  This file implements IR generation for function types in Swift.  This
//  includes creating the IR type as well as capturing variables and
//  performing calls.
//
//  Swift function types are always expanded as a struct containing
//  two opaque pointers.  The first pointer is to a function (should
//  this be a descriptor?) to which the second pointer is passed,
//  along with the formal arguments.  The function pointer is opaque
//  because the alternative would require infinite types to faithfully
//  represent, since aggregates containing function types can be
//  passed and returned by value, not necessary as first-class
//  aggregates.
//
//  There are several considerations for whether to pass the data
//  pointer as the first argument or the last:
//    - On CCs that pass anything in registers, dropping the last
//      argument is significantly more efficient than dropping the
//      first, and it's not that unlikely that the data might
//      be ignored.
//    - A specific instance of that:  we can use the address of a
//      global "data-free" function directly when taking an
//      address-of-function.
//    - Replacing a pointer argument with a different pointer is
//      quite efficient with pretty much any CC.
//    - Later arguments can be less efficient to access if they
//      actually get passed on the stack, but there's some leeway
//      with a decent CC.
//    - Passing the data pointer last inteferes with native variadic
//      arguments, but we probably don't ever want to use native
//      variadic arguments.
//  This works out to a pretty convincing argument for passing the
//  data pointer as the last argument.
//
//  On the other hand, it is not compatible with blocks.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Intrinsics.h"
#include "llvm/Module.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Target/TargetData.h"

#include "ASTVisitor.h"
#include "CallingConvention.h"
#include "Explosion.h"
#include "GenHeap.h"
#include "GenInit.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "Condition.h"
#include "FixedTypeInfo.h"
#include "ScalarTypeInfo.h"

#include "GenFunc.h"

using namespace swift;
using namespace irgen;

/// Return the number of potential curries of this function type.
/// This is equal to the number of "straight-line" arrows in the type.
static unsigned getNumCurries(AnyFunctionType *type) {
  unsigned count = 0;
  do {
    count++;
    type = type->getResult()->getAs<AnyFunctionType>();
  } while (type);

  return count;
}

/// Return the natural level at which to uncurry this function.  This
/// is the number of additional parameter clauses that are uncurried
/// in the function body.
static unsigned getNaturalUncurryLevel(FuncDecl *func) {
  assert(func->getBody());
  return func->getBody()->getParamPatterns().size() - 1;
}

/// Given a function type, return the formal result type at the given
/// uncurrying level.  For 'a -> b -> c', this is 'b' at 0 and 'c' at 1.
static Type getResultType(Type type, unsigned uncurryLevel) {
  do {
    type = type->castTo<AnyFunctionType>()->getResult();
  } while (uncurryLevel--);
  return type;
}

const TypeInfo &IRGenFunction::getResultTypeInfo() const {
  Type resultType = getResultType(CurFuncType, CurUncurryLevel);
  return IGM.getFragileTypeInfo(resultType);
}

static llvm::CallingConv::ID getFreestandingConvention(IRGenModule &IGM) {
  // TODO: use a custom CC that returns three scalars efficiently
  return llvm::CallingConv::C;
}

/// Expand the requirements of the given abstract calling convention
/// into a "physical" calling convention and a set of attributes.
llvm::CallingConv::ID irgen::expandAbstractCC(IRGenModule &IGM,
                                              AbstractCC convention,
                                              bool hasIndirectResult,
                          SmallVectorImpl<llvm::AttributeWithIndex> &attrs) {
  // If we have an indirect result, add the appropriate attributes.
  if (hasIndirectResult) {
    attrs.push_back(llvm::AttributeWithIndex::get(1,
                                  llvm::Attribute::StructRet |
                                  llvm::Attribute::NoAlias));
  }

  switch (convention) {
  case AbstractCC::C:
    return llvm::CallingConv::C;

  case AbstractCC::Method:
    //   TODO: maybe add 'inreg' to the first non-result argument.
    // fallthrough
  case AbstractCC::Freestanding:
    return getFreestandingConvention(IGM);
  }
  llvm_unreachable("bad calling convention!");
}

namespace {
  /// The natural form of the result of performing a call.  A call
  /// result may be indirect, in which case it is returned in memory
  /// whose address is passed as an implicit first argument, or it may
  /// be direct.
  class CallResult {
    union Value {
      /// The buffer for the set of direct values produced by the call.
      /// This can be greater than the normal cap on scalar values if
      /// the actual call is inlined or builtin.
      ///
      /// FIXME: when we commit to a proper C++11 compiler, this can just
      /// be "Explosion Direct;".
      char Direct[sizeof(Explosion)];

      Explosion &getDirect() { return *reinterpret_cast<Explosion*>(Direct); }

      /// The address into which to emit an indirect call.  If this is
      /// set, the call will be evaluated (as an initialization) into
      /// this address; otherwise, memory will be allocated on the stack.
      Address Indirect;

      Value() {}
      ~Value() {}
    };

    enum class State {
      Invalid, Indirect, Direct
    };

    Value CurValue;
    State CurState;

  public:
    CallResult() : CurState(State::Invalid) {}
    ~CallResult() { reset(); }

    /// Configure this result to carry a number of direct values at
    /// the given explosion level.
    Explosion &initForDirectValues(ExplosionKind level) {
      assert(CurState == State::Invalid);
      CurState = State::Direct;
      return *new (&CurValue.getDirect()) Explosion(level);
    }

    /// As a potential efficiency, set that this is a direct result
    /// with no values.
    void setAsEmptyDirect() {
      initForDirectValues(ExplosionKind::Maximal);
    }

    /// Set this result so that it carries a single directly-returned
    /// maximally-fragile value without management.
    void setAsSingleDirectUnmanagedFragileValue(llvm::Value *value) {
      initForDirectValues(ExplosionKind::Maximal).addUnmanaged(value);
    }

    void setAsIndirectAddress(Address address) {
      assert(CurState == State::Invalid);
      CurState = State::Indirect;
      CurValue.Indirect = address;
    }

    bool isInvalid() const { return CurState == State::Invalid; } 
    bool isDirect() const { return CurState == State::Direct; }
    bool isIndirect() const { return CurState == State::Indirect; }

    Callee getDirectValuesAsIndirectCallee(Type origFormalType,
                                           Type substResultType,
                                           ArrayRef<Substitution> subs) {
      assert(isDirect());
      Explosion &values = getDirectValues();
      assert(values.size() == 2);
      llvm::Value *fn = values.claimUnmanagedNext();
      ManagedValue data = values.claimNext();
      return Callee::forIndirectCall(origFormalType, substResultType, subs,
                                     fn, data);
    }

    Explosion &getDirectValues() {
      assert(isDirect());
      return CurValue.getDirect();
    }

    Address getIndirectAddress() const {
      assert(isIndirect());
      return CurValue.Indirect;
    }

    void reset() {
      if (CurState == State::Direct)
        CurValue.getDirect().~Explosion();
      CurState = State::Invalid;
    }
  };

  /// A signature represents something which can actually be called.
  class Signature {
    llvm::PointerIntPair<llvm::FunctionType*, 1, bool> TypeAndHasIndirectReturn;

  public:
    bool isValid() const {
      return TypeAndHasIndirectReturn.getPointer() != nullptr;
    }

    void set(llvm::FunctionType *type, bool hasIndirectReturn) {
      TypeAndHasIndirectReturn.setPointer(type);
      TypeAndHasIndirectReturn.setInt(hasIndirectReturn);
      assert(isValid());
    }

    llvm::FunctionType *getType() const {
      assert(isValid());
      return TypeAndHasIndirectReturn.getPointer();
    }

    bool hasIndirectReturn() const {
      assert(isValid());
      return TypeAndHasIndirectReturn.getInt();
    }
  };

  /// The type-info class.
  class FuncTypeInfo : public ScalarTypeInfo<FuncTypeInfo, FixedTypeInfo> {
    /// Each possible currying of a function type has different function
    /// type variants along each of two orthogonal axes:
    ///   - the explosion kind desired
    ///   - whether a data pointer argument is required
    struct Currying {
      Signature Signatures[2][2];

      Signature &select(ExplosionKind kind, bool needsData) {
        return Signatures[unsigned(kind)][unsigned(needsData)];
      }
    };

    /// The Swift function type being represented.
    AnyFunctionType * const FormalType;

    /// An array of Curryings is stored immediately after the FuncTypeInfo.
    /// A Currying is a cache, so the entire thing is effective mutable.
    Currying *getCurryingsBuffer() const {
      return const_cast<Currying*>(reinterpret_cast<const Currying*>(this+1));
    }

    FuncTypeInfo(AnyFunctionType *formalType, llvm::StructType *storageType,
                 Size size, Alignment align, unsigned numCurries)
      : ScalarTypeInfo(storageType, size, align, IsNotPOD),
        FormalType(formalType) {
      
      // Initialize the curryings.
      for (unsigned i = 0; i != numCurries; ++i) {
        new (&getCurryingsBuffer()[i]) Currying();
      }
    }

  public:
    static const FuncTypeInfo *create(AnyFunctionType *formalType,
                                      llvm::StructType *storageType,
                                      Size size, Alignment align) {
      unsigned numCurries = getNumCurries(formalType);
      void *buffer = new char[sizeof(FuncTypeInfo)
                                + numCurries * sizeof(Currying)];
      return new (buffer) FuncTypeInfo(formalType, storageType, size, align,
                                       numCurries);
    }

    /// The storage type of a function is always just a pair of i8*s:
    /// a function pointer and a retainable pointer.  We have to use
    /// i8* instead of an appropriate function-pointer type because we
    /// might be in the midst of recursively defining one of the types
    /// used as a parameter.
    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    Signature getSignature(IRGenModule &IGM, ExplosionKind explosionKind,
                           unsigned currying, bool needsData) const;

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 2;
    }

    void getSchema(ExplosionSchema &schema) const {
      llvm::StructType *Ty = getStorageType();
      assert(Ty->getNumElements() == 2);
      schema.add(ExplosionSchema::Element::forScalar(Ty->getElementType(0)));
      schema.add(ExplosionSchema::Element::forScalar(Ty->getElementType(1)));
    }

    static Address projectFunction(IRGenFunction &IGF, Address address) {
      return IGF.Builder.CreateStructGEP(address, 0, Size(0),
                                         address->getName() + ".fn");
    }

    static Address projectData(IRGenFunction &IGF, Address address) {
      return IGF.Builder.CreateStructGEP(address, 1, IGF.IGM.getPointerSize(),
                                         address->getName() + ".data");
    }

    void load(IRGenFunction &IGF, Address address, Explosion &e) const {
      // Load the function.
      Address fnAddr = projectFunction(IGF, address);
      e.addUnmanaged(IGF.Builder.CreateLoad(fnAddr, fnAddr->getName()+".load"));

      // Load the data.
      Address dataAddr = projectData(IGF, address);
      IGF.emitLoadAndRetain(dataAddr, e);
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // Load the function.
      Address fnAddr = projectFunction(IGF, addr);
      e.addUnmanaged(IGF.Builder.CreateLoad(fnAddr));

      // Load the data.
      Address dataAddr = projectData(IGF, addr);
      e.add(IGF.enterReleaseCleanup(IGF.Builder.CreateLoad(dataAddr)));
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the function pointer.
      Address fnAddr = projectFunction(IGF, address);
      IGF.Builder.CreateStore(e.claimUnmanagedNext(), fnAddr);

      // Store the data pointer.
      Address dataAddr = projectData(IGF, address);
      IGF.emitAssignRetained(e.forwardNext(IGF), dataAddr);
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the function pointer.
      Address fnAddr = projectFunction(IGF, address);
      IGF.Builder.CreateStore(e.claimUnmanagedNext(), fnAddr);

      // Store the data pointer, transferring the +1.
      Address dataAddr = projectData(IGF, address);
      IGF.emitInitializeRetained(e.forwardNext(IGF), dataAddr);
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
      IGF.emitRetain(src.claimNext().getValue(), dest);
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
      dest.add(IGF.enterReleaseCleanup(src.claimUnmanagedNext()));
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      IGF.emitRelease(IGF.Builder.CreateLoad(projectData(IGF, addr)));
    }
  };
}

const TypeInfo *TypeConverter::convertFunctionType(AnyFunctionType *T) {
  return FuncTypeInfo::create(T, IGM.FunctionPairTy,
                              IGM.getPointerSize() * 2,
                              IGM.getPointerAlignment());
}

/// Decompose a function type into its exploded parameter types
/// and its formal result type.
///
/// When dealing with non-trivial uncurryings, parameter clusters
/// are added in reverse order.  For example:
///   formal type:  (A, B) -> (C, D, E) -> F -> G
///   curry 0:      (A, B) -> ((C, D, E) -> F -> G)
///   curry 1:      (C, D, E, A, B) -> (F -> G)
///   curry 2:      (F, C, D, E, A, B) -> G
/// This is so that currying stubs can load their stored arguments
/// into position without disturbing their formal arguments.
/// This also interacts well with closures that save a single
/// retainable pointer which becomes the only curried argument
/// (and therefore the final argument) to a method call.
///
/// Generic arguments come last in a clause, also in order to make it
/// easier to drop or ignore them.
///
/// This is all somewhat optimized for register-passing CCs; it
/// probably makes extra work when the stack gets involved.
static Type decomposeFunctionType(IRGenModule &IGM, AnyFunctionType *fn,
                                  ExplosionKind explosionKind,
                                  unsigned uncurryLevel,
                                  SmallVectorImpl<llvm::Type*> &argTypes) {
  // Save up the formal parameter types in reverse order.
  llvm::SmallVector<AnyFunctionType*, 8> formalFnTypes(uncurryLevel + 1);
  formalFnTypes[uncurryLevel] = fn;
  while (uncurryLevel--) {
    fn = fn->getResult()->castTo<AnyFunctionType>();
    formalFnTypes[uncurryLevel] = fn;
  }

  // Explode the argument clusters in that reversed order.
  for (AnyFunctionType *fnTy : formalFnTypes) {
    ExplosionSchema schema(explosionKind);
    IGM.getSchema(fnTy->getInput(), schema);

    for (ExplosionSchema::Element &elt : schema) {
      if (elt.isAggregate())
        argTypes.push_back(elt.getAggregateType()->getPointerTo());
      else
        argTypes.push_back(elt.getScalarType());
    }

    if (auto polyTy = dyn_cast<PolymorphicFunctionType>(fnTy))
      expandPolymorphicSignature(IGM, polyTy->getGenericParams(), argTypes);
  }

  return fn->getResult();
}

Signature FuncTypeInfo::getSignature(IRGenModule &IGM,
                                     ExplosionKind explosionKind,
                                     unsigned uncurryLevel,
                                     bool needsData) const {
  // Compute a reference to the appropriate signature cache.
  assert(uncurryLevel < getNumCurries(FormalType));
  Currying &currying = getCurryingsBuffer()[uncurryLevel];
  Signature &signature = currying.select(explosionKind, needsData);

  // If it's already been filled in, we're done.
  if (signature.isValid())
    return signature;

  // The argument types.
  // Save a slot for the aggregate return.
  SmallVector<llvm::Type*, 16> argTypes;
  argTypes.push_back(nullptr);

  Type formalResultType = decomposeFunctionType(IGM, FormalType, explosionKind,
                                                uncurryLevel, argTypes);

  // Compute the result type.
  llvm::Type *resultType;
  bool hasAggregateResult;
  {
    ExplosionSchema schema(explosionKind);
    IGM.getSchema(formalResultType, schema);

    hasAggregateResult = schema.requiresIndirectResult();
    if (hasAggregateResult) {
      const TypeInfo &info = IGM.getFragileTypeInfo(formalResultType);
      argTypes[0] = info.StorageType->getPointerTo();
      resultType = IGM.VoidTy;
    } else if (schema.size() == 0) {
      resultType = IGM.VoidTy;
    } else if (schema.size() == 1) {
      resultType = schema.begin()->getScalarType();
    } else {
      llvm::SmallVector<llvm::Type*,
                        ExplosionSchema::MaxScalarsForDirectResult> elts;
      for (auto &elt : schema) elts.push_back(elt.getScalarType());
      resultType = llvm::StructType::get(IGM.getLLVMContext(), elts);
    }
  }

  // Data arguments are last.
  // See the comment in this file's header comment.
  if (needsData)
    argTypes.push_back(IGM.RefCountedPtrTy);

  // Ignore the first element of the array unless we have an aggregate result.
  llvm::ArrayRef<llvm::Type*> realArgTypes = argTypes;
  if (!hasAggregateResult)
    realArgTypes = realArgTypes.slice(1);

  // Create the appropriate LLVM type.
  llvm::FunctionType *llvmType =
    llvm::FunctionType::get(resultType, realArgTypes, /*variadic*/ false);

  // Update the cache and return.
  signature.set(llvmType, hasAggregateResult);
  return signature;
}

llvm::FunctionType *
IRGenModule::getFunctionType(Type type, ExplosionKind explosionKind,
                             unsigned curryingLevel, bool withData) {
  assert(type->is<AnyFunctionType>());
  const FuncTypeInfo &fnTypeInfo = getFragileTypeInfo(type).as<FuncTypeInfo>();
  Signature sig = fnTypeInfo.getSignature(*this, explosionKind,
                                          curryingLevel, withData);
  return sig.getType();
}

static bool isInstanceMethod(FuncDecl *fn) {
  return (!fn->isStatic() && fn->getDeclContext()->isTypeContext());
}

AbstractCC irgen::getAbstractCC(FuncDecl *fn) {
  if (isInstanceMethod(fn))
    return AbstractCC::Method;
  return AbstractCC::Freestanding;
}

/// Construct the best known limits on how we can call the given function.
static AbstractCallee getAbstractDirectCallee(IRGenFunction &IGF,
                                              FuncDecl *fn) {
  bool isLocal = fn->getDeclContext()->isLocalContext();

  // FIXME: be more aggressive about all this.
  ExplosionKind level;
  if (isLocal) {
    level = ExplosionKind::Maximal;
  } else {
    level = ExplosionKind::Minimal;
  }

  unsigned minUncurry = 0;
  if (!fn->isStatic() && fn->getDeclContext()->isTypeContext())
    minUncurry = 1;
  unsigned maxUncurry = getNaturalUncurryLevel(fn);
  
  bool needsData =
    (isLocal && !isa<llvm::ConstantPointerNull>(IGF.getLocalFuncData(fn)));
  AbstractCC convention = getAbstractCC(fn);

  return AbstractCallee(convention, level, minUncurry, maxUncurry, needsData);
}

/// Return the appropriate function pointer type at which to call the
/// given function.
llvm::PointerType *Callee::getFunctionPointerType(IRGenModule &IGM) const {
  return IGM.getFunctionType(getOrigFormalType(), ExplosionLevel, UncurryLevel,
                             hasDataPointer())->getPointerTo();
}

/// Return this function pointer, bitcasted to an i8*.
llvm::Value *Callee::getOpaqueFunctionPointer(IRGenFunction &IGF) const {
  if (FnPtr->getType() == IGF.IGM.Int8PtrTy)
    return FnPtr;
  return IGF.Builder.CreateBitCast(FnPtr, IGF.IGM.Int8PtrTy);
}

/// Return this function pointer, bitcasted to the appropriate formal type.
llvm::Value *Callee::getFunctionPointer(IRGenFunction &IGF) const {
  llvm::Type *ty = getFunctionPointerType(IGF.IGM);
  return IGF.Builder.CreateBitCast(FnPtr, ty);
}

/// Return this data pointer.
ManagedValue Callee::getDataPointer(IRGenFunction &IGF) const {
  if (hasDataPointer()) return DataPtr;
  return ManagedValue(IGF.IGM.RefCountedNull);
}

Callee Callee::forIndirectCall(Type origFormalType, Type substResultType,
                               ArrayRef<Substitution> subs,
                               llvm::Value *fn, ManagedValue data) {
  if (isa<llvm::ConstantPointerNull>(data.getValue()))
    data = ManagedValue(nullptr);
  return forKnownFunction(AbstractCC::Freestanding,
                          origFormalType, substResultType, subs,
                          fn, data, ExplosionKind::Minimal, 0);
}

/// Emit a reference to a function, using the best parameters possible
/// up to given limits.
static Callee emitCallee(IRGenFunction &IGF, FuncDecl *fn,
                         Type substResultType,
                         ArrayRef<Substitution> subs,
                         ExplosionKind bestExplosion, unsigned bestUncurry) {
  if (bestUncurry != 0 || bestExplosion != ExplosionKind::Minimal) {
    AbstractCallee absCallee = getAbstractDirectCallee(IGF, fn);
    bestUncurry = std::min(bestUncurry, absCallee.getMaxUncurryLevel());
    bestExplosion = absCallee.getBestExplosionLevel();
  }

  if (!fn->getDeclContext()->isLocalContext()) {
    llvm::Constant *fnPtr =
      IGF.IGM.getAddrOfFunction(fn, bestExplosion, bestUncurry, /*data*/ false);
    if (isInstanceMethod(fn)) {
      return Callee::forMethod(fn->getType(), substResultType, subs, fnPtr,
                               bestExplosion, bestUncurry);
    } else {
      return Callee::forFreestandingFunction(fn->getType(), substResultType,
                                             subs, fnPtr,
                                             bestExplosion, bestUncurry);
    }
  }

  auto fnPtr = IGF.getAddrOfLocalFunction(fn, bestExplosion, bestUncurry);
  Explosion e(ExplosionKind::Maximal);
  IGF.emitRetain(IGF.getLocalFuncData(fn), e);
  ManagedValue data = e.claimNext();
  return Callee::forKnownFunction(AbstractCC::Freestanding,
                                  fn->getType(), substResultType, subs,
                                  fnPtr, data,
                                  bestExplosion, bestUncurry);
}

namespace {
  /// A single call site, with argument expression and the type of
  /// function being applied.
  struct CallSite {
    CallSite(ApplyExpr *apply)
      : Apply(apply), FnType(apply->getFn()->getType()) {}

    ApplyExpr *Apply;

    /// The function type that we're actually calling.  This is
    /// "un-substituted" if necessary.
    Type FnType;

    Expr *getArg() const { return Apply->getArg(); }
    Type getOrigFnType() const { return FnType; }
    Type getSubstResultType() const { return Apply->getType(); }

    void emit(IRGenFunction &IGF, ArrayRef<Substitution> subs,
              Explosion &out) const {
      assert(!subs.empty() || !FnType->is<PolymorphicFunctionType>());

      // If we have substitutions, then (1) it's possible for this to
      // be a polymorphic function type that we need to expand and
      // (2) we might need to evaluate the r-value differently.
      if (!subs.empty()) {
        auto fnType = FnType->castTo<AnyFunctionType>();
        IGF.emitRValueUnderSubstitutions(getArg(), fnType->getInput(),
                                         subs, out);
        if (auto polyFn = dyn_cast<PolymorphicFunctionType>(fnType))
          emitPolymorphicArguments(IGF, polyFn->getGenericParams(), subs, out);
      } else {
        IGF.emitRValue(getArg(), out);
      }
    }
  };

  struct CalleeSource {
  public:
    enum class Kind {
      Indirect, Direct, DirectWithSideEffects, Existential, Archetype
    };

  private:
    union {
      struct {
        Expr *Fn;
      } Indirect;
      struct {
        FuncDecl *Fn;
        Expr *SideEffects;
      } Direct;
      struct {
        ExistentialMemberRefExpr *Fn;
      } Existential;
      struct {
        ArchetypeMemberRefExpr *Fn;
      } Archetype;
    };
    Kind TheKind;
    Type SubstResultType;
    ArrayRef<Substitution> Substitutions;
    SmallVector<CallSite, 4> CallSites;

  public:
    static CalleeSource decompose(Expr *fn);

    static CalleeSource forIndirect(Expr *fn) {
      CalleeSource result;
      result.TheKind = Kind::Indirect;
      result.Indirect.Fn = fn;
      return result;
    }

    static CalleeSource forDirect(FuncDecl *fn) {
      CalleeSource result;
      result.TheKind = Kind::Direct;
      result.Direct.Fn = fn;
      return result;
    }

    static CalleeSource forDirectWithSideEffects(FuncDecl *fn,
                                                 Expr *sideEffects) {
      CalleeSource result;
      result.TheKind = Kind::DirectWithSideEffects;
      result.Direct.Fn = fn;
      result.Direct.SideEffects = sideEffects;
      return result;
    }

    static CalleeSource forExistential(ExistentialMemberRefExpr *fn) {
      CalleeSource result;
      result.TheKind = Kind::Existential;
      result.Existential.Fn = fn;
      return result;
    }

    static CalleeSource forArchetype(ArchetypeMemberRefExpr *fn) {
      CalleeSource result;
      result.TheKind = Kind::Archetype;
      result.Archetype.Fn = fn;
      return result;
    }

    Kind getKind() const { return TheKind; }

    FuncDecl *getDirectFunction() const {
      assert(getKind() == Kind::Direct ||
             getKind() == Kind::DirectWithSideEffects);
      return Direct.Fn;
    }

    Expr *getDirectSideEffects() const {
      assert(getKind() == Kind::DirectWithSideEffects);
      return Direct.SideEffects;
    }

    Expr *getIndirectFunction() const {
      assert(getKind() == Kind::Indirect);
      return Indirect.Fn;
    }

    ExistentialMemberRefExpr *getExistentialFunction() const {
      assert(getKind() == Kind::Existential);
      return Existential.Fn;
    }

    ArchetypeMemberRefExpr *getArchetypeFunction() const {
      assert(getKind() == Kind::Archetype);
      return Archetype.Fn;
    }

    AbstractCallee getAbstractCallee(IRGenFunction &IGF) const {
      switch (getKind()) {
      case Kind::Indirect:
        return AbstractCallee::forIndirect();

      case Kind::Direct:
      case Kind::DirectWithSideEffects:
        return getAbstractDirectCallee(IGF, Direct.Fn);

      case Kind::Existential:
        return getAbstractProtocolCallee(IGF,
                                  cast<FuncDecl>(Existential.Fn->getDecl()));

      case Kind::Archetype:
        return getAbstractProtocolCallee(IGF,
                                    cast<FuncDecl>(Archetype.Fn->getDecl()));
      }
      llvm_unreachable("bad source kind!");
    }

    void addSubstitutions(ArrayRef<Substitution> subs) {
      // FIXME: collect these
      assert(Substitutions.empty());
      Substitutions = subs;
    }

    void setSubstResultType(Type type) {
      SubstResultType = type;
    }

    ArrayRef<Substitution> getSubstitutions() const { return Substitutions; }
    bool hasSubstitutions() const { return !Substitutions.empty(); }

    void addCallSite(CallSite site) {
      CallSites.push_back(site);
    }

    ArrayRef<CallSite> getCallSites() const {
      return CallSites;
    }

    /// Return the formal, unsubstituted type to which the call sites
    /// are applied.
    Type getFormalTypeForCallSites() const {
      switch (getKind()) {
      case Kind::DirectWithSideEffects:
      case Kind::Direct:
        return Direct.Fn->getType();

      case Kind::Indirect:
        return Indirect.Fn->getType();

      case Kind::Existential:
        return getFormalTypeForCallSites(getExistentialFunction()->getDecl());

      case Kind::Archetype:
        return getFormalTypeForCallSites(getArchetypeFunction()->getDecl());
      }
      llvm_unreachable("bad kind");
    }

  private:
    static Type getFormalTypeForCallSites(ValueDecl *val) {
      FuncDecl *fn = cast<FuncDecl>(val);
      Type formalType = fn->getType();
      if (!fn->isStatic())
        formalType = formalType->castTo<AnyFunctionType>()->getResult();
      return formalType;
    }

  public:
    /// Given that this is a potentially polymorphic call, update the
    /// function types on the CallSites to provide the unsubstituted
    /// types.  where it occurs.
    void unsubstituteCallSiteTypes() {
      assert(hasSubstitutions());
      Type formalType = getFormalTypeForCallSites();
      for (CallSite &site : CallSites) {
        // If the unsubstituted formal type gives us a function type,
        // that's what we should be working with.
        if (auto fnType = formalType->getAs<AnyFunctionType>()) {
          site.FnType = formalType;
          formalType = fnType->getResult();

        // Otherwise, we must be instantiating at a function type; use
        // the type we're instantiating to for the rest of the sites.
        } else {
          formalType = site.FnType->castTo<FunctionType>()->getResult();
        }
        assert(site.FnType->is<AnyFunctionType>());
      }
    }

    Callee emitCallee(IRGenFunction &IGF,
                      SmallVectorImpl<Arg> &calleeArgs,
                      unsigned maxUncurry = ~0U,
                      ExplosionKind maxExplosion = ExplosionKind::Maximal) {
      switch (getKind()) {
      case Kind::DirectWithSideEffects:
      case Kind::Direct:
        return ::emitCallee(IGF, getDirectFunction(),
                            SubstResultType, getSubstitutions(),
                            maxExplosion, maxUncurry);
      case Kind::Indirect: {
        Explosion fnValues(ExplosionKind::Maximal);
        Expr *fn = getIndirectFunction();
        IGF.emitRValue(fn, fnValues);
        llvm::Value *fnPtr = fnValues.claimUnmanagedNext();
        ManagedValue dataPtr = fnValues.claimNext();
        return Callee::forIndirectCall(fn->getType(), SubstResultType,
                                       getSubstitutions(),
                                       fnPtr, dataPtr);
      }
      case Kind::Existential:
        return emitExistentialMemberRefCallee(IGF, getExistentialFunction(),
                                              SubstResultType,
                                              getSubstitutions(),
                                              calleeArgs, maxExplosion,
                                              maxUncurry);
      case Kind::Archetype:
        return emitArchetypeMemberRefCallee(IGF, getArchetypeFunction(),
                                            SubstResultType,
                                            getSubstitutions(),
                                            calleeArgs, maxExplosion,
                                            maxUncurry);
      }
      llvm_unreachable("bad CalleeSource kind");
    }
  };
}

/// Emit a reference to the given function as a generic function pointer.
void irgen::emitRValueForFunction(IRGenFunction &IGF, FuncDecl *fn,
                                  Explosion &explosion) {
  // Function pointers are always fully curried and use ExplosionKind::Minimal.
  Type resultType = fn->getType()->castTo<AnyFunctionType>()->getResult();
  Callee callee = ::emitCallee(IGF, fn, resultType, ArrayRef<Substitution>(),
                               ExplosionKind::Minimal, 0);
  assert(callee.getExplosionLevel() == ExplosionKind::Minimal);
  assert(callee.getUncurryLevel() == 0);
  explosion.addUnmanaged(callee.getOpaqueFunctionPointer(IGF));
  explosion.add(callee.getDataPointer(IGF));
}

namespace {
  struct ArgList {
    ArgList(ExplosionKind kind) : Values(kind) {}

    Explosion Values;
    llvm::SmallVector<llvm::AttributeWithIndex, 4> Attrs;
  };
}


/// Given an address representing an unsafe pointer to the given type,
/// turn it into a valid Address.
static Address getAddressForUnsafePointer(IRGenFunction &IGF,
                                          const TypeInfo &type,
                                          llvm::Value *addr) {
  llvm::Value *castAddr =
  IGF.Builder.CreateBitCast(addr, type.getStorageType()->getPointerTo());
  return Address(castAddr, type.StorageAlignment);
}


static void emitCastBuiltin(llvm::Instruction::CastOps opcode, FuncDecl *Fn,
                            IRGenFunction &IGF, ArgList &args,
                            CallResult &result) {
  llvm::Value *input = args.Values.claimUnmanagedNext();
  Type DestType = Fn->getType()->castTo<AnyFunctionType>()->getResult();
  llvm::Type *destTy = IGF.IGM.getFragileTypeInfo(DestType).getStorageType();
  assert(args.Values.empty() && "wrong operands to cast operation");
  llvm::Value *output = IGF.Builder.CreateCast(opcode, input, destTy);
  result.setAsSingleDirectUnmanagedFragileValue(output);
}

static void emitCompareBuiltin(llvm::CmpInst::Predicate pred, FuncDecl *Fn,
                               IRGenFunction &IGF, ArgList &args,
                               CallResult &result) {
  llvm::Value *lhs = args.Values.claimUnmanagedNext();
  llvm::Value *rhs = args.Values.claimUnmanagedNext();
  assert(args.Values.empty() && "wrong operands to binary operation");
  
  llvm::Value *v;
  if (lhs->getType()->isFPOrFPVectorTy())
    v = IGF.Builder.CreateFCmp(pred, lhs, rhs);
  else
    v = IGF.Builder.CreateICmp(pred, lhs, rhs);
           
  return result.setAsSingleDirectUnmanagedFragileValue(v);
}
           
           

/// emitBuiltinCall - Emit a call to a builtin function.
static void emitBuiltinCall(IRGenFunction &IGF, FuncDecl *Fn,
                            ArrayRef<Substitution> Subs,
                            Expr *Arg, CallResult &result) {
  // Emit the arguments.  Maybe we'll get builtins that are more
  // complex than this.
  ArgList args(ExplosionKind::Minimal);
  IGF.emitRValue(Arg, args.Values);

  
  // Decompose the function's name into a builtin name and type list.
  SmallVector<Type, 4> Types;
  StringRef BuiltinName = getBuiltinBaseName(IGF.IGM.Context,
                                             Fn->getName().str(), Types);

  // If this is an LLVM IR intrinsic, lower it to an intrinsic call.
  if (unsigned IID = getLLVMIntrinsicID(BuiltinName, !Types.empty())) {
    SmallVector<llvm::Type*, 4> ArgTys;
    for (auto T : Types)
      ArgTys.push_back(IGF.IGM.getFragileTypeInfo(T).getStorageType());
      
    auto F = llvm::Intrinsic::getDeclaration(&IGF.IGM.Module,
                                             (llvm::Intrinsic::ID)IID, ArgTys);
    llvm::FunctionType *FT = F->getFunctionType();
    SmallVector<llvm::Value*, 8> IRArgs;
    for (unsigned i = 0, e = FT->getNumParams(); i != e; ++i)
      IRArgs.push_back(args.Values.claimUnmanagedNext());
    llvm::Value *TheCall = IGF.Builder.CreateCall(F, IRArgs);
    
    if (TheCall->getType()->isVoidTy())
      // Mark that we're not returning anything.
      result.setAsEmptyDirect();
    else if (!TheCall->getType()->isStructTy())
      result.setAsSingleDirectUnmanagedFragileValue(TheCall);
    else {
      Explosion &E = result.initForDirectValues(ExplosionKind::Maximal);

      auto STy = cast<llvm::StructType>(TheCall->getType());
      for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i)
        E.addUnmanaged(IGF.Builder.CreateExtractValue(TheCall, i));
    }
    return;
  }
  
  // TODO: A linear series of if's is suboptimal.
#define BUILTIN_CAST_OPERATION(id, name) \
  if (BuiltinName == name) \
    return emitCastBuiltin(llvm::Instruction::id, Fn, IGF, args, result);
  
#define BUILTIN_BINARY_OPERATION(id, name, overload) \
  if (BuiltinName == name) { \
    llvm::Value *lhs = args.Values.claimUnmanagedNext(); \
    llvm::Value *rhs = args.Values.claimUnmanagedNext(); \
    llvm::Value *v = IGF.Builder.Create ##id(lhs, rhs); \
    return result.setAsSingleDirectUnmanagedFragileValue(v); \
  }
  
#define BUILTIN_BINARY_PREDICATE(id, name, overload) \
  if (BuiltinName == name) \
    return emitCompareBuiltin(llvm::CmpInst::id, Fn, IGF, args, result);
#define BUILTIN(ID, Name)  // Ignore the rest.
#include "swift/AST/Builtins.def"

  
  if (BuiltinName == "gep") {
    llvm::Value *lhs = args.Values.claimUnmanagedNext();
    llvm::Value *rhs = args.Values.claimUnmanagedNext();
    assert(args.Values.empty() && "wrong operands to gep operation");
    
    // We don't expose a non-inbounds GEP operation.
    llvm::Value *gep = IGF.Builder.CreateInBoundsGEP(lhs, rhs);
    return result.setAsSingleDirectUnmanagedFragileValue(gep);
  }

  if (BuiltinName == "load") {
    // The type of the operation is the result type of the load function.
    Type valueTy = Subs[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    
    // Treat the raw pointer as a physical l-value of that type.
    llvm::Value *addrValue = args.Values.claimUnmanagedNext();
    Address addr = getAddressForUnsafePointer(IGF, valueTI, addrValue);
    assert(args.Values.empty() && "wrong operands to load operation");
    
    // Perform the load.
    Explosion &out = result.initForDirectValues(ExplosionKind::Maximal);
    return valueTI.load(IGF, addr, out);
  }
  
  if (BuiltinName == "assign") {
    // The type of the operation is the type of the first argument of
    // the store function.
    Type valueTy = Subs[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    
    // Treat the raw pointer as a physical l-value of that type.
    llvm::Value *addrValue = args.Values.takeLast().getUnmanagedValue();
    Address addr = getAddressForUnsafePointer(IGF, valueTI, addrValue);
    
    // Mark that we're not returning anything.
    result.setAsEmptyDirect();
    
    // Perform the assignment operation.
    return valueTI.assign(IGF, args.Values, addr);
  }
  
  if (BuiltinName == "init") {
    // The type of the operation is the type of the first argument of
    // the store function.
    Type valueTy = Subs[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    
    // Treat the raw pointer as a physical l-value of that type.
    llvm::Value *addrValue = args.Values.takeLast().getUnmanagedValue();
    Address addr = getAddressForUnsafePointer(IGF, valueTI, addrValue);
    
    // Mark that we're not returning anything.
    result.setAsEmptyDirect();
    
    // Perform the init operation.
    return valueTI.initialize(IGF, args.Values, addr);
  }

  if (BuiltinName == "sizeof") {
    Type valueTy = Subs[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    result.setAsSingleDirectUnmanagedFragileValue(valueTI.getSizeOnly(IGF));
    return;
  }

  if (BuiltinName == "alignof") {
    Type valueTy = Subs[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    result.setAsSingleDirectUnmanagedFragileValue(
                                                 valueTI.getAlignmentOnly(IGF));
    return;
  }

  llvm_unreachable("IRGen unimplemented for this builtin!");
}

namespace {
  /// A holistic plan for performing a call.
  struct CallPlan {
    CalleeSource CalleeSrc;

    CallPlan(Expr *E) : CalleeSrc(CalleeSource::decompose(E)) {}

    /// getFinalResultExplosionLevel - Returns the explosion level at
    /// which we will naturally emit the last call.
    ExplosionKind getFinalResultExplosionLevel(IRGenFunction &IGF) const {
      return CalleeSrc.getAbstractCallee(IGF).getBestExplosionLevel();
    }

    void emit(IRGenFunction &IGF, CallResult &result,
              const TypeInfo &resultType);
  };
}

namespace {
  /// A class for decomposing an expression into a function reference
  /// which can, hopefully, be called more efficiently.
  struct FunctionDecomposer :
      irgen::ExprVisitor<FunctionDecomposer, CalleeSource> {
    CalleeSource visitDeclRefExpr(DeclRefExpr *E) {
      if (FuncDecl *fn = dyn_cast<FuncDecl>(E->getDecl()))
        return CalleeSource::forDirect(fn);
      return CalleeSource::forIndirect(E);
    }

    CalleeSource visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      auto rhs = cast<DeclRefExpr>(E->getRHS());
      if (FuncDecl *fn = dyn_cast<FuncDecl>(rhs->getDecl()))
        return CalleeSource::forDirectWithSideEffects(fn, E);
      return CalleeSource::forIndirect(E);
    }

    CalleeSource visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
      return CalleeSource::forExistential(E);
    }

    CalleeSource visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
      return CalleeSource::forArchetype(E);
    }

    CalleeSource visitSpecializeExpr(SpecializeExpr *E) {
      CalleeSource src = visit(E->getSubExpr());
      src.addSubstitutions(E->getSubstitutions());
      return src;
    }

    CalleeSource visitApplyExpr(ApplyExpr *E) {
      CalleeSource source = visit(E->getFn());
      source.addCallSite(CallSite(E));
      return source;
    }

    CalleeSource visitExpr(Expr *E) {
      return CalleeSource::forIndirect(E);
    }
  };
}

/// Try to decompose a function reference into a known function
/// declaration.
CalleeSource CalleeSource::decompose(Expr *E) {
  // Do the basic decomposition.
  CalleeSource source = FunctionDecomposer().visit(E);

  // If we have substitutions, change the call sites to list their
  // non-subsituted types.
  if (source.hasSubstitutions())
    source.unsubstituteCallSiteTypes();

  // Remember the substituted result type, which is to say, the type
  // of the original expression.
  source.setSubstResultType(E->getType());
  return source;
}

/// Compute the plan for performing a sequence of call expressions.
static CallPlan getCallPlan(IRGenModule &IGM, ApplyExpr *E) {
  return CallPlan(E);
}

/// Emit an expression as a callee suitable for being passed to
/// irgen::emitCall or one its variants.
Callee irgen::emitCallee(IRGenFunction &IGF, Expr *fn,
                         ExplosionKind bestExplosion,
                         unsigned addedUncurrying,
                         SmallVectorImpl<Arg> &args) {
  CalleeSource source = CalleeSource::decompose(fn);
  Callee callee = source.emitCallee(IGF, args, 0, bestExplosion);
  assert(source.getCallSites().size() + args.size() + addedUncurrying
           == callee.getUncurryLevel());

  for (auto &site : source.getCallSites()) {
    Explosion *out = new Explosion(callee.getExplosionLevel());
    site.emit(IGF, source.getSubstitutions(), *out);
    args.push_back(Arg::forOwned(out));
  }

  // We need the callee to actually be appropriately typed.
  llvm::FunctionType *fnType =
    IGF.IGM.getFunctionType(fn->getType(), callee.getExplosionLevel(),
                            0, callee.hasDataPointer());
  llvm::Value *fnPtr = IGF.Builder.CreateBitCast(callee.getRawFunctionPointer(),
                                                 fnType->getPointerTo());

  ManagedValue dataPtr =
    (callee.hasDataPointer() ? callee.getDataPointer(IGF)
                             : ManagedValue(nullptr));
  return Callee::forKnownFunction(callee.getConvention(),
                                  callee.getOrigFormalType(),
                                  callee.getSubstResultType(),
                                  callee.getSubstitutions(),
                                  fnPtr, dataPtr,
                                  callee.getExplosionLevel(),
                                  callee.getUncurryLevel());
}

/// Extract the direct scalar results of a call instruction into an
/// explosion, registering cleanups as appropriate for the type.
static void extractScalarResults(IRGenFunction &IGF, llvm::Value *call,
                                 const TypeInfo &resultTI, Explosion &out) {
  // We need to make a temporary explosion to hold the values as we
  // tag them with cleanups.
  Explosion tempExplosion(out.getKind());

  // Extract the values.
  if (llvm::StructType *structType
        = dyn_cast<llvm::StructType>(call->getType())) {
    for (unsigned i = 0, e = structType->getNumElements(); i != e; ++i) {
      llvm::Value *scalar = IGF.Builder.CreateExtractValue(call, i);
      tempExplosion.addUnmanaged(scalar);
    }
  } else {
    assert(!call->getType()->isVoidTy());
    tempExplosion.addUnmanaged(call);
  }

  resultTI.manage(IGF, tempExplosion, out);
}

namespace {
  /// An abstract helper class for emitting a single call.
  class CallEmitter {
  protected:
    IRGenFunction &IGF;
    const Callee &TheCallee;

  private:
    const TypeInfo &SubstResultTI;
    CallResult &Result;
    Address FinalAddress;

    unsigned LastArgWritten;
    SmallVector<llvm::Value*, 16> Args;
    SmallVector<CleanupsDepth, 16> ArgCleanups;
    
  protected:
    CallEmitter(IRGenFunction &IGF, const Callee &callee,
                const TypeInfo &substResultTI,
                CallResult &result, Address finalAddress)
      : IGF(IGF), TheCallee(callee), SubstResultTI(substResultTI),
        Result(result), FinalAddress(finalAddress) {
    }

    bool hasSubstitutions() const {
      return TheCallee.hasSubstitutions();
    }
    ArrayRef<Substitution> getSubstitutions() const {
      return TheCallee.getSubstitutions();
    }

    virtual void emitArgs() = 0;

    /// Emit an individual argument explosion.  Generally, emitArgs
    /// should call this once for each uncurry level.
    void emitArg(Explosion &arg) {
      assert(LastArgWritten >= arg.size());
      LastArgWritten -= arg.size();
    
      auto argIterator = Args.begin() + LastArgWritten;
      for (auto value : arg.claimAll()) {
        *argIterator++ = value.split(ArgCleanups);
      }
    }

  public:
    void emit(llvm::Value *fnPtr);    
  };

  /// ArgCallEmitter - A helper class for emitting a single call based
  /// on an array of Arg objects.
  class ArgCallEmitter : public CallEmitter {
    ArrayRef<Arg> Args;

  public:
    ArgCallEmitter(IRGenFunction &IGF, const Callee &callee,
                   ArrayRef<Arg> args, const TypeInfo &substResultTI,
                   CallResult &result, Address finalAddress)
      : CallEmitter(IGF, callee, substResultTI, result, finalAddress),
        Args(args) {
      assert(TheCallee.getUncurryLevel() + 1 == Args.size());
    }

    void emitArgs() {
      for (auto &arg : Args) {
        // Ignore obviously empty arguments.
        if (arg.empty()) continue;

        Explosion &rawArgValue = arg.getValue();

        // If the argument was emitted at the appropriate level, we're okay.
        if (rawArgValue.getKind() == TheCallee.getExplosionLevel()) {
          emitArg(rawArgValue);
          continue;
        }

        // Otherwise, we need to re-explode it.
        Explosion reexploded(TheCallee.getExplosionLevel());
        arg.getType().reexplode(IGF, rawArgValue, reexploded);
        emitArg(reexploded);
      }
    }
  };

  /// CallSiteCallEmitter - A helper class for emitting a single call
  /// based on a stack of CallSites.
  class CallSiteCallEmitter : public CallEmitter {
    ArrayRef<Arg> CalleeArgs;
    ArrayRef<CallSite> &CallSites;

  public:
    CallSiteCallEmitter(IRGenFunction &IGF, const Callee &callee,
                        ArrayRef<Arg> calleeArgs,
                        ArrayRef<CallSite> &callSites,
                        const TypeInfo &substResultTI,
                        CallResult &result, Address finalAddress)
      : CallEmitter(IGF, callee, substResultTI, result, finalAddress),
        CalleeArgs(calleeArgs), CallSites(callSites) {
      assert(TheCallee.getUncurryLevel() <
               CallSites.size() + calleeArgs.size());
    }

    void emitArgs() {
      // The callee-determined arguments are the first logical arguments.
      for (auto &arg : CalleeArgs) {
        emitArg(arg.getValue());
      }

      // Emit all of the arguments we need to pass here.
      unsigned numArgs = TheCallee.getUncurryLevel() + 1 - CalleeArgs.size();
      auto callSites = CallSites.slice(0, numArgs);
      CallSites = CallSites.slice(numArgs);

      for (const CallSite &site : callSites) {
        // Emit the arguments for this clause.
        Explosion argExplosion(TheCallee.getExplosionLevel());
        site.emit(IGF, getSubstitutions(), argExplosion);
        emitArg(argExplosion);
      }
    }
  };
}

/// Emit a call to the given function pointer, which should have been
/// casted to exactly the right type.
void CallEmitter::emit(llvm::Value *fnPtr) {
  llvm::FunctionType *calleeType = 
    cast<llvm::FunctionType>(cast<llvm::PointerType>(fnPtr->getType())
                             ->getElementType());

  llvm::SmallVector<llvm::AttributeWithIndex, 4> attrs;

  // Build the arguments:
  unsigned numArgs = calleeType->getNumParams();
  Args.reserve(numArgs);
  Args.set_size(numArgs);
  LastArgWritten = numArgs;

  // - Add the data pointer in.
  if (TheCallee.hasDataPointer()) {
    Args[--LastArgWritten] = TheCallee.getDataPointer(IGF).split(ArgCleanups);
  }

  // - Emit all of the formal arguments we have.
  emitArgs();

  // Prepare the return value slot:
  Initialization indirectInit;
  InitializedObject indirectResultObject = InitializedObject::invalid();

  // - Compute the callee's formal result type and determine if we
  //   need to translate the result.
  bool resultDiffers;
  bool resultDiffersByAbstraction;
  Type calleeResultType;

  //   If we don't have substitutions, this is easy.
  if (!TheCallee.hasSubstitutions()) {
    calleeResultType = TheCallee.getSubstResultType();
    resultDiffersByAbstraction = false;
    resultDiffers = false;

  //   If we do, drill into the formal type and check for differences
  //   in abstraction.
  } else {
    calleeResultType =
      getResultType(TheCallee.getOrigFormalType(), TheCallee.getUncurryLevel());
    resultDiffers = !calleeResultType->isEqual(TheCallee.getSubstResultType());
    resultDiffersByAbstraction =
      (resultDiffers &&
       differsByAbstraction(IGF.IGM, calleeResultType->getCanonicalType(),
                            TheCallee.getSubstResultType()->getCanonicalType(),
                            AbstractionDifference::Memory));
  }

  const TypeInfo *calleeResultTI = nullptr;
  if (resultDiffers) calleeResultTI = &IGF.getFragileTypeInfo(calleeResultType);

  // Emit and insert the result slot if required.
  assert(LastArgWritten == 0 || LastArgWritten == 1);
  bool isCalleeAggregateResult = (LastArgWritten != 0);
  assert(isCalleeAggregateResult ==
         (IGF.IGM.requiresIndirectResult(calleeResultType,
                                         TheCallee.getExplosionLevel())
            != nullptr));
  if (isCalleeAggregateResult) {
    // Force there to be an indirect address.
    Address resultAddress;

    // If we have a final address, use that if we don't differ by
    // abstraction.
    if (!resultDiffersByAbstraction && FinalAddress.isValid()) {
      resultAddress = FinalAddress;

    // Otherwise, we need to allocate a temporary.
    } else {
      // Use the substituted type, which has the more accurate
      // information, unless we differ by abstraction.
      auto &resultTI =
        (resultDiffersByAbstraction ? *calleeResultTI : SubstResultTI);

      indirectResultObject = indirectInit.getObjectForTemporary();
      indirectInit.registerObject(IGF, indirectResultObject,
                                  NotOnHeap, resultTI);
      resultAddress =
        indirectInit.emitLocalAllocation(IGF, indirectResultObject,
                                         NotOnHeap, resultTI,
                                         "call.aggresult");
    }

    // Okay, remember resultAddress uncasted.  It's typed for the
    // substituted result unless we differ by abstraction.
    Result.setAsIndirectAddress(resultAddress);

    // However, we might need a cast before we pass it as an argument.
    if (resultDiffers && !resultDiffersByAbstraction) {
      auto calleeRetTy = calleeResultTI->getStorageType()->getPointerTo();
      resultAddress = IGF.Builder.CreateBitCast(resultAddress, calleeRetTy);
    }
    Args[0] = resultAddress.getAddress();
  }

  // Deactivate all the cleanups.
  for (auto cleanup : ArgCleanups)
    IGF.setCleanupState(cleanup, CleanupState::Dead);

  // Determine the calling convention.
  auto cc = expandAbstractCC(IGF.IGM, TheCallee.getConvention(),
                             isCalleeAggregateResult, attrs);

  // Make the call.
  llvm::CallSite call = IGF.emitInvoke(cc, fnPtr, Args,
                                       llvm::AttrListPtr::get(attrs));

  // Handle the return value.  We don't want to add any abstraction
  // penalties here:  the result should be 
    
  // If we have an aggregate result, set the sret and noalias
  // attributes on the agg return slot.
  if (isCalleeAggregateResult) {
    assert(Result.isIndirect());

    // Mark that we've successfully initialized the indirect result.
    if (indirectResultObject.isValid())
      indirectInit.markInitialized(IGF, indirectResultObject);

    // If we differ by abstraction, we need to do that translation here.
    // Otherwise we're done.
    if (!resultDiffersByAbstraction) return;

    IGF.unimplemented(SourceLoc(), "translation of aggregate return values");
    return;
  }

  // If we have a void result, there's nothing to do.
  if (call->getType()->isVoidTy()) {
    Result.setAsEmptyDirect();
    return;
  }

  // If the result differs by abstraction, we need to do that
  // translation here.
  if (resultDiffersByAbstraction) {
    IGF.unimplemented(SourceLoc(), "translation of scalar return values");
    return;
  }

  // Extract out the scalar results.
  Explosion &out = Result.initForDirectValues(TheCallee.getExplosionLevel());
  extractScalarResults(IGF, call.getInstruction(), SubstResultTI, out);
}

/// Emit a call with a set of arguments which have already been emitted.
static void emitCall(IRGenFunction &IGF, const Callee &callee,
                     ArrayRef<Arg> args,
                     const TypeInfo &substResultTI,
                     CallResult &result) {
  // Remember the requested final address, if applicable.
  Address finalAddress;
  if (!result.isInvalid()) {
    finalAddress = result.getIndirectAddress();
    result.reset();
  }

  // Find the function pointer.  In this world, the function pointer
  // needs to already have been appropriately casted.
  llvm::Value *fn = callee.getRawFunctionPointer();

  // Emit the call.
  ArgCallEmitter(IGF, callee, args,
                 substResultTI, result, finalAddress).emit(fn);
}

/// emitKnownCall - Emit a call to a known function.
// FIXME: This is a rather ugly, but it's the best way I can think
// of to avoid emitting calls to getLogicValue as external calls.
static bool emitKnownCall(IRGenFunction &IGF, FuncDecl *Fn,
                          ArrayRef<CallSite> &callSites,
                          CallResult &result) {
  if (Fn->getName().str() == "getLogicValue") {
    ExtensionDecl *ED = dyn_cast<ExtensionDecl>(Fn->getDeclContext());
    if (!ED)
      return false;

    OneOfType *OOT = ED->getExtendedType()->getAs<OneOfType>();
    if (!OOT)
      return false;

    if (OOT->getDecl()->getName().str() != "Bool")
      return false;

    Module *M = dyn_cast<Module>(ED->getDeclContext());
    if (!M)
      return false;

    if (M->Name.str() != "swift")
      return false;

    if (callSites.size() != 2)
      return false;

    Expr *Arg = callSites[0].getArg();
    Explosion &out = result.initForDirectValues(ExplosionKind::Maximal);
    Explosion temp(ExplosionKind::Maximal);
    Type ObjTy = Arg->getType()->castTo<LValueType>()->getObjectType();
    const TypeInfo &ObjTInfo = IGF.IGM.getFragileTypeInfo(ObjTy);
    IGF.emitLoad(IGF.emitLValue(Arg), ObjTInfo, out);
    return true;
  }

  if (Fn->getName().str() == "&&" || Fn->getName().str() == "||") {
    Module *M = dyn_cast<Module>(Fn->getDeclContext());
    if (!M)
      return false;

    if (M->Name.str() != "swift")
      return false;

    TupleExpr *Arg = cast<TupleExpr>(callSites.front().getArg());
    
    Condition cond = IGF.emitCondition(Arg->getElement(0), true,
                                       Fn->getName().str() == "||");
    Address CondBool = IGF.createAlloca(IGF.Builder.getInt1Ty(), Alignment(1),
                                        "logical.cond");
    if (cond.hasTrue()) {
      cond.enterTrue(IGF);
      Expr *Body = cast<ImplicitClosureExpr>(Arg->getElement(1))->getBody();
      llvm::Value *Val = IGF.emitAsPrimitiveScalar(Body);
      IGF.Builder.CreateStore(Val, CondBool);
      cond.exitTrue(IGF);
    }
    if (cond.hasFalse()) {
      cond.enterFalse(IGF);
      Explosion explosion(ExplosionKind::Minimal);
      llvm::Value *Val = IGF.Builder.getInt1(Fn->getName().str() == "||");
      IGF.Builder.CreateStore(Val, CondBool);
      cond.exitFalse(IGF);
    }
    cond.complete(IGF);
    Explosion &out = result.initForDirectValues(ExplosionKind::Maximal);
    out.addUnmanaged(IGF.Builder.CreateLoad(CondBool));
    return true;
  }

  return false;
}

/// Emit a function call.
void CallPlan::emit(IRGenFunction &IGF, CallResult &result,
                    const TypeInfo &resultType) {
  // Save the final result address if one was given, then reset
  // to establish the invariant that the result is always invalid.
  Address finalAddress;
  if (result.isIndirect()) {
    finalAddress = result.getIndirectAddress();
    result.reset();
  }

  // 1.  Emit the function expression.
  Callee callee;
  SmallVector<Arg, 2> calleeArgs;

  ArrayRef<CallSite> callSites = CalleeSrc.getCallSites();

  switch (CalleeSrc.getKind()) {

  // We can do a lot if we know we're calling a known function.
  case CalleeSource::Kind::DirectWithSideEffects:
    // Go ahead and emit the nontrivial function expression if we found one.
    IGF.emitIgnored(CalleeSrc.getDirectSideEffects());
    // fallthrough
  case CalleeSource::Kind::Direct: {
    FuncDecl *knownFn = CalleeSrc.getDirectFunction();

    // Handle calls to builtin functions.  These are never curried, but they
    // might return a function pointer.
    if (isa<BuiltinModule>(knownFn->getDeclContext())) {
      CallSite site = callSites.front();
      emitBuiltinCall(IGF, knownFn, CalleeSrc.getSubstitutions(),
                      site.getArg(), result);

      // If there are no trailing calls, we're done.
      if (callSites.size() == 1) return;

      // Otherwise, pop that call site off and set up the callee conservatively.
      callSites = callSites.slice(1);
      Type origFnType =
        site.getOrigFnType()->castTo<AnyFunctionType>()->getResult();
      Type substResultType =
        site.getSubstResultType()->castTo<AnyFunctionType>()->getResult();
      callee = result.getDirectValuesAsIndirectCallee(origFnType,
                                                      substResultType,
                                               CalleeSrc.getSubstitutions());
      result.reset();
      break;

    } else if (emitKnownCall(IGF, knownFn, callSites, result)) {
      return;
    }

    // fallthrough
  }

  // Otherwise, just use the normal rules for the kind.
  case CalleeSource::Kind::Indirect:
  case CalleeSource::Kind::Existential:
  case CalleeSource::Kind::Archetype:
    callee = CalleeSrc.emitCallee(IGF, calleeArgs, callSites.size() - 1);
    break;
  }

  // 3. Emit arguments and call.
  while (true) {
    assert(callee.getUncurryLevel() < callSites.size() + calleeArgs.size());

    // Find the formal type of the function we're calling.  This is the
    // least-uncurried function type.
    llvm::Value *fnPtr = callee.getFunctionPointer(IGF);

    // Additionally compute the information for the formal result
    // type.  This is the result of the uncurried function type.
    const TypeInfo &substResultTI =
      IGF.IGM.getFragileTypeInfo(callee.getSubstResultType());
    CallSiteCallEmitter(IGF, callee, calleeArgs, callSites,
                        substResultTI, result, finalAddress)
      .emit(fnPtr);

    // If this is the end of the call sites, we're done.
    if (callSites.empty())
      return;

    auto &nextSite = callSites.front();

    // Otherwise, we must have gotten a function back.  Set ourselves
    // up to call it, then continue emitting calls.
    callee = result.getDirectValuesAsIndirectCallee(nextSite.getOrigFnType(),
                                               nextSite.getSubstResultType(),
                                                    callee.getSubstitutions());
    calleeArgs.clear();
    result.reset();
  }
}

/// Emit a call for its exploded results.
void irgen::emitApplyExpr(IRGenFunction &IGF, ApplyExpr *E, Explosion &out) {
  CallPlan plan = getCallPlan(IGF.IGM, E);

  const TypeInfo &resultTI = IGF.getFragileTypeInfo(E->getType());

  CallResult result;
  plan.emit(IGF, result, resultTI);

  // If this was an indirect return, explode it.
  if (result.isIndirect()) {
    return resultTI.load(IGF, result.getIndirectAddress(), out);
  }

  Explosion &directValues = result.getDirectValues();

  // If the explosion kind of the direct values matches that of the
  // result, we're okay.
  if (directValues.getKind() == out.getKind())
    return out.add(directValues.claimAll());

  // Otherwise we need to re-explode.
  resultTI.reexplode(IGF, directValues, out);
}

/// Emit a call as the initializer for an object in memory.
void swift::irgen::emitApplyExprToMemory(IRGenFunction &IGF, ApplyExpr *E,
                                         Address resultAddress,
                                         const TypeInfo &resultTI) {
  CallPlan plan = getCallPlan(IGF.IGM, E);

  CallResult result;
  result.setAsIndirectAddress(resultAddress);
  plan.emit(IGF, result, resultTI);

  if (result.isIndirect()) {
    assert(result.getIndirectAddress().getAddress()
             == resultAddress.getAddress());
    return;
  }

  Explosion &directValues = result.getDirectValues();
  resultTI.initialize(IGF, directValues, resultAddress);
}

/// See whether we can emit the result of the given call as an object
/// naturally located in memory.
Optional<Address>
swift::irgen::tryEmitApplyAsAddress(IRGenFunction &IGF, ApplyExpr *E,
                                    const TypeInfo &resultTI) {
  CallPlan plan = getCallPlan(IGF.IGM, E);

  // Give up if the call won't be returned indirectly.
  ExplosionSchema schema(plan.getFinalResultExplosionLevel(IGF));
  resultTI.getSchema(schema);
  if (!schema.requiresIndirectResult())
    return Nothing;

  CallResult result;
  plan.emit(IGF, result, resultTI);
  assert(result.isIndirect());
  return result.getIndirectAddress();
}

/// Emit a call against a set of arguments which have already been
/// emitted, placing the result in memory.
void irgen::emitCallToMemory(IRGenFunction &IGF, const Callee &callee,
                             ArrayRef<Arg> args,
                             const TypeInfo &substResultTI,
                             Address resultAddress) {
  CallResult result;
  result.setAsIndirectAddress(resultAddress);
  ::emitCall(IGF, callee, args, substResultTI, result);

  if (result.isIndirect()) {
    assert(result.getIndirectAddress().getAddress()
             == resultAddress.getAddress());
    return;
  }

  Explosion &directValues = result.getDirectValues();
  substResultTI.initialize(IGF, directValues, resultAddress);
}

/// Emit a call against a set of arguments which have already been
/// emitted, placing the result in an explosion.
void irgen::emitCall(IRGenFunction &IGF, const Callee &callee,
                     ArrayRef<Arg> args, const TypeInfo &substResultTI,
                     Explosion &resultExplosion) {
  CallResult result;
  ::emitCall(IGF, callee, args, substResultTI, result);

  if (result.isIndirect()) {
    substResultTI.load(IGF, result.getIndirectAddress(), resultExplosion);
    return;
  }

  Explosion &directValues = result.getDirectValues();
  if (directValues.getKind() == resultExplosion.getKind())
    return resultExplosion.add(directValues.claimAll());

  substResultTI.reexplode(IGF, directValues, resultExplosion);
}

/// Emit a void-returning call against a set of arguments which have
/// already been emitted.
void irgen::emitVoidCall(IRGenFunction &IGF, const Callee &callee,
                         ArrayRef<Arg> args) {
  Type voidTy = callee.getSubstResultType();
  const TypeInfo &voidTI = IGF.getFragileTypeInfo(voidTy);

  CallResult result;
  ::emitCall(IGF, callee, args, voidTI, result);
  assert(result.isDirect() && result.getDirectValues().empty());
}

/// Emit a nullary call to the given monomorphic function, using the
/// standard calling-convention and so on, and explode the result.
void IRGenFunction::emitNullaryCall(llvm::Value *fnPtr,
                                    Type resultType,
                                    Explosion &resultExplosion) {
  Type formalType = TupleType::getEmpty(IGM.Context);
  formalType = FunctionType::get(formalType, resultType, IGM.Context);

  Callee callee =
    Callee::forKnownFunction(AbstractCC::Freestanding,
                             formalType, resultType,
                             ArrayRef<Substitution>(),
                             fnPtr, ManagedValue(nullptr),
                             resultExplosion.getKind(),
                             /*uncurry level*/ 0);

  const TypeInfo &resultTI = getFragileTypeInfo(resultType);
  irgen::emitCall(*this, callee, Arg(), resultTI, resultExplosion);
}

/// Initialize an Explosion with the parameters of the current
/// function.  All of the objects will be added unmanaged.  This is
/// really only useful when writing prologue code.
Explosion IRGenFunction::collectParameters() {
  Explosion params(CurExplosionLevel);
  for (auto i = CurFn->arg_begin(), e = CurFn->arg_end(); i != e; ++i)
    params.addUnmanaged(i);
  return params;
}

OwnedAddress IRGenFunction::getAddrForParameter(VarDecl *param,
                                                Explosion &paramValues) {
  const TypeInfo &paramType = IGM.getFragileTypeInfo(param->getType());

  ExplosionSchema paramSchema(paramValues.getKind());
  paramType.getSchema(paramSchema);

  Twine name = param->getName().str();

  // If the parameter is byref, the next parameter is the value we
  // should use.
  if (param->getType()->is<LValueType>()) {
    llvm::Value *addr = paramValues.claimUnmanagedNext();
    addr->setName(name);

    llvm::Value *owner = IGM.RefCountedNull;
    if (param->getType()->castTo<LValueType>()->isHeap()) {
      owner = paramValues.claimUnmanagedNext();
      owner->setName(name + ".owner");
      enterReleaseCleanup(owner);
    }

    return OwnedAddress(Address(addr, paramType.StorageAlignment), owner);
  }

  OnHeap_t onHeap = param->hasFixedLifetime() ? NotOnHeap : OnHeap;

  // If the schema contains a single aggregate, assume we can
  // just treat the next parameter as that type.
  if (paramSchema.size() == 1 && paramSchema.begin()->isAggregate()) {
    llvm::Value *addr = paramValues.claimUnmanagedNext();
    addr->setName(name);
    addr = Builder.CreateBitCast(addr,
                    paramSchema.begin()->getAggregateType()->getPointerTo());
    Address paramAddr(addr, paramType.StorageAlignment);

    // If we don't locally need the variable on the heap, just use the
    // original address.
    if (!onHeap) {
      // Enter a cleanup to destroy the element.
      if (!paramType.isPOD(ResilienceScope::Local))
        enterDestroyCleanup(paramAddr, paramType);

      return OwnedAddress(paramAddr, IGM.RefCountedNull);
    }

    // Otherwise, we have to move it to the heap.
    Initialization paramInit;
    InitializedObject paramObj = paramInit.getObjectForDecl(param);
    paramInit.registerObject(*this, paramObj, OnHeap, paramType);

    OwnedAddress paramHeapAddr =
      paramInit.emitLocalAllocation(*this, paramObj, OnHeap, paramType,
                                    name + ".heap");

    // Do a 'take' initialization to directly transfer responsibility.
    paramType.initializeWithTake(*this, paramHeapAddr, paramAddr);
    paramInit.markInitialized(*this, paramObj);

    return paramHeapAddr;
  }

  // Otherwise, make an alloca and load into it.
  Initialization paramInit;
  InitializedObject paramObj = paramInit.getObjectForDecl(param);
  paramInit.registerObject(*this, paramObj, onHeap, paramType);

  OwnedAddress paramAddr =
    paramInit.emitLocalAllocation(*this, paramObj, onHeap, paramType,
                                  name + ".addr");

  // FIXME: This way of getting a list of arguments claimed by storeExplosion
  // is really ugly.
  auto storedStart = paramValues.begin();

  paramType.initialize(*this, paramValues, paramAddr);
  paramInit.markInitialized(*this, paramObj);

  // Set names for argument(s)
  for (auto i = storedStart, e = paramValues.begin(); i != e; ++i) {
    if (e - storedStart == 1)
      i->getValue()->setName(name);
    else
      i->getValue()->setName(name + "." + Twine(i - storedStart));
  }

  return paramAddr;
}

namespace {
  /// A recursive emitter for parameter patterns.
  class ParamPatternEmitter :
      public irgen::PatternVisitor<ParamPatternEmitter> {
    IRGenFunction &IGF;
    Explosion &Args;

  public:
    ParamPatternEmitter(IRGenFunction &IGF, Explosion &args)
      : IGF(IGF), Args(args) {}

    void visitTuplePattern(TuplePattern *tuple) {
      for (auto &field : tuple->getFields())
        visit(field.getPattern());
    }

    void visitNamedPattern(NamedPattern *pattern) {
      VarDecl *decl = pattern->getDecl();
      OwnedAddress addr = IGF.getAddrForParameter(decl, Args);

      // FIXME: heap byrefs.
      IGF.setLocalVar(decl, addr);
    }

    void visitAnyPattern(AnyPattern *pattern) {
      unsigned numIgnored =
        IGF.IGM.getExplosionSize(pattern->getType(), Args.getKind());
      Args.claim(numIgnored);
    }
  };
}

/// Emit a specific parameter clause.
static void emitParameterClause(IRGenFunction &IGF, AnyFunctionType *fnType,
                                Pattern *param, Explosion &args) {
  assert(param->getType()->isEqual(fnType->getInput()));

  // Emit the pattern.
  ParamPatternEmitter(IGF, args).visit(param);

  // If the function type at this level is polymorphic, bind all the
  // archetypes.
  if (auto polyFn = dyn_cast<PolymorphicFunctionType>(fnType))
    emitPolymorphicParameters(IGF, polyFn->getGenericParams(), args);
}

/// Emit all the parameter clauses of the given function type.  This
/// is basically making sure that we have mappings for all the
/// VarDecls bound by the pattern.
static void emitParameterClauses(IRGenFunction &IGF,
                                 Type type,
                                 llvm::ArrayRef<Pattern*> paramClauses,
                                 Explosion &args) {
  assert(!paramClauses.empty());

  AnyFunctionType *fnType = type->castTo<AnyFunctionType>();

  // When uncurrying, later argument clauses are emitted first.
  if (paramClauses.size() != 1)
    emitParameterClauses(IGF, fnType->getResult(), paramClauses.slice(1), args);

  // Finally, emit this clause.
  emitParameterClause(IGF, fnType, paramClauses[0], args);
}

/// Emit the prologue for the function.
void IRGenFunction::emitPrologue() {
  // Set up the IRBuilder.
  llvm::BasicBlock *EntryBB = createBasicBlock("entry");
  assert(CurFn->getBasicBlockList().empty() && "prologue already emitted?");
  CurFn->getBasicBlockList().push_back(EntryBB);
  Builder.SetInsertPoint(EntryBB);

  // Set up the alloca insertion point.
  AllocaIP = Builder.CreateAlloca(IGM.Int1Ty, /*array size*/ nullptr,
                                  "alloca point");

  // That's it for the 'bare' prologue.
  if (CurPrologue == Prologue::Bare)
    return;

  // Set up the return block and insert it.  This creates a second
  // insertion point that most blocks should be inserted before.
  ReturnBB = createBasicBlock("return");
  CurFn->getBasicBlockList().push_back(ReturnBB);

  // List out the parameter values in an Explosion.
  Explosion values = collectParameters();

  // Set up the return slot, stealing the first argument if necessary.
  {
    // Find the 'code' result type of this function.
    const TypeInfo &resultType = getResultTypeInfo();

    ExplosionSchema resultSchema(CurExplosionLevel);
    resultType.getSchema(resultSchema);

    if (resultSchema.requiresIndirectResult()) {
      ReturnSlot = Address(values.claimUnmanagedNext(),
                           resultType.StorageAlignment);
    } else if (resultSchema.empty()) {
      assert(!ReturnSlot.isValid());
    } else {
      // Prepare the return slot.  We intentionally do not create
      // a destroy cleanup, because the return slot doesn't really
      // work in the normal way.
      Initialization returnInit;
      auto returnObject = returnInit.getObjectForTemporary();
      returnInit.registerObjectWithoutDestroy(returnObject);

      // Allocate the slot and leave its allocation cleanup hanging
      // around.
      ReturnSlot = returnInit.emitLocalAllocation(*this, returnObject,
                                                  NotOnHeap, resultType,
                                                  "return_value");
    }
  }

  // Set up the parameters.
  auto params = CurFuncParamPatterns.slice(0, CurUncurryLevel + 1);
  emitParameterClauses(*this, CurFuncType, params, values);

  if (CurPrologue == Prologue::StandardWithContext) {
    ContextPtr = values.claimUnmanagedNext();
    ContextPtr->setName(".context");
    enterReleaseCleanup(ContextPtr);
  }

  assert(values.empty() && "didn't exhaust all parameters?");
}

/// Given an alloca, destroy it if its uses are all stores.
static void eraseAllocaIfOnlyStoredTo(llvm::AllocaInst *alloca) {
  for (auto i = alloca->use_begin(), e = alloca->use_end(); i != e; ++i) {
    // Check if this use is a store.
    llvm::StoreInst *store = dyn_cast<llvm::StoreInst>(*i);
    if (!store) return;
    assert(i.getOperandNo() == 1 && "address of alloca was taken");
  }

  // If we got here, all the uses are stores;  kill them.
  for (auto i = alloca->use_begin(), e = alloca->use_end(); i != e; ) {
    llvm::StoreInst *store = cast<llvm::StoreInst>(*i);
    ++i; // advance now to avoid being invalidated

    // TODO: maybe clean up the stored value?
    store->eraseFromParent();
  }

  alloca->eraseFromParent();
}

/// Emit the epilogue for the function.
void IRGenFunction::emitEpilogue() {
  // Leave the cleanups created for the parameters if we've got a full
  // prologue.
  if (CurPrologue != Prologue::Bare)
    endScope(Cleanups.stable_end());

  // Destroy the alloca insertion point.
  AllocaIP->eraseFromParent();

  // That's it for the 'bare' epilogue.
  if (CurPrologue == Prologue::Bare)
    return;

  // If there are no edges to the return block, we never want to emit it.
  if (ReturnBB->use_empty()) {
    ReturnBB->eraseFromParent();

    // Normally this means that we'll just insert the epilogue in the
    // current block, but if the current IP is unreachable then so is
    // the entire epilogue.
    if (!Builder.hasValidIP()) return;

  // Otherwise, branch to it if the current IP is reachable.
  } else if (Builder.hasValidIP()) {
    Builder.CreateBr(ReturnBB);
    Builder.SetInsertPoint(ReturnBB);

  // Otherwise, if there is exactly one use of the return block, merge
  // it into its predecessor.
  } else if (ReturnBB->hasOneUse()) {
    // return statements are never emitted as conditional branches.
    llvm::BranchInst *Br = cast<llvm::BranchInst>(*ReturnBB->use_begin());
    assert(Br->isUnconditional());
    Builder.SetInsertPoint(Br->getParent());
    Br->eraseFromParent();
    ReturnBB->eraseFromParent();

  // Otherwise, just move the IP to the return block.
  } else {
    Builder.SetInsertPoint(ReturnBB);
  }

  const TypeInfo &resultType = getResultTypeInfo();
  ExplosionSchema resultSchema(CurExplosionLevel);
  resultType.getSchema(resultSchema);

  if (resultSchema.requiresIndirectResult()) {
    assert(isa<llvm::Argument>(ReturnSlot.getAddress()));
    Builder.CreateRetVoid();
  } else if (resultSchema.empty()) {
    assert(!ReturnSlot.isValid());
    Builder.CreateRetVoid();
  } else {
    Explosion result(CurExplosionLevel);
    resultType.loadAsTake(*this, ReturnSlot, result);
    emitScalarReturn(result);
  }

  // Destroy the unreachable block if it's unused.
  if (UnreachableBB && UnreachableBB->use_empty())
    UnreachableBB->eraseFromParent();

  // Destroy the jump-destination slot if it's unused.
  // TODO: also destroy it if it's only used for stores.
  if (JumpDestSlot)
    eraseAllocaIfOnlyStoredTo(cast<llvm::AllocaInst>(JumpDestSlot));
}

void IRGenFunction::emitScalarReturn(Explosion &result) {
  if (result.size() == 0) {
    Builder.CreateRetVoid();
  } else if (result.size() == 1) {
    Builder.CreateRet(result.forwardNext(*this));
  } else {
    assert(cast<llvm::StructType>(CurFn->getReturnType())->getNumElements()
             == result.size());
    llvm::Value *resultAgg = llvm::UndefValue::get(CurFn->getReturnType());
    for (unsigned i = 0, e = result.size(); i != e; ++i) {
      llvm::Value *elt = result.forwardNext(*this);
      resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
    }
    Builder.CreateRet(resultAgg);
  }
}

namespace {
  class CurriedData {
    IRGenModule &IGM;
    FuncExpr *Func;
    ExplosionKind ExplosionLevel;
    unsigned CurClause;

    /// The TypeInfos for all the parameters, in the standard
    /// reversed-clause order.  To make certain optimizations easier,
    /// only non-empty types are listed.  Concatenating the explosion
    /// schemas of these types would give us the signature of the
    /// function.
    llvm::SmallVector<const TypeInfo *, 8> AllDataTypes;

    struct Clause {
      unsigned DataTypesBeginIndex;
      Type ForwardingFnType;
    };

    /// The clauses of the function that we're actually going to curry.
    llvm::SmallVector<Clause, 4> Clauses;

  public:
    CurriedData(IRGenModule &IGM, FuncExpr *funcExpr,
                ExplosionKind explosionLevel,
                unsigned minUncurryLevel,
                unsigned maxUncurryLevel)
      : IGM(IGM), Func(funcExpr), ExplosionLevel(explosionLevel),
        CurClause(minUncurryLevel) {
      accumulateClauses(funcExpr->getType(), maxUncurryLevel);
    }

    void emitCurriedEntrypoint(llvm::Function *entrypoint,
                               llvm::Function *nextEntrypoint) {
      // We need to fill in a function of this type:
      //   (A) -> (B -> C)
      // Therefore we need to store all the values in A (no matter how
      // many uncurried arguments they came from) and return a pointer
      // to a function which will expand them back out.

      // TODO: future optimization: if we have an intermediate
      // currying which adds no data, re-use the old data pointer
      // instead of unpacking and repacking.

      // TODO: future optimization: if the only data value is a a
      // single retainable object pointer, don't do a layout.

      // Compute the layout of the data we have now.
      llvm::ArrayRef<const TypeInfo *> dataTypes = AllDataTypes;
      dataTypes = dataTypes.slice(Clauses[CurClause].DataTypesBeginIndex);
      HeapLayout layout(IGM, LayoutStrategy::Optimal, dataTypes);

      // Create an internal function to serve as the forwarding
      // function (B -> C).
      llvm::Function *forwarder =
        getAddrOfForwardingStub(nextEntrypoint, /*hasData*/ !layout.empty());

      // Emit the curried entrypoint.
      emitCurriedEntrypointBody(entrypoint, forwarder, layout);

      // Emit the forwarding stub.
      emitCurriedForwarderBody(forwarder, nextEntrypoint, layout);

      CurClause++;
    }

  private:
    /// Decompose all the argument types in the proper order.
    /// This leaves AllDataTypes containing all the types in
    /// the fully-uncurried function type in clause-reversed
    /// order and DataTypesStart containing a reversed stack of
    /// indexes at which to start.
    /// 
    /// For example, for these inputs:
    ///   fnType = (A, B) -> (C, D) -> (E) -> (F) -> G
    ///   maxUncurryLevel = 2
    /// we will have these results:
    ///   AllDataTypes = [E, C, D, A, B]
    ///   DataTypesStart = [ 0, 2, 3 ]
    void accumulateClauses(Type fnType, unsigned maxUncurryLevel) {
      if (maxUncurryLevel == 0) return;

      unsigned clauseIndex = Clauses.size();
      Clauses.push_back(Clause());

      AnyFunctionType *fn = cast<AnyFunctionType>(fnType);
      accumulateClauses(fn->getResult(), maxUncurryLevel - 1);

      Clauses[clauseIndex].DataTypesBeginIndex = AllDataTypes.size();
      Clauses[clauseIndex].ForwardingFnType = fn->getResult();

      assert(!isa<PolymorphicFunctionType>(fn) && "not implemented!");
      accumulateParameterDataTypes(fn->getInput());
    }

    /// Accumulate the given parameter type.
    void accumulateParameterDataTypes(Type ty) {
      // As an optimization, expand tuples instead of grabbing their TypeInfo.
      if (TupleType *tuple = ty->getAs<TupleType>()) {
        for (const TupleTypeElt &field : tuple->getFields())
          accumulateParameterDataTypes(field.getType());
        return;
      }

      // Add data for an individual type unless it's known to be empty.
      // This is for layout local to this tunit, so we can use our
      // full knowledge.
      const TypeInfo &type = IGM.getFragileTypeInfo(ty);
      if (!type.isEmpty(ResilienceScope::Local))
        AllDataTypes.push_back(&type);
    }

    /// Create a forwarding stub.
    llvm::Function *getAddrOfForwardingStub(llvm::Function *nextEntrypoint,
                                            bool hasData) {
      llvm::FunctionType *fnType =
        IGM.getFunctionType(Clauses[CurClause].ForwardingFnType,
                            ExplosionLevel, /*uncurry*/ 0, hasData);

      // Create the function and place it immediately before the next stub.
      llvm::Function *forwarder =
        llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                               nextEntrypoint->getName() + ".curry");
      forwarder->setCallingConv(getFreestandingConvention(IGM));
      IGM.Module.getFunctionList().insert(nextEntrypoint, forwarder);

      return forwarder;
    }

    /// Emit the body of a curried entrypoint, a function of type:
    ///   (A, B) -> (C -> D)
    /// which returns a function of type (C -> D) by allocating
    /// an AB_stored_t, copying its parameters into that, and returning
    /// a function value consisting of the pointer to the forwarder
    /// stub and the allocated data.
    void emitCurriedEntrypointBody(llvm::Function *entrypoint,
                                   llvm::Function *forwarder,
                                   const HeapLayout &layout) {
      PrettyStackTraceExpr stackTrace(IGM.Context,
                                      "emitting IR for curried entrypoint to",
                                      Func);

      IRGenFunction IGF(IGM, Func->getType(), Func->getParamPatterns(),
                        ExplosionLevel, CurClause, entrypoint, Prologue::Bare);

      Explosion params = IGF.collectParameters();

      // We're returning a function, so no need to worry about an
      // aggregate return slot.

      // Compute a data object.
      llvm::Value *data;
      if (layout.empty()) {
        data = IGM.RefCountedNull;
      } else {
        // Allocate a new object.  FIXME: if this can throw, we need to
        // do a lot of setup beforehand.
        data = IGF.emitUnmanagedAlloc(layout, "data");

        Address dataAddr = layout.emitCastOfAlloc(IGF, data);

        // Perform the store.
        for (auto &fieldLayout : layout.getElements()) {
          Address fieldAddr = fieldLayout.project(IGF, dataAddr);
          fieldLayout.Type->initialize(IGF, params, fieldAddr);
        }
      }

      // Build the function result.
      llvm::Value *result = llvm::UndefValue::get(IGM.FunctionPairTy);
      result = IGF.Builder.CreateInsertValue(result,
                      llvm::ConstantExpr::getBitCast(forwarder, IGM.Int8PtrTy),
                                             0);
      result = IGF.Builder.CreateInsertValue(result, data, 1);

      // Return that.
      IGF.Builder.CreateRet(result);
    }

    /// Emit the body of a forwarder stub, a function of type:
    ///   (C -> D)
    /// which accepts an implicit extra data parameter holding
    /// all the previous parameters, and which produces a D by
    /// adding all those parameters to the C parameters just
    /// receiver, then tail-calling the next entrypoint in
    /// the sequence.
    void emitCurriedForwarderBody(llvm::Function *forwarder,
                                  llvm::Function *nextEntrypoint,
                                  const HeapLayout &layout) {
      PrettyStackTraceExpr stackTrace(IGM.Context,
                                      "emitting IR for currying forwarder of",
                                      Func);

      IRGenFunction IGF(IGM, Func->getType(), Func->getParamPatterns(),
                        ExplosionLevel, CurClause, forwarder, Prologue::Bare);

      // Accumulate the function's immediate parameters.
      Explosion params = IGF.collectParameters();

      // If there's a data pointer required, grab it (it's always the
      // last parameter) and load out the extra, previously-curried
      // parameters.
      if (!layout.empty()) {
        llvm::Value *rawData = params.takeLast().getUnmanagedValue();
        Address data = layout.emitCastOfAlloc(IGF, rawData);

        // Perform the loads.
        for (auto &fieldLayout : layout.getElements()) {
          Address fieldAddr = fieldLayout.project(IGF, data);
          fieldLayout.Type->load(IGF, fieldAddr, params);
        }

        // Kill the allocated data pointer immediately.  The safety of
        // this assumes that neither this release nor any of the loads
        // can throw.
        IGF.emitRelease(rawData);
      }

      llvm::SmallVector<llvm::Value*, 8> args;
      params.forward(IGF, params.size(), args);

      llvm::CallSite callSite =
        IGF.emitInvoke(nextEntrypoint->getCallingConv(), nextEntrypoint, args,
                       forwarder->getAttributes());

      llvm::CallInst *call = cast<llvm::CallInst>(callSite.getInstruction());
      call->setTailCall();

      if (call->getType()->isVoidTy()) {
        IGF.Builder.CreateRetVoid();
      } else {
        IGF.Builder.CreateRet(call);
      }
    }
  };
}

/// Emit a function declaration, starting at the given uncurry level.
static void emitFunction(IRGenModule &IGM, FuncDecl *func,
                         unsigned startingUncurryLevel) {
  // Nothing to do if the function has no body.
  if (!func->getBody()->getBody()) return;
  FuncExpr *funcExpr = func->getBody();

  // FIXME: support defining currying entrypoints for local functions.
  bool needsData = false;

  // FIXME: variant currying levels!
  // FIXME: also emit entrypoints with maximal explosion when all types are known!
  unsigned naturalUncurryLevel = getNaturalUncurryLevel(func);
  assert(startingUncurryLevel <= naturalUncurryLevel);

  ExplosionKind explosionLevel = ExplosionKind::Minimal;

  // Get the address of the first entrypoint we're going to emit.
  llvm::Function *entrypoint;
  if (Decl *var = func->getGetterDecl()) {
    entrypoint = IGM.getAddrOfGetter(cast<ValueDecl>(var), explosionLevel);
  } else if (Decl *var = func->getSetterDecl()) {
    entrypoint = IGM.getAddrOfSetter(cast<ValueDecl>(var), explosionLevel);
  } else {
    entrypoint = IGM.getAddrOfFunction(func, explosionLevel,
                                       startingUncurryLevel, needsData);
  }

  CurriedData curriedData(IGM, funcExpr, explosionLevel,
                          startingUncurryLevel, naturalUncurryLevel);

  // Emit the curried entrypoints.  At the end of each iteration,
  // fnAddr will point to the next entrypoint in the currying sequence.
  for (unsigned uncurryLevel = startingUncurryLevel;
         uncurryLevel != naturalUncurryLevel; ++uncurryLevel) {
    llvm::Function *nextEntrypoint =
      IGM.getAddrOfFunction(func, explosionLevel, uncurryLevel + 1,
                            needsData);

    curriedData.emitCurriedEntrypoint(entrypoint, nextEntrypoint);

    entrypoint = nextEntrypoint;
  }

  // Finally, emit the uncurried entrypoint.
  PrettyStackTraceDecl stackTrace("emitting IR for", func);
  IRGenFunction(IGM, funcExpr->getType(), funcExpr->getParamPatterns(),
                explosionLevel, naturalUncurryLevel, entrypoint)
    .emitFunctionTopLevel(funcExpr->getBody());
}

/// Emit the definition for the given instance method.
void IRGenModule::emitInstanceMethod(FuncDecl *func) {
  assert(!func->isStatic());
  emitFunction(*this, func, 1);
}

/// Emit the definition for the given static method.
void IRGenModule::emitStaticMethod(FuncDecl *func) {
  assert(func->isStatic());
  emitFunction(*this, func, 0);
}

/// Emit the definition for the given global function.
void IRGenModule::emitGlobalFunction(FuncDecl *func) {
  emitFunction(*this, func, 0);
}

/// Emit the code for the top-level of a function.
void IRGenFunction::emitFunctionTopLevel(BraceStmt *S) {
  emitBraceStmt(S);
}

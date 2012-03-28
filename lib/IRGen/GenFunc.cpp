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
#include "llvm/Module.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "Explosion.h"
#include "StructLayout.h"

#include "GenFunc.h"

using namespace swift;
using namespace irgen;

/// Return the number of potential curries of this function type.
/// This is equal to the number of "straight-line" arrows in the type.
static unsigned getNumCurries(FunctionType *type) {
  unsigned count = 0;
  do {
    count++;
    type = type->getResult()->getAs<FunctionType>();
  } while (type);

  return count;
}

/// Return the natural level at which to uncurry this function.  This
/// is the number of additional parameter clauses that are uncurried
/// in the function body.
static unsigned getNaturalUncurryLevel(FuncDecl *func) {
  if (func->getBody())
    return func->getBody()->getParamPatterns().size() - 1;

  FunctionType *type = func->getType()->castTo<FunctionType>();
  unsigned count = 0;
  do {
    count++;
    type = dyn_cast<FunctionType>(type->getResult());
  } while (type);

  assert(count <= getNumCurries(func->getType()->castTo<FunctionType>()));
  return count - 1;
}

/// Given a function type, return the formal result type at the given
/// uncurrying level.  For 'a -> b -> c', this is 'b' at 0 and 'c' at 1.
static Type getResultType(Type type, unsigned uncurryLevel) {
  do {
    type = type->castTo<FunctionType>()->getResult();
  } while (uncurryLevel--);
  return type;
}

const TypeInfo &IRGenFunction::getResultTypeInfo() const {
  Type resultType = getResultType(CurFuncType, CurUncurryLevel);
  return IGM.getFragileTypeInfo(resultType);
}

namespace {
  /// The natural form of the result of performing a call.  A call
  /// result may be indirect, in which case it is returned in memory
  /// whose address is passed as an implicit first argument, or it may
  /// be direct.
  struct CallResult {
  private:
    llvm::Value *DirectValues[ExplosionSchema::MaxScalarsForDirectResult];
    unsigned char NumDirectValues;
  public:
    ExplosionKind DirectExplosionLevel;

    /// The address into which to emit an indirect call.  If this is
    /// set, the call will be evaluated (as an initialization) into
    /// this address; otherwise, memory will be allocated on the stack.
    Address IndirectAddress;

    CallResult() : NumDirectValues(0) {}

    void addDirectValue(llvm::Value *value) {
      assert(NumDirectValues < ExplosionSchema::MaxScalarsForDirectResult);
      DirectValues[NumDirectValues++] = value;
    }
    llvm::Value *getDirectValue(unsigned i) const {
      assert(i < NumDirectValues);
      return DirectValues[i];
    }
    void clearDirectValues() {
      NumDirectValues = 0;
    }
    ArrayRef<llvm::Value*> getDirectValues() const {
      return ArrayRef<llvm::Value*>(DirectValues, DirectValues+NumDirectValues);
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

  /// The type-info class
  class FuncTypeInfo : public TypeInfo {
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
    FunctionType * const FormalType;

    /// An array of Curryings is stored immediately after the FuncTypeInfo.
    /// A Currying is a cache, so the entire thing is effective mutable.
    Currying *getCurryingsBuffer() const {
      return const_cast<Currying*>(reinterpret_cast<const Currying*>(this+1));
    }

    FuncTypeInfo(FunctionType *formalType, llvm::StructType *storageType,
                 Size size, Alignment align, unsigned numCurries)
      : TypeInfo(storageType, size, align), FormalType(formalType) {
      
      // Initialize the curryings.
      for (unsigned i = 0; i != numCurries; ++i) {
        new (&getCurryingsBuffer()[i]) Currying();
      }
    }

  public:
    static const FuncTypeInfo *create(FunctionType *formalType,
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
      e.add(IGF.Builder.CreateLoad(fnAddr, fnAddr->getName() + ".load"));

      // Load the data.
      Address dataAddr = projectData(IGF, address);
      IGF.emitLoadAndRetain(dataAddr, e);
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the function pointer.
      Address fnAddr = projectFunction(IGF, address);
      IGF.Builder.CreateStore(e.claimNext(), fnAddr);

      // Store the data pointer.
      Address dataAddr = projectData(IGF, address);
      IGF.emitAssignRetained(e.claimNext(), dataAddr);
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the function pointer.
      Address fnAddr = projectFunction(IGF, address);
      IGF.Builder.CreateStore(e.claimNext(), fnAddr);

      // Store the data pointer, transferring the +1.
      Address dataAddr = projectData(IGF, address);
      IGF.emitInitializeRetained(e.claimNext(), dataAddr);
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      dest.add(src.claimNext());
      dest.add(src.claimNext());
    }
  };
}

const TypeInfo *
TypeConverter::convertFunctionType(IRGenModule &IGM, FunctionType *T) {
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
/// This is all somewhat optimized for register-passing CCs; it
/// probably makes extra work when the stack gets involved.
static Type decomposeFunctionType(IRGenModule &IGM, FunctionType *fn,
                                  ExplosionKind explosionKind,
                                  unsigned uncurryLevel,
                                  SmallVectorImpl<llvm::Type*> &argTypes) {
  // Save up the formal parameter types in reverse order.
  llvm::SmallVector<Type, 8> formalArgTypes(uncurryLevel + 1);
  formalArgTypes[uncurryLevel] = fn->getInput();
  while (uncurryLevel--) {
    fn = fn->getResult()->castTo<FunctionType>();
    formalArgTypes[uncurryLevel] = fn->getInput();
  }

  // Explode the argument clusters in that reversed order.
  for (Type type : formalArgTypes) {
    ExplosionSchema schema(explosionKind);
    IGM.getSchema(type, schema);

    for (ExplosionSchema::Element &elt : schema) {
      if (elt.isAggregate())
        argTypes.push_back(elt.getAggregateType()->getPointerTo());
      else
        argTypes.push_back(elt.getScalarType());
    }
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
      llvm::SmallVector<llvm::Type*, ExplosionSchema::MaxScalarsForDirectResult> elts;
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
  assert(type->is<FunctionType>());
  const FuncTypeInfo &fnTypeInfo = getFragileTypeInfo(type).as<FuncTypeInfo>();
  Signature sig = fnTypeInfo.getSignature(*this, explosionKind,
                                          curryingLevel, withData);
  return sig.getType();
}

namespace {
  struct Callee {
    /// The explosion level of the function to call.
    ExplosionKind ExplosionLevel;

    /// The uncurry level of the function to call.
    unsigned UncurryLevel;

  private:
    /// The function to call.
    llvm::Value *FnPtr;

    /// The data pointer to pass, if required.  Null otherwise.
    llvm::Value *DataPtr;

  public:
    void set(llvm::Value *fnPtr, llvm::Value *dataPtr) {
      assert(fnPtr->getType()->getContainedType(0)->isFunctionTy() ||
             fnPtr->getType()->getContainedType(0)->isIntegerTy(8));
      assert(dataPtr == nullptr ||
             (dataPtr->getType()->getContainedType(0)->isStructTy() &&
              cast<llvm::StructType>(dataPtr->getType()->getContainedType(0))
                ->getName() == "swift.refcounted"));
      assert(dataPtr == nullptr || !isa<llvm::ConstantPointerNull>(dataPtr));

      FnPtr = fnPtr;
      DataPtr = dataPtr;
    }

    void setForIndirectCall(llvm::Value *fn, llvm::Value *data) {
      ExplosionLevel = ExplosionKind::Minimal;
      UncurryLevel = 0;
      set(fn, isa<llvm::ConstantPointerNull>(data) ? nullptr : data);
    }

    llvm::Value *getOpaqueFunctionPointer(IRGenFunction &IGF) const {
      return IGF.Builder.CreateBitCast(FnPtr, IGF.IGM.Int8PtrTy);
    }

    llvm::Type *getFunctionPointerType(IRGenModule &IGM, Type type) const {
      return IGM.getFunctionType(type, ExplosionLevel, UncurryLevel,
                                 hasDataPointer())->getPointerTo();
    }

    llvm::Value *getFunctionPointer(IRGenFunction &IGF, Type type) const {
      if (FnPtr->getType() != IGF.IGM.Int8PtrTy) {
        assert(FnPtr->getType() == getFunctionPointerType(IGF.IGM, type));
        return FnPtr;
      }
      return IGF.Builder.CreateBitCast(FnPtr,
                                       getFunctionPointerType(IGF.IGM, type));
    }

    bool hasDataPointer() const { return DataPtr != nullptr; }

    llvm::Value *getDataPointer(IRGenModule &IGM) const {
      if (DataPtr) return DataPtr;
      return IGM.RefCountedNull;
    }
  };
}

/// Emit a reference to a function, using the best parameters possible
/// up to given limits.
static Callee emitCallee(IRGenModule &IGM, FuncDecl *fn,
                         ExplosionKind bestExplosion, unsigned bestUncurry) {
  // Use the apparent natural uncurrying level of a function as a
  // maximum on the uncurrying to do.
  if (bestUncurry != 0)
    bestUncurry = std::min(bestUncurry, getNaturalUncurryLevel(fn));

  // TODO: be less conservative
  bestExplosion = ExplosionKind::Minimal;

  Callee callee;
  callee.UncurryLevel = bestUncurry;
  callee.ExplosionLevel = bestExplosion;

  if (!fn->getDeclContext()->isLocalContext()) {
    callee.set(IGM.getAddrOfGlobalFunction(fn, bestExplosion, bestUncurry),
               nullptr);
  } else {
    IGM.unimplemented(fn->getLocStart(), "local function emission");
    llvm::Value *undef = llvm::UndefValue::get(IGM.Int8PtrTy);
    callee.set(undef, nullptr);
  }

  return callee;
}

/// Emit a reference to the given function as a generic function pointer.
void swift::irgen::emitRValueForFunction(IRGenFunction &IGF, FuncDecl *fn,
                                         Explosion &explosion) {
  // Function pointers are always fully curried and use ExplosionKind::Minimal.
  Callee callee = emitCallee(IGF.IGM, fn, ExplosionKind::Minimal, 0);
  assert(callee.ExplosionLevel == ExplosionKind::Minimal);
  assert(callee.UncurryLevel == 0);
  explosion.add(callee.getOpaqueFunctionPointer(IGF));
  explosion.add(callee.getDataPointer(IGF.IGM));
}

namespace {
  struct ArgList {
    ArgList(ExplosionKind kind) : Values(kind) {}

    Explosion Values;
    llvm::SmallVector<llvm::AttributeWithIndex, 4> Attrs;
  };
}

/// emitBuiltinCall - Emit a call to a builtin function.
static void emitBuiltinCall(IRGenFunction &IGF, FuncDecl *Fn,
                            Expr *Arg, CallResult &result) {
  // Emit the arguments.  Maybe we'll get builtins that are more
  // complex than this.
  ArgList args(ExplosionKind::Minimal);
  IGF.emitRValue(Arg, args.Values);

  Type BuiltinType;
  switch (isBuiltinValue(IGF.IGM.Context, Fn->getName().str(), BuiltinType)) {
  case BuiltinValueKind::None: llvm_unreachable("not a builtin after all!");

/// A macro which expands to the emission of a simple unary operation
/// or predicate.
#define UNARY_OPERATION(Op) {                                               \
    llvm::Value *op = args.Values.claimNext();                              \
    assert(args.Values.empty() && "wrong operands to unary operation");     \
    return result.addDirectValue(IGF.Builder.Create##Op(op));               \
  }

/// A macro which expands to the emission of a simple binary operation
/// or predicate.
#define BINARY_OPERATION(Op) {                                              \
    llvm::Value *lhs = args.Values.claimNext();                             \
    llvm::Value *rhs = args.Values.claimNext();                             \
    assert(args.Values.empty() && "wrong operands to binary operation");    \
    return result.addDirectValue(IGF.Builder.Create##Op(lhs, rhs));         \
  }

/// A macro which expands to the emission of a simple binary operation
/// or predicate defined over both floating-point and integer types.
#define BINARY_ARITHMETIC_OPERATION(IntOp, FPOp) {                          \
    llvm::Value *lhs = args.Values.claimNext();                             \
    llvm::Value *rhs = args.Values.claimNext();                             \
    assert(args.Values.empty() && "wrong operands to binary operation");    \
    if (lhs->getType()->isFloatingPointTy()) {                              \
      return result.addDirectValue(IGF.Builder.Create##FPOp(lhs, rhs));     \
    } else {                                                                \
      return result.addDirectValue(IGF.Builder.Create##IntOp(lhs, rhs));    \
    }                                                                       \
  }

  case BuiltinValueKind::Neg:       UNARY_OPERATION(Neg)
  case BuiltinValueKind::Not:       UNARY_OPERATION(Not)
  case BuiltinValueKind::Add:       BINARY_ARITHMETIC_OPERATION(Add, FAdd)
  case BuiltinValueKind::And:       BINARY_OPERATION(And)
  case BuiltinValueKind::FDiv:      BINARY_OPERATION(FDiv)
  case BuiltinValueKind::Mul:       BINARY_ARITHMETIC_OPERATION(Mul, FMul)
  case BuiltinValueKind::Or:        BINARY_OPERATION(Or)
  case BuiltinValueKind::SDiv:      BINARY_OPERATION(SDiv)
  case BuiltinValueKind::SDivExact: BINARY_OPERATION(ExactSDiv)
  case BuiltinValueKind::SRem:      BINARY_OPERATION(SRem)
  case BuiltinValueKind::Sub:       BINARY_ARITHMETIC_OPERATION(Sub, FSub)
  case BuiltinValueKind::UDiv:      BINARY_OPERATION(UDiv)
  case BuiltinValueKind::UDivExact: BINARY_OPERATION(ExactUDiv)
  case BuiltinValueKind::URem:      BINARY_OPERATION(URem)
  case BuiltinValueKind::Xor:       BINARY_OPERATION(Xor)
  case BuiltinValueKind::CmpEQ:     BINARY_OPERATION(ICmpEQ)
  case BuiltinValueKind::CmpNE:     BINARY_OPERATION(ICmpNE)
  case BuiltinValueKind::CmpSLE:    BINARY_OPERATION(ICmpSLE)
  case BuiltinValueKind::CmpSLT:    BINARY_OPERATION(ICmpSLT)
  case BuiltinValueKind::CmpSGE:    BINARY_OPERATION(ICmpSGE)
  case BuiltinValueKind::CmpSGT:    BINARY_OPERATION(ICmpSGT)
  case BuiltinValueKind::CmpULE:    BINARY_OPERATION(ICmpULE)
  case BuiltinValueKind::CmpULT:    BINARY_OPERATION(ICmpULT)
  case BuiltinValueKind::CmpUGE:    BINARY_OPERATION(ICmpUGE)
  case BuiltinValueKind::CmpUGT:    BINARY_OPERATION(ICmpUGT)
  case BuiltinValueKind::FCmpOEQ:   BINARY_OPERATION(FCmpOEQ)
  case BuiltinValueKind::FCmpOGT:   BINARY_OPERATION(FCmpOGT)
  case BuiltinValueKind::FCmpOGE:   BINARY_OPERATION(FCmpOGE)
  case BuiltinValueKind::FCmpOLT:   BINARY_OPERATION(FCmpOLT)
  case BuiltinValueKind::FCmpOLE:   BINARY_OPERATION(FCmpOLE)
  case BuiltinValueKind::FCmpONE:   BINARY_OPERATION(FCmpONE)
  case BuiltinValueKind::FCmpORD:   BINARY_OPERATION(FCmpORD)
  case BuiltinValueKind::FCmpUEQ:   BINARY_OPERATION(FCmpUEQ)
  case BuiltinValueKind::FCmpUGT:   BINARY_OPERATION(FCmpUGT)
  case BuiltinValueKind::FCmpUGE:   BINARY_OPERATION(FCmpUGE)
  case BuiltinValueKind::FCmpULT:   BINARY_OPERATION(FCmpULT)
  case BuiltinValueKind::FCmpULE:   BINARY_OPERATION(FCmpULE)
  case BuiltinValueKind::FCmpUNE:   BINARY_OPERATION(FCmpUNE)
  case BuiltinValueKind::FCmpUNO:   BINARY_OPERATION(FCmpUNO)
  }
  llvm_unreachable("bad builtin kind!");
}

namespace {
  /// A single call site, with argument expression and the type of
  /// function being applied.
  struct CallSite {
    CallSite(Expr *arg, Type fnType) : Arg(arg), FnType(fnType) {}

    Expr *Arg;
    Type FnType;
  };

  /// A holistic plan for performing a call.
  struct CallPlan {
    /// All the call sites we're going to evaluate.  Note that the
    /// call sites are in reversed order of application, i.e. the
    /// first site is the last call which will logically be performed.
    llvm::SmallVector<CallSite, 8> CallSites;
    Expr *NontrivialFn;
    FuncDecl *KnownFn;

    /// getFinalResultExplosionLevel - Returns the explosion level at
    /// which we will naturally emit the last call.
    ExplosionKind getFinalResultExplosionLevel(IRGenModule &IGM) const {
      // If we don't have a known function, we have to use
      // indirect-call rules.
      if (!KnownFn) return ExplosionKind::Minimal;

      Callee callee = emitCallee(IGM, KnownFn, ExplosionKind::Maximal,
                                 CallSites.size() - 1);
      return callee.ExplosionLevel;
    }

    void emit(IRGenFunction &IGF, CallResult &result,
              const TypeInfo &resultType);
  };
}

/// Given a function application, try to form an uncurried call.  If
/// successful, argExprs contains all the arguments applied, but in
/// reversed order.
static Expr *uncurry(ApplyExpr *E, SmallVectorImpl<CallSite> &callSites) {
  callSites.push_back(CallSite(E->getArg(),  E->getFn()->getType()));
  Expr *fnExpr = E->getFn()->getSemanticsProvidingExpr();
  if (ApplyExpr *fnApply = dyn_cast<ApplyExpr>(fnExpr))
    return uncurry(fnApply, callSites);
  return fnExpr;
}

static void decomposeFunctionReference(CallPlan &plan, DeclRefExpr *E) {
  plan.KnownFn = dyn_cast<FuncDecl>(E->getDecl());
  plan.NontrivialFn = (plan.KnownFn ? nullptr : E);
}

/// Try to decompose a function reference into a known function
/// declaration.
static void decomposeFunctionReference(CallPlan &plan, Expr *E) {
  E = E->getSemanticsProvidingExpr();
  if (DeclRefExpr *declRef = dyn_cast<DeclRefExpr>(E))
    return decomposeFunctionReference(plan, declRef);
  if (DotSyntaxBaseIgnoredExpr *baseIgnored
        = dyn_cast<DotSyntaxBaseIgnoredExpr>(E)) {
    decomposeFunctionReference(plan, baseIgnored->getRHS());
    plan.NontrivialFn = E; // overwrite
    return;
  }

  plan.KnownFn = nullptr;
  plan.NontrivialFn = E;
}

/// Compute the plan for performing a sequence of call expressions.
static CallPlan getCallPlan(IRGenModule &IGM, ApplyExpr *E) {
  CallPlan plan;

  decomposeFunctionReference(plan, uncurry(E, plan.CallSites));
  return plan;
}

/// Set attributes on the given call site consistent with it returning
/// an aggregate result.
static void setAggResultAttributes(llvm::CallSite call) {
  llvm::SmallVector<llvm::AttributeWithIndex, 1> attrs;
  attrs.push_back(llvm::AttributeWithIndex::get(1,
                                llvm::Attribute::StructRet |
                                llvm::Attribute::NoAlias));
  call.setAttributes(llvm::AttrListPtr::get(attrs.data(), attrs.size()));
}

/// Emit a function call.
void CallPlan::emit(IRGenFunction &IGF, CallResult &result,
                    const TypeInfo &resultType) {
  // 1.  Emit the function expression.
  Callee callee;

  // We can do a lot if we know we're calling a known function.
  if (KnownFn) {
    // Go ahead and emit the nontrivial function expression if we found one.
    if (NontrivialFn) IGF.emitIgnored(NontrivialFn);

    // Handle calls to builtin functions.  These are never curried, but they
    // might return a function pointer.
    if (isa<BuiltinModule>(KnownFn->getDeclContext())) {
      emitBuiltinCall(IGF, KnownFn, CallSites.back().Arg, result);

      // If there are no trailing calls, we're done.
      if (CallSites.size() == 1) return;

      // Otherwise, pop that call site off and set up the callee conservatively.
      callee.setForIndirectCall(result.getDirectValue(0),
                                result.getDirectValue(1));
      result.clearDirectValues();

    // Otherwise, compute information about the function we're calling.
    } else {
      callee = emitCallee(IGF.IGM, KnownFn, ExplosionKind::Maximal,
                          CallSites.size() - 1);
    }

  // Otherwise, emit as a function pointer and use the pessimistic
  // rules for calling such.
  } else {
    Explosion fnValues(ExplosionKind::Maximal);
    IGF.emitRValue(NontrivialFn, fnValues);
    llvm::Value *fnPtr = fnValues.claimNext();
    llvm::Value *dataPtr = fnValues.claimNext();
    callee.setForIndirectCall(fnPtr, dataPtr);
  }

  // 3. Emit arguments and call.
  while (true) {
    assert(callee.UncurryLevel < CallSites.size());

    // Find the formal type we're calling.
    unsigned calleeIndex = CallSites.size() - 1;
    Type calleeFormalType = CallSites[calleeIndex].FnType;
    llvm::Value *fnPtr =
      callee.getFunctionPointer(IGF, calleeFormalType);
    llvm::FunctionType *calleeType = 
      cast<llvm::FunctionType>(cast<llvm::PointerType>(fnPtr->getType())
                                 ->getElementType());

    SmallVector<llvm::Value*, 16> args(calleeType->getNumParams());
    unsigned lastArgWritten = calleeType->getNumParams();

    // Add the data pointer in.
    if (callee.hasDataPointer())
      args[--lastArgWritten] = callee.getDataPointer(IGF.IGM);

    // Emit all of the arguments we need to pass here.
    for (unsigned i = callee.UncurryLevel + 1; i != 0; --i) {
      Expr *arg = CallSites.back().Arg;
      CallSites.pop_back();

      // Emit the argument, exploded at the appropriate level.
      Explosion argExplosion(callee.ExplosionLevel);
      IGF.emitRValue(arg, argExplosion);

      assert(lastArgWritten >= argExplosion.size());
      lastArgWritten -= argExplosion.size();

      // Now copy that into place in the argument list.
      std::copy(argExplosion.begin(), argExplosion.end(),
                args.begin() + lastArgWritten);
    }

    // Emit and insert the result type if required.
    assert(lastArgWritten == 0 || lastArgWritten == 1);
    bool isAggregateResult = (lastArgWritten != 0);
    if (isAggregateResult) {
      assert(CallSites.empty() && "aggregate result on non-final call?");
      Type formalResultType =
        calleeFormalType->castTo<FunctionType>()->getResult();
      const TypeInfo &type = IGF.IGM.getFragileTypeInfo(formalResultType);

      // Force there to be an indirect address.
      if (!result.IndirectAddress.isValid()) {
        result.IndirectAddress =
          IGF.createFullExprAlloca(type, NotOnHeap, "call.aggresult");
      }
      args[0] = result.IndirectAddress.getAddress();
    }

    // Make the call.
    // TODO: exceptions, calling conventions
    llvm::CallInst *call = IGF.Builder.CreateCall(fnPtr, args);
    
    // If we have an aggregate result, set the sret and noalias
    // attributes on the agg return slot, then return, since agg
    // results can only be final.
    if (isAggregateResult) {
      setAggResultAttributes(call);
      assert(result.getDirectValues().empty());
      assert(result.IndirectAddress.isValid());
      return;
    }

    // Extract out the scalar results.
    llvm::SmallVector<llvm::Value*, ExplosionSchema::MaxScalarsForDirectResult> scalars;
    if (llvm::StructType *structType
          = dyn_cast<llvm::StructType>(call->getType())) {
      for (unsigned i = 0, e = structType->getNumElements(); i != e; ++i) {
        llvm::Value *scalar = IGF.Builder.CreateExtractValue(call, i);
        result.addDirectValue(scalar);
      }
    } else if (!call->getType()->isVoidTy()) {
      result.addDirectValue(call);
    }

    // If this is the end of the call sites, we're done.
    if (CallSites.empty()) {
      assert(!result.IndirectAddress.isValid() &&
             "returning direct values when indirect result was requested!");
      result.DirectExplosionLevel = callee.ExplosionLevel;
      return;
    }

    // Otherwise, we must have gotten a function back.  Set ourselves
    // up to call it, then continue emitting calls.
    assert(result.getDirectValues().size() == 2);
    callee.setForIndirectCall(result.getDirectValue(0),
                              result.getDirectValue(1));
    result.clearDirectValues();
  }
}

/// Emit a call for its exploded results.
void swift::irgen::emitApplyExpr(IRGenFunction &IGF, ApplyExpr *E,
                                 Explosion &explosion) {
  CallPlan plan = getCallPlan(IGF.IGM, E);

  const TypeInfo &resultTI = IGF.getFragileTypeInfo(E->getType());

  CallResult result;
  plan.emit(IGF, result, resultTI);

  // If this was an indirect return, explode it.
  if (result.IndirectAddress.isValid()) {
    return resultTI.load(IGF, result.IndirectAddress, explosion);
  }

  if (result.DirectExplosionLevel == explosion.getKind())
    return explosion.add(result.getDirectValues());

  Explosion resultExplosion(result.DirectExplosionLevel);
  resultExplosion.add(result.getDirectValues());
  resultTI.reexplode(IGF, resultExplosion, explosion);
}

/// See whether we can emit the result of the given call as an object
/// naturally located in memory.
Optional<Address>
swift::irgen::tryEmitApplyAsAddress(IRGenFunction &IGF, ApplyExpr *E,
                                    const TypeInfo &resultTI) {
  CallPlan plan = getCallPlan(IGF.IGM, E);

  // Give up if the call won't be returned indirectly.
  ExplosionSchema schema(plan.getFinalResultExplosionLevel(IGF.IGM));
  resultTI.getSchema(schema);
  if (!schema.requiresIndirectResult())
    return Nothing;

  CallResult result;
  plan.emit(IGF, result, resultTI);
  assert(result.IndirectAddress.isValid());
  return result.IndirectAddress;
}

/// Emit a nullary call to the given function, using the standard
/// calling-convention and so on, and explode the result.
void IRGenFunction::emitNullaryCall(llvm::Value *fnPtr,
                                    Type resultType,
                                    Explosion &resultExplosion) {
  ExplosionSchema resultSchema(resultExplosion.getKind());
  const TypeInfo &resultTI = getFragileTypeInfo(resultType);
  resultTI.getSchema(resultSchema);

  llvm::SmallVector<llvm::Value*, 1> args;
  Address resultAddress;
  if (resultSchema.requiresIndirectResult()) {
    resultAddress = createFullExprAlloca(resultTI, NotOnHeap, "call.aggresult");
    args.push_back(resultAddress.getAddress());
  }

  // FIXME: exceptions
  llvm::CallInst *call = Builder.CreateCall(fnPtr, args);

  if (resultSchema.requiresIndirectResult()) {
    setAggResultAttributes(call);
    resultTI.load(*this, resultAddress, resultExplosion);
    return;
  }

  unsigned numScalars = resultSchema.size();
  if (numScalars == 0) return;
  if (numScalars == 1) return resultExplosion.add(call);

  for (unsigned i = 0; i != numScalars; ++i) {
    llvm::Value *scalar = Builder.CreateExtractValue(call, i);
    resultExplosion.add(scalar);
  }
}

/// Initialize an Explosion with the parameters of the current
/// function.  This is really only useful when writing prologue code.
Explosion IRGenFunction::collectParameters() {
  Explosion params(CurExplosionLevel);
  for (auto i = CurFn->arg_begin(), e = CurFn->arg_end(); i != e; ++i)
    params.add(i);
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
  if (param->getAttrs().isByref()) {
    llvm::Value *addr = paramValues.claimNext();
    addr->setName(name);

    llvm::Value *owner = IGM.RefCountedNull;
    if (param->getAttrs().isByrefHeap()) {
      owner = paramValues.claimNext();
      owner->setName(name + ".owner");
    }

    return OwnedAddress(Address(addr, paramType.StorageAlignment), owner);
  }

  // If the schema contains a single aggregate, assume we can
  // just treat the next parameter as that type.
  if (paramSchema.size() == 1 && paramSchema.begin()->isAggregate()) {
    llvm::Value *addr = paramValues.claimNext();
    addr->setName(name);
    addr = Builder.CreateBitCast(addr,
                    paramSchema.begin()->getAggregateType()->getPointerTo());
    Address paramAddr(addr, paramType.StorageAlignment);

    // If it's not referenced on the heap, we can use that directly.
    if (!param->hasUseAsHeapLValue())
      return OwnedAddress(paramAddr, IGM.RefCountedNull);

    // Otherwise, we might have to move it to the heap.
    OwnedAddress paramHeapAddr = createScopeAlloca(paramType, OnHeap,
                                                   name + ".heap");

    // Do a 'take' initialization to directly transfer responsibility.
    paramType.initializeWithTake(*this, paramHeapAddr, paramAddr);
    return paramHeapAddr;
  }

  // Otherwise, make an alloca and load into it.
  OwnedAddress paramAddr = createScopeAlloca(paramType,
                                             param->hasUseAsHeapLValue()
                                               ? OnHeap : NotOnHeap,
                                             name + ".addr");

  // FIXME: This way of getting a list of arguments claimed by storeExplosion
  // is really ugly.
  auto storedStart = paramValues.begin();

  paramType.initialize(*this, paramValues, paramAddr);

  // Set names for argument(s)
  for (auto i = storedStart, e = paramValues.begin(); i != e; ++i) {
    if (e - storedStart == 1)
      (*i)->setName(name);
    else
      (*i)->setName(name + "." + Twine(i - storedStart));
  }

  return paramAddr;
}

/// Emit a specific parameter clause by walking into any literal tuple
/// types and matching 
static void emitParameterClause(IRGenFunction &IGF, Pattern *param,
                                Explosion &paramValues) {
  switch (param->getKind()) {
  // Explode tuple patterns.
  case PatternKind::Tuple:
    for (auto &field : cast<TuplePattern>(param)->getFields())
      emitParameterClause(IGF, field.getPattern(), paramValues);
    return;

  // Look through a couple kinds of patterns.
  case PatternKind::Paren:
    return emitParameterClause(IGF, cast<ParenPattern>(param)->getSubPattern(),
                               paramValues);
  case PatternKind::Typed:
    return emitParameterClause(IGF, cast<TypedPattern>(param)->getSubPattern(),
                               paramValues);

  // Bind names.
  case PatternKind::Named: {
    VarDecl *decl = cast<NamedPattern>(param)->getDecl();
    OwnedAddress addr = IGF.getAddrForParameter(decl, paramValues);

    // FIXME: heap byrefs.
    IGF.setLocal(decl, addr);
    return;
  }

  // Ignore ignored parameters by consuming the right number of values.
  case PatternKind::Any: {
    ExplosionSchema paramSchema(paramValues.getKind());
    IGF.IGM.getSchema(param->getType(), paramSchema);
    paramValues.claim(paramSchema.size());
    return;
  }
  }
  llvm_unreachable("bad pattern kind!");
}

/// Emit all the parameter clauses of the given function type.  This
/// is basically making sure that we have mappings for all the
/// VarDecls bound by the pattern.
static void emitParameterClauses(IRGenFunction &IGF,
                                 llvm::ArrayRef<Pattern*> params,
                                 Explosion &paramValues) {
  assert(!params.empty());

  // When uncurrying, later argument clauses are emitted first.
  if (params.size() != 1)
    emitParameterClauses(IGF, params.slice(1), paramValues);

  // Finally, emit this clause.
  emitParameterClause(IGF, params[0], paramValues);
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
      ReturnSlot = Address(values.claimNext(), resultType.StorageAlignment);
    } else if (resultSchema.empty()) {
      assert(!ReturnSlot.isValid());
    } else {
      ReturnSlot = createScopeAlloca(resultType, NotOnHeap, "return_value");
    }
  }

  // Set up the parameters.
  auto params = CurFuncParamPatterns.slice(0, CurUncurryLevel + 1);
  emitParameterClauses(*this, params, values);

  if (CurPrologue == Prologue::StandardWithContext) {
    ContextPtr = values.claimNext();
    ContextPtr->setName(".context");
  }

  assert(values.empty() && "didn't exhaust all parameters?");
}

/// Emit the epilogue for the function.
void IRGenFunction::emitEpilogue() {
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
    resultType.load(*this, ReturnSlot, result);
    emitScalarReturn(result);
  }
}

void IRGenFunction::emitScalarReturn(Explosion &result) {
  if (result.size() == 0) {
    Builder.CreateRetVoid();
  } else if (result.size() == 1) {
    Builder.CreateRet(result.claimNext());
  } else {
    assert(cast<llvm::StructType>(CurFn->getReturnType())->getNumElements()
             == result.size());
    llvm::Value *resultAgg = llvm::UndefValue::get(CurFn->getReturnType());
    for (unsigned i = 0, e = result.size(); i != e; ++i)
      resultAgg = Builder.CreateInsertValue(resultAgg, result.claimNext(), i);
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
      StructLayout layout(IGM, LayoutKind::HeapObject,
                          LayoutStrategy::Optimal, dataTypes);

      // Create an internal function to serve as the forwarding
      // function (B -> C).
      llvm::Function *forwarder =
        getAddrOfForwardingStub(nextEntrypoint, /*hasData*/ !layout.empty());

      // Emit the curried entrypoint.
      emitCurriedEntrypointBody(entrypoint, forwarder, dataTypes, layout);

      // Emit the forwarding stub.
      emitCurriedForwarderBody(forwarder, nextEntrypoint, dataTypes, layout);

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

      FunctionType *fn = cast<FunctionType>(fnType);
      accumulateClauses(fn->getResult(), maxUncurryLevel - 1);

      Clauses[clauseIndex].DataTypesBeginIndex = AllDataTypes.size();
      Clauses[clauseIndex].ForwardingFnType = fn->getResult();

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
                                   llvm::ArrayRef<const TypeInfo *> dataTypes,
                                   const StructLayout &layout) {
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
        // Allocate a new object.  FIXME: refcounting, exceptions.
        llvm::Value *size = llvm::ConstantInt::get(IGM.SizeTy,
                                                   layout.getSize().getValue());
        llvm::CallInst *call =
          IGF.Builder.CreateCall(IGM.getAllocFn(), size);
        call->setDoesNotThrow();
        data = call;

        // Cast to the appropriate struct type.
        llvm::Value *addr =
          IGF.Builder.CreateBitCast(data, layout.getType()->getPointerTo());

        // Perform the store.
        for (unsigned i = 0, e = dataTypes.size(); i != e; ++i) {
          auto &fieldLayout = layout.getElements()[i];
          llvm::Value *fieldAddr =
            IGF.Builder.CreateStructGEP(addr, fieldLayout.StructIndex);
          Alignment fieldAlign =
            layout.getAlignment().alignmentAtOffset(fieldLayout.ByteOffset);
          dataTypes[i]->initialize(IGF, params, Address(fieldAddr, fieldAlign));
        }

        // Reset the cursor in the explosion.
        params.resetClaim();
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
                                  llvm::ArrayRef<const TypeInfo *> dataTypes,
                                  const StructLayout &layout) {
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
        llvm::Value *rawData = params.takeLast();
        llvm::Value *data =
          IGF.Builder.CreateBitCast(rawData, layout.getType()->getPointerTo());

        // Perform the loads.
        for (unsigned i = 0, e = dataTypes.size(); i != e; ++i) {
          auto &fieldLayout = layout.getElements()[i];
          llvm::Value *fieldAddr =
            IGF.Builder.CreateStructGEP(data, fieldLayout.StructIndex);
          Alignment fieldAlign =
            layout.getAlignment().alignmentAtOffset(fieldLayout.ByteOffset);
          dataTypes[i]->load(IGF, Address(fieldAddr, fieldAlign), params);
        }

        // Kill the allocated data pointer immediately.  The safety of
        // this assumes that neither this release nor any of the loads
        // can throw.
        IGF.emitRelease(rawData);
      }

      llvm::CallInst *call =
        IGF.Builder.CreateCall(nextEntrypoint, params.claimAll());
      call->setTailCall();

      if (!layout.empty())
        IGF.popCleanup();

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
  if (!func->getBody()) return;
  FuncExpr *funcExpr = func->getBody();

  // FIXME: variant currying levels!
  // FIXME: also emit entrypoints with maximal explosion when all types are known!
  unsigned naturalUncurryLevel = getNaturalUncurryLevel(func);
  assert(startingUncurryLevel <= naturalUncurryLevel);

  ExplosionKind explosionLevel = ExplosionKind::Minimal;

  // Get the address of the first entrypoint we're going to emit.
  llvm::Function *entrypoint =
    IGM.getAddrOfGlobalFunction(func, explosionLevel, startingUncurryLevel);

  CurriedData curriedData(IGM, funcExpr, explosionLevel,
                          startingUncurryLevel, naturalUncurryLevel);

  // Emit the curried entrypoints.  At the end of each iteration,
  // fnAddr will point to the next entrypoint in the currying sequence.
  for (unsigned uncurryLevel = startingUncurryLevel;
         uncurryLevel != naturalUncurryLevel; ++uncurryLevel) {
    llvm::Function *nextEntrypoint =
      IGM.getAddrOfGlobalFunction(func, explosionLevel, uncurryLevel + 1);

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
  assert(!func->isPlus());
  emitFunction(*this, func, 1);
}

/// Emit the definition for the given plus method.
void IRGenModule::emitPlusMethod(FuncDecl *func) {
  assert(func->isPlus());
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

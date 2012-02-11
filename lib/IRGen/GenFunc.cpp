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
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "RValue.h"
#include "Explosion.h"
#include "StructLayout.h"

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
  Type resultType = getResultType(CurFuncExpr->getType(), CurUncurryLevel);
  return IGM.getFragileTypeInfo(resultType);
}

namespace {
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

    RValueSchema getSchema() const {
      llvm::StructType *Ty = getStorageType();
      assert(Ty->getNumElements() == 2);
      return RValueSchema::forScalars(Ty->getElementType(0),
                                      Ty->getElementType(1));
    }

    RValue load(IRGenFunction &IGF, Address address) const {
      llvm::Value *addr = address.getAddress();

      // Load the function.
      llvm::Value *fnAddr =
        IGF.Builder.CreateStructGEP(addr, 0, addr->getName() + ".fn");
      llvm::LoadInst *fn =
        IGF.Builder.CreateLoad(fnAddr, address.getAlignment(),
                               fnAddr->getName() + ".load");

      // Load the data.  This load is offset by sizeof(void*) from the
      // base and so may have a lesser alignment.
      // FIXME: retains?
      llvm::Value *dataAddr =
        IGF.Builder.CreateStructGEP(addr, 1, addr->getName() + ".data");
      llvm::Value *data =
        IGF.Builder.CreateLoad(dataAddr,
                               address.getAlignment().alignmentAtOffset(
                                          Size(StorageAlignment.getValue())),
                               dataAddr->getName() + ".load");

      return RValue::forScalars(fn, data);
    }

    void store(IRGenFunction &IGF, const RValue &RV, Address address) const {
      assert(RV.isScalar() && RV.getScalars().size() == 2);
      llvm::Value *addr = address.getAddress();

      // Store the function pointer.
      llvm::Value *fnAddr =
        IGF.Builder.CreateStructGEP(addr, 0, addr->getName() + ".fn");
      IGF.Builder.CreateStore(RV.getScalars()[0], fnAddr,
                              address.getAlignment());

      // Store the data.
      // FIXME: retains?
      llvm::Value *dataAddr =
        IGF.Builder.CreateStructGEP(addr, 1, addr->getName() + ".data");
      IGF.Builder.CreateStore(RV.getScalars()[1], dataAddr,
                              address.getAlignment().alignmentAtOffset(
                                         Size(StorageAlignment.getValue())));
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 2;
    }

    void getExplosionSchema(ExplosionSchema &schema) const {
      llvm::StructType *Ty = getStorageType();
      assert(Ty->getNumElements() == 2);
      schema.add(ExplosionSchema::Element::forScalar(Ty->getElementType(0)));
      schema.add(ExplosionSchema::Element::forScalar(Ty->getElementType(1)));
    }

    void loadExplosion(IRGenFunction &IGF, Address addr, Explosion &e) const {
      RValue rv = load(IGF, addr);
      e.add(rv.getScalars());
    }

    void storeExplosion(IRGenFunction &IGF, Explosion &e, Address addr) const {
      llvm::Value *func = e.claimNext();
      llvm::Value *data = e.claimNext();
      store(IGF, RValue::forScalars(func, data), addr);
    }
  };
}

const TypeInfo *
TypeConverter::convertFunctionType(IRGenModule &IGM, FunctionType *T) {
  Size StructSize = Size(IGM.TargetData.getPointerSize()) * 2;
  Alignment StructAlign = Alignment(IGM.TargetData.getPointerABIAlignment());
  return FuncTypeInfo::create(T, IGM.Int8PtrPairTy, StructSize, StructAlign);
}

/// Given the explosion schema for the result type of a function, does
/// it require an aggregate result?
static bool requiresAggregateResult(const ExplosionSchema &schema) {
  if (schema.size() > RValue::MaxScalars) return true;
  for (auto &elt : schema)
    if (elt.isAggregate())
      return true;
  return false;
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
    IGM.getExplosionSchema(type, schema);

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
    IGM.getExplosionSchema(formalResultType, schema);

    hasAggregateResult = requiresAggregateResult(schema);
    if (hasAggregateResult) {
      const TypeInfo &info = IGM.getFragileTypeInfo(formalResultType);
      argTypes[0] = info.StorageType->getPointerTo();
      resultType = llvm::Type::getVoidTy(IGM.getLLVMContext());
    } else if (schema.size() == 0) {
      resultType = llvm::Type::getVoidTy(IGM.getLLVMContext());
    } else if (schema.size() == 1) {
      resultType = schema.begin()->getScalarType();
    } else {
      llvm::SmallVector<llvm::Type*, RValue::MaxScalars> elts;
      for (auto &elt : schema) elts.push_back(elt.getScalarType());
      resultType = llvm::StructType::get(IGM.getLLVMContext(), elts);
    }
  }

  // Data arguments are last.
  // See the comment in this file's header comment.
  if (needsData)
    argTypes.push_back(IGM.Int8PtrTy);

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
             dataPtr->getType()->getContainedType(0)->isIntegerTy(8));
      assert(dataPtr == nullptr || !isa<llvm::UndefValue>(dataPtr));

      FnPtr = fnPtr;
      DataPtr = dataPtr;
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
      return llvm::UndefValue::get(IGM.Int8PtrTy);
    }
  };
}

/// Emit a reference to a function, using the best parameters possible
/// up to given limits.
static Callee emitCallee(IRGenFunction &IGF, FuncDecl *fn,
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
    callee.set(IGF.IGM.getAddrOfGlobalFunction(fn, bestExplosion, bestUncurry),
               nullptr);
  } else {
    IGF.unimplemented(fn->getLocStart(), "local function emission");
    llvm::Value *undef = llvm::UndefValue::get(IGF.IGM.Int8PtrTy);
    callee.set(undef, nullptr);
  }

  return callee;
}

/// Emit a reference to the given function as a generic function pointer.
void IRGenFunction::emitExplodedRValueForFunction(FuncDecl *fn,
                                                  Explosion &explosion) {
  // Function pointers are always fully curried and use ExplosionKind::Minimal.
  Callee callee = emitCallee(*this, fn, ExplosionKind::Minimal, 0);
  assert(callee.ExplosionLevel == ExplosionKind::Minimal);
  assert(callee.UncurryLevel == 0);
  explosion.add(callee.getOpaqueFunctionPointer(*this));
  explosion.add(callee.getDataPointer(IGM));
}

namespace {
  struct ArgList {
    ArgList(ExplosionKind kind) : Values(kind) {}

    Explosion Values;
    llvm::SmallVector<llvm::AttributeWithIndex, 4> Attrs;
  };
}

/// emitBuiltinCall - Emit a call to a builtin function.
static RValue emitBuiltinCall(IRGenFunction &IGF, FuncDecl *Fn, Expr *Arg,
                              const TypeInfo &resultType) {
  assert(resultType.getSchema().isScalar() && "builtin type with agg return");

  // Emit the arguments.  Maybe we'll get builtins that are more
  // complex than this.
  ArgList args(ExplosionKind::Minimal);
  IGF.emitExplodedRValue(Arg, args.Values);

  Type BuiltinType;
  switch (isBuiltinValue(IGF.IGM.Context, Fn->getName().str(), BuiltinType)) {
  case BuiltinValueKind::None: llvm_unreachable("not a builtin after all!");

/// A macro which expands to the emission of a simple unary operation
/// or predicate.
#define UNARY_OPERATION(Op) {                                               \
    llvm::Value *op = args.Values.claimNext();                              \
    assert(args.Values.empty() && "wrong operands to unary operation");     \
    return RValue::forScalars(IGF.Builder.Create##Op(op));                  \
  }

/// A macro which expands to the emission of a simple binary operation
/// or predicate.
#define BINARY_OPERATION(Op) {                                              \
    llvm::Value *lhs = args.Values.claimNext();                             \
    llvm::Value *rhs = args.Values.claimNext();                             \
    assert(args.Values.empty() && "wrong operands to binary operation");    \
    return RValue::forScalars(IGF.Builder.Create##Op(lhs, rhs));            \
  }

/// A macro which expands to the emission of a simple binary operation
/// or predicate defined over both floating-point and integer types.
#define BINARY_ARITHMETIC_OPERATION(IntOp, FPOp) {                          \
    llvm::Value *lhs = args.Values.claimNext();                             \
    llvm::Value *rhs = args.Values.claimNext();                             \
    assert(args.Values.empty() && "wrong operands to binary operation");    \
    if (lhs->getType()->isFloatingPointTy()) {                              \
      return RValue::forScalars(IGF.Builder.Create##FPOp(lhs, rhs));        \
    } else {                                                                \
      return RValue::forScalars(IGF.Builder.Create##IntOp(lhs, rhs));       \
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

void IRGenFunction::emitExplodedApplyExpr(ApplyExpr *E, Explosion &explosion) {
  const TypeInfo &type = getFragileTypeInfo(E->getType());
  RValue rvalue = emitApplyExpr(E, type);
  return type.explode(*this, rvalue, explosion);
}

Optional<Address>
IRGenFunction::tryEmitApplyAsAddress(ApplyExpr *E, const TypeInfo &resultType) {
  RValueSchema resultSchema = resultType.getSchema();
  if (!resultSchema.isAggregate())
    return Nothing;

  RValue result = emitApplyExpr(E, resultType);
  assert(result.isAggregate());
  return Address(result.getAggregateAddress(), resultType.StorageAlignment);
}

namespace {
  struct CallSite {
    CallSite(Expr *arg, Type fnType) : Arg(arg), FnType(fnType) {}

    Expr *Arg;
    Type FnType;
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

/// Emit the given expression, trying to tie it down to a known
/// function.
static FuncDecl *emitAsKnownFunctionReference(IRGenFunction &IGF, Expr *E) {
  E = E->getSemanticsProvidingExpr();
  if (DeclRefExpr *declRef = dyn_cast<DeclRefExpr>(E))
    return dyn_cast<FuncDecl>(declRef->getDecl());
  return nullptr;
}

/// Emit a function call.
RValue IRGenFunction::emitApplyExpr(ApplyExpr *E, const TypeInfo &resultType) {
  // 1.  Try to uncurry the source expression.  Note that the argument
  // expressions will appear in reverse order.
  llvm::SmallVector<CallSite, 8> callSites;
  Expr *fnExpr = uncurry(E, callSites);

  // 2.  Emit the function expression.
  Callee callee;

  // We can do a lot if we know we're calling a known function.
  if (FuncDecl *fn = emitAsKnownFunctionReference(*this, fnExpr)) {
    // Handle calls to builtin functions.
    if (isa<BuiltinModule>(fn->getDeclContext())) {
      assert(callSites.size() == 1);
      return emitBuiltinCall(*this, cast<FuncDecl>(fn), callSites[0].Arg,
                             resultType);
    }

    // Otherwise, compute information about the function we're calling.
    callee = emitCallee(*this, fn, ExplosionKind::Maximal, callSites.size()-1);

  // Otherwise, emit as a function pointer and use the pessimistic
  // rules for calling such.
  } else {
    Explosion fnValues(ExplosionKind::Maximal);
    emitExplodedRValue(fnExpr, fnValues);
    callee.ExplosionLevel = ExplosionKind::Minimal;
    callee.UncurryLevel = 0;
    llvm::Value *fnPtr = fnValues.claimNext();
    llvm::Value *dataPtr = fnValues.claimNext();
    callee.set(fnPtr, isa<llvm::UndefValue>(dataPtr) ? nullptr : dataPtr);
  }

  // 3. Emit arguments and call.
  while (true) {
    assert(callee.UncurryLevel < callSites.size());

    // Find the formal type we're calling.
    unsigned calleeIndex = callSites.size() - 1;
    Type calleeFormalType = callSites[calleeIndex].FnType;
    llvm::Value *fnPtr =
      callee.getFunctionPointer(*this, calleeFormalType);
    llvm::FunctionType *calleeType = 
      cast<llvm::FunctionType>(cast<llvm::PointerType>(fnPtr->getType())
                                 ->getElementType());

    SmallVector<llvm::Value*, 16> args(calleeType->getNumParams());
    unsigned lastArgWritten = calleeType->getNumParams();

    // Add the data pointer in.
    if (callee.hasDataPointer())
      args[--lastArgWritten] = callee.getDataPointer(IGM);

    // Emit all of the arguments we need to pass here.
    for (unsigned i = callee.UncurryLevel + 1; i != 0; --i) {
      Expr *arg = callSites.back().Arg;
      callSites.pop_back();

      // Emit the argument, exploded at the appropriate level.
      Explosion argExplosion(callee.ExplosionLevel);
      emitExplodedRValue(arg, argExplosion);

      assert(lastArgWritten >= argExplosion.size());
      lastArgWritten -= argExplosion.size();

      // Now copy that into place in the argument list.
      std::copy(argExplosion.begin(), argExplosion.end(),
                args.begin() + lastArgWritten);
    }

    // Emit and insert the result type if required.
    Address resultAddress;
    assert(lastArgWritten == 0 || lastArgWritten == 1);
    bool isAggregateResult = (lastArgWritten != 0);
    if (isAggregateResult) {
      assert(callSites.empty() && "aggregate result on non-final call?");
      Type formalResultType =
        calleeFormalType->castTo<FunctionType>()->getResult();
      const TypeInfo &type = IGM.getFragileTypeInfo(formalResultType);

      resultAddress = createFullExprAlloca(type.StorageType,
                                           type.StorageAlignment,
                                           "call.aggresult");
      args[0] = resultAddress.getAddress();
    }

    // TODO: exceptions, calling conventions
    llvm::CallInst *call = Builder.CreateCall(fnPtr, args);
    
    // If we have an aggregate result, set the sret and noalias
    // attributes on the agg return slot, then return, since agg
    // results can only be final.
    if (isAggregateResult) {
      llvm::SmallVector<llvm::AttributeWithIndex, 1> attrs;
      attrs.push_back(llvm::AttributeWithIndex::get(1,
                                llvm::Attribute::StructRet |
                                llvm::Attribute::NoAlias));
      call->setAttributes(llvm::AttrListPtr::get(attrs.data(), attrs.size()));

      return RValue::forAggregate(resultAddress.getAddress());
    }

    // Extract out the scalar results.
    llvm::SmallVector<llvm::Value*, RValue::MaxScalars> scalars;
    if (llvm::StructType *structType
        = dyn_cast<llvm::StructType>(call->getType())) {
      for (unsigned i = 0, e = structType->getNumElements(); i != e; ++i) {
        llvm::Value *scalar = Builder.CreateExtractValue(call, i);
        scalars.push_back(scalar);
      }
    } else if (!call->getType()->isVoidTy()) {
      scalars.push_back(call);
    }

    // If this is the end of the call sites, we're done.
    if (callSites.empty())
      return RValue::forScalars(scalars);

    // Otherwise, we must have gotten a function back.  Set ourselves
    // up to call it, then continue emitting calls.
    assert(scalars.size() == 2);
    callee.ExplosionLevel = ExplosionKind::Minimal;
    callee.UncurryLevel = 0;
    callee.set(scalars[0], scalars[1]);
  }
}

/// Initialize an Explosion with the parameters of the current function.
static Explosion collectParameters(IRGenFunction &IGF) {
  Explosion params(IGF.CurExplosionLevel);
  for (auto i = IGF.CurFn->arg_begin(), e = IGF.CurFn->arg_end(); i != e; ++i)
    params.add(i);
  return params;
}

/// Emit a specific parameter.
static void emitParameter(IRGenFunction &IGF, VarDecl *param,
                          Explosion &paramValues) {
  const TypeInfo &paramType = IGF.IGM.getFragileTypeInfo(param->getType());

  ExplosionSchema paramSchema(paramValues.getKind());
  paramType.getExplosionSchema(paramSchema);

  Address paramAddr;

  // If the parameter is byref, the next parameter is the value we
  // should use.
  if (param->getAttrs().isByref()) {
    llvm::Value *addr = paramValues.claimNext();
    paramAddr = Address(addr, paramType.StorageAlignment);
    
  // If the schema contains a single aggregate, assume we can
  // just treat the next parameter as that type.
  } else if (paramSchema.size() == 1 && paramSchema.begin()->isAggregate()) {
    llvm::Value *addr = paramValues.claimNext();
    addr = IGF.Builder.CreateBitCast(addr,
                    paramSchema.begin()->getAggregateType()->getPointerTo());
    paramAddr = Address(addr, paramType.StorageAlignment);

  // Otherwise, make an alloca and load into it.
  } else {
    paramAddr = IGF.createScopeAlloca(paramType.getStorageType(),
                                      paramType.StorageAlignment,
                                      param->getName().str());

    paramType.storeExplosion(IGF, paramValues, paramAddr);
  }

  IGF.setLocal(param, paramAddr);
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
  case PatternKind::Named:
    emitParameter(IGF, cast<NamedPattern>(param)->getDecl(), paramValues);
    return;

  // Ignore ignored parameters by consuming the right number of values.
  case PatternKind::Any: {
    ExplosionSchema paramSchema(paramValues.getKind());
    IGF.IGM.getExplosionSchema(param->getType(), paramSchema);
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
  if (CurPrologue != Prologue::Standard)
    return;

  // Set up the return block and insert it.  This creates a second
  // insertion point that most blocks should be inserted before.
  ReturnBB = createBasicBlock("return");
  CurFn->getBasicBlockList().push_back(ReturnBB);

  // List out the parameter values in an Explosion.
  Explosion values = collectParameters(*this);

  // Set up the return slot, stealing the first argument if necessary.
  {
    // Find the 'code' result type of this function.
    const TypeInfo &resultType = getResultTypeInfo();

    ExplosionSchema resultSchema(CurExplosionLevel);
    resultType.getExplosionSchema(resultSchema);

    if (requiresAggregateResult(resultSchema)) {
      ReturnSlot = Address(values.claimNext(), resultType.StorageAlignment);
    } else if (resultSchema.empty()) {
      assert(!ReturnSlot.isValid());
    } else {
      ReturnSlot = createScopeAlloca(resultType.getStorageType(),
                                     resultType.StorageAlignment,
                                     "return_value");
    }
  }

  // Set up the parameters.
  auto params = CurFuncExpr->getParamPatterns().slice(0, CurUncurryLevel + 1);
  emitParameterClauses(*this, params, values);

  // TODO: set up the data pointer.

  assert(values.empty() && "didn't exhaust all parameters?");
}

/// Emit the epilogue for the function.
void IRGenFunction::emitEpilogue() {
  // Destroy the alloca insertion point.
  AllocaIP->eraseFromParent();

  // That's it for the 'bare' epilogue.
  if (CurPrologue != Prologue::Standard)
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
  resultType.getExplosionSchema(resultSchema);

  if (requiresAggregateResult(resultSchema)) {
    assert(isa<llvm::Argument>(ReturnSlot.getAddress()));
    Builder.CreateRetVoid();
  } else if (resultSchema.empty()) {
    assert(!ReturnSlot.isValid());
    Builder.CreateRetVoid();
  } else {
    Explosion result(CurExplosionLevel);
    resultType.loadExplosion(*this, ReturnSlot, result);
    if (result.size() == 1) {
      Builder.CreateRet(result.claimNext());
    } else {
      llvm::Value *resultAgg = llvm::UndefValue::get(CurFn->getReturnType());
      for (unsigned i = 0, e = result.size(); i != e; ++i)
        resultAgg = Builder.CreateInsertValue(resultAgg, result.claimNext(), i);
      Builder.CreateRet(resultAgg);
    }
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
      StructLayout layout(IGM, LayoutStrategy::Optimal, dataTypes);

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
        for (const TupleTypeElt &field : tuple->Fields)
          accumulateParameterDataTypes(field.getType());
        return;
      }

      const TypeInfo &type = IGM.getFragileTypeInfo(ty);
      if (!type.isEmpty()) AllDataTypes.push_back(&type);
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

      IRGenFunction IGF(IGM, Func, ExplosionLevel, CurClause, entrypoint,
                        Prologue::Bare);

      Explosion params = collectParameters(IGF);

      // We're returning a function, so no need to worry about an
      // aggregate return slot.

      // Compute a data object.
      llvm::Value *data = nullptr;
      if (!layout.empty()) {
        // Allocate a new object.  FIXME: refcounting, exceptions.
        llvm::Value *size = llvm::ConstantInt::get(IGM.SizeTy,
                                                   layout.getSize().getValue());
        llvm::CallInst *call =
          IGF.Builder.CreateCall(IGM.getAllocationFunction(), size);
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
          dataTypes[i]->storeExplosion(IGF, params,
                                       Address(fieldAddr, fieldAlign));
        }

        // Reset the cursor in the explosion.
        params.resetClaim();
      }

      // Build the function result.
      llvm::Value *result = llvm::UndefValue::get(IGM.Int8PtrPairTy);
      result = IGF.Builder.CreateInsertValue(result,
                      llvm::ConstantExpr::getBitCast(forwarder, IGM.Int8PtrTy),
                                             0);
      if (!layout.empty())
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

      IRGenFunction IGF(IGM, Func, ExplosionLevel, CurClause, forwarder,
                        Prologue::Bare);

      // Accumulate the function's immediate parameters.
      Explosion params = collectParameters(IGF);

      // If there's a data pointer required, grab it (it's always the
      // last parameter) and load out the extra, previously-curried
      // parameters.
      if (!layout.empty()) {
        llvm::Value *data = params.takeLast();
        data = IGF.Builder.CreateBitCast(data, layout.getType()->getPointerTo());

        // Perform the loads.
        for (unsigned i = 0, e = dataTypes.size(); i != e; ++i) {
          auto &fieldLayout = layout.getElements()[i];
          llvm::Value *fieldAddr =
            IGF.Builder.CreateStructGEP(data, fieldLayout.StructIndex);
          Alignment fieldAlign =
            layout.getAlignment().alignmentAtOffset(fieldLayout.ByteOffset);
          dataTypes[i]->loadExplosion(IGF, Address(fieldAddr, fieldAlign),
                                      params);
        }
      }

      llvm::CallInst *call =
        IGF.Builder.CreateCall(nextEntrypoint, params.claimAll());
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
  IRGenFunction(IGM, funcExpr, explosionLevel,
                naturalUncurryLevel, entrypoint)
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

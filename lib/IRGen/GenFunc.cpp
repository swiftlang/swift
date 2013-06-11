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
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/Optional.h"
#include "swift/SIL/SILModule.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/StringSwitch.h"

#include "ASTVisitor.h"
#include "CallingConvention.h"
#include "CallEmission.h"
#include "Explosion.h"
#include "FunctionRef.h"
#include "GenClass.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenObjC.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "IRGenSIL.h"
#include "Linking.h"
#include "FixedTypeInfo.h"
#include "ScalarTypeInfo.h"
#include "GenFunc.h"

using namespace swift;
using namespace irgen;

llvm::Type *ExplosionSchema::getScalarResultType(IRGenModule &IGM) const {
  if (size() == 0) {
    return IGM.VoidTy;
  } else if (size() == 1) {
    return begin()->getScalarType();
  } else {
    SmallVector<llvm::Type*, MaxScalarsForDirectResult> elts;
    for (auto &elt : *this) elts.push_back(elt.getScalarType());
    return llvm::StructType::get(IGM.getLLVMContext(), elts);
  }
}

void ExplosionSchema::addToArgTypes(IRGenModule &IGM,
                                    SmallVectorImpl<llvm::Type*> &types) const {
  for (auto &elt : *this) {
    if (elt.isAggregate())
      types.push_back(elt.getAggregateType()->getPointerTo());
    else
      types.push_back(elt.getScalarType());
  }
}

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
unsigned irgen::getDeclNaturalUncurryLevel(ValueDecl *val) {
  if (FuncDecl *func = dyn_cast<FuncDecl>(val)) {
    return func->getBody()->getNaturalArgumentCount() - 1;
  }
  if (isa<ConstructorDecl>(val) || isa<OneOfElementDecl>(val)) {
    return 1;
  }
  llvm_unreachable("Unexpected ValueDecl");
}

/// Given a function type, return the formal result type at the given
/// uncurrying level.  For 'a -> b -> c', this is 'b' at 0 and 'c' at 1.
CanType irgen::getResultType(CanType type, unsigned uncurryLevel) {
  do {
    type = CanType(cast<AnyFunctionType>(type)->getResult());
  } while (uncurryLevel--);
  return type;
}

static llvm::CallingConv::ID getFreestandingConvention(IRGenModule &IGM) {
  // TODO: use a custom CC that returns three scalars efficiently
  return llvm::CallingConv::C;
}

/// Expand the requirements of the given abstract calling convention
/// into a "physical" calling convention.
llvm::CallingConv::ID irgen::expandAbstractCC(IRGenModule &IGM,
                                              AbstractCC convention) {
  switch (convention) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    return llvm::CallingConv::C;

  case AbstractCC::Method:
    //   TODO: maybe add 'inreg' to the first non-result argument.
    SWIFT_FALLTHROUGH;
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
      initForDirectValues(ExplosionKind::Maximal).add(value);
    }

    void setAsIndirectAddress(Address address) {
      assert(CurState == State::Invalid);
      CurState = State::Indirect;
      CurValue.Indirect = address;
    }

    bool isInvalid() const { return CurState == State::Invalid; } 
    bool isDirect() const { return CurState == State::Direct; }
    bool isIndirect() const { return CurState == State::Indirect; }

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
    llvm::AttributeSet Attributes;

  public:
    bool isValid() const {
      return TypeAndHasIndirectReturn.getPointer() != nullptr;
    }

    void set(llvm::FunctionType *type, bool hasIndirectReturn,
             llvm::AttributeSet attrs) {
      TypeAndHasIndirectReturn.setPointer(type);
      TypeAndHasIndirectReturn.setInt(hasIndirectReturn);
      Attributes = attrs;
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
    
    llvm::AttributeSet getAttributes() const {
      return Attributes;
    }
  };

  /// The type-info class.
  class FuncTypeInfo : public ScalarTypeInfo<FuncTypeInfo, FixedTypeInfo> {
    /// Each possible currying of a function type has different function
    /// type variants along each of three orthogonal axes:
    ///   - the calling convention
    ///   - the explosion kind desired
    ///   - whether a data pointer argument is required
    struct Currying {
      Signature Signatures[unsigned(AbstractCC::Last_AbstractCC) + 1]
                          [unsigned(ExplosionKind::Last_ExplosionKind) + 1]
                          [unsigned(ExtraData::Last_ExtraData) + 1];

      Signature &select(AbstractCC cc, ExplosionKind kind, ExtraData extraData) {
        return Signatures[unsigned(cc)][unsigned(kind)][unsigned(extraData)];
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

    Signature getSignature(IRGenModule &IGM, AbstractCC cc,
                           ExplosionKind explosionKind,
                           unsigned currying, ExtraData extraData) const;

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
      e.add(IGF.Builder.CreateLoad(fnAddr, fnAddr->getName()+".load"));

      // Load the data.
      Address dataAddr = projectData(IGF, address);
      IGF.emitLoadAndRetain(dataAddr, e);
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // Load the function.
      Address fnAddr = projectFunction(IGF, addr);
      e.add(IGF.Builder.CreateLoad(fnAddr));

      // Load the data.
      Address dataAddr = projectData(IGF, addr);
      e.add(IGF.Builder.CreateLoad(dataAddr));
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

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
      IGF.emitRetain(src.claimNext(), dest);
    }

    void retain(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitRetainCall(e.claimNext());
    }
    
    void release(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitRelease(e.claimNext());
    }
    
    void destroy(IRGenFunction &IGF, Address addr) const {
      IGF.emitRelease(IGF.Builder.CreateLoad(projectData(IGF, addr)));
    }
  };

  /// The type-info class for ObjC blocks, which are represented by an ObjC
  /// heap pointer.
  class BlockTypeInfo : public HeapTypeInfo<BlockTypeInfo> {
  public:
    BlockTypeInfo(llvm::PointerType *storageType,
                  Size size, Alignment align)
      : HeapTypeInfo(storageType, size, align) {
    }

    bool hasSwiftRefcount() const { return false; }
  };
}

const TypeInfo *TypeConverter::convertFunctionType(AnyFunctionType *T) {
  if (T->isBlock())
    return new BlockTypeInfo(IGM.ObjCPtrTy,
                             IGM.getPointerSize(),
                             IGM.getPointerAlignment());
  
  return FuncTypeInfo::create(T, IGM.FunctionPairTy,
                              IGM.getPointerSize() * 2,
                              IGM.getPointerAlignment());
}

void irgen::addIndirectReturnAttributes(IRGenModule &IGM,
                                        llvm::AttributeSet &attrs) {
  static const llvm::Attribute::AttrKind attrKinds[] = {
    llvm::Attribute::StructRet,
    llvm::Attribute::NoAlias
  };
  auto resultAttrs = llvm::AttributeSet::get(IGM.LLVMContext, 1, attrKinds);
  attrs = attrs.addAttributes(IGM.LLVMContext, 1, resultAttrs);
}

void irgen::addByvalArgumentAttributes(IRGenModule &IGM,
                                       llvm::AttributeSet &attrs,
                                       unsigned argIndex,
                                       Alignment align) {
  llvm::AttrBuilder b;
  b.addAttribute(llvm::Attribute::ByVal);
  b.addAttribute(llvm::Attribute::getWithAlignment(IGM.LLVMContext,
                                                   align.getValue()));
  auto resultAttrs = llvm::AttributeSet::get(IGM.LLVMContext, argIndex+1, b);
  attrs = attrs.addAttributes(IGM.LLVMContext,
                              argIndex+1,
                              resultAttrs);
}

static void decomposeFunctionArg(IRGenModule &IGM, CanType argTy,
                       AbstractCC cc, ExplosionKind explosionKind,
                       SmallVectorImpl<llvm::Type*> &argTypes,
                       SmallVectorImpl<std::pair<unsigned, Alignment>> &byvals,
                       llvm::AttributeSet &attrs) {
  switch (cc) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    if (requiresExternalByvalArgument(IGM, argTy)) {
      const TypeInfo &ti = IGM.getFragileTypeInfo(argTy);
      assert(isa<FixedTypeInfo>(ti) &&
             "emitting 'byval' argument with non-fixed layout?");
      byvals.push_back({argTypes.size(), ti.getBestKnownAlignment()});
      argTypes.push_back(ti.getStorageType()->getPointerTo());
      break;
    }
    SWIFT_FALLTHROUGH;
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    auto schema = IGM.getSchema(argTy, explosionKind);
    schema.addToArgTypes(IGM, argTypes);
    break;
  }
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
static CanType decomposeFunctionType(IRGenModule &IGM, CanType type,
                       AbstractCC cc,
                       ExplosionKind explosionKind,
                       unsigned uncurryLevel,
                       SmallVectorImpl<llvm::Type*> &argTypes,
                       SmallVectorImpl<std::pair<unsigned, Alignment>> &byvals,
                       llvm::AttributeSet &attrs) {
  // Ask SIL's TypeLowering to uncurry the function type.
  type = CanType(Lowering::getThinFunctionType(type, cc));
  auto fn = cast<AnyFunctionType>(type);
  fn = IGM.SILMod->Types.getUncurriedFunctionType(fn, uncurryLevel);

  // Explode the argument.
  auto decomposeTopLevelArg = [&](CanType inputTy) {
    if (TupleType *tupleTy = inputTy->getAs<TupleType>()) {
      for (auto &field : tupleTy->getFields()) {
        decomposeFunctionArg(IGM, CanType(field.getType()), cc, explosionKind,
                             argTypes, byvals, attrs);
      }
    } else {
      decomposeFunctionArg(IGM, inputTy, cc, explosionKind,
                           argTypes, byvals, attrs);
    }
  };

  CanType inputTy = CanType(fn->getInput());
  switch (cc) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::C:
    decomposeTopLevelArg(inputTy);
    break;

  case AbstractCC::ObjCMethod: {
    // ObjC methods take an implicit _cmd argument after the self argument.
    TupleType *inputTuple = cast<TupleType>(inputTy);
    assert(inputTuple->getFields().size() == 2 && "invalid objc method type");
    decomposeTopLevelArg(CanType(inputTuple->getFields()[0].getType()));
    argTypes.push_back(IGM.Int8PtrTy);
    decomposeTopLevelArg(CanType(inputTuple->getFields()[1].getType()));
    break;
  }
  }

  if (auto polyTy = dyn_cast<PolymorphicFunctionType>(fn))
    expandPolymorphicSignature(IGM, polyTy, argTypes);

  return CanType(fn->getResult());
}

Signature FuncTypeInfo::getSignature(IRGenModule &IGM,
                                     AbstractCC cc,
                                     ExplosionKind explosionKind,
                                     unsigned uncurryLevel,
                                     ExtraData extraData) const {
  // Compute a reference to the appropriate signature cache.
  assert(uncurryLevel < getNumCurries(FormalType));
  Currying &currying = getCurryingsBuffer()[uncurryLevel];
  Signature &signature = currying.select(cc, explosionKind, extraData);

  // If it's already been filled in, we're done.
  if (signature.isValid())
    return signature;

  llvm::AttributeSet attrs;

  // The argument types.
  // Save a slot for the aggregate return.
  SmallVector<llvm::Type*, 16> argTypes;
  SmallVector<std::pair<unsigned, Alignment>, 16> byvals;
  argTypes.push_back(nullptr);

  CanType formalResultType = decomposeFunctionType(IGM, CanType(FormalType),
                                                   cc, explosionKind,
                                                   uncurryLevel,
                                                   argTypes, byvals,
                                                   attrs);

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
      
      addIndirectReturnAttributes(IGM, attrs);
    } else {
      resultType = schema.getScalarResultType(IGM);
    }
  }
  
  // Apply 'byval' argument attributes.
  for (auto &byval : byvals) {
    // If we didn't have an indirect result, the indices will be off-by-one
    // because of the argument we reserved for it and didn't use.
    addByvalArgumentAttributes(IGM, attrs,
                               hasAggregateResult ? byval.first : byval.first-1,
                               byval.second);
  }

  // Data arguments are last.
  // See the comment in this file's header comment.
  switch (extraData) {
  case ExtraData::None: break;
  case ExtraData::Retainable: argTypes.push_back(IGM.RefCountedPtrTy); break;
  case ExtraData::Metatype: argTypes.push_back(IGM.TypeMetadataPtrTy); break;
  }

  // Ignore the first element of the array unless we have an aggregate result.
  llvm::ArrayRef<llvm::Type*> realArgTypes = argTypes;
  if (!hasAggregateResult)
    realArgTypes = realArgTypes.slice(1);

  // Create the appropriate LLVM type.
  llvm::FunctionType *llvmType =
    llvm::FunctionType::get(resultType, realArgTypes, /*variadic*/ false);

  // Update the cache and return.
  signature.set(llvmType, hasAggregateResult, attrs);
  return signature;
}

llvm::FunctionType *
IRGenModule::getFunctionType(AbstractCC cc,
                             CanType type, ExplosionKind explosionKind,
                             unsigned curryingLevel, ExtraData extraData,
                             llvm::AttributeSet &attrs) {
  assert(isa<AnyFunctionType>(type));
  const FuncTypeInfo &fnTypeInfo = getFragileTypeInfo(type).as<FuncTypeInfo>();
  Signature sig = fnTypeInfo.getSignature(*this, cc, explosionKind,
                                          curryingLevel, extraData);
  attrs = sig.getAttributes();
  return sig.getType();
}

llvm::FunctionType *
IRGenModule::getFunctionType(SILType type, ExplosionKind explosionKind,
                             ExtraData extraData,
                             llvm::AttributeSet &attrs) {
  assert(!type.isAddress());
  assert(type.is<AnyFunctionType>());
  return getFunctionType(type.getAbstractCC(), type.getSwiftType(),
                         explosionKind, 0,
                         extraData, attrs);
}

static bool isClassMethod(ValueDecl *vd) {
  if (!vd->getDeclContext())
    return false;
  if (!vd->getDeclContext()->getDeclaredTypeInContext())
    return false;
  return vd->getDeclContext()->getDeclaredTypeInContext()
  ->getClassOrBoundGenericClass();
}

AbstractCC irgen::getAbstractCC(ValueDecl *fn) {
  if (fn->isInstanceMember())
    return AbstractCC::Method;
  if (fn->hasClangNode()) {
    if (isClassMethod(fn))
      return AbstractCC::ObjCMethod;
    return AbstractCC::C;
  }
  return AbstractCC::Freestanding;
}

static AbstractCallee getAbstractDirectCallee(ValueDecl *val,
                                              ExplosionKind level,
                                              ExtraData extraData) {
  unsigned minUncurry = 0;
  if (val->getDeclContext()->isTypeContext())
    minUncurry = 1;
  unsigned maxUncurry = getDeclNaturalUncurryLevel(val);
  
  AbstractCC convention = getAbstractCC(val);

  return AbstractCallee(convention, level, minUncurry, maxUncurry, extraData);
}

/// Construct the best known limits on how we can call the given
/// global function.
AbstractCallee AbstractCallee::forDirectGlobalFunction(IRGenModule &IGM,
                                                       ValueDecl *val) {
  assert(!val->getDeclContext()->isLocalContext());

  // FIXME: be more aggressive about this.
  ExplosionKind level = ExplosionKind::Minimal;

  return getAbstractDirectCallee(val, level, ExtraData::None);
}

/// Return this function pointer, bitcasted to an i8*.
llvm::Value *Callee::getOpaqueFunctionPointer(IRGenFunction &IGF) const {
  if (FnPtr->getType() == IGF.IGM.Int8PtrTy)
    return FnPtr;
  return IGF.Builder.CreateBitCast(FnPtr, IGF.IGM.Int8PtrTy);
}

/// Return this data pointer.
llvm::Value *Callee::getDataPointer(IRGenFunction &IGF) const {
  if (hasDataPointer()) return DataPtr;
  return IGF.IGM.RefCountedNull;
}

static void extractScalarResults(IRGenFunction &IGF, llvm::Value *call,
                                 Explosion &out) {
  if (llvm::StructType *structType
        = dyn_cast<llvm::StructType>(call->getType())) {
    for (unsigned i = 0, e = structType->getNumElements(); i != e; ++i) {
      llvm::Value *scalar = IGF.Builder.CreateExtractValue(call, i);
      out.add(scalar);
    }
  } else {
    assert(!call->getType()->isVoidTy());
    out.add(call);
  }
}

static void emitCastBuiltin(IRGenFunction &IGF, FuncDecl *fn,
                            Explosion &result,
                            Explosion &args,
                            llvm::Instruction::CastOps opcode) {
  llvm::Value *input = args.claimNext();
  Type DestType = fn->getType()->castTo<AnyFunctionType>()->getResult();
  llvm::Type *destTy = IGF.IGM.getFragileTypeInfo(DestType).getStorageType();
  assert(args.empty() && "wrong operands to cast operation");
  llvm::Value *output = IGF.Builder.CreateCast(opcode, input, destTy);
  result.add(output);
}

static void emitCompareBuiltin(IRGenFunction &IGF, FuncDecl *fn,
                               Explosion &result,
                               Explosion &args,
                               llvm::CmpInst::Predicate pred) {
  llvm::Value *lhs = args.claimNext();
  llvm::Value *rhs = args.claimNext();
  
  llvm::Value *v;
  if (lhs->getType()->isFPOrFPVectorTy())
    v = IGF.Builder.CreateFCmp(pred, lhs, rhs);
  else
    v = IGF.Builder.CreateICmp(pred, lhs, rhs);
  
  result.add(v);
}

/// decodeLLVMAtomicOrdering - turn a string like "release" into the LLVM enum.
static llvm::AtomicOrdering decodeLLVMAtomicOrdering(StringRef O) {
  using namespace llvm;
  return StringSwitch<AtomicOrdering>(O)
    .Case("unordered", Unordered)
    .Case("monotonic", Monotonic)
    .Case("acquire", Acquire)
    .Case("release", Release)
    .Case("acqrel", AcquireRelease)
    .Case("seqcst", SequentiallyConsistent);
}

/// emitBuiltinCall - Emit a call to a builtin function.
void irgen::emitBuiltinCall(IRGenFunction &IGF, FuncDecl *fn,
                            Explosion &args, Explosion *out,
                            Address indirectOut,
                            ArrayRef<Substitution> substitutions) {
  assert(((out != nullptr) ^ indirectOut.isValid()) &&
         "cannot emit builtin to both explosion and memory");
  
  // Decompose the function's name into a builtin name and type list.
  SmallVector<Type, 4> Types;
  StringRef BuiltinName = getBuiltinBaseName(IGF.IGM.Context,
                                             fn->getName().str(), Types);

  // These builtins don't care about their argument:
  if (BuiltinName == "sizeof") {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    out->add(valueTI.getSize(IGF));
    return;
  }

  if (BuiltinName == "strideof") {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    out->add(valueTI.getStride(IGF));
    return;
  }

  if (BuiltinName == "alignof") {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    // The alignof value is one greater than the alignment mask.
    out->add(IGF.Builder.CreateAdd(valueTI.getAlignmentMask(IGF),
                                   IGF.IGM.getSize(Size(1))));
    return;
  }
  
  // addressof expects an lvalue argument.
  if (BuiltinName == "addressof") {
    llvm::Value *address = args.claimNext();
    llvm::Value *value = IGF.Builder.CreateBitCast(address,
                                                   IGF.IGM.Int8PtrTy);
    out->add(value);
    return;
  }

  // Everything else cares about the (rvalue) argument.
  
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
      IRArgs.push_back(args.claimNext());
    llvm::Value *TheCall = IGF.Builder.CreateCall(F, IRArgs);

    if (!TheCall->getType()->isVoidTy())
      extractScalarResults(IGF, TheCall, *out);

    return;
  }
  
  // TODO: A linear series of ifs is suboptimal.
#define BUILTIN_SIL_OPERATION(id, name, overload) \
  if (BuiltinName == name) \
    llvm_unreachable(name " builtin should be lowered away by SILGen!");

#define BUILTIN_CAST_OPERATION(id, name) \
  if (BuiltinName == name) \
    return emitCastBuiltin(IGF, fn, *out, args, llvm::Instruction::id);
  
#define BUILTIN_BINARY_OPERATION(id, name, overload) \
  if (BuiltinName == name) { \
    llvm::Value *lhs = args.claimNext(); \
    llvm::Value *rhs = args.claimNext(); \
    llvm::Value *v = IGF.Builder.Create##id(lhs, rhs); \
    return out->add(v); \
  }

#define BUILTIN_BINARY_PREDICATE(id, name, overload) \
  if (BuiltinName == name) \
    return emitCompareBuiltin(IGF, fn, *out, args, llvm::CmpInst::id);
#define BUILTIN(ID, Name)  // Ignore the rest.
#include "swift/AST/Builtins.def"

  if (BuiltinName == "fneg") {
    llvm::Value *rhs = args.claimNext();
    llvm::Value *lhs = llvm::ConstantFP::get(rhs->getType(), "-0.0");
    llvm::Value *v = IGF.Builder.CreateFSub(lhs, rhs);
    return out->add(v);
  }
  
  if (BuiltinName == "allocRaw") {
    auto size = args.claimNext();
    auto align = args.claimNext();
    // Translate the alignment to a mask.
    auto alignMask = IGF.Builder.CreateSub(align, IGF.IGM.getSize(Size(1)));
    auto alloc = IGF.emitAllocRawCall(size, alignMask, "builtin-allocRaw");
    out->add(alloc);
    return;
  }

  if (BuiltinName == "deallocRaw") {
    auto pointer = args.claimNext();
    auto size = args.claimNext();
    IGF.emitDeallocRawCall(pointer, size);
    return;
  }

  if (BuiltinName.startswith("fence_")) {
    BuiltinName = BuiltinName.drop_front(strlen("fence_"));
    // Decode the ordering argument, which is required.
    auto underscore = BuiltinName.find('_');
    auto ordering = decodeLLVMAtomicOrdering(BuiltinName.substr(0, underscore));
    BuiltinName = BuiltinName.substr(underscore);
    
    // Accept singlethread if present.
    bool isSingleThread = BuiltinName.startswith("_singlethread");
    if (isSingleThread)
      BuiltinName = BuiltinName.drop_front(strlen("_singlethread"));
    assert(BuiltinName.empty() && "Mismatch with sema");
    
    IGF.Builder.CreateFence(ordering,
                      isSingleThread ? llvm::SingleThread : llvm::CrossThread);
    return;
  }

  
  if (BuiltinName.startswith("cmpxchg_")) {
    BuiltinName = BuiltinName.drop_front(strlen("cmpxchg_"));
    // Decode the ordering argument, which is required.
    auto underscore = BuiltinName.find('_');
    auto ordering = decodeLLVMAtomicOrdering(BuiltinName.substr(0, underscore));
    BuiltinName = BuiltinName.substr(underscore);
    
    // Accept volatile and singlethread if present.
    bool isVolatile = BuiltinName.startswith("_volatile");
    if (isVolatile) BuiltinName = BuiltinName.drop_front(strlen("_volatile"));
    
    bool isSingleThread = BuiltinName.startswith("_singlethread");
    if (isSingleThread)
      BuiltinName = BuiltinName.drop_front(strlen("_singlethread"));
    assert(BuiltinName.empty() && "Mismatch with sema");

    auto pointer = args.claimNext();
    auto cmp = args.claimNext();
    auto newval = args.claimNext();

    llvm::Type *origTy = cmp->getType();
    if (origTy->isPointerTy()) {
      cmp = IGF.Builder.CreatePtrToInt(cmp, IGF.IGM.IntPtrTy);
      newval = IGF.Builder.CreatePtrToInt(newval, IGF.IGM.IntPtrTy);
    }

    pointer = IGF.Builder.CreateBitCast(pointer,
                                  llvm::PointerType::getUnqual(cmp->getType()));
    llvm::Value *value = IGF.Builder.CreateAtomicCmpXchg(pointer, cmp, newval,
                                                          ordering,
                      isSingleThread ? llvm::SingleThread : llvm::CrossThread);
    cast<llvm::AtomicCmpXchgInst>(value)->setVolatile(isVolatile);

    if (origTy->isPointerTy())
      value = IGF.Builder.CreateIntToPtr(value, origTy);

    out->add(value);
    return;
  }
  
  if (BuiltinName.startswith("atomicrmw_")) {
    using namespace llvm;
    
    BuiltinName = BuiltinName.drop_front(strlen("atomicrmw_"));
    auto underscore = BuiltinName.find('_');
    StringRef SubOp = BuiltinName.substr(0, underscore);
    
    auto SubOpcode = StringSwitch<AtomicRMWInst::BinOp>(SubOp)
      .Case("xchg", AtomicRMWInst::Xchg)
      .Case("add",  AtomicRMWInst::Add)
      .Case("sub",  AtomicRMWInst::Sub)
      .Case("and",  AtomicRMWInst::And)
      .Case("nand", AtomicRMWInst::Nand)
      .Case("or",   AtomicRMWInst::Or)
      .Case("xor",  AtomicRMWInst::Xor)
      .Case("max",  AtomicRMWInst::Max)
      .Case("min",  AtomicRMWInst::Min)
      .Case("umax", AtomicRMWInst::UMax)
      .Case("umin", AtomicRMWInst::UMin);
    BuiltinName = BuiltinName.drop_front(underscore+1);
    
    // Decode the ordering argument, which is required.
    underscore = BuiltinName.find('_');
    auto ordering = decodeLLVMAtomicOrdering(BuiltinName.substr(0, underscore));
    BuiltinName = BuiltinName.substr(underscore);
    
    // Accept volatile and singlethread if present.
    bool isVolatile = BuiltinName.startswith("_volatile");
    if (isVolatile) BuiltinName = BuiltinName.drop_front(strlen("_volatile"));
    
    bool isSingleThread = BuiltinName.startswith("_singlethread");
    if (isSingleThread)
      BuiltinName = BuiltinName.drop_front(strlen("_singlethread"));
    assert(BuiltinName.empty() && "Mismatch with sema");
    
    auto pointer = args.claimNext();
    auto val = args.claimNext();

    // Handle atomic ops on pointers by casting to intptr_t.
    llvm::Type *origTy = val->getType();
    if (origTy->isPointerTy())
      val = IGF.Builder.CreatePtrToInt(val, IGF.IGM.IntPtrTy);

    pointer = IGF.Builder.CreateBitCast(pointer,
                                  llvm::PointerType::getUnqual(val->getType()));
    llvm::Value *value = IGF.Builder.CreateAtomicRMW(SubOpcode, pointer, val,
                                                      ordering,
                      isSingleThread ? llvm::SingleThread : llvm::CrossThread);
    cast<AtomicRMWInst>(value)->setVolatile(isVolatile);

    if (origTy->isPointerTy())
      value = IGF.Builder.CreateIntToPtr(value, origTy);

    out->add(value);
    return;
  }

  llvm_unreachable("IRGen unimplemented for this builtin!");
}


/// Emit the unsubstituted result of this call into the given explosion.
/// The unsubstituted result must be naturally returned directly.
void CallEmission::emitToUnmappedExplosion(Explosion &out) {
  assert(RemainingArgsForCallee == 0);
  assert(LastArgWritten == 0 && "emitting unnaturally to explosion");
  assert(out.getKind() == getCallee().getExplosionLevel());

  auto call = emitCallSite(false);

  // Bail out immediately on a void result.
  llvm::Value *result = call.getInstruction();
  if (result->getType()->isVoidTy()) return;

  // Extract out the scalar results.
  extractScalarResults(IGF, result, out);
}

/// Emit the unsubstituted result of this call to the given address.
/// The unsubstituted result must be naturally returned indirectly.
void CallEmission::emitToUnmappedMemory(Address result) {
  assert(RemainingArgsForCallee == 0);
  assert(LastArgWritten == 1 && "emitting unnaturally to indirect result");

  Args[0] = result.getAddress();
  addIndirectReturnAttributes(IGF.IGM, Attrs);
#ifndef NDEBUG
  LastArgWritten = 0; // appease an assert
#endif
  
  emitCallSite(true);
}

// FIXME: This doesn't belong on IGF.
llvm::CallSite CallEmission::emitInvoke(llvm::CallingConv::ID convention,
                                        llvm::Value *fn,
                                        ArrayRef<llvm::Value*> args,
                                        const llvm::AttributeSet &attrs) {
  // TODO: exceptions!
  llvm::CallInst *call = IGF.Builder.CreateCall(fn, args);
  call->setAttributes(attrs);
  call->setCallingConv(convention);
  return call;
}

/// The private routine to ultimately emit a call or invoke instruction.
llvm::CallSite CallEmission::emitCallSite(bool hasIndirectResult) {
  assert(RemainingArgsForCallee == 0);
  assert(LastArgWritten == 0);
  assert(!EmittedCall);
  EmittedCall = true;

  // Determine the calling convention.
  // FIXME: collect attributes in the CallEmission.
  auto cc = expandAbstractCC(IGF.IGM, getCallee().getAbstractCC());

  // Make the call and clear the arguments array.
  auto fnPtr = getCallee().getFunctionPointer();  
  llvm::CallSite call = emitInvoke(cc, fnPtr, Args,
                                   llvm::AttributeSet::get(fnPtr->getContext(),
                                                           Attrs));
  Args.clear();

  // Return.
  return call;
}

enum class ResultDifference {
  /// The substituted result type is the same as the original result type.
  Identical,

  /// The substituted result type is a different formal type from, but
  /// has the same layout and interpretation as, the original result type.
  Aliasable,

  /// The substitued result type has the same layout as the original
  /// result type, but may differ in interpretation.
  // Reinterpretable,

  /// The substituted result type differs not just in interpretation,
  /// but in layout, from the original result type.
  Divergent
};

static ResultDifference computeResultDifference(IRGenModule &IGM,
                                                CanType origResultType,
                                                CanType substResultType) {
  if (origResultType == substResultType)
    return ResultDifference::Identical;

  if (differsByAbstractionInMemory(IGM, origResultType, substResultType))
    return ResultDifference::Divergent;

  return ResultDifference::Aliasable;
}

/// Emit the result of this call to memory.
void CallEmission::emitToMemory(Address addr, const TypeInfo &substResultTI) {
  assert(RemainingArgsForCallee == 0);
  assert(LastArgWritten <= 1);

  // If the call is naturally to an explosion, emit it that way and
  // then initialize the temporary.
  if (LastArgWritten == 0) {
    Explosion result(getCallee().getExplosionLevel());
    emitToExplosion(result);
    substResultTI.initialize(IGF, result, addr);
    return;
  }

  // Okay, we're naturally emitting to memory.
  Address origAddr = addr;

  // Figure out how the substituted result differs from the original.
  auto resultDiff = computeResultDifference(IGF.IGM, CurOrigType,
                       getCallee().getSubstResultType()->getCanonicalType());
  switch (resultDiff) {

  // For aliasable types, just bitcast the output address.
  case ResultDifference::Aliasable: {
    auto origTy = IGF.IGM.getStoragePointerType(CurOrigType);
    origAddr = IGF.Builder.CreateBitCast(origAddr, origTy);
    SWIFT_FALLTHROUGH;
  }

  case ResultDifference::Identical:
    emitToUnmappedMemory(origAddr);
    return;

  case ResultDifference::Divergent:
    // We need to do layout+allocation under substitution rules.
    return IGF.unimplemented(SourceLoc(), "divergent emission to memory");
  }
    
  llvm_unreachable("bad difference kind");
}

/// Emit the result of this call to an explosion.
void CallEmission::emitToExplosion(Explosion &out) {
  assert(RemainingArgsForCallee == 0);
  assert(LastArgWritten <= 1);

  // If the call is naturally to memory, emit it that way and then
  // explode that temporary.
  if (LastArgWritten == 1) {
    Type substResultType = getCallee().getSubstResultType();
    const TypeInfo &substResultTI = IGF.getFragileTypeInfo(substResultType);

    Address temp = substResultTI.allocate(IGF, NotOnHeap, "call.aggresult");
    emitToMemory(temp, substResultTI);
 
    // If the subst result is passed as an aggregate, don't uselessly
    // copy the temporary.
    auto substSchema = substResultTI.getSchema(out.getKind());
    if (substSchema.isSingleAggregate()) {
      auto substType = substSchema.begin()->getAggregateType()->getPointerTo();
      temp = IGF.Builder.CreateBitCast(temp, substType);
      out.add(temp.getAddress());

    } else {
      // Otherwise, we need to load.
      substResultTI.loadAsTake(IGF, temp, out);
    }
    return;
  }

  // Okay, we're naturally emitting to an explosion.
  // Figure out how the substituted result differs from the original.
  CanType substType = getCallee().getSubstResultType()->getCanonicalType();
  auto resultDiff = computeResultDifference(IGF.IGM, CurOrigType, substType);
  const TypeInfo &substResultTI =
    IGF.getFragileTypeInfo(getCallee().getSubstResultType());

  switch (resultDiff) {
  // If they don't differ at all, we're good. 
  case ResultDifference::Identical:
  case ResultDifference::Aliasable:
    // We can emit directly if the explosion levels match.
    if (out.getKind() == getCallee().getExplosionLevel()) {
      emitToUnmappedExplosion(out);

    // Otherwise we have to re-explode.
    } else {
      Explosion temp(getCallee().getExplosionLevel());
      emitToUnmappedExplosion(temp);

      substResultTI.reexplode(IGF, temp, out);
    }
    return;

  // If they do differ, we need to remap.
  case ResultDifference::Divergent:
    if (isa<MetaTypeType>(substType) && isa<MetaTypeType>(CurOrigType)) {
      // If we got here, it's because the substituted metatype is trivial.
      // Remapping is easy--the substituted type is empty, so we drop the
      // nontrivial representation of the original type.
      assert(IGF.IGM.hasTrivialMetatype(
                      CanType(cast<MetaTypeType>(substType)->getInstanceType()))
             && "remapping to nontrivial metatype?!");
      
      Explosion temp(getCallee().getExplosionLevel());
      emitToUnmappedExplosion(temp);
      temp.claimAll();
      return;
    }
      
    if (auto *origArchetype = dyn_cast<ArchetypeType>(CurOrigType)) {
      if (origArchetype->isClassBounded()) {
        // Remap a class-bounded archetype to an instance.
        assert(substType->getClassOrBoundGenericClass() &&
               "remapping class-bounded archetype to non-class?!");
        Explosion temp(getCallee().getExplosionLevel());
        emitToUnmappedExplosion(temp);
        llvm::Value *pointer = temp.claimNext();
        pointer = IGF.Builder.CreateBitCast(pointer,
                                            substResultTI.getStorageType());
        out.add(pointer);
        return;
      }
    }
      
    // There's a related FIXME in the Builtin.load/move code.
    IGF.unimplemented(SourceLoc(), "remapping explosion");
    const TypeInfo &substResultTI =
      IGF.getFragileTypeInfo(getCallee().getSubstResultType());
    IGF.emitFakeExplosion(substResultTI, out);
    return;
  }
    
  llvm_unreachable("bad difference kind");
}

CallEmission::CallEmission(CallEmission &&other)
  : IGF(other.IGF),
    Args(std::move(other.Args)),
    CurCallee(std::move(other.CurCallee)),
    CurOrigType(other.CurOrigType),
    RemainingArgsForCallee(other.RemainingArgsForCallee),
    LastArgWritten(other.LastArgWritten),
    EmittedCall(other.EmittedCall) {
  // Prevent other's destructor from asserting.
  other.invalidate();
}

CallEmission::~CallEmission() {
  assert(LastArgWritten == 0);
  assert(RemainingArgsForCallee == 0);
  assert(EmittedCall);
}

void CallEmission::invalidate() {
  LastArgWritten = 0;
  RemainingArgsForCallee = 0;
  EmittedCall = true;
}


/// Set up this emitter afresh from the current callee specs.
void CallEmission::setFromCallee() {
  RemainingArgsForCallee = CurCallee.getUncurryLevel() + 1;
  CurOrigType = CurCallee.getOrigFormalType()->getCanonicalType();
  EmittedCall = false;

  llvm::Type *fnType = CurCallee.getFunction()->getType();
  fnType = cast<llvm::PointerType>(fnType)->getElementType();
  unsigned numArgs = cast<llvm::FunctionType>(fnType)->getNumParams();

  // Set up the args array.
  assert(Args.empty());
  Args.reserve(numArgs);
  Args.set_size(numArgs);
  LastArgWritten = numArgs;

  // Add the data pointer if we have one.
  if (CurCallee.hasDataPointer()) {
    assert(LastArgWritten > 0);
    Args[--LastArgWritten] = CurCallee.getDataPointer(IGF);
  }
}

/// Drill down to the result type of the original function type.
static void drillIntoOrigFnType(Type &origFnType) {
  if (auto fnType = origFnType->getAs<AnyFunctionType>()) {
    origFnType = fnType->getResult();
  } else {
    // This can happen if we're substituting a function type in.
    // In this case, we should interpret arguments using a
    // fully-abstracted function type, i.e. T -> U.  We don't
    // really need U to be any *specific* archetype, though,
    // so we just leave it as the original archetype.
    assert(origFnType->is<ArchetypeType>());
  }
}

/// We're about to pass arguments to something. Ensure the current
/// callee has additional arguments.
void CallEmission::forceCallee() {
  assert(RemainingArgsForCallee && "callee doesn't take any more args?!");
  --RemainingArgsForCallee;
}

/// Does the given convention grow clauses left-to-right?
/// Swift generally grows right-to-left, but ObjC needs us
/// to go left-to-right.
static bool isLeftToRight(AbstractCC cc) {
  switch (cc) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    return true;
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    return false;
  }
}

/// Does an ObjC method or C function returning the given type require an
/// sret indirect result?
llvm::PointerType *irgen::requiresExternalIndirectResult(IRGenModule &IGM,
                                                         SILType type) {
  // FIXME: we need to consider the target's C calling convention.
  return IGM.requiresIndirectResult(type.getSwiftRValueType(),
                                    ExplosionKind::Minimal);
}

/// Does an argument of this type need to be passed by value on the stack to
/// C or ObjC arguments?
llvm::PointerType *irgen::requiresExternalByvalArgument(IRGenModule &IGM,
                                                        CanType type) {
  // FIXME: we need to consider the target's C calling convention.
  return IGM.requiresIndirectResult(type, ExplosionKind::Minimal);
}

llvm::PointerType *irgen::requiresExternalByvalArgument(IRGenModule &IGM,
                                                        SILType type) {
  return requiresExternalByvalArgument(IGM, type.getSwiftRValueType());
}

void CallEmission::externalizeArgument(Explosion &out, Explosion &in,
                     SmallVectorImpl<std::pair<unsigned, Alignment>> &newByvals,
                     CanType ty) {
  TypeInfo const &ti = IGF.getFragileTypeInfo(ty);
  if (requiresExternalByvalArgument(IGF.IGM, ty)) {
    OwnedAddress addr = ti.allocate(IGF, NotOnHeap, "byval-temporary");
    ti.initialize(IGF, in, addr.getAddress());
     
    newByvals.push_back({out.size(), addr.getAlignment()});
    out.add(addr.getAddress().getAddress());
  } else {
    ti.reexplode(IGF, in, out);
  }
}

/// Convert exploded Swift arguments into C-compatible arguments.
void CallEmission::externalizeArguments(Explosion &out, Explosion &arg,
                    SmallVectorImpl<std::pair<unsigned, Alignment>> &newByvals,
                    CanType inputsTy) {
  if (TupleType *tupleTy = inputsTy->getAs<TupleType>()) {
    for (auto &elt : tupleTy->getFields()) {
      externalizeArgument(out, arg, newByvals,
                          elt.getType()->getCanonicalType());
    }
  } else {
    externalizeArgument(out, arg, newByvals, inputsTy);
  }  
}

/// Add a new set of arguments to the function.
void CallEmission::addArg(Explosion &arg) {
  forceCallee();
  llvm::SmallVector<std::pair<unsigned, Alignment>, 2> newByvals;
  
  // Convert arguments to a representation appropriate to the calling
  // convention.
  AbstractCC cc = CurCallee.getAbstractCC();
  switch (cc) {
  case AbstractCC::C: {
    Explosion externalized(arg.getKind());
    externalizeArguments(externalized, arg, newByvals,
                   CanType(CurOrigType->castTo<AnyFunctionType>()->getInput()));
    arg = std::move(externalized);
    break;
  }
  case AbstractCC::ObjCMethod: {
    // The method will be uncurried to (Self, (Args...)). The _cmd argument
    // goes in between.
    Explosion externalized(arg.getKind());
    // self
    externalized.add(arg.claimNext());
    // _cmd
    externalized.add(arg.claimNext());
    // method args
    TupleType *inputTuple = CurOrigType->castTo<AnyFunctionType>()->getInput()
      ->castTo<TupleType>();
    assert(inputTuple->getFields().size() == 2 && "invalid objc method type");
    externalizeArguments(externalized, arg, newByvals,
                         CanType(inputTuple->getFields()[1].getType()));
    arg = std::move(externalized);
    break;
  }
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
    // Nothing to do.
    break;
  }

  // Add the given number of arguments.
  assert(getCallee().getExplosionLevel() == arg.getKind());
  assert(LastArgWritten >= arg.size());
  unsigned newLastArgWritten = LastArgWritten - arg.size();

  size_t targetIndex;

  if (isLeftToRight(getCallee().getAbstractCC())) {
    // Shift the existing arguments to the left.
    size_t numArgsToMove = Args.size() - LastArgWritten;
    for (size_t i = 0, e = numArgsToMove; i != e; ++i) {
      Args[newLastArgWritten + i] = Args[LastArgWritten + i];
    }
    targetIndex = newLastArgWritten + numArgsToMove;
  } else {
    targetIndex = newLastArgWritten;
  }
  
  // Add byval attributes.
  // FIXME: These should in theory be moved around with the arguments when
  // isLeftToRight, but luckily ObjC methods and C functions should only ever
  // have byvals in the last argument clause.
  for (auto &byval : newByvals)
    addByvalArgumentAttributes(IGF.IGM, Attrs, byval.first+targetIndex,
                               byval.second);

  auto argIterator = Args.begin() + targetIndex;
  auto values = arg.claimAll();
  // fIXME: Should be w ritten as a std::copy.
  for (unsigned i = 0, e = values.size(); i != e; ++i) {
    auto value = values[i];
    *argIterator++ = value;
  }

  LastArgWritten = newLastArgWritten;

  // Walk into the original function type.
  drillIntoOrigFnType(CurOrigType);
}

/// Add a new set of arguments to the function, adjusting their abstraction
/// level as needed for the active substitutions.
void CallEmission::addSubstitutedArg(CanType substInputType, Explosion &arg) {
  // If we're calling something with polymorphic type, we'd better have
  // substitutions.
  auto subs = getSubstitutions();
  assert(!subs.empty() || !isa<PolymorphicFunctionType>(CurOrigType));

  // If we have no substitutions, go through the default path.
  if (subs.empty()) {
    addArg(arg);
    return;
  }
  
  // If we have substitutions, then (1) it's possible for this to
  // be a polymorphic function type that we need to expand and
  // (2) we might need to reexplode the value differently.
  Explosion argE(arg.getKind());
  CanType origInputType;
  auto fnType = dyn_cast<AnyFunctionType>(CurOrigType);
  if (fnType) {
    origInputType = CanType(fnType->getInput());
  } else {
    assert(isa<ArchetypeType>(CurOrigType));
    origInputType = CurOrigType;
  }
  
  reemitAsUnsubstituted(IGF, origInputType, substInputType, subs, arg, argE);
  
  // FIXME: this doesn't handle instantiating at a generic type.
  if (auto polyFn = dyn_cast_or_null<PolymorphicFunctionType>(fnType)) {
    emitPolymorphicArguments(IGF, polyFn, substInputType, subs, argE);
  }
  
  addArg(argE);
}


/// Initialize an Explosion with the parameters of the current
/// function.  All of the objects will be added unmanaged.  This is
/// really only useful when writing prologue code.
Explosion IRGenFunction::collectParameters() {
  Explosion params(CurExplosionLevel);
  for (auto i = CurFn->arg_begin(), e = CurFn->arg_end(); i != e; ++i)
    params.add(i);
  return params;
}

/// Emit a specific parameter clause.
static void emitParameterClause(IRGenFunction &IGF, AnyFunctionType *fnType,
                                Pattern *param, Explosion &args) {
  assert(param->getType()->getUnlabeledType(IGF.IGM.Context)
         ->isEqual(fnType->getInput()->getUnlabeledType(IGF.IGM.Context)));
  
  // If the function type at this level is polymorphic, bind all the
  // archetypes.
  if (auto polyFn = dyn_cast<PolymorphicFunctionType>(fnType))
    emitPolymorphicParameters(IGF, polyFn, args);
}

/// Emit all the parameter clauses of the given function type.  This
/// is basically making sure that we have mappings for all the
/// VarDecls bound by the pattern.
void irgen::emitParameterClauses(IRGenFunction &IGF,
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

/// Emit the basic block that 'return' should branch to and insert it into
/// the current function. This creates a second
/// insertion point that most blocks should be inserted before.
void IRGenFunction::emitBBForReturn() {
  ReturnBB = createBasicBlock("return");
  CurFn->getBasicBlockList().push_back(ReturnBB);
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
}

/// Emit a branch to the return block and set the insert point there.
/// Returns true if the return block is reachable, false otherwise.
bool IRGenFunction::emitBranchToReturnBB() {
  // If there are no edges to the return block, we never want to emit it.
  if (ReturnBB->use_empty()) {
    ReturnBB->eraseFromParent();
    
    // Normally this means that we'll just insert the epilogue in the
    // current block, but if the current IP is unreachable then so is
    // the entire epilogue.
    if (!Builder.hasValidIP())
      return false;
    
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
  return true;
}

/// Emit the epilogue for the function.
void IRGenFunction::emitEpilogue() {
  // Destroy the alloca insertion point.
  AllocaIP->eraseFromParent();
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
    for (unsigned i = 0, e = result.size(); i != e; ++i) {
      llvm::Value *elt = result.claimNext();
      resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
    }
    Builder.CreateRet(resultAgg);
  }
}

/// Emit the definition for the given SIL constant.
void IRGenModule::emitSILFunction(SILFunction *f) {
  if (f->isExternalDeclaration())
    return;
    
  // FIXME: Emit all needed explosion levels.
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  IRGenSILFunction(*this, f, explosionLevel).emitSILFunction();
}

/// Emit the forwarding stub function for a partial application.
static llvm::Function *emitPartialApplicationForwarder(IRGenModule &IGM,
                                       llvm::Function *fnPtr,
                                       ExplosionKind explosionLevel,
                                       SILType outType,
                                       HeapLayout const &layout) {
  llvm::AttributeSet attrs;
  ExtraData extraData
    = layout.isKnownEmpty() ? ExtraData::None : ExtraData::Retainable;
  llvm::FunctionType *fwdTy = IGM.getFunctionType(AbstractCC::Freestanding,
                                                  outType.getSwiftRValueType(),
                                                  explosionLevel,
                                                  /*curryLevel=*/ 0,
                                                  extraData,
                                                  attrs);
  // FIXME: Give the thunk a real name.
  // FIXME: Maybe cache the thunk by function and closure types? Could there
  // be multiple same-type closures off the same 
  llvm::Function *fwd =
    llvm::Function::Create(fwdTy, llvm::Function::InternalLinkage,
                           "partial_apply", &IGM.Module);
  fwd->setAttributes(attrs);

  IRGenFunction subIGF(IGM, explosionLevel, fwd);
  
  Explosion params = subIGF.collectParameters();

  // FIXME: support
  NonFixedOffsets offsets = Nothing;
  
  // If there's a data pointer required, grab it (it's always the
  // last parameter) and load out the extra, previously-curried
  // parameters.
  if (!layout.isKnownEmpty()) {
    llvm::Value *rawData = params.takeLast();
    Address data = layout.emitCastTo(subIGF, rawData);
    
    // Perform the loads.
    for (auto &fieldLayout : layout.getElements()) {
      Address fieldAddr = fieldLayout.project(subIGF, data, offsets);
      fieldLayout.getType().load(subIGF, fieldAddr, params);
    }
    
    // Kill the allocated data pointer immediately.  The safety of
    // this assumes that neither this release nor any of the loads
    // can throw.
    subIGF.emitRelease(rawData);
  }
  
  llvm::CallInst *call = subIGF.Builder.CreateCall(fnPtr, params.claimAll());
  call->setAttributes(fnPtr->getAttributes());
  call->setCallingConv(fnPtr->getCallingConv());
  call->setTailCall();
  
  if (call->getType()->isVoidTy())
    subIGF.Builder.CreateRetVoid();
  else
    subIGF.Builder.CreateRet(call);
  
  return fwd;
}

/// Emit a partial application thunk for a function pointer applied to a partial
/// set of argument values.
void irgen::emitFunctionPartialApplication(IRGenFunction &IGF,
                                           llvm::Function *fnPtr,
                                           Explosion &args,
                                           ArrayRef<SILType> argTypes,
                                           ArrayRef<Substitution> subs,
                                           SILType origType,
                                           SILType substType,
                                           SILType outType,
                                           Explosion &out) {
  // Collect the type infos for the context types.
  // FIXME: Keep LValueTypes out of this.
  llvm::SmallVector<const TypeInfo *, 4> argTypeInfos;
  for (SILType argType : argTypes) {
    argTypeInfos.push_back(&IGF.getFragileTypeInfo(argType.getSwiftType()));
  }
  
  // Collect the polymorphic arguments.
  Explosion polymorphicArgs(IGF.CurExplosionLevel);
  
  if (PolymorphicFunctionType *pft = origType.getAs<PolymorphicFunctionType>()) {
    assert(!subs.empty() && "no substitutions for polymorphic argument?!");
    emitPolymorphicArguments(IGF, pft,
                         CanType(substType.castTo<FunctionType>()->getInput()),
                         subs,
                         polymorphicArgs);

    const TypeInfo &metatypeTI = IGF.IGM.getTypeMetadataPtrTypeInfo(),
                   &witnessTI = IGF.IGM.getWitnessTablePtrTypeInfo();
    for (llvm::Value *arg : polymorphicArgs.getAll()) {
      if (arg->getType() == IGF.IGM.TypeMetadataPtrTy)
        argTypeInfos.push_back(&metatypeTI);
      else if (arg->getType() == IGF.IGM.WitnessTablePtrTy)
        argTypeInfos.push_back(&witnessTI);
      else
        llvm_unreachable("unexpected polymorphic argument");
    }
    
    args.add(polymorphicArgs.claimAll());
  } else {
    assert(subs.empty() && "substitutions for non-polymorphic function?!");
  }

  // Store the context arguments on the heap.
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, argTypeInfos);
  llvm::Value *data;
  if (layout.isKnownEmpty()) {
    data = IGF.IGM.RefCountedNull;
  } else {
    // Allocate a new object.
    data = IGF.emitUnmanagedAlloc(layout, "closure");
    Address dataAddr = layout.emitCastTo(IGF, data);

    // FIXME: preserve non-fixed offsets
    NonFixedOffsets offsets = Nothing;
    
    // Perform the store.
    for (auto &fieldLayout : layout.getElements()) {
      Address fieldAddr = fieldLayout.project(IGF, dataAddr, offsets);
      fieldLayout.getType().initialize(IGF, args, fieldAddr);
    }
  }
  assert(args.empty() && "unused args in partial application?!");
  
  // Create the forwarding stub.
  llvm::Function *forwarder = emitPartialApplicationForwarder(IGF.IGM,
                                                              fnPtr,
                                                              args.getKind(),
                                                              outType,
                                                              layout);
  llvm::Value *forwarderValue = IGF.Builder.CreateBitCast(forwarder,
                                                          IGF.IGM.Int8PtrTy);
  out.add(forwarderValue);
  out.add(data);
}

/// Emit a specialization thunk from a generic function to a specialized
/// function type.
llvm::Function *irgen::emitFunctionSpecialization(IRGenModule &IGM,
                                        llvm::Function *fnPtr,
                                        SILType genericType,
                                        SILType substType,
                                        ArrayRef<Substitution> substitutions,
                                        ExplosionKind explosionLevel) {
  // FIXME: Specializations to local archetypes need to take metadata/wtable
  // arguments, either as a closure box or as additional parameters.
  
  // Create the thunk function.
  llvm::AttributeSet attrs;
  llvm::FunctionType *specTy = IGM.getFunctionType(substType,
                                                   explosionLevel,
                                                   ExtraData::None,
                                                   attrs);
  // FIXME: Give the thunk a real name.
  // FIXME: Cache the thunk by to/from types
  llvm::Function *spec =
    llvm::Function::Create(specTy, llvm::Function::InternalLinkage,
                           "specialize", &IGM.Module);
  spec->setAttributes(attrs);
  
  IRGenFunction subIGF(IGM, explosionLevel, spec);

  Explosion params = subIGF.collectParameters();
  
  // Collect the indirect return address, if present.
  Address indirectReturn;
  SILType retTy
    = substType.getFunctionTypeInfo(*IGM.SILMod)->getSemanticResultType();
  TypeInfo const &retTI = IGM.getFragileTypeInfo(retTy);
                
  ExplosionSchema schema = retTI.getSchema(explosionLevel);
  if (schema.requiresIndirectResult())
    indirectReturn = retTI.getAddressForPointer(params.claimNext());
  
  // Apply the arguments in a call to the generic function.
  Callee callee = Callee::forKnownFunction(genericType,
                                           retTy,
                                           substitutions,
                                           fnPtr,
                                           nullptr,
                                           explosionLevel);
  CallEmission emission(subIGF, callee);
  FunctionType *ft = substType.castTo<FunctionType>();
  emission.addSubstitutedArg(CanType(ft->getInput()), params);
  assert(params.empty() && "did not claim all parameters?!");
  
  // Return the result of the call.
  if (indirectReturn.isValid()) {
    emission.emitToMemory(indirectReturn, retTI);
    subIGF.Builder.CreateRetVoid();
  } else {
    Explosion result(explosionLevel);
    emission.emitToExplosion(result);
    subIGF.emitScalarReturn(result);
  }
  
  // Return the specialization thunk, cast to the void pointer type.
  return spec;
}

/// Fetch the declaration of the given block-to-
llvm::Function *IRGenModule::getAddrOfBridgeToBlockConverter(SILType blockType)
{
  LinkEntity entity
    = LinkEntity::forBridgeToBlockConverter(blockType);
  
  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) return cast<llvm::Function>(entry);
  
  // The block converter is a C function with signature
  // __typeof__(R (^)(A...)) converter(R (*)(A..., swift_refcounted*),
  //                                   swift_refcounted*)
  // We simplify that to the llvm type %objc(i8*, %swift.refcounted*)*.
  llvm::Type *fnParams[] = {Int8PtrTy, RefCountedPtrTy};
  llvm::FunctionType *fnType = llvm::FunctionType::get(ObjCPtrTy,
                                                       fnParams,
                                                       /*isVarArg=*/ false);
  
  
  llvm::AttributeSet attrs;
  auto cc = expandAbstractCC(*this, AbstractCC::C);
  
  LinkInfo link = LinkInfo::get(*this, entity);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// Emit a call to convert a Swift closure to an Objective-C block via a
/// shim function defined in Objective-C.
void irgen::emitBridgeToBlock(IRGenFunction &IGF,
                              SILType blockTy,
                              Explosion &swiftClosure,
                              Explosion &outBlock) {
  // Get the function pointer as an i8*.
  llvm::Value *fn = swiftClosure.claimNext();
  fn = IGF.Builder.CreateBitCast(fn, IGF.IGM.Int8PtrTy);

  // Get the context pointer as a %swift.refcounted*.
  llvm::Value *mContext = swiftClosure.claimNext();
  llvm::Value *context = IGF.Builder.CreateBitCast(mContext,
                                                   IGF.IGM.RefCountedPtrTy);
  // Get the shim function we'll call.
  llvm::Function *converter = IGF.IGM.getAddrOfBridgeToBlockConverter(blockTy);
  
  // Emit the call.
  outBlock.add(IGF.Builder.CreateCall2(converter, fn, context));
}

namespace {

struct EmitLocalDecls : public ASTWalker {
  IRGenModule &IGM;
  
  EmitLocalDecls(IRGenModule &IGM) : IGM(IGM) {}
  
  bool walkToDeclPre(Decl *D) override {
    switch (D->getKind()) {
    case DeclKind::Import:
    case DeclKind::Subscript:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::OneOfElement:
    case DeclKind::Constructor:
    case DeclKind::Destructor:
    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
      llvm_unreachable("declaration cannot appear in local scope");
      
    case DeclKind::TypeAlias:
      // no IR generation support required.
    case DeclKind::PatternBinding:
    case DeclKind::Var:
      // These get lowered by SIL.
      return false;
      
    case DeclKind::Func:
      // The body gets lowered by SIL, but we need to check for local decls.
      IGM.emitLocalDecls(cast<FuncDecl>(D));
      return false;
      
    case DeclKind::OneOf:
      IGM.emitOneOfDecl(cast<OneOfDecl>(D));
      return false;
      
    case DeclKind::Struct:
      IGM.emitStructDecl(cast<StructDecl>(D));
      return false;
      
    case DeclKind::Class:
      IGM.emitClassDecl(cast<ClassDecl>(D));
      return false;
    }
  }
  
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (auto *FE = dyn_cast<FuncExpr>(E)) {
      IGM.emitLocalDecls(FE->getBody());
      return { false, E };
    }
    if (auto *CE = dyn_cast<PipeClosureExpr>(E)) {
      IGM.emitLocalDecls(CE->getBody());
      return { false, E };
    }
    return { true, E };
  }
};

} // end anonymous namespace

void IRGenModule::emitLocalDecls(BraceStmt *body) {
  EmitLocalDecls walker(*this);
  body->walk(walker);
}

void IRGenModule::emitLocalDecls(FuncDecl *fd) {
  if (fd->getBody() && fd->getBody()->getBody())
    emitLocalDecls(fd->getBody()->getBody());
}

void IRGenModule::emitLocalDecls(ConstructorDecl *cd) {
  if (cd->getBody())
    emitLocalDecls(cd->getBody());
}

void IRGenModule::emitLocalDecls(DestructorDecl *dd) {
  if (dd->getBody())
    emitLocalDecls(dd->getBody());
}

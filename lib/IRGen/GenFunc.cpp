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
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Types.h"
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
#include "GenInit.h"
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

const TypeInfo &IRGenFunction::getResultTypeInfo() const {
  CanType resultType = getResultType(CurFuncType, CurUncurryLevel);
  return IGM.getFragileTypeInfo(resultType);
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
    return llvm::CallingConv::C;

  case AbstractCC::Method:
    //   TODO: maybe add 'inreg' to the first non-result argument.
    [[clang::fallthrough]];
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

    Callee getDirectValuesAsIndirectCallee(CanType origFormalType,
                                           CanType substResultType,
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
      Signature Signatures[3][2][3];

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

    static void doLoad(IRGenFunction &IGF, Address address, Explosion &e) {
      // Load the function.
      Address fnAddr = projectFunction(IGF, address);
      e.addUnmanaged(IGF.Builder.CreateLoad(fnAddr, fnAddr->getName()+".load"));

      // Load the data.
      Address dataAddr = projectData(IGF, address);
      IGF.emitLoadAndRetain(dataAddr, e);
    }

    void load(IRGenFunction &IGF, Address address, Explosion &e) const {
      doLoad(IGF, address, e);
    }

    static void doLoadUnmanaged(IRGenFunction &IGF, Address address,
                                Explosion &e) {
      // Load the function.
      Address fnAddr = projectFunction(IGF, address);
      e.addUnmanaged(IGF.Builder.CreateLoad(fnAddr, fnAddr->getName()+".load"));
      
      // Load the data.
      Address dataAddr = projectData(IGF, address);
      e.addUnmanaged(IGF.Builder.CreateLoad(dataAddr));
    }
    
    void loadUnmanaged(IRGenFunction &IGF, Address address,
                       Explosion &e) const {
      doLoadUnmanaged(IGF, address, e);
    }
    
    static void doLoadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) {
      // Load the function.
      Address fnAddr = projectFunction(IGF, addr);
      e.addUnmanaged(IGF.Builder.CreateLoad(fnAddr));

      // Load the data.
      Address dataAddr = projectData(IGF, addr);
      e.add(ManagedValue(IGF.Builder.CreateLoad(dataAddr)));
    }

    void loadAsTake(IRGenFunction &IGF, Address address, Explosion &e) const {
      doLoadAsTake(IGF, address, e);
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
      dest.add(ManagedValue(src.claimUnmanagedNext()));
    }

    void retain(IRGenFunction &IGF, Explosion &e) const {
      e.claimUnmanagedNext();
      IGF.emitRetainCall(e.claimNext().getValue());
    }
    
    void release(IRGenFunction &IGF, Explosion &e) const {
      e.claimUnmanagedNext();
      IGF.emitRelease(e.claimNext().getValue());
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

bool irgen::isBlockFunctionType(Type t) {
  if (FunctionType *ft = t->getAs<FunctionType>()) {
    return ft->isBlock();
  }
  return false;
}

const TypeInfo *TypeConverter::convertFunctionType(AnyFunctionType *T) {
  if (isBlockFunctionType(T))
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
  if (cc == AbstractCC::C && requiresExternalByvalArgument(IGM, argTy)) {
    const TypeInfo &ti = IGM.getFragileTypeInfo(argTy);
    llvm::Constant *alignConstant = ti.getStaticAlignment(IGM);
    // FIXME: non-static alignment?
    size_t alignValue = alignConstant
      ? alignConstant->getUniqueInteger().getZExtValue()
      : 1;
    byvals.push_back({argTypes.size(), Alignment(alignValue)});
    argTypes.push_back(ti.getStorageType()->getPointerTo());
  } else {
    auto schema = IGM.getSchema(argTy, explosionKind);
    schema.addToArgTypes(IGM, argTypes);
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
  auto fn = cast<AnyFunctionType>(type);

  // Save up the formal parameter types in reverse order.
  llvm::SmallVector<AnyFunctionType*, 8> formalFnTypes(uncurryLevel + 1);
  formalFnTypes[uncurryLevel] = fn;
  while (uncurryLevel--) {
    fn = cast<AnyFunctionType>(CanType(fn->getResult()));
    formalFnTypes[uncurryLevel] = fn;
  }

  // Explode the argument clusters in that reversed order.
  for (AnyFunctionType *fnTy : formalFnTypes) {
    CanType inputTy = CanType(fnTy->getInput());
    if (TupleType *tupleTy = inputTy->getAs<TupleType>()) {
      for (auto &field : tupleTy->getFields()) {
        decomposeFunctionArg(IGM, CanType(field.getType()), cc, explosionKind,
                             argTypes, byvals, attrs);
      }
    } else {
      decomposeFunctionArg(IGM, inputTy, cc, explosionKind,
                           argTypes, byvals, attrs);
    }
    
    if (auto polyTy = dyn_cast<PolymorphicFunctionType>(fnTy))
      expandPolymorphicSignature(IGM, polyTy, argTypes);
  }

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

AbstractCC irgen::getAbstractCC(ValueDecl *fn) {
  if (fn->isInstanceMember())
    return AbstractCC::Method;
  if (fn->hasClangNode())
    return AbstractCC::C;
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
ManagedValue Callee::getDataPointer(IRGenFunction &IGF) const {
  if (hasDataPointer()) return DataPtr;
  return ManagedValue(IGF.IGM.RefCountedNull);
}

static llvm::Value *emitCastOfIndirectFunction(IRGenFunction &IGF,
                                               llvm::Value *fnPtr,
                                               ManagedValue dataPtr,
                                               CanType origFnType) {
  bool hasData = !isa<llvm::ConstantPointerNull>(dataPtr.getValue());
  auto extraData = (hasData ? ExtraData::Retainable : ExtraData::None);
  llvm::AttributeSet attrs;
  auto fnPtrTy = IGF.IGM.getFunctionType(AbstractCC::Freestanding,
                                         origFnType, ExplosionKind::Minimal,
                                         0, extraData, attrs)->getPointerTo();
  return IGF.Builder.CreateBitCast(fnPtr, fnPtrTy);
}

/// Given a function pointer derived from an indirect source,
/// construct a callee appropriately.
static Callee emitIndirectCallee(IRGenFunction &IGF,
                                 llvm::Value *fnPtr,
                                 ManagedValue dataPtr,
                                 ArrayRef<Substitution> subs,
                                 CanType origFnType,
                                 CanType substResultType) {
  // Cast the function pointer appropriately.
  fnPtr = emitCastOfIndirectFunction(IGF, fnPtr, dataPtr, origFnType);

  return Callee::forIndirectCall(origFnType, substResultType, subs,
                                 fnPtr, dataPtr);
}


static void extractUnmanagedScalarResults(IRGenFunction &IGF,
                                          llvm::Value *call,
                                          Explosion &out) {
  if (llvm::StructType *structType
        = dyn_cast<llvm::StructType>(call->getType())) {
    for (unsigned i = 0, e = structType->getNumElements(); i != e; ++i) {
      llvm::Value *scalar = IGF.Builder.CreateExtractValue(call, i);
      out.addUnmanaged(scalar);
    }
  } else {
    assert(!call->getType()->isVoidTy());
    out.addUnmanaged(call);
  }
}

/// Extract the direct scalar results of a call instruction into an
/// explosion, registering cleanups as appropriate for the type.
static void extractScalarResults(IRGenFunction &IGF, llvm::Value *call,
                                 const TypeInfo &resultTI, Explosion &out) {
  // We need to make a temporary explosion to hold the values as we
  // tag them with cleanups.
  Explosion tempExplosion(out.getKind());

  // Extract the values.
  extractUnmanagedScalarResults(IGF, call, tempExplosion);

  // Take ownership.
  resultTI.manage(IGF, tempExplosion, out);
}


/// Given an address representing an unsafe pointer to the given type,
/// turn it into a valid Address.
static Address getAddressForUnsafePointer(IRGenFunction &IGF,
                                          const TypeInfo &type,
                                          llvm::Value *addr) {
  llvm::Value *castAddr =
  IGF.Builder.CreateBitCast(addr, type.getStorageType()->getPointerTo());
  return type.getAddressForPointer(castAddr);
}


static void emitCastBuiltin(IRGenFunction &IGF, FuncDecl *fn,
                            Explosion &result,
                            Explosion &args,
                            llvm::Instruction::CastOps opcode) {
  llvm::Value *input = args.claimUnmanagedNext();
  Type DestType = fn->getType()->castTo<AnyFunctionType>()->getResult();
  llvm::Type *destTy = IGF.IGM.getFragileTypeInfo(DestType).getStorageType();
  assert(args.empty() && "wrong operands to cast operation");
  llvm::Value *output = IGF.Builder.CreateCast(opcode, input, destTy);
  result.addUnmanaged(output);
}

static void emitCompareBuiltin(IRGenFunction &IGF, FuncDecl *fn,
                               Explosion &result,
                               Explosion &args,
                               llvm::CmpInst::Predicate pred) {
  llvm::Value *lhs = args.claimUnmanagedNext();
  llvm::Value *rhs = args.claimUnmanagedNext();
  
  llvm::Value *v;
  if (lhs->getType()->isFPOrFPVectorTy())
    v = IGF.Builder.CreateFCmp(pred, lhs, rhs);
  else
    v = IGF.Builder.CreateICmp(pred, lhs, rhs);
  
  result.addUnmanaged(v);
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
  
  // True if the builtin returns void.
  bool voidResult = false;
  
  // Decompose the function's name into a builtin name and type list.
  SmallVector<Type, 4> Types;
  StringRef BuiltinName = getBuiltinBaseName(IGF.IGM.Context,
                                             fn->getName().str(), Types);

  // These builtins don't care about their argument:
  if (BuiltinName == "sizeof") {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    out->addUnmanaged(valueTI.getSize(IGF));
    return;
  }

  if (BuiltinName == "strideof") {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    out->addUnmanaged(valueTI.getStride(IGF));
    return;
  }

  if (BuiltinName == "alignof") {
    args.claimAll();
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    out->addUnmanaged(valueTI.getAlignment(IGF));
    return;
  }
  
  // addressof expects an lvalue argument.
  if (BuiltinName == "addressof") {
    llvm::Value *address = args.claimUnmanagedNext();
    llvm::Value *value = IGF.Builder.CreateBitCast(address,
                                                   IGF.IGM.Int8PtrTy);
    out->addUnmanaged(value);
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
      IRArgs.push_back(args.claimUnmanagedNext());
    llvm::Value *TheCall = IGF.Builder.CreateCall(F, IRArgs);

    if (TheCall->getType()->isVoidTy())
      voidResult = true;
    else
      extractUnmanagedScalarResults(IGF, TheCall, *out);

    return;
  }
  
  // TODO: A linear series of ifs is suboptimal.
#define BUILTIN_CAST_OPERATION(id, name) \
  if (BuiltinName == name) \
    return emitCastBuiltin(IGF, fn, *out, args, llvm::Instruction::id);
  
#define BUILTIN_BINARY_OPERATION(id, name, overload) \
  if (BuiltinName == name) { \
    llvm::Value *lhs = args.claimUnmanagedNext(); \
    llvm::Value *rhs = args.claimUnmanagedNext(); \
    llvm::Value *v = IGF.Builder.Create##id(lhs, rhs); \
    return out->addUnmanaged(v); \
  }

#define BUILTIN_BINARY_PREDICATE(id, name, overload) \
  if (BuiltinName == name) \
    return emitCompareBuiltin(IGF, fn, *out, args, llvm::CmpInst::id);
#define BUILTIN(ID, Name)  // Ignore the rest.
#include "swift/AST/Builtins.def"

  if (BuiltinName == "fneg") {
    llvm::Value *rhs = args.claimUnmanagedNext();
    llvm::Value *lhs = llvm::ConstantFP::get(rhs->getType(), "-0.0");
    llvm::Value *v = IGF.Builder.CreateFSub(lhs, rhs);
    return out->addUnmanaged(v);
  }
  
  if (BuiltinName == "gep") {
    llvm::Value *lhs = args.claimUnmanagedNext();
    llvm::Value *rhs = args.claimUnmanagedNext();
    assert(args.empty() && "wrong operands to gep operation");
    
    // We don't expose a non-inbounds GEP operation.
    llvm::Value *gep = IGF.Builder.CreateInBoundsGEP(lhs, rhs);
    return out->addUnmanaged(gep);
  }

  if (BuiltinName == "load" || BuiltinName == "move") {
    // The type of the operation is the generic parameter type.
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);

    // Treat the raw pointer as a physical l-value of that type.
    // FIXME: remapping
    llvm::Value *addrValue = args.claimUnmanagedNext();
    Address addr = getAddressForUnsafePointer(IGF, valueTI, addrValue);

    // Handle the result being naturally in memory.
    if (indirectOut.isValid()) {
      if (BuiltinName == "move")
        return valueTI.initializeWithTake(IGF, indirectOut, addr);
      return valueTI.initializeWithCopy(IGF, indirectOut, addr);
    }
    
    // Perform the load.
    if (BuiltinName == "move")
      return valueTI.loadAsTake(IGF, addr, *out);
    return valueTI.load(IGF, addr, *out);
  }

  if (BuiltinName == "destroy") {
    // The type of the operation is the generic parameter type.
    CanType valueTy = substitutions[0].Replacement->getCanonicalType();

    // Skip the metatype if it has a non-trivial representation.
    if (!IGF.IGM.hasTrivialMetatype(valueTy))
      args.claimUnmanagedNext();

    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);

    llvm::Value *addrValue = args.claimUnmanagedNext();
    Address addr = getAddressForUnsafePointer(IGF, valueTI, addrValue);
    valueTI.destroy(IGF, addr);

    voidResult = true;
    return;
  }

  if (BuiltinName == "assign") {
    // The type of the operation is the generic parameter type.
    CanType valueTy = substitutions[0].Replacement->getCanonicalType();
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    
    // Treat the raw pointer as a physical l-value of that type.
    llvm::Value *addrValue = args.takeLast().getUnmanagedValue();
    Address addr = getAddressForUnsafePointer(IGF, valueTI, addrValue);
    
    // Mark that we're not returning anything.
    voidResult = true;
    
    // Perform the assignment operation.
    return valueTI.assign(IGF, args, addr);
  }
  
  if (BuiltinName == "init") {
    // The type of the operation is the type of the first argument of
    // the store function.
    CanType valueTy = substitutions[0].Replacement->getCanonicalType();
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    
    // Treat the raw pointer as a physical l-value of that type.
    llvm::Value *addrValue = args.takeLast().getUnmanagedValue();
    Address addr = getAddressForUnsafePointer(IGF, valueTI, addrValue);
    
    // Mark that we're not returning anything.
    voidResult = true;    
    
    // Perform the init operation.
    return valueTI.initialize(IGF, args, addr);
  }

  if (BuiltinName == "allocRaw") {
    auto size = args.claimUnmanagedNext();
    auto align = args.claimUnmanagedNext();
    auto alloc = IGF.emitAllocRawCall(size, align, "builtin-allocRaw");
    out->addUnmanaged(alloc);
    return;
  }

  if (BuiltinName == "deallocRaw") {
    auto pointer = args.claimUnmanagedNext();
    auto size = args.claimUnmanagedNext();
    IGF.emitDeallocRawCall(pointer, size);
    
    voidResult = true;
    return;
  }

  if (BuiltinName == "castToObjectPointer" ||
      BuiltinName == "castFromObjectPointer" ||
      BuiltinName == "bridgeToRawPointer" ||
      BuiltinName == "bridgeFromRawPointer") {
    Type valueTy = substitutions[0].Replacement;
    const TypeInfo &valueTI = IGF.IGM.getFragileTypeInfo(valueTy);
    if (!valueTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      IGF.unimplemented(SourceLoc(), "builtin pointer cast on invalid type");
      IGF.emitFakeExplosion(valueTI, *out);
      return;
    }

    if (BuiltinName == "castToObjectPointer") {
      // Just bitcast and rebuild the cleanup.
      llvm::Value *value = args.forwardNext(IGF);
      value = IGF.Builder.CreateBitCast(value, IGF.IGM.RefCountedPtrTy);
      out->add(ManagedValue(value));
    } else if (BuiltinName == "castFromObjectPointer") {
      // Just bitcast and rebuild the cleanup.
      llvm::Value *value = args.forwardNext(IGF);
      value = IGF.Builder.CreateBitCast(value, valueTI.StorageType);
      out->add(ManagedValue(value));
    } else if (BuiltinName == "bridgeToRawPointer") {
      // Bitcast and immediately release the operand.
      // FIXME: Should annotate the ownership semantics of this builtin
      // so that SILGen can emit the release and expose it to ARC optimization
      llvm::Value *value = args.forwardNext(IGF);
      IGF.emitRelease(value);
      value = IGF.Builder.CreateBitCast(value, IGF.IGM.Int8PtrTy);
      out->addUnmanaged(value);
    } else if (BuiltinName == "bridgeFromRawPointer") {
      // Bitcast, and immediately retain (and introduce a release cleanup).
      // FIXME: Should annotate the ownership semantics of this builtin
      // so that SILGen can emit the retain and expose it to ARC optimization
      llvm::Value *value = args.claimUnmanagedNext();
      value = IGF.Builder.CreateBitCast(value, valueTI.StorageType);
      IGF.emitRetain(value, *out);
    }
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
    voidResult = true;
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

    auto pointer = args.claimUnmanagedNext();
    auto cmp = args.claimUnmanagedNext();
    auto newval = args.claimUnmanagedNext();

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

    out->addUnmanaged(value);
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
    
    auto pointer = args.claimUnmanagedNext();
    auto val = args.claimUnmanagedNext();

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

    out->addUnmanaged(value);
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
  auto &origResultTI = IGF.getFragileTypeInfo(CurOrigType);
  extractScalarResults(IGF, result, origResultTI, out);
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

/// The private routine to ultimately emit a call or invoke instruction.
llvm::CallSite CallEmission::emitCallSite(bool hasIndirectResult) {
  assert(RemainingArgsForCallee == 0);
  assert(LastArgWritten == 0);
  assert(!EmittedCall);
  EmittedCall = true;

  // Determine the calling convention.
  // FIXME: collect attributes in the CallEmission.
  auto cc = expandAbstractCC(IGF.IGM, getCallee().getConvention());

  // Make the call and clear the arguments array.
  auto fnPtr = getCallee().getFunctionPointer();  
  llvm::CallSite call = IGF.emitInvoke(cc, fnPtr, Args,
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
    auto origTy = IGF.IGM.getFragileType(CurOrigType)->getPointerTo();
    origAddr = IGF.Builder.CreateBitCast(origAddr, origTy);
    [[clang::fallthrough]];
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

    Initialization init;
    InitializedObject obj = init.getObjectForTemporary();
    auto cleanup = init.registerObject(IGF, obj, NotOnHeap, substResultTI);
    Address temp = init.emitLocalAllocation(IGF, obj, NotOnHeap, substResultTI,
                                            "call.aggresult");
    emitToMemory(temp, substResultTI);
 
    // If the subst result is passed as an aggregate, don't uselessly
    // copy the temporary.
    auto substSchema = substResultTI.getSchema(out.getKind());
    if (substSchema.isSingleAggregate()) {
      auto substType = substSchema.begin()->getAggregateType()->getPointerTo();
      temp = IGF.Builder.CreateBitCast(temp, substType);
      out.add(ManagedValue(temp.getAddress(), cleanup));

    // Otherwise, we need to load.  Do a take-load and deactivate the cleanup.
    } else {
      substResultTI.loadAsTake(IGF, temp, out);
    }
    return;
  }

  // Okay, we're naturally emitting to an explosion.
  // Figure out how the substituted result differs from the original.
  auto resultDiff = computeResultDifference(IGF.IGM, CurOrigType,
                       getCallee().getSubstResultType()->getCanonicalType());
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

      const TypeInfo &substResultTI =
        IGF.getFragileTypeInfo(getCallee().getSubstResultType());
      substResultTI.reexplode(IGF, temp, out);
    }
    return;

  // If they do differ, we need to remap.
  case ResultDifference::Divergent:
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
    Args[--LastArgWritten] = CurCallee.getDataPointer(IGF).getValue();
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

/// We're about to pass arguments to something.  Force the current
/// callee to ensure that we're calling something with arguments.
void CallEmission::forceCallee() {
  // Nothing to do if there are args remaining for the callee.
  if (RemainingArgsForCallee--) return;
  RemainingArgsForCallee = 0; // 1 minus the one we're about to add

  // Otherwise, we need to compute the new, indirect callee.
  Explosion fn(CurCallee.getExplosionLevel());

  // If the original function is formally typed to return a function,
  // then we just emit to that (unmapped).
  assert(LastArgWritten <= 1);
  if (LastArgWritten == 0) {
    assert(CurOrigType->is<AnyFunctionType>());
    emitToUnmappedExplosion(fn);
  } else {
    assert(CurOrigType->is<ArchetypeType>());
    // Use the substituted function type to create a temporary.  This
    // isn't actually the right type --- that would be a T -> U type
    // with the corresponding generic arguments from the substitued
    // type --- but it's good enough for our work here, which is just
    // copying back and forth.
    auto &substTI = IGF.getFragileTypeInfo(CurCallee.getSubstResultType());

    // Allocate the temporary.
    Initialization init;
    auto object = init.getObjectForTemporary();
    init.registerObjectWithoutDestroy(object);
    Address addr = init.emitLocalAllocation(IGF, object, NotOnHeap, substTI,
                                            "polymorphic-currying-temp")
      .getUnownedAddress();

    // Emit the current call into that temporary.
    Address castAddr = IGF.Builder.CreateBitCast(addr, IGF.IGM.OpaquePtrTy);
    emitToUnmappedMemory(castAddr);

    // Claim the values from the temporary.
    substTI.loadAsTake(IGF, addr, fn);
  }

  // Grab the values.
  llvm::Value *fnPtr = fn.claimUnmanagedNext();
  ManagedValue dataPtr = fn.claimNext();

  // Set up for an indirect call.
  auto substFnType = cast<AnyFunctionType>(CurCallee.getSubstResultType());
  CanType substResultType = CanType(substFnType->getResult());
  CurCallee = emitIndirectCallee(IGF, fnPtr, dataPtr,
                                 CurCallee.getSubstitutions(),
                                 CurOrigType, substResultType);
}

/// Does the given convention grow clauses left-to-right?
/// Swift generally grows right-to-left, but ObjC needs us
/// to go left-to-right.
static bool isLeftToRight(AbstractCC cc) {
  return cc == AbstractCC::C;
}

/// Does an ObjC method or C function returning the given type require an
/// sret indirect result?
llvm::PointerType *irgen::requiresExternalIndirectResult(IRGenModule &IGM,
                                                         CanType type) {
  // FIXME: we need to consider the target's C calling convention.
  return IGM.requiresIndirectResult(type, ExplosionKind::Minimal);
}

/// Does an argument of this type need to be passed by value on the stack to
/// C or ObjC arguments?
llvm::PointerType *irgen::requiresExternalByvalArgument(IRGenModule &IGM,
                                                        CanType type) {
  // FIXME: we need to consider the target's C calling convention.
  return IGM.requiresIndirectResult(type, ExplosionKind::Minimal);
}

void CallEmission::externalizeArgument(Explosion &out, Explosion &in,
                     SmallVectorImpl<std::pair<unsigned, Alignment>> &newByvals,
                     CanType ty) {
  TypeInfo const &ti = IGF.getFragileTypeInfo(ty);
  if (requiresExternalByvalArgument(IGF.IGM, ty)) {
    Initialization I;
    InitializedObject object = I.getObjectForTemporary();
    I.registerObject(IGF, object, NotOnHeap, ti);
    OwnedAddress addr = ti.allocate(IGF,
                                    I,
                                    object,
                                    NotOnHeap,
                                    "byval-temporary");
    ti.initialize(IGF, in, addr.getAddress());
     
    newByvals.push_back({out.size(), addr.getAlignment()});
    out.addUnmanaged(addr.getAddress().getAddress());
  } else {
    ti.reexplode(IGF, in, out);
  }
}

/// Convert exploded Swift arguments into C-compatible arguments.
void CallEmission::externalizeArguments(Explosion &arg,
                    SmallVectorImpl<std::pair<unsigned, Alignment>> &newByvals,
                    CanType inputsTy) {
  Explosion externalized(arg.getKind());

  if (TupleType *tupleTy = inputsTy->getAs<TupleType>()) {
    for (auto &elt : tupleTy->getFields()) {
      externalizeArgument(externalized, arg, newByvals,
                          elt.getType()->getCanonicalType());
    }
  } else {
    externalizeArgument(externalized, arg, newByvals, inputsTy);
  }
  
  // Sometimes we get extra args such as the selector argument. Pass those
  // through.
  externalized.add(arg.claimAll());
  arg = std::move(externalized);
}

/// Add a new set of arguments to the function.
void CallEmission::addArg(Explosion &arg) {
  forceCallee();
  llvm::SmallVector<std::pair<unsigned, Alignment>, 2> newByvals;
  
  if (CurCallee.getConvention() == AbstractCC::C) {
    externalizeArguments(arg, newByvals,
                   CanType(CurOrigType->castTo<AnyFunctionType>()->getInput()));
  }

  // Add the given number of arguments.
  assert(getCallee().getExplosionLevel() == arg.getKind());
  assert(LastArgWritten >= arg.size());
  unsigned newLastArgWritten = LastArgWritten - arg.size();

  size_t targetIndex;

  if (isLeftToRight(getCallee().getConvention())) {
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
  for (unsigned i = 0, e = values.size(); i != e; ++i) {
    auto value = values[i];
    // The default rule is that arguments are consumed, in which case
    // we need to deactivate cleanups when making the call.
    *argIterator++ = value.getValue();
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
    params.addUnmanaged(i);
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

  // That's it for the 'bare' prologue.
  if (CurPrologue == Prologue::Bare)
    return;

  // Set up the return block.
  emitBBForReturn();

  // List out the parameter values in an Explosion.
  Explosion values = collectParameters();

  // Set up the return slot, stealing the first argument if necessary.
  {
    // Find the 'code' result type of this function.
    const TypeInfo &resultType = getResultTypeInfo();

    ExplosionSchema resultSchema(CurExplosionLevel);
    resultType.getSchema(resultSchema);

    if (resultSchema.requiresIndirectResult()) {
      ReturnSlot = resultType.getAddressForPointer(values.claimUnmanagedNext());
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

  // That's it for the 'bare' epilogue.
  if (CurPrologue == Prologue::Bare)
    return;
  
  // Jump to the return block.
  if (!emitBranchToReturnBB())
    return;

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

/// Emit a SIL function.
static void emitSILFunction(IRGenModule &IGM, SILConstant c,
                            SILFunction *f,
                            llvm::Function *entrypoint,
                            BraceStmt *body = nullptr) {
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  
  // Emit the code for the function.
  PrettyStackTraceSILConstant stackTrace("emitting IR from SIL for", c);
  
  IRGenSILFunction igs(IGM,
                       f->getLoweredType().getSwiftType(),
                       explosionLevel,
                       entrypoint);
  igs.emitSILFunction(c, f);
  
  // Walk the function body to look for local types or other decls.
  if (body)
    igs.emitLocalDecls(body);
  
  // If the function was a destroying destructor, emit the corresponding
  // deallocating destructor.
  // FIXME: Deallocating destructors are currently trivial and never explicitly
  // referenced in SIL, but may eventually benefit from SIL representation.
  if (c.isDestructor()) {
    ClassDecl *cd = cast<ClassDecl>(c.getDecl());
    llvm::Function *deallocator
      = IGM.getAddrOfDestructor(cd, DestructorKind::Deallocating);
    emitDeallocatingDestructor(IGM, cd, deallocator, entrypoint);
  }
}

/// Emit the definition for the given SIL constant.
void IRGenModule::emitSILConstant(SILConstant c, SILFunction *f) {
  llvm::Function *entrypoint;
  unsigned naturalCurryLevel;
  AbstractCC cc;
  BraceStmt *body;
  getAddrOfSILConstant(c, entrypoint, naturalCurryLevel, cc, body);
  emitSILFunction(*this, c, f, entrypoint, body);
}

/// Emit the forwarding stub function for a partial application.
static llvm::Function *emitPartialApplicationForwarder(IRGenModule &IGM,
                                       llvm::Function *fnPtr,
                                       ExplosionKind explosionLevel,
                                       CanType outType,
                                       HeapLayout const &layout) {
  llvm::AttributeSet attrs;
  ExtraData extraData
    = layout.empty() ? ExtraData::None : ExtraData::Retainable;
  llvm::FunctionType *fwdTy = IGM.getFunctionType(AbstractCC::Freestanding,
                                                  outType,
                                                  explosionLevel,
                                                  /*curryLevel=*/ 0,
                                                  extraData,
                                                  attrs);
  // FIXME: Give the thunk a real name.
  // FIXME: Maybe cache the thunk by function and closure types? Could there
  // be multiple same-type closures off the same 
  llvm::Function *fwd =
    llvm::Function::Create(fwdTy, llvm::Function::InternalLinkage,
                           "closureinst", &IGM.Module);
  fwd->setAttributes(attrs);

  IRGenFunction subIGF(IGM, outType, {}, explosionLevel, /*curryLevel=*/ 0,
                       fwd, Prologue::Bare);
  
  Explosion params = subIGF.collectParameters();
  
  // If there's a data pointer required, grab it (it's always the
  // last parameter) and load out the extra, previously-curried
  // parameters.
  if (!layout.empty()) {
    llvm::Value *rawData = params.takeLast().getUnmanagedValue();
    Address data = layout.emitCastTo(subIGF, rawData);
    
    // Perform the loads.
    for (auto &fieldLayout : layout.getElements()) {
      Address fieldAddr = fieldLayout.project(subIGF, data);
      fieldLayout.Type->load(subIGF, fieldAddr, params);
    }
    
    // Kill the allocated data pointer immediately.  The safety of
    // this assumes that neither this release nor any of the loads
    // can throw.
    subIGF.emitRelease(rawData);
  }
  
  llvm::SmallVector<llvm::Value*, 8> args;
  params.forward(subIGF, params.size(), args);

  llvm::CallSite callSite = subIGF.emitInvoke(fnPtr->getCallingConv(),
                                              fnPtr,
                                              args,
                                              fnPtr->getAttributes());
  llvm::CallInst *call = cast<llvm::CallInst>(callSite.getInstruction());
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
                                           ArrayRef<const TypeInfo *> argTypes,
                                           CanType outType,
                                           Explosion &out) {
  // Store the context arguments on the heap.
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, argTypes);
  llvm::Value *data;
  if (layout.empty()) {
    data = IGF.IGM.RefCountedNull;
  } else {
    // Allocate a new object.
    data = IGF.emitUnmanagedAlloc(layout, "closure");
    Address dataAddr = layout.emitCastTo(IGF, data);
    
    // Perform the store.
    for (auto &fieldLayout : layout.getElements()) {
      Address fieldAddr = fieldLayout.project(IGF, dataAddr);
      fieldLayout.Type->initialize(IGF, args, fieldAddr);
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
  out.addUnmanaged(forwarderValue);
  out.addUnmanaged(data);
}

/// Fetch the declaration of the given block-to-
llvm::Function *IRGenModule::getAddrOfBridgeToBlockConverter(CanType blockType)
{
  LinkEntity entity = LinkEntity::forBridgeToBlockConverter(blockType);
  
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
                              CanType blockTy,
                              Explosion &swiftClosure,
                              Explosion &outBlock) {
  // Get the function pointer as an i8*.
  llvm::Value *fn = swiftClosure.claimUnmanagedNext();
  fn = IGF.Builder.CreateBitCast(fn, IGF.IGM.Int8PtrTy);

  // Get the context pointer as a %swift.refcounted*.
  ManagedValue mContext = swiftClosure.claimNext();
  llvm::Value *context = IGF.Builder.CreateBitCast(mContext.forward(IGF),
                                                   IGF.IGM.RefCountedPtrTy);
  // Get the shim function we'll call.
  llvm::Function *converter = IGF.IGM.getAddrOfBridgeToBlockConverter(blockTy);
  
  // Emit the call.
  llvm::Value *result = IGF.Builder.CreateCall2(converter, fn, context);
  
  // Tag the result with a cleanup and pass it on.
  outBlock.add(ManagedValue(result));
}

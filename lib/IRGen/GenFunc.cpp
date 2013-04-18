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
#include "Scope.h"

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
      e.add(IGF.enterReleaseCleanup(IGF.Builder.CreateLoad(dataAddr)));
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
      dest.add(IGF.enterReleaseCleanup(src.claimUnmanagedNext()));
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

/// Construct the best known limits on how we can call the given function.
AbstractCallee AbstractCallee::forDirectFunction(IRGenFunction &IGF,
                                                 ValueDecl *val) {
  bool isLocal = val->getDeclContext()->isLocalContext();
  if (!isLocal) return forDirectGlobalFunction(IGF.IGM, val);

  auto extraData = ExtraData::None;
  if (FuncDecl *fn = dyn_cast<FuncDecl>(val)) {
    if (isLocal && !isa<llvm::ConstantPointerNull>(IGF.getLocalFuncData(fn)))
      extraData = ExtraData::Retainable;
  }

  return getAbstractDirectCallee(val, ExplosionKind::Maximal, extraData);
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

/// Emit a reference to a function, using the best parameters possible
/// up to given limits.
static Callee emitDirectCallee(IRGenFunction &IGF, ValueDecl *val,
                               CanType substResultType,
                               ArrayRef<Substitution> subs,
                               ExplosionKind bestExplosion,
                               unsigned bestUncurry) {
  llvm::AttributeSet unusedAttrs;

  if (bestUncurry != 0 || bestExplosion != ExplosionKind::Minimal) {
    auto absCallee = AbstractCallee::forDirectFunction(IGF, val);
    bestUncurry = std::min(bestUncurry, absCallee.getMaxUncurryLevel());
    bestExplosion = absCallee.getBestExplosionLevel();
  }

  if (ConstructorDecl *ctor = dyn_cast<ConstructorDecl>(val)) {
    llvm::Constant *fnPtr;
    if (bestUncurry != 1) {
      IGF.unimplemented(val->getLoc(), "uncurried reference to constructor");
      fnPtr = llvm::UndefValue::get(
          IGF.IGM.getFunctionType(AbstractCC::Freestanding,
                                  val->getType()->getCanonicalType(),
                                  bestExplosion, bestUncurry,
                                  ExtraData::None, unusedAttrs));
    } else {
      fnPtr = IGF.IGM.getAddrOfConstructor(ctor, ConstructorKind::Allocating,
                                           bestExplosion);
    }
    return Callee::forFreestandingFunction(AbstractCC::Freestanding,
                                           ctor->getType()->getCanonicalType(),
                                           substResultType, subs, fnPtr,
                                           bestExplosion, bestUncurry);
  }

  if (OneOfElementDecl *oneofelt = dyn_cast<OneOfElementDecl>(val)) {
    llvm::Constant *fnPtr;
    if (bestUncurry != (oneofelt->hasArgumentType() ? 1 : 0)) {
      IGF.unimplemented(val->getLoc(), "uncurried reference to oneof element");
      fnPtr = llvm::UndefValue::get(
          IGF.IGM.getFunctionType(AbstractCC::Freestanding,
                                  val->getType()->getCanonicalType(),
                                  bestExplosion, bestUncurry,
                                  ExtraData::None,
                                  unusedAttrs));
    } else {
      fnPtr = IGF.IGM.getAddrOfInjectionFunction(oneofelt);
    }
    return Callee::forFreestandingFunction(AbstractCC::Freestanding,
                                           oneofelt->getType()->getCanonicalType(),
                                           substResultType, subs, fnPtr,
                                           bestExplosion, bestUncurry);
  }

  FuncDecl *fn = cast<FuncDecl>(val);
  FunctionRef fnRef = FunctionRef(fn, bestExplosion, bestUncurry);
  if (!fn->getDeclContext()->isLocalContext()) {
    llvm::Constant *fnPtr = IGF.IGM.getAddrOfFunction(fnRef, ExtraData::None);
    if (fn->isInstanceMember()) {
      return Callee::forMethod(fn->getType()->getCanonicalType(),
                               substResultType, subs, fnPtr,
                               bestExplosion, bestUncurry);
    } else {
      return Callee::forFreestandingFunction(getAbstractCC(fn),
                                             fn->getType()->getCanonicalType(),
                                             substResultType, subs, fnPtr,
                                             bestExplosion, bestUncurry);
    }
  }

  auto fnPtr = IGF.getAddrOfLocalFunction(fnRef);
  Explosion e(ExplosionKind::Maximal);
  IGF.emitRetain(IGF.getLocalFuncData(fn), e);
  ManagedValue data = e.claimNext();
  if (isa<llvm::ConstantPointerNull>(data.getValue()))
    data = ManagedValue(nullptr);
  return Callee::forKnownFunction(AbstractCC::Freestanding,
                                  fn->getType()->getCanonicalType(),
                                  substResultType, subs, fnPtr, data,
                                  bestExplosion, bestUncurry);
}

/// Emit a reference to a superclass initializing constructor, using the best
/// parameters possible up to given limits.
static Callee emitSuperConstructorCallee(IRGenFunction &IGF,
                                         ConstructorDecl *ctor,
                                         CanType substResultType,
                                         ArrayRef<Substitution> subs,
                                         ExplosionKind bestExplosion,
                                         unsigned bestUncurry) {
  if (bestExplosion != ExplosionKind::Minimal) {
    auto absCallee = AbstractCallee::forDirectFunction(IGF, ctor);
    bestExplosion = absCallee.getBestExplosionLevel();
  }

  llvm::Constant *fnPtr;
  if (bestUncurry != 1) {
    IGF.unimplemented(ctor->getLoc(),
                      "uncurried reference to super.constructor");
    llvm::AttributeSet unusedAttrs;
    fnPtr = llvm::UndefValue::get(
                  IGF.IGM.getFunctionType(AbstractCC::Method,
                                          ctor->getType()->getCanonicalType(),
                                          bestExplosion, bestUncurry,
                                          ExtraData::None, unusedAttrs));

  } else {
    fnPtr = IGF.IGM.getAddrOfConstructor(ctor,
                                         ConstructorKind::Initializing,
                                         bestExplosion);
  }
  return Callee::forFreestandingFunction(AbstractCC::Method,
                                         ctor->getType()->getCanonicalType(),
                                         substResultType, subs, fnPtr,
                                         bestExplosion, bestUncurry);
}


namespace {
  /// A single call site, with argument expression and the type of
  /// function being applied.
  struct CallSite {
    CallSite(ApplyExpr *apply)
      : Apply(apply), FnType(apply->getFn()->getType()->getCanonicalType()) {}

    ApplyExpr *Apply;

    /// The function type that we're actually calling.  This is
    /// "un-substituted" if necessary.
    CanType FnType;

    Expr *getArg() const { return Apply->getArg(); }
    CanType getSubstResultType() const {
      return Apply->getType()->getCanonicalType();
    }

    void emit(IRGenFunction &IGF, ArrayRef<Substitution> subs,
              Explosion &out) const {
      assert(!subs.empty() || !FnType->is<PolymorphicFunctionType>());

      // If we have substitutions, then (1) it's possible for this to
      // be a polymorphic function type that we need to expand and
      // (2) we might need to evaluate the r-value differently.
      if (!subs.empty()) {
        auto fnType = cast<AnyFunctionType>(FnType);
        IGF.emitRValueAsUnsubstituted(getArg(), CanType(fnType->getInput()),
                                      subs, out);
        if (auto polyFn = dyn_cast<PolymorphicFunctionType>(fnType)) {
          auto substInputType = getArg()->getType()->getCanonicalType();
          emitPolymorphicArguments(IGF, polyFn, substInputType, subs, out);
        }
      } else {
        IGF.emitRValue(getArg(), out);
      }
    }
  };

  struct CalleeSource {
  public:
    enum class Kind {
      /// A totally abstracted call.
      Indirect,

      /// An abstracted call with known values.  This is a sort of
      /// internal convenience.
      IndirectLiteral,

      /// A direct call to a known function.
      Direct,
      
      /// A direct call to a superclass's constructor.
      InitializingConstructor,

      /// A virtual call to a class member.  The first argument is a
      /// pointer to a class instance or metatype.
      Virtual,

      /// An Objective-C message send.
      ObjCMessage,
      
      /// An Objective-C super send.
      ObjCSuper,
      
      /// An Objective-C super send to a constructor (init method).
      ObjCSuperConstructor,

      /// A call to a protocol member on a value of existential type.
      Existential,

      /// A call to a protocol member on a value of archetype type.
      Archetype
    };

  private:
    union {
      struct {
        Expr *Fn;
      } Indirect;
      struct {
        llvm::Value *Fn;
        ManagedValue Data;
        TypeBase *OrigFormalType;
        TypeBase *SubstResultType;
      } IndirectLiteral;
      struct {
        ValueDecl *Fn;
      } Direct;
      struct {
        ConstructorDecl *Ctor;
      } InitializingConstructor;
      struct {
        FuncDecl *Fn;
      } Virtual;
      struct {
        FuncDecl *Fn;
      } ObjCMessage;
      struct {
        FuncDecl *Fn;
        TypeBase *SearchClass;
      } ObjCSuper;
      struct {
        ConstructorDecl *Ctor;
        TypeBase *SearchClass;
      } ObjCSuperConstructor;
      struct {
        ExistentialMemberRefExpr *Fn;
      } Existential;
      struct {
        ArchetypeMemberRefExpr *Fn;
      } Archetype;
    };
    Kind TheKind;
    CanType SubstResultType;
    std::vector<Substitution> Substitutions;
    SmallVector<CallSite, 4> CallSites;
    Expr *SideEffects = nullptr;

  public:
    static CalleeSource decompose(Expr *fn);

    static CalleeSource forIndirect(Expr *fn) {
      CalleeSource result;
      result.TheKind = Kind::Indirect;
      result.Indirect.Fn = fn;
      return result;
    }

    static CalleeSource forDirect(ValueDecl *fn) {
      CalleeSource result;
      result.TheKind = Kind::Direct;
      result.Direct.Fn = fn;
      return result;
    }
    
    static CalleeSource forInitializingConstructor(ConstructorDecl *ctor) {
      CalleeSource result;
      result.TheKind = Kind::InitializingConstructor;
      result.InitializingConstructor.Ctor = ctor;
      return result;
    }

    static CalleeSource forObjCMessage(FuncDecl *fn) {
      CalleeSource result;
      result.TheKind = Kind::ObjCMessage;
      result.ObjCMessage.Fn = fn;
      return result;
    }
    
    static CalleeSource forObjCSuper(FuncDecl *fn, CanType searchClass) {
      CalleeSource result;
      result.TheKind = Kind::ObjCSuper;
      result.ObjCSuper.Fn = fn;
      result.ObjCSuper.SearchClass = searchClass.getPointer();
      return result;
    }
    
    static CalleeSource forObjCSuperConstructor(ConstructorDecl *ctor,
                                                CanType searchClass) {
      CalleeSource result;
      result.TheKind = Kind::ObjCSuperConstructor;
      result.ObjCSuperConstructor.Ctor = ctor;
      result.ObjCSuperConstructor.SearchClass = searchClass.getPointer();
      return result;
    }

    static CalleeSource forVirtual(FuncDecl *fn) {
      CalleeSource result;
      result.TheKind = Kind::Virtual;
      result.Virtual.Fn = fn;
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

    bool isDirect() const {
      return getKind() == Kind::Direct;
    }

    ValueDecl *getDirectFunction() const {
      assert(isDirect());
      return Direct.Fn;
    }
    
    ConstructorDecl *getInitializingConstructor() const {
      assert(getKind() == Kind::InitializingConstructor);
      return InitializingConstructor.Ctor;
    }

    Expr *getSideEffects() const {
      return SideEffects;
    }
    void addSideEffect(Expr *sideEffect) {
      assert(SideEffects == nullptr && "adding multiple side effects?");
      SideEffects = sideEffect;
    }

    Expr *getIndirectFunction() const {
      assert(getKind() == Kind::Indirect);
      return Indirect.Fn;
    }

    FuncDecl *getVirtualFunction() const {
      assert(getKind() == Kind::Virtual);
      return Virtual.Fn;
    }

    ValueDecl *getObjCMethod() const {
      switch (getKind()) {
      case Kind::ObjCMessage:
        return ObjCMessage.Fn;
      case Kind::ObjCSuper:
        return ObjCSuper.Fn;
      case Kind::ObjCSuperConstructor:
        return ObjCSuperConstructor.Ctor;
      default:
        llvm_unreachable("not an objc callee");
      }
    }
    
    CanType getObjCSuperSearchTypeOrNull() const {
      switch (getKind()) {
      case Kind::ObjCSuper:
        return CanType(ObjCSuper.SearchClass);
      case Kind::ObjCSuperConstructor:
        return CanType(ObjCSuperConstructor.SearchClass);
      case Kind::ObjCMessage:
        return CanType();
      default:
        llvm_unreachable("not an objc callee");
      }
    }
    
    ConstructorDecl *getObjCSuperConstructor() const {
      assert(getKind() == Kind::ObjCSuperConstructor);
      return ObjCSuperConstructor.Ctor;
    }

    ExistentialMemberRefExpr *getExistentialFunction() const {
      assert(getKind() == Kind::Existential);
      return Existential.Fn;
    }

    ArchetypeMemberRefExpr *getArchetypeFunction() const {
      assert(getKind() == Kind::Archetype);
      return Archetype.Fn;
    }

    /// Return the abstract base callee.  The base callee is the
    /// deepest thing being called.
    AbstractCallee getAbstractBaseCallee(IRGenFunction &IGF) const {
      switch (getKind()) {
      case Kind::Indirect:
      case Kind::IndirectLiteral:
        return AbstractCallee::forIndirect();

      case Kind::Direct:
        return AbstractCallee::forDirectFunction(IGF, getDirectFunction());
          
      case Kind::InitializingConstructor:
        return AbstractCallee::forDirectFunction(IGF,
                                                 getInitializingConstructor());

      case Kind::Virtual:
        return getAbstractVirtualCallee(IGF, getVirtualFunction());

      case Kind::ObjCSuper:
      case Kind::ObjCMessage:
      case Kind::ObjCSuperConstructor:
        return getAbstractObjCMethodCallee(IGF, getObjCMethod());

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
      Substitutions.insert(Substitutions.end(),
                           subs.begin(), subs.end());
    }

    void setSubstResultType(CanType type) {
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

    CallEmission prepareCall(IRGenFunction &IGF,
                             unsigned numExtraArgs = 0,
                             ExplosionKind maxExplosion
                               = ExplosionKind::Maximal);

    bool trySpecializeToExplosion(IRGenFunction &IGF, Explosion &out);
    bool trySpecializeToMemory(IRGenFunction &IGF,
                               Address resultAddr,
                               const TypeInfo &resultTI);

    /// Change this to use a literal indirect source.
    void updateToIndirect(unsigned numCallSitesToDrop,
                          CanType origFormalType,
                          CanType substResultType,
                          llvm::Value *fnPtr,
                          ManagedValue dataPtr) {
      // Drop the requested number of call sites.
      assert(numCallSitesToDrop < CallSites.size());
      CallSites.erase(CallSites.begin(),
                      CallSites.begin() + numCallSitesToDrop);

      TheKind = Kind::IndirectLiteral;
      IndirectLiteral.Fn = fnPtr;
      IndirectLiteral.Data = dataPtr;
      IndirectLiteral.OrigFormalType = origFormalType.getPointer();
      IndirectLiteral.SubstResultType = substResultType.getPointer();
    }

    /// getFinalResultExplosionLevel - Returns the explosion level at
    /// which we will naturally emit the last call.
    ExplosionKind getFinalResultExplosionLevel(IRGenFunction &IGF) const {
      AbstractCallee absCallee = getAbstractBaseCallee(IGF);
      unsigned numArgs = getCallSites().size();

      // If there are more arguments than the base callee can take,
      // then the final call uses indirect call rules.
      if (numArgs > absCallee.getMaxUncurryLevel() + 1)
        return ExplosionKind::Minimal;

      // If there are fewer arguments than the base callee can take,
      // then the call is to a thunk and can use maximal rules.
      if (numArgs < absCallee.getMinUncurryLevel() + 1)
        return ExplosionKind::Minimal;

      // Otherwise, we can use the best rules that the callee can dish out.
      return absCallee.getBestExplosionLevel();
    }

  private:
    CallEmission prepareRootCall(IRGenFunction &IGF, unsigned numArgs,
                                 ExplosionKind maxExplosion);
  };
}

/// Emit a reference to the given function as a generic function pointer.
void irgen::emitRValueForFunction(IRGenFunction &IGF, FuncDecl *fn,
                                  Explosion &explosion) {
  // Function pointers are always fully curried and use ExplosionKind::Minimal.
  CanType fnType = fn->getType()->getCanonicalType();
  CanType resultType = CanType(cast<AnyFunctionType>(fnType)->getResult());
  Callee callee = emitDirectCallee(IGF, fn, resultType,
                                   ArrayRef<Substitution>(),
                                   ExplosionKind::Minimal, 0);
  assert(callee.getExplosionLevel() == ExplosionKind::Minimal);
  assert(callee.getUncurryLevel() == 0);
  explosion.addUnmanaged(callee.getOpaqueFunctionPointer(IGF));
  explosion.add(callee.getDataPointer(IGF));
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

namespace {
  class SpecializedCallEmission {
  protected:
    IRGenFunction &IGF;
    CalleeSource &Source;

  private:
    /// The substituted result type.  Only valid after claiming.
    CanType SubstResultType;

    /// A temporary for when we have intermediate results.
    union FnTemp_t {
      FnTemp_t() {}
      ~FnTemp_t() {}

      Explosion TempExplosion;
      Address TempAddress;
    } FnTemp;

    /// The number of arguments claimed.
    unsigned ArgsClaimed = 0;

    enum class FnTempKind : unsigned char {
      None, Explosion, Address
    } FnTempState = FnTempKind::None;

    bool Completed = false;

  protected:
    SpecializedCallEmission(IRGenFunction &IGF, CalleeSource &source)
      : IGF(IGF), Source(source) {}

    ~SpecializedCallEmission() {
      assert(ArgsClaimed == 0 || Completed);
      assert(FnTempState == FnTempKind::None);
    }

    virtual Explosion &getFinalSubstExplosion() = 0;
    virtual Address getFinalSubstResultAddress() = 0;
    virtual void completeFinal() = 0;

  public:
    /// Try to specialize this emission.  Returns true if the
    /// specialization was complete and there's no more calling to be
    /// done.
    bool trySpecialize();

    /// Try to claim N argument clauses and place them in the array.
    /// This should be called before fetching a result.  It cannot
    /// fail for N=1.
    ///
    /// Once arguments are claimed, the call must be specialized.
    ///
    /// Returns true on success.
    bool tryClaimArgs(MutableArrayRef<Expr*> args);

    Expr *claimArg() {
      Expr *result;
      bool ok = tryClaimArgs(MutableArrayRef<Expr*>(&result, 1));
      assert(ok); (void) ok;
      return result;
    }

    CanType getSubstResultType() const {
      assert(ArgsClaimed && "arguments not yet claimed!");
      return SubstResultType;
    }

    ArrayRef<Substitution> getSubstitutions() const {
      return Source.getSubstitutions();
    }

    /// Is this emission naturally to memory?  Emitters don't need to
    /// use this, but if they can emit specialized code for it, all
    /// the better.
    virtual bool isNaturallyToMemory() const = 0;

    // The client should use exactly one of these after claiming:

    /// Produce an address into which to emit substituted data.
    Address getSubstResultAddress() {
      assert(ArgsClaimed != 0);

      // Fast case: we've claimed all the arguments.
      if (ArgsClaimed == Source.getCallSites().size())
        return getFinalSubstResultAddress();

      // Otherwise, we need a function temporary.
      Address temp = IGF.createAlloca(IGF.IGM.FunctionPairTy,
                                      IGF.IGM.getPointerAlignment(),
                                      "specialized.uncurry.temp");
      FnTempState = FnTempKind::Address;
      FnTemp.TempAddress = temp;
      return temp;
    }

    /// Returns an explosion into which to add substituted values.
    Explosion &getSubstExplosion() {
      assert(ArgsClaimed != 0);

      // Fast case: we've claimed all the arguments.
      if (ArgsClaimed == Source.getCallSites().size())
        return getFinalSubstExplosion();

      // Otherwise, we need an explosion into which to emit the
      // function value.
      assert(FnTempState == FnTempKind::None);
      FnTempState = FnTempKind::Explosion;
      return *new (&FnTemp.TempExplosion) Explosion(ExplosionKind::Maximal);
    }

    /// Indicates that the substituted result was void.
    void setVoidResult() {
      // This can only be the final call.
      assert(ArgsClaimed != 0);
      assert(ArgsClaimed == Source.getCallSites().size());
    }

    /// Indicates that the result is a single scalar value.
    void setScalarUnmanagedSubstResult(llvm::Value *value) {
      // This can only be the final call.
      assert(ArgsClaimed != 0);
      assert(ArgsClaimed == Source.getCallSites().size());
      getFinalSubstExplosion().addUnmanaged(value);
    }

  private:
    void completeAsIndirect(Explosion &out);
  };
}

/// Attempt to claim a number of arguments for the current specialized
/// emission.
bool SpecializedCallEmission::tryClaimArgs(llvm::MutableArrayRef<Expr*> args) {
  assert(!ArgsClaimed && "arguments have already been claimed");
  assert(args.size() >= 1);

  // If we want more arguments than there are call sites, we're done.
  auto n = args.size();
  auto callSites = Source.getCallSites();
  if (n > callSites.size()) return false;

  // Otherwise, fill in the array and set the approproiate state.
  for (unsigned i = 0; i != n; ++i) {
    args[i] = callSites[i].getArg();
  }
  SubstResultType = callSites[n-1].getSubstResultType();
  ArgsClaimed = n;
  return true;
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
      out->add(IGF.enterReleaseCleanup(value));
    } else if (BuiltinName == "castFromObjectPointer") {
      // Just bitcast and rebuild the cleanup.
      llvm::Value *value = args.forwardNext(IGF);
      value = IGF.Builder.CreateBitCast(value, valueTI.StorageType);
      out->add(IGF.enterReleaseCleanup(value));
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

/// Prepare a CallEmission for this callee source.  All the arguments
/// will have been streamed into it.
CallEmission CalleeSource::prepareCall(IRGenFunction &IGF,
                                       unsigned numExtraArgs,
                                       ExplosionKind maxExplosion) {
  // Prepare the root.
  unsigned numArgs = getCallSites().size() + numExtraArgs;
  CallEmission emission = prepareRootCall(IGF, numArgs, maxExplosion);

  // Collect call sites.
  for (auto site : getCallSites())
    emission.addArg(site.getArg());

  return emission;
}

/// Prepare a CallEmission for the root callee, i.e. the one that's
/// not necessarily indirect.
CallEmission CalleeSource::prepareRootCall(IRGenFunction &IGF,
                                           unsigned numArgs,
                                           ExplosionKind maxExplosion) {
  unsigned bestUncurry = numArgs - 1;
  assert(bestUncurry != -1U);

  switch (getKind()) {
  case Kind::Direct:
    return CallEmission(IGF, emitDirectCallee(IGF, getDirectFunction(),
                                              SubstResultType,
                                              getSubstitutions(),
                                              maxExplosion, bestUncurry));

  case Kind::InitializingConstructor: {
    Callee callee = emitSuperConstructorCallee(IGF, getInitializingConstructor(),
                                               SubstResultType,
                                               getSubstitutions(),
                                               maxExplosion, bestUncurry);
    return CallEmission(IGF, callee);
  }
      
  case Kind::ObjCMessage:
  case Kind::ObjCSuper:
  case Kind::ObjCSuperConstructor: {
    // Find the 'self' expression and pass it separately.
    Expr *self = CallSites[0].getArg();
    CallSites.erase(CallSites.begin());

    return prepareObjCMethodCall(IGF, getObjCMethod(), self,
                                 SubstResultType, getSubstitutions(),
                                 maxExplosion, bestUncurry,
                                 getObjCSuperSearchTypeOrNull());
  }

  case Kind::Virtual: {
    // Emit the base.
    auto baseExpr = getCallSites().front().getArg();
    Explosion baseValues(ExplosionKind::Minimal);
    IGF.emitRValue(baseExpr, baseValues);
    CallSites.erase(CallSites.begin());

    // Grab the base value before adding it as an argument.
    llvm::Value *base = baseValues.begin()->getValue();
    auto baseType = baseExpr->getType()->getCanonicalType();

    CallEmission emission(IGF, emitVirtualCallee(IGF, base, baseType,
                                                 getVirtualFunction(),
                                                 SubstResultType,
                                                 getSubstitutions(),
                                                 maxExplosion, bestUncurry));
    emission.addArg(baseValues);
    return emission;
  }

  case Kind::IndirectLiteral:
    return CallEmission(IGF, Callee::forIndirectCall(
                                   CanType(IndirectLiteral.OrigFormalType),
                                   CanType(IndirectLiteral.SubstResultType),
                                           getSubstitutions(),
                                           IndirectLiteral.Fn,
                                           IndirectLiteral.Data));

  case Kind::Indirect: {
    Explosion fnValues(ExplosionKind::Maximal);
    Expr *fn = getIndirectFunction();
    IGF.emitRValue(fn, fnValues);

    llvm::Value *fnPtr = fnValues.claimUnmanagedNext();
    ManagedValue dataPtr = fnValues.claimNext();
    return CallEmission(IGF, emitIndirectCallee(IGF, fnPtr, dataPtr,
                                                getSubstitutions(),
                                            fn->getType()->getCanonicalType(),
                                                SubstResultType));
  }

  case Kind::Existential:
    return prepareExistentialMemberRefCall(IGF, getExistentialFunction(),
                                           SubstResultType,
                                           getSubstitutions(),
                                           maxExplosion, bestUncurry);

  case Kind::Archetype:
    return prepareArchetypeMemberRefCall(IGF, getArchetypeFunction(),
                                         SubstResultType,
                                         getSubstitutions(),
                                         maxExplosion, bestUncurry);
  }
  llvm_unreachable("bad CalleeSource kind");
}

namespace {
  /// A class for decomposing an expression into a function reference
  /// which can, hopefully, be called more efficiently.
  struct FunctionDecomposer :
      irgen::ExprVisitor<FunctionDecomposer, CalleeSource> {
        
    /// Get the original uncoerced type of a super call for objc_msgSendSuper2
    /// search.
    CanType getSuperSearchClass(Expr *e) {
      while (ImplicitConversionExpr *d = dyn_cast<ImplicitConversionExpr>(e)) {
        e = d->getSubExpr();
      }
      assert(isa<SuperRefExpr>(e)
             && "apply expr flagged super with no 'super' expr in argument?!");
      
      Type origType = e->getType()->getRValueType();
      if (MetaTypeType *meta = origType->getAs<MetaTypeType>()) {
        origType = meta->getInstanceType();
      }
      return origType->getCanonicalType();
    }
        
    CalleeSource visitOtherConstructorDeclRefExpr(
                                              OtherConstructorDeclRefExpr *E) {
      // Reference the initializing entry point of the other constructor.
      CalleeSource source =
        CalleeSource::forInitializingConstructor(E->getDecl());
      return source;
    }
        
    CalleeSource visitDeclRefExpr(DeclRefExpr *E) {
      if (FuncDecl *fn = dyn_cast<FuncDecl>(E->getDecl())) {
        // FIXME: The test we really want is to ask if this function ONLY has an
        // Objective-C implementation, but for now we'll just always go through
        // the Objective-C version.
        if (fn->isObjC()) {
          return CalleeSource::forObjCMessage(fn);
        } else if (isa<ClassDecl>(fn->getDeclContext())) {
          return CalleeSource::forVirtual(fn);
        } else {
          return CalleeSource::forDirect(fn);
        }
      }
      if (ConstructorDecl *ctor = dyn_cast<ConstructorDecl>(E->getDecl()))
        return CalleeSource::forDirect(ctor);
      if (OneOfElementDecl *ctor = dyn_cast<OneOfElementDecl>(E->getDecl()))
        return CalleeSource::forDirect(ctor);
      return CalleeSource::forIndirect(E);
    }

    CalleeSource visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      CalleeSource source = visit(E->getRHS());
      source.addSideEffect(E);
      return source;
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
        
    CalleeSource getSourceForSuper(Expr *FnRef, CanType SuperTy) {
      // The callee should only ever be a constructor or member method.
      if (auto *ctorRef = dyn_cast<OtherConstructorDeclRefExpr>(FnRef)) {
        if (SuperTy->getClassOrBoundGenericClass()->isObjC())
          // Use ObjC super dispatch to the super init method.
          return CalleeSource::forObjCSuperConstructor(ctorRef->getDecl(),
                                                       SuperTy);
        else
          // Swift super.constructor calls are direct, like other constructor-
          // to-constructor calls.
          return visitOtherConstructorDeclRefExpr(ctorRef);
      }
      if (auto *declRef = dyn_cast<DeclRefExpr>(FnRef)) {
        FuncDecl *fn = cast<FuncDecl>(declRef->getDecl());
        
        if (fn->isObjC())
          // Use ObjC super dispatch.
          // FIXME: The test we really want is to ask if this function ONLY has an
          // Objective-C implementation, but for now we'll just always go through
          // the Objective-C version.
          return CalleeSource::forObjCSuper(fn, SuperTy);
        else
          // Pure Swift super calls can be direct.
          // FIXME: Need to consider the resilience of the base class hierarchy,
          // in case we picked up a super from an ancestor class--a nearer
          // ancestor could end up intervening.
          return CalleeSource::forDirect(fn);
      }
      llvm_unreachable("unexpected super call expr?!");
    }

    CalleeSource visitApplyExpr(ApplyExpr *E) {
      // If the argument to the call is 'super', the callee has different
      // semantics.
      CalleeSource source = E->isSuper()
        ? getSourceForSuper(E->getFn(), getSuperSearchClass(E->getArg()))
        : visit(E->getFn());

      source.addCallSite(CallSite(E));
      return source;
    }

    CalleeSource visitFunctionConversionExpr(FunctionConversionExpr *E) {
      return visit(E->getSubExpr());
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
  
  // Remember the substituted result type, which is to say, the type
  // of the original expression.
  source.setSubstResultType(E->getType()->getCanonicalType());

  return source;
}

/// Emit an expression as a callee.
CallEmission irgen::prepareCall(IRGenFunction &IGF, Expr *fn,
                                ExplosionKind bestExplosion,
                                unsigned numExtraArgs,
                                CanType substResultType) {
  CalleeSource source = CalleeSource::decompose(fn);
  source.setSubstResultType(substResultType);
  return source.prepareCall(IGF, numExtraArgs, bestExplosion);
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

  // If the callee has non-standard conventions, we may need to
  // reclaim an autoreleased result.
  if (getCallee().hasOwnershipConventions() &&
      getCallee().getOwnershipConventions()
                 .isResultAutoreleased(IGF.IGM, getCallee())) {
    result = emitObjCRetainAutoreleasedReturnValue(IGF, result);
  }

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

  // Deactivate all the cleanups.
  for (auto cleanup : Cleanups)
    IGF.setCleanupState(cleanup, CleanupState::Dead);
  Cleanups.clear();

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

/// Emit a call, whose result is known to be void.
void CallEmission::emitVoid() {
  Explosion out(getCallee().getExplosionLevel());
  emitToExplosion(out);
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
    init.markInitialized(IGF, obj);

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
      if (cleanup.isValid())
        IGF.setCleanupState(cleanup, CleanupState::Dead);
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
    Cleanups(std::move(other.Cleanups)),
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

/// Try to emit a callee source as a specialized call.
bool SpecializedCallEmission::trySpecialize() {
  assert(Source.isDirect() && "trying to specialize a non-direct call");

  // Okay, we emitted a specialized call.  Complete it.
  assert(ArgsClaimed != 0);
  assert((ArgsClaimed == Source.getCallSites().size())
           == (FnTempState == FnTempKind::None));
  Completed = true;

  switch (FnTempState) {
  case FnTempKind::None:
    completeFinal();
    return true;

  case FnTempKind::Address: {
    // Clean up the address for assert purposes.
    FnTempState = FnTempKind::None;

    Explosion temp(ExplosionKind::Maximal);
    FuncTypeInfo::doLoadAsTake(IGF, FnTemp.TempAddress, temp);
    completeAsIndirect(temp);
    return false;
  }

  case FnTempKind::Explosion:
    completeAsIndirect(FnTemp.TempExplosion);
    FnTemp.TempExplosion.~Explosion();
    FnTempState = FnTempKind::None;
    return false;
  }
  llvm_unreachable("bad function temp state");
}

void SpecializedCallEmission::completeAsIndirect(Explosion &fn) {
  assert(fn.size() == 2);
  llvm::Value *fnPtr = fn.claimUnmanagedNext();
  ManagedValue dataPtr = fn.claimNext();

  // The function pointer we've got here should be totally substituted.
  // That's not necessarily optimal, but it's what we've got.
  CanType origFnType = getSubstResultType();
  fnPtr = emitCastOfIndirectFunction(IGF, fnPtr, dataPtr, origFnType);

  // Update the CalleeSource.
  CanType substResultType =
    CanType(cast<FunctionType>(origFnType)->getResult());
  Source.updateToIndirect(ArgsClaimed, origFnType, substResultType,
                          fnPtr, dataPtr);

  // We didn't actually do anything final, so don't completeFinal().
}

namespace {
  /// An implement of SpecializedCallEmission for ultimately emitting
  /// to an explosion.
  class ExplosionSpecializedCallEmission : public SpecializedCallEmission {
    Explosion &Out;

    Initialization Init;
    InitializedObject TempObject;
  public:
    ExplosionSpecializedCallEmission(IRGenFunction &IGF, CalleeSource &source,
                                     Explosion &out)
      : SpecializedCallEmission(IGF, source), Out(out),
        TempObject(InitializedObject::invalid()) {}

  private:
    bool hasTemporary() const { return TempObject.isValid(); }

    bool isNaturallyToMemory() const { return false; }

    Explosion &getFinalSubstExplosion() {
      assert(!hasTemporary());
      return Out;
    }

    Address getFinalSubstResultAddress() {
      assert(!hasTemporary());
      TempObject = Init.getObjectForTemporary();

      auto &substResultTI = IGF.getFragileTypeInfo(getSubstResultType());
      return Init.emitLocalAllocation(IGF, TempObject, NotOnHeap,
                                      substResultTI, "specialized.temp")
                 .getAddress();
    }

    void completeFinal() {
      if (!hasTemporary()) return;
      Init.markInitialized(IGF, TempObject);
    }
  };
}

bool CalleeSource::trySpecializeToExplosion(IRGenFunction &IGF,
                                            Explosion &out) {
  if (!isDirect()) return false;

  ExplosionSpecializedCallEmission emission(IGF, *this, out);
  return emission.trySpecialize();
}

namespace {
  /// An implement of SpecializedCallEmission for ultimately emitting
  /// to memory.
  class MemorySpecializedCallEmission : public SpecializedCallEmission {
    Address ResultAddress;
    const TypeInfo &SubstResultTI;

    Explosion TempExplosion;
  public:
    MemorySpecializedCallEmission(IRGenFunction &IGF, CalleeSource &source,
                                  Address addr, const TypeInfo &substResultTI)
      : SpecializedCallEmission(IGF, source),
        ResultAddress(addr), SubstResultTI(substResultTI),
        TempExplosion(ExplosionKind::Maximal) {}

  private:
    bool isNaturallyToMemory() const { return true; }

    Explosion &getFinalSubstExplosion() {
      return TempExplosion;
    }

    Address getFinalSubstResultAddress() {
      return ResultAddress;
    }

    void completeFinal() {
      if (TempExplosion.empty()) return;
      SubstResultTI.initialize(IGF, TempExplosion, ResultAddress);
    }
  };
}

bool CalleeSource::trySpecializeToMemory(IRGenFunction &IGF,
                                         Address resultAddress,
                                         const TypeInfo &substResultTI) {
  if (!isDirect()) return false;

  MemorySpecializedCallEmission emission(IGF, *this, resultAddress,
                                         substResultTI);
  return emission.trySpecialize();
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

  // We should not have any cleanups at this point.
  assert(Cleanups.empty());

  // Add the data pointer if we have one.
  if (CurCallee.hasDataPointer()) {
    assert(LastArgWritten > 0);
    Args[--LastArgWritten] = CurCallee.getDataPointer(IGF).split(Cleanups);
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

/// Add a new, empty argument to the function.
void CallEmission::addEmptyArg() {
  Explosion temp(getCurExplosionLevel());
  addArg(temp);
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
    I.markAllocated(IGF, object, addr, CleanupsDepth::invalid());
    ti.initialize(IGF, in, addr.getAddress());
    I.markInitialized(IGF, object);
    
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

  // The argument index of the first value in this explosion, for
  // the purposes of the ownership conventions.
  bool hasAbnormalOwnership = getCallee().hasOwnershipConventions();
  unsigned firstArgIndex = unsigned(Args.size() - LastArgWritten);
  SmallVector<unsigned, 8> consumedArgs;
  const unsigned *nextConsumedArg = nullptr;
  if (hasAbnormalOwnership && !arg.empty()) {
    getCallee().getOwnershipConventions()
               .getConsumedArgs(IGF.IGM, getCallee(), consumedArgs);

    // We're going to scan forward through this list.  Add a
    // terminator that we'll never reach.
    consumedArgs.push_back(~0U);

    // Start the scan, and scan past indexes that are lower than we
    // care about.
    nextConsumedArg = &consumedArgs[0];
    while (*nextConsumedArg < firstArgIndex) {
      assert(nextConsumedArg[0] < nextConsumedArg[1]);
      ++nextConsumedArg;
    }
  }

  auto argIterator = Args.begin() + targetIndex;
  auto values = arg.claimAll();
  for (unsigned i = 0, e = values.size(); i != e; ++i) {
    auto value = values[i];
    // The default rule is that arguments are consumed, in which case
    // we need to deactivate cleanups when making the call.
    if (!hasAbnormalOwnership) {
      *argIterator++ = value.split(Cleanups);

    // If we're not using the default rule, but the next argument is
    // marked as consumed, advance and consume it.
    } else if (*nextConsumedArg == firstArgIndex + i) {
      *argIterator++ = value.split(Cleanups);

      assert(nextConsumedArg[0] < nextConsumedArg[1]);
      nextConsumedArg++;

    // Otherwise, don't collect the cleanup.
    } else {
      assert(nextConsumedArg[0] > firstArgIndex + i);
      *argIterator++ = value.getValue();
    }
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

/// Evaluate the given expression as a new set of arguments to the
/// function.
void CallEmission::addArg(Expr *arg) {
  // If we're calling something with polymorphic type, we'd better have
  // substitutions.
  auto subs = getSubstitutions();
  assert(!subs.empty() || !isa<PolymorphicFunctionType>(CurOrigType));

  Explosion argE(CurCallee.getExplosionLevel());

  // If we have substitutions, then (1) it's possible for this to
  // be a polymorphic function type that we need to expand and
  // (2) we might need to evaluate the r-value differently.
  if (!subs.empty()) {
    CanType origInputType;
    auto fnType = dyn_cast<AnyFunctionType>(CurOrigType);
    if (fnType) {
      origInputType = CanType(fnType->getInput());
    } else {
      assert(isa<ArchetypeType>(CurOrigType));
      origInputType = CurOrigType;
    }

    IGF.emitRValueAsUnsubstituted(arg, origInputType, subs, argE);

    // FIXME: this doesn't handle instantiating at a generic type.
    if (auto polyFn = dyn_cast_or_null<PolymorphicFunctionType>(fnType)) {
      auto substInputType = arg->getType()->getCanonicalType();
      emitPolymorphicArguments(IGF, polyFn, substInputType, subs, argE);
    }
  } else {
    IGF.emitRValue(arg, argE);
  }

  addArg(argE);
}

/// Load from the given address to produce an argument.
void CallEmission::addMaterializedArg(Address substAddr, bool asTake) {
  // If we're calling something with polymorphic type, we'd better have
  // substitutions.
  auto subs = getSubstitutions();
  (void)subs;
  assert(subs.empty() && "can't handle substituted dematerialization now!");

  auto fnType = cast<FunctionType>(CurOrigType);
  auto &argTI = IGF.getFragileTypeInfo(fnType->getInput());

  Explosion argE(CurCallee.getExplosionLevel());
  if (asTake) {
    argTI.loadAsTake(IGF, substAddr, argE);
  } else {
    argTI.load(IGF, substAddr, argE);
  }
  addArg(argE);
}

/// Create a CallEmission for an arbitrary expression.
/// Note that this currently bypasses specialization and builtin
/// emission, so don't attempt to emit such things.
CallEmission CallEmission::forExpr(IRGenFunction &IGF, Expr *E,
                                   ExplosionKind outputLevel,
                                   unsigned numExtraArgs) {
  CalleeSource source = CalleeSource::decompose(E);
  return source.prepareCall(IGF, numExtraArgs, outputLevel);
}

/// Emit a call for its exploded results.
void irgen::emitApplyExpr(IRGenFunction &IGF, ApplyExpr *E, Explosion &out) {
  CalleeSource source = CalleeSource::decompose(E);
  if (source.trySpecializeToExplosion(IGF, out))
    return;

  CallEmission emission = source.prepareCall(IGF, 0, out.getKind());
  emission.emitToExplosion(out);
}

/// Emit a call as the initializer for an object in memory.
static void emitCalleeToMemory(IRGenFunction &IGF, CalleeSource &source,
                               Address addr, const TypeInfo &substResultTI) {
  CallEmission emission = source.prepareCall(IGF, 0, ExplosionKind::Maximal);
  emission.emitToMemory(addr, substResultTI);
}

/// Emit a call as the initializer for an object in memory.
void irgen::emitApplyExprToMemory(IRGenFunction &IGF, ApplyExpr *E,
                                  Address resultAddress,
                                  const TypeInfo &substResultTI) {
  CalleeSource source = CalleeSource::decompose(E);
  if (source.trySpecializeToMemory(IGF, resultAddress, substResultTI))
    return;

  emitCalleeToMemory(IGF, source, resultAddress, substResultTI);
}

/// Emit a nullary call to the given monomorphic function, using the
/// standard calling-convention and so on, and explode the result.
void IRGenFunction::emitNullaryCall(llvm::Value *fnPtr,
                                    CanType resultType,
                                    Explosion &resultExplosion) {
  CanType formalType = CanType(TupleType::getEmpty(IGM.Context));
  formalType = CanType(FunctionType::get(formalType, resultType, IGM.Context));

  Callee callee =
    Callee::forKnownFunction(AbstractCC::Freestanding,
                             formalType, resultType,
                             ArrayRef<Substitution>(),
                             fnPtr, ManagedValue(nullptr),
                             resultExplosion.getKind(),
                             /*uncurry level*/ 0);

  CallEmission emission(*this, callee);
  emission.addEmptyArg();
  emission.emitToExplosion(resultExplosion);
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

    return OwnedAddress(paramType.getAddressForPointer(addr), owner);
  }

  OnHeap_t onHeap = param->hasFixedLifetime() ? NotOnHeap : OnHeap;

  // If the schema contains a single aggregate, assume we can
  // just treat the next parameter as that type.
  if (paramSchema.size() == 1 && paramSchema.begin()->isAggregate()) {
    llvm::Value *addr = paramValues.claimUnmanagedNext();
    addr->setName(name);
    addr = Builder.CreateBitCast(addr,
                    paramSchema.begin()->getAggregateType()->getPointerTo());
    Address paramAddr = paramType.getAddressForPointer(addr);

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
        IGF.IGM.getExplosionSize(pattern->getType()->getCanonicalType(),
                                 Args.getKind());
      Args.claim(numIgnored);
    }
  };
}

/// Emit a specific parameter clause.
static void emitParameterClause(IRGenFunction &IGF, AnyFunctionType *fnType,
                                Pattern *param, Explosion &args) {
  assert(param->getType()->getUnlabeledType(IGF.IGM.Context)
         ->isEqual(fnType->getInput()->getUnlabeledType(IGF.IGM.Context)));

  // Emit the pattern.
  ParamPatternEmitter(IGF, args).visit(param);

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
  // Leave the cleanups created for the parameters if we've got a full
  // prologue.
  if (CurPrologue != Prologue::Bare)
    endScope(Cleanups.stable_end());

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
  outBlock.add(IGF.enterObjCReleaseCleanup(result));
}

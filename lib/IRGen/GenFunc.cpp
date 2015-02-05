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
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILModule.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclGroup.h"
#include "clang/AST/RecordLayout.h"
#include "clang/CodeGen/CodeGenABITypes.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/StringSwitch.h"

#include "IndirectTypeInfo.h"
#include "CallingConvention.h"
#include "CallEmission.h"
#include "Explosion.h"
#include "GenClass.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenObjC.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "FixedTypeInfo.h"
#include "ScalarTypeInfo.h"
#include "GenFunc.h"

using namespace swift;
using namespace irgen;

bool ExplosionSchema::requiresIndirectResult(IRGenModule &IGM) const {
  return containsAggregate() ||
         size() > IGM.TargetInfo.MaxScalarsForDirectResult;
}

llvm::Type *ExplosionSchema::getScalarResultType(IRGenModule &IGM) const {
  if (size() == 0) {
    return IGM.VoidTy;
  } else if (size() == 1) {
    return begin()->getScalarType();
  } else {
    SmallVector<llvm::Type*, 16> elts;
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

/// Return the natural level at which to uncurry this function.  This
/// is the number of additional parameter clauses that are uncurried
/// in the function body.
unsigned irgen::getDeclNaturalUncurryLevel(ValueDecl *val) {
  if (FuncDecl *FD = dyn_cast<FuncDecl>(val)) {
    return FD->getNaturalArgumentCount() - 1;
  }
  if (isa<ConstructorDecl>(val) || isa<EnumElementDecl>(val)) {
    return 1;
  }
  if (isa<DestructorDecl>(val)) {
    return 0;
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
  case AbstractCC::WitnessMethod:
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
      Explosion Direct;

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
    Explosion &initForDirectValues() {
      assert(CurState == State::Invalid);
      CurState = State::Direct;
      return *new (&CurValue.Direct) Explosion();
    }

    /// As a potential efficiency, set that this is a direct result
    /// with no values.
    void setAsEmptyDirect() {
      initForDirectValues();
    }

    /// Set this result so that it carries a single directly-returned
    /// maximally-fragile value without management.
    void setAsSingleDirectUnmanagedFragileValue(llvm::Value *value) {
      initForDirectValues().add(value);
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
      return CurValue.Direct;
    }

    Address getIndirectAddress() const {
      assert(isIndirect());
      return CurValue.Indirect;
    }

    void reset() {
      if (CurState == State::Direct)
        CurValue.Direct.~Explosion();
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

  /// Information about the IR-level signature of a function type.
  class FuncSignatureInfo {
  private:
    /// Each possible currying of a function type has different function
    /// type variants along each of three orthogonal axes:
    ///   - the explosion kind desired
    ///   - whether a data pointer argument is required
    struct Currying {
      Signature Signatures[unsigned(ExtraData::Last_ExtraData) + 1];

      Signature &select(ExtraData extraData) {
        return Signatures[unsigned(extraData)];
      }
    };

    /// The SIL function type being represented.
    const CanSILFunctionType FormalType;
    
    /// The ExtraData kind associated with the function reference.
    ExtraData ExtraDataKind;

    mutable Currying TheSignatures;
    
  public:
    FuncSignatureInfo(CanSILFunctionType formalType,
                      ExtraData extraDataKind)
      : FormalType(formalType), ExtraDataKind(extraDataKind) {}
    
    ExtraData getExtraDataKind() const {
      return ExtraDataKind;
    }
    
    Signature getSignature(IRGenModule &IGM, ExtraData extraData) const;

  };

  /// The @thin function type-info class.
  class ThinFuncTypeInfo : public PODSingleScalarTypeInfo<ThinFuncTypeInfo,
                                                          LoadableTypeInfo>,
                           public FuncSignatureInfo {
    ThinFuncTypeInfo(CanSILFunctionType formalType, llvm::Type *storageType,
                     Size size, Alignment align,
                     const SpareBitVector &spareBits)
      : PODSingleScalarTypeInfo(storageType, size, spareBits, align),
        FuncSignatureInfo(formalType, ExtraData::None)
    {
    }

  public:
    static const ThinFuncTypeInfo *create(CanSILFunctionType formalType,
                                          llvm::Type *storageType,
                                          Size size, Alignment align,
                                          const SpareBitVector &spareBits) {
      return new ThinFuncTypeInfo(formalType, storageType, size, align,
                                  spareBits);
    }

    bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
      return true;
    }

    unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
      return getFunctionPointerExtraInhabitantCount(IGM);
    }

    llvm::ConstantInt *getFixedExtraInhabitantValue(IRGenModule &IGM,
                                                  unsigned bits,
                                                  unsigned index) const override {
      return getFunctionPointerFixedExtraInhabitantValue(IGM, bits, index, 0);
    }

    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                         SILType T)
    const override {
      return getFunctionPointerExtraInhabitantIndex(IGF, src);
    }

    void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                              Address dest, SILType T) const override {
      return storeFunctionPointerExtraInhabitant(IGF, index, dest);
    }
  };

  /// The @thick function type-info class.
  class FuncTypeInfo : public ScalarTypeInfo<FuncTypeInfo, ReferenceTypeInfo>,
                       public FuncSignatureInfo {
    FuncTypeInfo(CanSILFunctionType formalType, llvm::StructType *storageType,
                 Size size, Alignment align, SpareBitVector &&spareBits)
      : ScalarTypeInfo(storageType, size, std::move(spareBits), align),
        FuncSignatureInfo(formalType, ExtraData::Retainable)
    {
    }

  public:
    static const FuncTypeInfo *create(CanSILFunctionType formalType,
                                      llvm::StructType *storageType,
                                      Size size, Alignment align,
                                      SpareBitVector &&spareBits) {
      return new FuncTypeInfo(formalType, storageType, size, align,
                              std::move(spareBits));
    }
    
    // Function types do not satisfy allowsOwnership.
    const WeakTypeInfo *
    createWeakStorageType(TypeConverter &TC) const override {
      llvm_unreachable("[weak] function type");
    }
    const UnownedTypeInfo *
    createUnownedStorageType(TypeConverter &TC) const override {
      llvm_unreachable("[unowned] function type");
    }
    const TypeInfo *
    createUnmanagedStorageType(TypeConverter &TC) const override {
      llvm_unreachable("@unowned(unsafe) function type");
    }

    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    unsigned getExplosionSize() const override {
      return 2;
    }

    void getSchema(ExplosionSchema &schema) const {
      llvm::StructType *structTy = cast<llvm::StructType>(getStorageType());
      schema.add(ExplosionSchema::Element::forScalar(structTy->getElementType(0)));
      schema.add(ExplosionSchema::Element::forScalar(structTy->getElementType(1)));
    }

    Address projectFunction(IRGenFunction &IGF, Address address) const {
      return IGF.Builder.CreateStructGEP(address, 0, Size(0),
                                         address->getName() + ".fn");
    }

    Address projectData(IRGenFunction &IGF, Address address) const {
      return IGF.Builder.CreateStructGEP(address, 1, IGF.IGM.getPointerSize(),
                                         address->getName() + ".data");
    }

    void loadAsCopy(IRGenFunction &IGF, Address address, Explosion &e) const {
      // Load the function.
      Address fnAddr = projectFunction(IGF, address);
      e.add(IGF.Builder.CreateLoad(fnAddr, fnAddr->getName()+".load"));

      Address dataAddr = projectData(IGF, address);
      IGF.emitLoadAndRetain(dataAddr, e);
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // Load the function.
      Address fnAddr = projectFunction(IGF, addr);
      e.add(IGF.Builder.CreateLoad(fnAddr));

      Address dataAddr = projectData(IGF, addr);
      e.add(IGF.Builder.CreateLoad(dataAddr));
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the function pointer.
      Address fnAddr = projectFunction(IGF, address);
      IGF.Builder.CreateStore(e.claimNext(), fnAddr);

      Address dataAddr = projectData(IGF, address);
      IGF.emitAssignRetained(e.claimNext(), dataAddr);
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the function pointer.
      Address fnAddr = projectFunction(IGF, address);
      IGF.Builder.CreateStore(e.claimNext(), fnAddr);

      // Store the data pointer, if any, transferring the +1.
      Address dataAddr = projectData(IGF, address);
      IGF.emitInitializeRetained(e.claimNext(), dataAddr);
    }

    void copy(IRGenFunction &IGF, Explosion &src,
              Explosion &dest) const override {
      src.transferInto(dest, 1);
      
      IGF.emitRetain(src.claimNext(), dest);
    }
    
    void consume(IRGenFunction &IGF, Explosion &src) const override {
      src.claimNext();
      IGF.emitRelease(src.claimNext());
    }

    void fixLifetime(IRGenFunction &IGF, Explosion &src) const override {
      src.claimNext();      
      IGF.emitFixLifetime(src.claimNext());
    }

    void retain(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitRetainCall(e.claimNext());
    }
    
    void release(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitRelease(e.claimNext());
    }

    void retainUnowned(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitRetainUnowned(e.claimNext());
    }
    
    void unownedRetain(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitUnownedRetain(e.claimNext());
    }

    void unownedRelease(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitUnownedRelease(e.claimNext());
    }
    
    void destroy(IRGenFunction &IGF, Address addr, SILType T) const {
      IGF.emitRelease(IGF.Builder.CreateLoad(projectData(IGF, addr)));
    }
    
    llvm::Value *packEnumPayload(IRGenFunction &IGF,
                                  Explosion &src,
                                  unsigned bitWidth,
                                  unsigned offset) const override {
      PackEnumPayload pack(IGF, bitWidth);
      pack.addAtOffset(src.claimNext(), offset);
      pack.add(src.claimNext());
      return pack.get();
    }
    
    void unpackEnumPayload(IRGenFunction &IGF,
                            llvm::Value *payload,
                            Explosion &dest,
                            unsigned offset) const override {
      UnpackEnumPayload unpack(IGF, payload);
      auto storageTy = getStorageType();
      dest.add(unpack.claimAtOffset(storageTy->getElementType(0),
                                    offset));
      dest.add(unpack.claim(storageTy->getElementType(1)));
    }

    bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
      return true;
    }

    unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
      return getFunctionPointerExtraInhabitantCount(IGM);
    }

    llvm::ConstantInt *getFixedExtraInhabitantValue(IRGenModule &IGM,
                                                    unsigned bits,
                                                    unsigned index) const override {
      return getFunctionPointerFixedExtraInhabitantValue(IGM, bits, index, 0);
    }

    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                         SILType T) const override {
      src = projectFunction(IGF, src);
      return getFunctionPointerExtraInhabitantIndex(IGF, src);
    }

    SpareBitVector
    getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
      SpareBitVector mask;
      mask.appendSetBits(IGM.getPointerSize().getValueInBits());
      mask.appendClearBits(IGM.getPointerSize().getValueInBits());
      return mask;
    }

    llvm::Value *maskFixedExtraInhabitant(IRGenFunction &IGF,
                                          llvm::Value *bits) const override {
      // Truncate down to the function-pointer type and zext back again.
      llvm::Type *originalType = bits->getType();
      bits = IGF.Builder.CreateTrunc(bits, IGF.IGM.IntPtrTy);
      bits = IGF.Builder.CreateZExt(bits, originalType);
      return bits;
    }

    void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                              Address dest, SILType T) const override {
      dest = projectFunction(IGF, dest);
      return storeFunctionPointerExtraInhabitant(IGF, index, dest);
    }
  };

  /// The type-info class for ObjC blocks, which are represented by an ObjC
  /// heap pointer.
  class BlockTypeInfo : public HeapTypeInfo<BlockTypeInfo>,
                        public FuncSignatureInfo
  {
  public:
    BlockTypeInfo(CanSILFunctionType ty,
                  llvm::PointerType *storageType,
                  Size size, SpareBitVector spareBits, Alignment align)
      : HeapTypeInfo(storageType, size, spareBits, align),
        FuncSignatureInfo(ty, ExtraData::Block)
    {
    }

    ReferenceCounting getReferenceCounting() const {
      return ReferenceCounting::Block;
    }
  };
  
  /// The type info class for the on-stack representation of an ObjC block.
  ///
  /// TODO: May not be fixed-layout if we capture generics.
  class BlockStorageTypeInfo final
    : public IndirectTypeInfo<BlockStorageTypeInfo, FixedTypeInfo>
  {
    Size CaptureOffset;
  public:
    BlockStorageTypeInfo(llvm::Type *type, Size size, Alignment align,
                         SpareBitVector &&spareBits,
                         IsPOD_t pod, IsBitwiseTakable_t bt, Size captureOffset)
      : IndirectTypeInfo(type, size, std::move(spareBits), align, pod, bt),
        CaptureOffset(captureOffset)
    {}
    
    // The lowered type should be an LLVM struct comprising the block header
    // (IGM.ObjCBlockStructTy) as its first element and the capture as its
    // second.
    
    Address projectBlockHeader(IRGenFunction &IGF, Address storage) const {
      return IGF.Builder.CreateStructGEP(storage, 0, Size(0));
    }
    
    Address projectCapture(IRGenFunction &IGF, Address storage) const {
      return IGF.Builder.CreateStructGEP(storage, 1, CaptureOffset);
    }
    
    // TODO
    // The frontend will currently never emit copy_addr or destroy_addr for
    // block storage.
    
    void assignWithCopy(IRGenFunction &IGF, Address dest,
                        Address src, SILType T) const override {
      IGF.unimplemented(SourceLoc(), "copying @block_storage");
    }
    void initializeWithCopy(IRGenFunction &IGF, Address dest,
                            Address src, SILType T) const override {
      IGF.unimplemented(SourceLoc(), "copying @block_storage");
    }
    void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {
      IGF.unimplemented(SourceLoc(), "destroying @block_storage");
    }
  };
}

const TypeInfo *TypeConverter::convertBlockStorageType(SILBlockStorageType *T) {
  // The block storage consists of the block header (ObjCBlockStructTy)
  // followed by the lowered type of the capture.
  auto &capture = IGM.getTypeInfoForLowered(T->getCaptureType());
  
  // TODO: Support dynamic-sized captures.
  const FixedTypeInfo *fixedCapture = dyn_cast<FixedTypeInfo>(&capture);
  llvm::Type *fixedCaptureTy;
  // The block header is pointer aligned. The capture may be worse aligned.
  Alignment align = IGM.getPointerAlignment();
  Size captureOffset(
    IGM.DataLayout.getStructLayout(IGM.ObjCBlockStructTy)->getSizeInBytes());
  Size size = captureOffset;
  SpareBitVector spareBits =
    SpareBitVector::getConstant(size.getValueInBits(), false);
  IsPOD_t pod = IsNotPOD;
  IsBitwiseTakable_t bt = IsNotBitwiseTakable;
  if (!fixedCapture) {
    IGM.unimplemented(SourceLoc(), "dynamic @block_storage capture");
    fixedCaptureTy = llvm::StructType::get(IGM.getLLVMContext(), {});
  } else {
    fixedCaptureTy = cast<FixedTypeInfo>(capture).getStorageType();
    align = std::max(align, fixedCapture->getFixedAlignment());
    captureOffset = captureOffset.roundUpToAlignment(align);
    spareBits.extendWithSetBits(captureOffset.getValueInBits());
    size = captureOffset + fixedCapture->getFixedSize();
    spareBits.append(fixedCapture->getSpareBits());
    pod = fixedCapture->isPOD(ResilienceScope::Component);
    bt = fixedCapture->isBitwiseTakable(ResilienceScope::Component);
  }
  
  llvm::Type *storageElts[] = {
    IGM.ObjCBlockStructTy,
    fixedCaptureTy,
  };
  
  auto storageTy = llvm::StructType::get(IGM.getLLVMContext(), storageElts,
                                         /*packed*/ false);
  return new BlockStorageTypeInfo(storageTy, size, align, std::move(spareBits),
                                  pod, bt, captureOffset);
}

Address irgen::projectBlockStorageCapture(IRGenFunction &IGF,
                                          Address storageAddr,
                                          CanSILBlockStorageType storageTy) {
  auto &tl = IGF.getTypeInfoForLowered(storageTy).as<BlockStorageTypeInfo>();
  return tl.projectCapture(IGF, storageAddr);
}

const TypeInfo *TypeConverter::convertFunctionType(SILFunctionType *T) {
  switch (T->getRepresentation()) {
  case AnyFunctionType::Representation::Block:
    return new BlockTypeInfo(CanSILFunctionType(T),
                             IGM.ObjCBlockPtrTy,
                             IGM.getPointerSize(),
                             IGM.getHeapObjectSpareBits(),
                             IGM.getPointerAlignment());
      
  case AnyFunctionType::Representation::Thin:
    return ThinFuncTypeInfo::create(CanSILFunctionType(T),
                                    IGM.FunctionPtrTy,
                                    IGM.getPointerSize(),
                                    IGM.getPointerAlignment(),
                                    IGM.getFunctionPointerSpareBits());

  case AnyFunctionType::Representation::Thick: {
#ifndef NDEBUG
    // For non-witness methods, 'thick' always indicates a retainable context
    // pointer.
    switch (T->getAbstractCC()) {
    case AbstractCC::Freestanding:
    case AbstractCC::Method:
      break;

    case AbstractCC::WitnessMethod:
      // TODO: This should fall into the "retainable" bucket, but we
      // historically abused thick witness methods for other purposes.
      // Fail here as a safety against miscompiling code that tries to
      // work the old way.
      llvm_unreachable("thick witness method not supported");

    case AbstractCC::C:
    case AbstractCC::ObjCMethod:
      llvm_unreachable("thick foreign functions should be lowered to a "
                       "block type");
    }
#endif

    SpareBitVector spareBits;
    spareBits.append(IGM.getFunctionPointerSpareBits());
    spareBits.append(IGM.getHeapObjectSpareBits());
    
    return FuncTypeInfo::create(CanSILFunctionType(T),
                                IGM.FunctionPairTy,
                                IGM.getPointerSize() * 2,
                                IGM.getPointerAlignment(),
                                std::move(spareBits));
  }
  }
  llvm_unreachable("bad function type representation");
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

static void addNoAliasAttribute(IRGenModule &IGM,
                                llvm::AttributeSet &attrs,
                                unsigned argIndex) {
  static const llvm::Attribute::AttrKind attrKinds[] = {
    llvm::Attribute::NoAlias
  };
  auto resultAttrs = llvm::AttributeSet::get(IGM.LLVMContext, argIndex+1,
                                             attrKinds);
  attrs = attrs.addAttributes(IGM.LLVMContext, argIndex+1, resultAttrs);
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

void irgen::addExtendAttribute(IRGenModule &IGM,
                               llvm::AttributeSet &attrs,
                               unsigned index, bool signExtend) {
  llvm::AttrBuilder b;
  if (signExtend)
    b.addAttribute(llvm::Attribute::SExt);
  else
    b.addAttribute(llvm::Attribute::ZExt);
  auto resultAttrs = llvm::AttributeSet::get(IGM.LLVMContext, index, b);
  attrs = attrs.addAttributes(IGM.LLVMContext, index, resultAttrs);
}

namespace {
  class SignatureExpansion {
    IRGenModule &IGM;
    CanSILFunctionType FnType;
  public:
    SmallVector<llvm::Type*, 8> ParamIRTypes;
    llvm::AttributeSet Attrs;
    bool HasIndirectResult = false;

    SignatureExpansion(IRGenModule &IGM, CanSILFunctionType fnType)
      : IGM(IGM), FnType(fnType) {}

    llvm::Type *expandSignatureTypes();

  private:
    void expand(SILParameterInfo param);
    llvm::Type *addIndirectResult();

    unsigned getCurParamIndex() {
      return ParamIRTypes.size();
    }

    /// Add a pointer to the given type as the next parameter.
    void addPointerParameter(llvm::Type *storageType) {
      ParamIRTypes.push_back(storageType->getPointerTo());
    }

    llvm::Type *expandResult();
    void expandParameters();
    llvm::Type *expandExternalSignatureTypes();
  };
}

llvm::Type *SignatureExpansion::addIndirectResult() {
  auto resultType = FnType->getResult().getSILType();
  const TypeInfo &resultTI = IGM.getTypeInfo(resultType);
  addPointerParameter(resultTI.getStorageType());
  addIndirectReturnAttributes(IGM, Attrs);
  return IGM.VoidTy;
}

llvm::Type *SignatureExpansion::expandResult() {
  // Handle the direct result type, checking for supposedly scalar
  // result types that we actually want to return indirectly.
  auto resultType = FnType->getResult().getSILType();

  // Fast-path the empty tuple type.
  if (auto tuple = resultType.getAs<TupleType>())
    if (tuple->getNumElements() == 0)
      return IGM.VoidTy;

  ExplosionSchema schema = IGM.getSchema(resultType);
  switch (FnType->getAbstractCC()) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    llvm_unreachable("Expanding C/ObjC parameters in the wrong place!");
    break;
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod: {
    if (schema.requiresIndirectResult(IGM))
      return addIndirectResult();
    return schema.getScalarResultType(IGM);
  }
  }
}

static const clang::FieldDecl *
getLargestUnionField(const clang::RecordDecl *record,
                     const clang::ASTContext &ctx) {
  const clang::FieldDecl *largestField = nullptr;
  clang::CharUnits unionSize = clang::CharUnits::Zero();

  for (auto field : record->fields()) {
    assert(!field->isBitField());
    clang::CharUnits fieldSize = ctx.getTypeSizeInChars(field->getType());
    if (unionSize < fieldSize) {
      unionSize = fieldSize;
      largestField = field;
    }
  }
  assert(largestField && "empty union?");
  return largestField;
}

namespace {
  /// A CRTP class for working with Clang's ABIArgInfo::Expand
  /// argument type expansions.
  template <class Impl, class... Args> struct ClangExpand {
    IRGenModule &IGM;
    const clang::ASTContext &Ctx;
    ClangExpand(IRGenModule &IGM) : IGM(IGM), Ctx(IGM.getClangASTContext()) {}

    Impl &asImpl() { return *static_cast<Impl*>(this); }

    void visit(clang::CanQualType type, Args... args) {
      switch (type->getTypeClass()) {
#define TYPE(Class, Base)
#define NON_CANONICAL_TYPE(Class, Base) \
      case clang::Type::Class:
#define DEPENDENT_TYPE(Class, Base) \
      case clang::Type::Class:
#define NON_CANONICAL_UNLESS_DEPENDENT_TYPE(Class, Base) \
      case clang::Type::Class:
#include "clang/AST/TypeNodes.def"
        llvm_unreachable("canonical or dependent type in ABI lowering");

      // These shouldn't occur in expandable struct types.
      case clang::Type::IncompleteArray:
      case clang::Type::VariableArray:
        llvm_unreachable("variable-sized or incomplete array in ABI lowering");

      // We should only ever get ObjC pointers, not underlying objects.
      case clang::Type::ObjCInterface:
      case clang::Type::ObjCObject:
        llvm_unreachable("ObjC object type in ABI lowering");

      // We should only ever get function pointers.
      case clang::Type::FunctionProto:
      case clang::Type::FunctionNoProto:
        llvm_unreachable("non-pointer function type in ABI lowering");

      // We currently never import C++ code, and we should be able to
      // kill Expand before we do.
      case clang::Type::LValueReference:
      case clang::Type::RValueReference:
      case clang::Type::MemberPointer:
      case clang::Type::Auto:
        llvm_unreachable("C++ type in ABI lowering?");

      case clang::Type::ConstantArray: {
        auto array = Ctx.getAsConstantArrayType(type);
        auto elt = Ctx.getCanonicalType(array->getElementType());
        auto &&context = asImpl().beginArrayElements(elt);
        uint64_t n = array->getSize().getZExtValue();
        for (uint64_t i = 0; i != n; ++i) {
          asImpl().visitArrayElement(elt, i, context, args...);
        }
        return;
      }

      case clang::Type::Record: {
        auto record = cast<clang::RecordType>(type)->getDecl();
        if (record->isUnion()) {
          auto largest = getLargestUnionField(record, Ctx);
          asImpl().visitUnionField(record, largest, args...);
        } else {
          auto &&context = asImpl().beginStructFields(record);
          for (auto field : record->fields()) {
            asImpl().visitStructField(record, field, context, args...);
          }
        }
        return;
      }

      case clang::Type::Complex: {
        auto elt = type.castAs<clang::ComplexType>().getElementType();
        asImpl().visitComplexElement(elt, 0, args...);
        asImpl().visitComplexElement(elt, 1, args...);
        return;
      }

      // Just handle this types as opaque integers.
      case clang::Type::Enum:
      case clang::Type::Atomic:
        asImpl().visitScalar(convertTypeAsInteger(type), args...);
        return;

      case clang::Type::Builtin:
        asImpl().visitScalar(
                      convertBuiltinType(type.castAs<clang::BuiltinType>()),
                             args...);
        return;

      case clang::Type::Vector:
      case clang::Type::ExtVector:
        asImpl().visitScalar(
                      convertVectorType(type.castAs<clang::VectorType>()),
                             args...);
        return;

      case clang::Type::Pointer:
      case clang::Type::BlockPointer:
      case clang::Type::ObjCObjectPointer:
        asImpl().visitScalar(IGM.Int8PtrTy, args...);
        return;
      }
      llvm_unreachable("bad type kind");
    }
    
    Size getSizeOfType(clang::QualType type) {
      auto clangSize = Ctx.getTypeSizeInChars(type);
      return Size(clangSize.getQuantity());
    }

  private:
    llvm::Type *convertVectorType(clang::CanQual<clang::VectorType> type) {
      auto eltTy =
        convertBuiltinType(type->getElementType().castAs<clang::BuiltinType>());
      return llvm::VectorType::get(eltTy, type->getNumElements());
    }

    llvm::Type *convertBuiltinType(clang::CanQual<clang::BuiltinType> type) {
      switch (type.getTypePtr()->getKind()) {
#define BUILTIN_TYPE(Id, SingletonId)
#define PLACEHOLDER_TYPE(Id, SingletonId) \
      case clang::BuiltinType::Id:
#include "clang/AST/BuiltinTypes.def"
      case clang::BuiltinType::Dependent:
        llvm_unreachable("placeholder type in ABI lowering");

      // We should never see these unadorned.
      case clang::BuiltinType::ObjCId:
      case clang::BuiltinType::ObjCClass:
      case clang::BuiltinType::ObjCSel:
        llvm_unreachable("bare Objective-C object type in ABI lowering");

      // This should never be the type of an argument or field.
      case clang::BuiltinType::Void:
        llvm_unreachable("bare void type in ABI lowering");

      // We should never see the OpenCL builtin types at all.
      case clang::BuiltinType::OCLImage1d:
      case clang::BuiltinType::OCLImage1dArray:
      case clang::BuiltinType::OCLImage1dBuffer:
      case clang::BuiltinType::OCLImage2d:
      case clang::BuiltinType::OCLImage2dArray:
      case clang::BuiltinType::OCLImage3d:
      case clang::BuiltinType::OCLSampler:
      case clang::BuiltinType::OCLEvent:
        llvm_unreachable("OpenCL type in ABI lowering");

      // Handle all the integer types as opaque values.
#define BUILTIN_TYPE(Id, SingletonId)
#define SIGNED_TYPE(Id, SingletonId) \
      case clang::BuiltinType::Id:
#define UNSIGNED_TYPE(Id, SingletonId) \
      case clang::BuiltinType::Id:
#include "clang/AST/BuiltinTypes.def"
        return convertTypeAsInteger(type);

      // Lower all the floating-point values by their semantics.
      case clang::BuiltinType::Half:
        return convertFloatingType(Ctx.getTargetInfo().getHalfFormat());
      case clang::BuiltinType::Float:
        return convertFloatingType(Ctx.getTargetInfo().getFloatFormat());
      case clang::BuiltinType::Double:
        return convertFloatingType(Ctx.getTargetInfo().getDoubleFormat());
      case clang::BuiltinType::LongDouble:
        return convertFloatingType(Ctx.getTargetInfo().getLongDoubleFormat());

      // nullptr_t -> void*
      case clang::BuiltinType::NullPtr:
        return IGM.Int8PtrTy;
      }
      llvm_unreachable("bad builtin type");
    }

    llvm::Type *convertFloatingType(const llvm::fltSemantics &format) {
      if (&format == &llvm::APFloat::IEEEhalf)
        return llvm::Type::getHalfTy(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::IEEEsingle)
        return llvm::Type::getFloatTy(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::IEEEdouble)
        return llvm::Type::getDoubleTy(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::IEEEquad)
        return llvm::Type::getFP128Ty(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::PPCDoubleDouble)
        return llvm::Type::getPPC_FP128Ty(IGM.getLLVMContext());
      if (&format == &llvm::APFloat::x87DoubleExtended)
        return llvm::Type::getX86_FP80Ty(IGM.getLLVMContext());
      llvm_unreachable("bad float format");
    }

    llvm::Type *convertTypeAsInteger(clang::QualType type) {
      auto size = getSizeOfType(type);
      return llvm::IntegerType::get(IGM.getLLVMContext(),
                                    size.getValueInBits());
    }
  };

  /// A CRTP specialization of ClangExpand which projects down to
  /// various aggregate elements of an address.
  ///
  /// Subclasses should only have to define visitScalar.
  template <class Impl>
  class ClangExpandProjection : public ClangExpand<Impl, Address> {
    using super = ClangExpand<Impl, Address>;
    using super::asImpl;
    using super::IGM;
    using super::Ctx;
    using super::getSizeOfType;

  protected:
    IRGenFunction &IGF;
    ClangExpandProjection(IRGenFunction &IGF)
      : super(IGF.IGM), IGF(IGF) {}

  public:
    void visit(clang::CanQualType type, Address addr) {
      assert(addr.getType() == IGM.Int8PtrTy);
      super::visit(type, addr);
    }
    
    Size beginArrayElements(clang::CanQualType element) {
      return getSizeOfType(element);
    }
    void visitArrayElement(clang::CanQualType element, unsigned i,
                           Size elementSize, Address arrayAddr) {
      asImpl().visit(element, createGEPAtOffset(arrayAddr, elementSize * i));
    }

    void visitComplexElement(clang::CanQualType element, unsigned i,
                             Address complexAddr) {
      Address addr = complexAddr;
      if (i) { addr = createGEPAtOffset(complexAddr, getSizeOfType(element)); }
      asImpl().visit(element, addr);
    }

    void visitUnionField(const clang::RecordDecl *record,
                         const clang::FieldDecl *field,
                         Address structAddr) {
      asImpl().visit(Ctx.getCanonicalType(field->getType()), structAddr);
    }

    const clang::ASTRecordLayout &
    beginStructFields(const clang::RecordDecl *record) {
      return Ctx.getASTRecordLayout(record);
    }
    void visitStructField(const clang::RecordDecl *record,
                          const clang::FieldDecl *field,
                          const clang::ASTRecordLayout &layout,
                          Address structAddr) {
      auto fieldIndex = field->getFieldIndex();
      auto fieldOffset = Size(layout.getFieldOffset(fieldIndex) / 8);
      asImpl().visit(Ctx.getCanonicalType(field->getType()),
                     createGEPAtOffset(structAddr, fieldOffset));
    }

  private:
    Address createGEPAtOffset(Address addr, Size offset) {
      if (offset.isZero()) {
        return addr;
      } else {
        return IGF.Builder.CreateConstByteArrayGEP(addr, offset);
      }
    }
  };

  /// A class for collecting the types of a Clang ABIArgInfo::Expand
  /// argument expansion.
  struct ClangExpandTypeCollector : ClangExpand<ClangExpandTypeCollector> {
    SmallVectorImpl<llvm::Type*> &Types;
    ClangExpandTypeCollector(IRGenModule &IGM,
                             SmallVectorImpl<llvm::Type*> &types)
      : ClangExpand(IGM), Types(types) {}

    bool beginArrayElements(clang::CanQualType element) { return true; }
    void visitArrayElement(clang::CanQualType element, unsigned i, bool _) {
      visit(element);
    }

    void visitComplexElement(clang::CanQualType element, unsigned i) {
      visit(element);
    }

    void visitUnionField(const clang::RecordDecl *record,
                         const clang::FieldDecl *field) {
      visit(Ctx.getCanonicalType(field->getType()));
    }

    bool beginStructFields(const clang::RecordDecl *record) { return true; }
    void visitStructField(const clang::RecordDecl *record,
                          const clang::FieldDecl *field,
                          bool _) {
      visit(Ctx.getCanonicalType(field->getType()));
    }

    void visitScalar(llvm::Type *type) {
      Types.push_back(type);
    }
  };
}

static bool doesClangExpansionMatchSchema(IRGenModule &IGM,
                                          clang::CanQualType type,
                                          const ExplosionSchema &schema) {
  assert(!schema.containsAggregate());
  SmallVector<llvm::Type *, 4> expansion;
  ClangExpandTypeCollector(IGM, expansion).visit(type);

  if (expansion.size() != schema.size())
    return false;

  for (size_t i = 0, e = schema.size(); i != e; ++i) {
    if (schema[i].getScalarType() != expansion[i])
      return false;
  }

  return true;
}

/// Expand the result and parameter types to the appropriate LLVM IR
/// types for C and Objective-C signatures.
llvm::Type *SignatureExpansion::expandExternalSignatureTypes() {
  assert(FnType->getAbstractCC() == AbstractCC::ObjCMethod ||
         FnType->getAbstractCC() == AbstractCC::C);

  // Convert the SIL result type to a Clang type.
  auto resultTy = FnType->getResult().getSILType();
  auto clangResultTy = IGM.getClangType(resultTy);

  // Now convert the parameters to Clang types.
  auto params = FnType->getParameters();
  unsigned paramOffset = 0;

  SmallVector<clang::CanQualType,4> paramTys;
  auto const &clangCtx = IGM.getClangASTContext();

  if (FnType->getAbstractCC() == AbstractCC::ObjCMethod) {
    // ObjC methods take their 'self' argument first, followed by an
    // implicit _cmd argument.
    auto &self = params.back();
    auto clangTy = IGM.getClangType(self.getSILType());
    paramTys.push_back(clangTy);
    paramTys.push_back(clangCtx.VoidPtrTy);
    params = params.slice(0, params.size() - 1);
    paramOffset = 2;
  }

  // Convert each parameter to a Clang type.
  for (auto param : params) {
    auto clangTy = IGM.getClangType(param.getSILType());
    paramTys.push_back(clangTy);
  }

  // We shouldn't have any LLVM parameter types yet, aside from a block context
  // pointer.
  assert((FnType->getRepresentation() == FunctionType::Representation::Block
            ? ParamIRTypes.size() == 1
            : ParamIRTypes.empty())
         && "Expected empty ParamIRTypes");

  // Generate function info for this signature.
  auto extInfo = clang::FunctionType::ExtInfo();
  auto &FI = IGM.ABITypes->arrangeFreeFunctionCall(clangResultTy, paramTys,
                                                   extInfo,
                                             clang::CodeGen::RequiredArgs::All);

  assert(FI.arg_size() == paramTys.size() &&
         "Expected one ArgInfo for each parameter type!");

  auto &returnInfo = FI.getReturnInfo();

  // Does the result need an extension attribute?
  if (returnInfo.isExtend()) {
    bool signExt = clangResultTy->hasSignedIntegerRepresentation();
    assert((signExt || clangResultTy->hasUnsignedIntegerRepresentation()) &&
           "Invalid attempt to add extension attribute to argument!");
    addExtendAttribute(IGM, Attrs, llvm::AttributeSet::ReturnIndex, signExt);
  }

  // If we return indirectly, that is the first parameter type.
  if (returnInfo.isIndirect())
    addIndirectResult();

  for (auto i : indices(paramTys)) {
    auto &AI = FI.arg_begin()[i].info;

    // Add a padding argument if required.
    if (auto *padType = AI.getPaddingType())
      ParamIRTypes.push_back(padType);

    switch (AI.getKind()) {
    case clang::CodeGen::ABIArgInfo::Extend: {
      bool signExt = paramTys[i]->hasSignedIntegerRepresentation();
      assert((signExt || paramTys[i]->hasUnsignedIntegerRepresentation()) &&
             "Invalid attempt to add extension attribute to argument!");
      addExtendAttribute(IGM, Attrs, getCurParamIndex()+1, signExt);
      SWIFT_FALLTHROUGH;
    }
    case clang::CodeGen::ABIArgInfo::Direct: {
      // If the coercion type is a struct, we need to expand it.
      auto type = AI.getCoerceToType();
      if (auto expandedType = dyn_cast<llvm::StructType>(type)) {
        for (size_t j = 0, e = expandedType->getNumElements(); j != e; ++j)
          ParamIRTypes.push_back(expandedType->getElementType(j));
      } else {
        ParamIRTypes.push_back(type);
      }
      break;
    }
    case clang::CodeGen::ABIArgInfo::Indirect: {
      assert(i >= paramOffset &&
             "Unexpected index for indirect byval argument");
      auto &param = params[i - paramOffset];
      auto &paramTI = cast<FixedTypeInfo>(IGM.getTypeInfo(param.getSILType()));
      if (AI.getIndirectByVal())
        addByvalArgumentAttributes(IGM, Attrs, getCurParamIndex(),
                                   paramTI.getFixedAlignment());
      addPointerParameter(paramTI.getStorageType());
      break;
    }
    case clang::CodeGen::ABIArgInfo::Expand:
      ClangExpandTypeCollector(IGM, ParamIRTypes).visit(paramTys[i]);
      break;
    case clang::CodeGen::ABIArgInfo::Ignore:
      break;
    case clang::CodeGen::ABIArgInfo::InAlloca:
      llvm_unreachable("Need to handle InAlloca during signature expansion");
    }
  }

  if (returnInfo.isIndirect() || returnInfo.isIgnore())
    return IGM.VoidTy;

  return returnInfo.getCoerceToType();
}

void SignatureExpansion::expand(SILParameterInfo param) {
  switch (param.getConvention()) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_Out:
  case ParameterConvention::Indirect_In_Guaranteed:
    if (param.isIndirectResult()) {
      assert(ParamIRTypes.empty());
      addIndirectReturnAttributes(IGM, Attrs);
      HasIndirectResult = true;
    } else {
      addNoAliasAttribute(IGM, Attrs, getCurParamIndex());
    }
    addPointerParameter(IGM.getStorageType(param.getSILType()));
    return;

  case ParameterConvention::Direct_Owned:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
    // Go ahead and further decompose tuples.
    if (auto tuple = dyn_cast<TupleType>(param.getType())) {
      for (auto elt : tuple.getElementTypes()) {
        // Propagate the same ownedness down to the element.
        expand(SILParameterInfo(elt, param.getConvention()));
      }
      return;
    }

    switch (FnType->getAbstractCC()) {
    case AbstractCC::C:
    case AbstractCC::ObjCMethod: {
      llvm_unreachable("Unexpected C/ObjC method in parameter expansion!");
      return;
    }
    case AbstractCC::Freestanding:
    case AbstractCC::Method:
    case AbstractCC::WitnessMethod: {
      auto schema = IGM.getSchema(param.getSILType());
      schema.addToArgTypes(IGM, ParamIRTypes);
      return;
    }
    }
    llvm_unreachable("bad abstract CC");
  }
  llvm_unreachable("bad parameter convention");
}

/// Expand the abstract parameters of a SIL function type into the
/// physical parameters of an LLVM function type.
void SignatureExpansion::expandParameters() {
  // Some CCs secretly rearrange the parameters.
  switch (FnType->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod: {
    auto params = FnType->getParameters();

    for (auto param : params) {
      expand(param);
    }

    if (hasPolymorphicParameters(FnType))
      expandPolymorphicSignature(IGM, FnType, ParamIRTypes);
    break;
  }
  case AbstractCC::ObjCMethod:
  case AbstractCC::C:
    llvm_unreachable("Expanding C/ObjC parameters in the wrong place!");
    break;
  }

}

/// Expand the result and parameter types of a SIL function into the
/// phyical parameter types of an LLVM function and return the result
/// type.
llvm::Type *SignatureExpansion::expandSignatureTypes() {
  switch (FnType->getAbstractCC()) {
  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod: {
    llvm::Type *resultType = expandResult();
    expandParameters();
    return resultType;
  }
  case AbstractCC::ObjCMethod:
  case AbstractCC::C:
    return expandExternalSignatureTypes();
    break;
  }
}

Signature FuncSignatureInfo::getSignature(IRGenModule &IGM,
                                          ExtraData extraData) const {
  // Compute a reference to the appropriate signature cache.
  Signature &signature = TheSignatures.select(extraData);

  // If it's already been filled in, we're done.
  if (signature.isValid())
    return signature;

  GenericContextScope scope(IGM, FormalType->getGenericSignature());
  SignatureExpansion expansion(IGM, FormalType);
  
  // Blocks are passed into themselves as their first argument.
  if (FormalType->getRepresentation() == FunctionType::Representation::Block)
    expansion.ParamIRTypes.push_back(IGM.ObjCBlockPtrTy);
  
  llvm::Type *resultType = expansion.expandSignatureTypes();

  // Non-block data arguments are last.
  // See the comment in this file's header comment.
  switch (extraData) {
  case ExtraData::Block:
  case ExtraData::None:
    break;
  case ExtraData::Retainable:
    expansion.ParamIRTypes.push_back(IGM.RefCountedPtrTy);
    break;
  }

  // Create the appropriate LLVM type.
  llvm::FunctionType *llvmType =
    llvm::FunctionType::get(resultType, expansion.ParamIRTypes,
                            /*variadic*/ false);

  // Update the cache and return.
  signature.set(llvmType, expansion.HasIndirectResult, expansion.Attrs);
  return signature;
}

static const FuncSignatureInfo &
getFuncSignatureInfoForLowered(IRGenModule &IGM, CanSILFunctionType type) {
  auto &ti = IGM.getTypeInfoForLowered(type);
  switch (type->getRepresentation()) {
  case AnyFunctionType::Representation::Block:
    return ti.as<BlockTypeInfo>();
  case AnyFunctionType::Representation::Thin:
    return ti.as<ThinFuncTypeInfo>();
  case AnyFunctionType::Representation::Thick:
    return ti.as<FuncTypeInfo>();
  }
  llvm_unreachable("bad function type representation");
}

llvm::FunctionType *
IRGenModule::getFunctionType(CanSILFunctionType type,
                             ExtraData extraData,
                             llvm::AttributeSet &attrs) {
  auto &sigInfo = getFuncSignatureInfoForLowered(*this, type);
  Signature sig = sigInfo.getSignature(*this, extraData);
  attrs = sig.getAttributes();
  return sig.getType();
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

static void extractScalarResults(IRGenFunction &IGF, llvm::Type *bodyType,
                                  llvm::Value *call, Explosion &out) {
  assert(!bodyType->isVoidTy() && "Unexpected void result type!");

  auto *returned = call;
  auto *callType = call->getType();

  // If the type of the result of the call differs from the type used
  // elsewhere in the caller due to ABI type coercion, we need to
  // coerce the result back from the ABI type before extracting the
  // elements.
  if (bodyType != callType)
    returned = IGF.coerceValue(returned, bodyType, IGF.IGM.DataLayout);

  if (llvm::StructType *structType = dyn_cast<llvm::StructType>(bodyType))
    for (unsigned i = 0, e = structType->getNumElements(); i != e; ++i)
      out.add(IGF.Builder.CreateExtractValue(returned, i));
  else
    out.add(returned);
}

static void emitCastBuiltin(IRGenFunction &IGF, SILType destType,
                            Explosion &result,
                            Explosion &args,
                            llvm::Instruction::CastOps opcode) {
  llvm::Value *input = args.claimNext();
  assert(args.empty() && "wrong operands to cast operation");

  llvm::Type *destTy = IGF.IGM.getStorageType(destType);
  llvm::Value *output = IGF.Builder.CreateCast(opcode, input, destTy);
  result.add(output);
}

static void emitCastOrBitCastBuiltin(IRGenFunction &IGF,
                                     SILType destType,
                                     Explosion &result,
                                     Explosion &args,
                                     BuiltinValueKind BV) {
  llvm::Value *input = args.claimNext();
  assert(args.empty() && "wrong operands to cast operation");

  llvm::Type *destTy = IGF.IGM.getStorageType(destType);
  llvm::Value *output;
  switch (BV) {
  default: llvm_unreachable("Not a cast-or-bitcast operation");
  case BuiltinValueKind::TruncOrBitCast:
    output = IGF.Builder.CreateTruncOrBitCast(input, destTy); break;
  case BuiltinValueKind::ZExtOrBitCast:
    output = IGF.Builder.CreateZExtOrBitCast(input, destTy); break;
  case BuiltinValueKind::SExtOrBitCast:
    output = IGF.Builder.CreateSExtOrBitCast(input, destTy); break;
  }
  result.add(output);
}

static void emitCompareBuiltin(IRGenFunction &IGF, Explosion &result,
                               Explosion &args, llvm::CmpInst::Predicate pred) {
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

static void emitTypeTraitBuiltin(IRGenFunction &IGF,
                                 Explosion &out,
                                 Explosion &args,
                                 ArrayRef<Substitution> substitutions,
                                 TypeTraitResult (TypeBase::*trait)()) {
  assert(substitutions.size() == 1
         && "type trait should have gotten single type parameter");
  args.claimNext();
  
  // Lower away the trait to a tristate 0 = no, 1 = yes, 2 = maybe.
  unsigned result;
  switch ((substitutions[0].getReplacement().getPointer()->*trait)()) {
  case TypeTraitResult::IsNot:
    result = 0;
    break;
  case TypeTraitResult::Is:
    result = 1;
    break;
  case TypeTraitResult::CanBe:
    result = 2;
    break;
  }

  out.add(llvm::ConstantInt::get(IGF.IGM.Int8Ty, result));
}

static std::pair<SILType, const TypeInfo &>
getLoweredTypeAndTypeInfo(IRGenModule &IGM, Type unloweredType) {
  auto lowered = IGM.SILMod->Types.getLoweredType(
                                            unloweredType->getCanonicalType());
  return {lowered, IGM.getTypeInfo(lowered)};
}

/// emitBuiltinCall - Emit a call to a builtin function.
void irgen::emitBuiltinCall(IRGenFunction &IGF, Identifier FnId,
                            SILType resultType,
                            Explosion &args, Explosion &out,
                            ArrayRef<Substitution> substitutions) {
  // Decompose the function's name into a builtin name and type list.
  const BuiltinInfo &Builtin = IGF.IGM.SILMod->getBuiltinInfo(FnId);

  // These builtins don't care about their argument:
  if (Builtin.ID == BuiltinValueKind::Sizeof) {
    args.claimAll();
    auto valueTy = getLoweredTypeAndTypeInfo(IGF.IGM,
                                             substitutions[0].getReplacement());
    out.add(valueTy.second.getSize(IGF, valueTy.first));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::Strideof) {
    args.claimAll();
    auto valueTy = getLoweredTypeAndTypeInfo(IGF.IGM,
                                             substitutions[0].getReplacement());
    out.add(valueTy.second.getStride(IGF, valueTy.first));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::Alignof) {
    args.claimAll();
    auto valueTy = getLoweredTypeAndTypeInfo(IGF.IGM,
                                             substitutions[0].getReplacement());
    // The alignof value is one greater than the alignment mask.
    out.add(IGF.Builder.CreateAdd(
                           valueTy.second.getAlignmentMask(IGF, valueTy.first),
                           IGF.IGM.getSize(Size(1))));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::StrideofNonZero) {
    // Note this case must never return 0.
    // It is implemented as max(strideof, 1)
    args.claimAll();
    auto valueTy = getLoweredTypeAndTypeInfo(IGF.IGM,
                                             substitutions[0].getReplacement());
    // Strideof should never return 0, so return 1 if the type has a 0 stride.
    llvm::Value *StrideOf = valueTy.second.getStride(IGF, valueTy.first);
    llvm::IntegerType *IntTy = cast<llvm::IntegerType>(StrideOf->getType());
    auto *Zero = llvm::ConstantInt::get(IntTy, 0);
    auto *One = llvm::ConstantInt::get(IntTy, 1);
    llvm::Value *Cmp = IGF.Builder.CreateICmpEQ(StrideOf, Zero);
    out.add(IGF.Builder.CreateSelect(Cmp, One, StrideOf));
    return;
  }

  // addressof expects an lvalue argument.
  if (Builtin.ID == BuiltinValueKind::AddressOf) {
    llvm::Value *address = args.claimNext();
    llvm::Value *value = IGF.Builder.CreateBitCast(address,
                                                   IGF.IGM.Int8PtrTy);
    out.add(value);
    return;
  }

  // Everything else cares about the (rvalue) argument.

  // If this is an LLVM IR intrinsic, lower it to an intrinsic call.
  const IntrinsicInfo &IInfo = IGF.IGM.SILMod->getIntrinsicInfo(FnId);
  llvm::Intrinsic::ID IID = IInfo.ID;
  if (IID != llvm::Intrinsic::not_intrinsic) {
    SmallVector<llvm::Type*, 4> ArgTys;
    for (auto T : IInfo.Types)
      ArgTys.push_back(IGF.IGM.getStorageTypeForLowered(T->getCanonicalType()));
      
    auto F = llvm::Intrinsic::getDeclaration(&IGF.IGM.Module,
                                             (llvm::Intrinsic::ID)IID, ArgTys);
    llvm::FunctionType *FT = F->getFunctionType();
    SmallVector<llvm::Value*, 8> IRArgs;
    for (unsigned i = 0, e = FT->getNumParams(); i != e; ++i)
      IRArgs.push_back(args.claimNext());
    llvm::Value *TheCall = IGF.Builder.CreateCall(F, IRArgs);

    if (!TheCall->getType()->isVoidTy())
      extractScalarResults(IGF, TheCall->getType(), TheCall, out);

    return;
  }

  // TODO: A linear series of ifs is suboptimal.
#define BUILTIN_SIL_OPERATION(id, name, overload) \
  if (Builtin.ID == BuiltinValueKind::id) \
    llvm_unreachable(name " builtin should be lowered away by SILGen!");

#define BUILTIN_CAST_OPERATION(id, name, attrs) \
  if (Builtin.ID == BuiltinValueKind::id) \
    return emitCastBuiltin(IGF, resultType, out, args, \
                           llvm::Instruction::id);

#define BUILTIN_CAST_OR_BITCAST_OPERATION(id, name, attrs) \
  if (Builtin.ID == BuiltinValueKind::id) \
    return emitCastOrBitCastBuiltin(IGF, resultType, out, args, \
                                    BuiltinValueKind::id);
  
#define BUILTIN_BINARY_OPERATION(id, name, attrs, overload) \
  if (Builtin.ID == BuiltinValueKind::id) { \
    llvm::Value *lhs = args.claimNext(); \
    llvm::Value *rhs = args.claimNext(); \
    llvm::Value *v = IGF.Builder.Create##id(lhs, rhs); \
    return out.add(v); \
  }

#define BUILTIN_BINARY_OPERATION_WITH_OVERFLOW(id, name, uncheckedID, attrs, overload) \
if (Builtin.ID == BuiltinValueKind::id) { \
  SmallVector<llvm::Type*, 2> ArgTys; \
  auto opType = Builtin.Types[0]->getCanonicalType(); \
  ArgTys.push_back(IGF.IGM.getStorageTypeForLowered(opType)); \
  auto F = llvm::Intrinsic::getDeclaration(&IGF.IGM.Module, \
    getLLVMIntrinsicIDForBuiltinWithOverflow(Builtin.ID), ArgTys); \
  SmallVector<llvm::Value*, 2> IRArgs; \
  IRArgs.push_back(args.claimNext()); \
  IRArgs.push_back(args.claimNext()); \
  args.claimNext();\
  llvm::Value *TheCall = IGF.Builder.CreateCall(F, IRArgs); \
  extractScalarResults(IGF, TheCall->getType(), TheCall, out);  \
  return; \
}
  // FIXME: We could generate the code to dynamically report the overflow if the
  // thrid argument is true. Now, we just ignore it.

#define BUILTIN_BINARY_PREDICATE(id, name, attrs, overload) \
  if (Builtin.ID == BuiltinValueKind::id) \
    return emitCompareBuiltin(IGF, out, args, llvm::CmpInst::id);
  
#define BUILTIN_TYPE_TRAIT_OPERATION(id, name) \
  if (Builtin.ID == BuiltinValueKind::id) \
    return emitTypeTraitBuiltin(IGF, out, args, substitutions, &TypeBase::name);
  
#define BUILTIN(ID, Name, Attrs)  // Ignore the rest.
#include "swift/AST/Builtins.def"

  if (Builtin.ID == BuiltinValueKind::FNeg) {
    llvm::Value *rhs = args.claimNext();
    llvm::Value *lhs = llvm::ConstantFP::get(rhs->getType(), "-0.0");
    llvm::Value *v = IGF.Builder.CreateFSub(lhs, rhs);
    return out.add(v);
  }
  
  if (Builtin.ID == BuiltinValueKind::AssumeNonNegative) {
    llvm::Value *v = args.claimNext();
    // Set a value range on the load instruction, which must be the argument of
    // the builtin.
    if (isa<llvm::LoadInst>(v) || isa<llvm::CallInst>(v)) {
      // The load must be post-dominated by the builtin. Otherwise we would get
      // a wrong assumption in the else-branch in this example:
      //    x = f()
      //    if condition {
      //      y = assumeNonNegative(x)
      //    } else {
      //      // x might be negative here!
      //    }
      // For simplicity we just enforce that both the load and the builtin must
      // be in the same block.
      llvm::Instruction *I = static_cast<llvm::Instruction *>(v);
      if (I->getParent() == IGF.Builder.GetInsertBlock()) {
        llvm::LLVMContext &ctx = IGF.IGM.Module.getContext();
        llvm::IntegerType *intType = dyn_cast<llvm::IntegerType>(v->getType());
        llvm::Metadata *rangeElems[] = {
          llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(intType, 0)),
          llvm::ConstantAsMetadata::get(
              llvm::ConstantInt::get(intType,
                  APInt::getSignedMaxValue(intType->getBitWidth())))
        };
        llvm::MDNode *range = llvm::MDNode::get(ctx, rangeElems);
        I->setMetadata(llvm::LLVMContext::MD_range, range);
      }
    }
    // Don't generate any code for the builtin.
    return out.add(v);
  }
  
  if (Builtin.ID == BuiltinValueKind::AllocRaw) {
    auto size = args.claimNext();
    auto align = args.claimNext();
    // Translate the alignment to a mask.
    auto alignMask = IGF.Builder.CreateSub(align, IGF.IGM.getSize(Size(1)));
    auto alloc = IGF.emitAllocRawCall(size, alignMask, "builtin-allocRaw");
    out.add(alloc);
    return;
  }

  if (Builtin.ID == BuiltinValueKind::DeallocRaw) {
    auto pointer = args.claimNext();
    auto size = args.claimNext();
    auto align = args.claimNext();
    // Translate the alignment to a mask.
    auto alignMask = IGF.Builder.CreateSub(align, IGF.IGM.getSize(Size(1)));
    IGF.emitDeallocRawCall(pointer, size, alignMask);
    return;
  }

  if (Builtin.ID == BuiltinValueKind::Fence) {
    SmallVector<Type, 4> Types;
    StringRef BuiltinName =
      getBuiltinBaseName(IGF.IGM.Context, FnId.str(), Types);
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

  
  if (Builtin.ID == BuiltinValueKind::CmpXChg) {
    SmallVector<Type, 4> Types;
    StringRef BuiltinName =
      getBuiltinBaseName(IGF.IGM.Context, FnId.str(), Types);
    BuiltinName = BuiltinName.drop_front(strlen("cmpxchg_"));

    // Decode the success- and failure-ordering arguments, which are required.
    SmallVector<StringRef, 4> Parts;
    BuiltinName.split(Parts, "_");
    assert(Parts.size() >= 2 && "Mismatch with sema");
    auto successOrdering = decodeLLVMAtomicOrdering(Parts[0]);
    auto failureOrdering = decodeLLVMAtomicOrdering(Parts[1]);
    auto NextPart = Parts.begin() + 2;

    // Accept weak, volatile, and singlethread if present.
    bool isWeak = false, isVolatile = false, isSingleThread = false;
    if (NextPart != Parts.end() && *NextPart == "weak") {
      isWeak = true;
      NextPart++;
    }
    if (NextPart != Parts.end() && *NextPart == "volatile") {
      isVolatile = true;
      NextPart++;
    }
    if (NextPart != Parts.end() && *NextPart == "singlethread") {
      isSingleThread = true;
      NextPart++;
    }
    assert(NextPart == Parts.end() && "Mismatch with sema");

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
                                                         successOrdering,
                                                         failureOrdering,
                                                         isSingleThread ? llvm::SingleThread : llvm::CrossThread);
    cast<llvm::AtomicCmpXchgInst>(value)->setVolatile(isVolatile);
    cast<llvm::AtomicCmpXchgInst>(value)->setWeak(isWeak);

    auto valueLoaded = IGF.Builder.CreateExtractValue(value, {0});
    auto loadSuccessful = IGF.Builder.CreateExtractValue(value, {1});

    if (origTy->isPointerTy())
      valueLoaded = IGF.Builder.CreateIntToPtr(valueLoaded, origTy);

    out.add(valueLoaded);
    out.add(loadSuccessful);

    return;
  }
  
  if (Builtin.ID == BuiltinValueKind::AtomicRMW) {
    using namespace llvm;

    SmallVector<Type, 4> Types;
    StringRef BuiltinName = getBuiltinBaseName(IGF.IGM.Context,
                                               FnId.str(), Types);
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

    out.add(value);
    return;
  }

  if (Builtin.ID == BuiltinValueKind::ExtractElement) {
    using namespace llvm;

    auto vector = args.claimNext();
    auto index = args.claimNext();
    out.add(IGF.Builder.CreateExtractElement(vector, index));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::InsertElement) {
    using namespace llvm;

    auto vector = args.claimNext();
    auto newValue = args.claimNext();
    auto index = args.claimNext();
    out.add(IGF.Builder.CreateInsertElement(vector, newValue, index));
    return;
  }

  if (Builtin.ID == BuiltinValueKind::SToSCheckedTrunc ||
      Builtin.ID == BuiltinValueKind::UToUCheckedTrunc ||
      Builtin.ID == BuiltinValueKind::SToUCheckedTrunc) {
    auto FromTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[0]->getCanonicalType());
    auto ToTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[1]->getCanonicalType());

    // Compute the result for SToSCheckedTrunc_IntFrom_IntTo(Arg):
    //   Res = trunc_IntTo(Arg)
    //   Ext = sext_IntFrom(Res)
    //   OverflowFlag = (Arg == Ext) ? 0 : 1
    //   return (resultVal, OverflowFlag)
    //
    // Compute the result for UToUCheckedTrunc_IntFrom_IntTo(Arg)
    // and SToUCheckedTrunc_IntFrom_IntTo(Arg):
    //   Res = trunc_IntTo(Arg)
    //   Ext = zext_IntFrom(Res)
    //   OverflowFlag = (Arg == Ext) ? 0 : 1
    //   return (Res, OverflowFlag)
    llvm::Value *Arg = args.claimNext();
    llvm::Value *Res = IGF.Builder.CreateTrunc(Arg, ToTy);
    bool Signed = (Builtin.ID == BuiltinValueKind::SToSCheckedTrunc);
    llvm::Value *Ext = Signed ? IGF.Builder.CreateSExt(Res, FromTy) :
                                IGF.Builder.CreateZExt(Res, FromTy);
    llvm::Value *OverflowCond = IGF.Builder.CreateICmpEQ(Arg, Ext);
    llvm::Value *OverflowFlag = IGF.Builder.CreateSelect(OverflowCond,
                                  llvm::ConstantInt::get(IGF.IGM.Int1Ty, 0),
                                  llvm::ConstantInt::get(IGF.IGM.Int1Ty, 1));
    // Return the tuple - the result + the overflow flag.
    out.add(Res);
    return out.add(OverflowFlag);
  }

  if (Builtin.ID == BuiltinValueKind::UToSCheckedTrunc) {
    auto FromTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[0]->getCanonicalType());
    auto ToTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[1]->getCanonicalType());
    llvm::Type *ToMinusOneTy =
      llvm::Type::getIntNTy(ToTy->getContext(), ToTy->getIntegerBitWidth() - 1);

    // Compute the result for UToSCheckedTrunc_IntFrom_IntTo(Arg):
    //   Res = trunc_IntTo(Arg)
    //   Trunc = trunc_'IntTo-1bit'(Arg)
    //   Ext = zext_IntFrom(Trunc)
    //   OverflowFlag = (Arg == Ext) ? 0 : 1
    //   return (Res, OverflowFlag)
    llvm::Value *Arg = args.claimNext();
    llvm::Value *Res = IGF.Builder.CreateTrunc(Arg, ToTy);
    llvm::Value *Trunc = IGF.Builder.CreateTrunc(Arg, ToMinusOneTy);
    llvm::Value *Ext = IGF.Builder.CreateZExt(Trunc, FromTy);
    llvm::Value *OverflowCond = IGF.Builder.CreateICmpEQ(Arg, Ext);
    llvm::Value *OverflowFlag = IGF.Builder.CreateSelect(OverflowCond,
                                  llvm::ConstantInt::get(IGF.IGM.Int1Ty, 0),
                                  llvm::ConstantInt::get(IGF.IGM.Int1Ty, 1));
    // Return the tuple: (the result, the overflow flag).
    out.add(Res);
    return out.add(OverflowFlag);
  }

  if (Builtin.ID == BuiltinValueKind::SUCheckedConversion ||
      Builtin.ID == BuiltinValueKind::USCheckedConversion) {
    auto Ty =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[0]->getCanonicalType());

    // Report a sign error if the input parameter is a negative number, when
    // interpreted as signed.
    llvm::Value *Arg = args.claimNext();
    llvm::Value *Zero = llvm::ConstantInt::get(Ty, 0);
    llvm::Value *OverflowFlag = IGF.Builder.CreateICmpSLT(Arg, Zero);

    // Return the tuple: (the result (same as input), the overflow flag).
    out.add(Arg);
    return out.add(OverflowFlag);
  }

  // We are currently emiting code for '_convertFromBuiltinIntegerLiteral',
  // which will call the builtin and pass it a non-compile-time-const parameter.
  if (Builtin.ID == BuiltinValueKind::IntToFPWithOverflow) {
    auto TruncTy = IGF.IGM.Int32Ty;
    auto ToTy =
      IGF.IGM.getStorageTypeForLowered(Builtin.Types[1]->getCanonicalType());
    llvm::Value *Arg = args.claimNext();
    llvm::Value *Truncated = IGF.Builder.CreateTrunc(Arg, TruncTy);
    llvm::Value *V = IGF.Builder.CreateSIToFP(Truncated, ToTy);
    return out.add(V);
  }

  if (Builtin.ID == BuiltinValueKind::Once) {
    // The input type is statically (Builtin.RawPointer, () -> ()).
    llvm::Value *Pred = args.claimNext();
    // Cast the predicate to a OnceTy pointer.
    Pred = IGF.Builder.CreateBitCast(Pred, IGF.IGM.OnceTy->getPointerTo());
    llvm::Value *FnCode = args.claimNext();
    llvm::Value *FnContext = args.claimNext();
    
    auto call
      = IGF.Builder.CreateCall3(IGF.IGM.getOnceFn(), Pred, FnCode, FnContext);
    call->setCallingConv(IGF.IGM.RuntimeCC);
    // No return value.
    return;
  }

  if (Builtin.ID == BuiltinValueKind::AssertConf) {
    // Replace the call to assert_configuration by the Debug configuration
    // value.
    // TODO: assert(IGF.IGM.getOptions().AssertConfig ==
    //              SILOptions::DisableReplacement);
    // Make sure this only happens in a mode where we build a library dylib.

    llvm::Value *DebugAssert = IGF.Builder.getInt32(SILOptions::Debug);
    out.add(DebugAssert);
    return;
  }
  
  if (Builtin.ID == BuiltinValueKind::DestroyArray) {
    // The input type is (T.Type, Builtin.RawPointer, Builtin.Word).
    /* metatype (which may be thin) */
    if (args.size() == 3)
      args.claimNext();
    llvm::Value *ptr = args.claimNext();
    llvm::Value *count = args.claimNext();
    
    auto valueTy = getLoweredTypeAndTypeInfo(IGF.IGM,
                                             substitutions[0].getReplacement());
    
    ptr = IGF.Builder.CreateBitCast(ptr,
                              valueTy.second.getStorageType()->getPointerTo());
    Address array = valueTy.second.getAddressForPointer(ptr);
    valueTy.second.destroyArray(IGF, array, count, valueTy.first);
    return;
  }
  
  if (Builtin.ID == BuiltinValueKind::CopyArray
      || Builtin.ID == BuiltinValueKind::TakeArrayFrontToBack
      || Builtin.ID == BuiltinValueKind::TakeArrayBackToFront) {
    // The input type is (T.Type, Builtin.RawPointer, Builtin.RawPointer, Builtin.Word).
    /* metatype (which may be thin) */
    if (args.size() == 4)
      args.claimNext();
    llvm::Value *dest = args.claimNext();
    llvm::Value *src = args.claimNext();
    llvm::Value *count = args.claimNext();
    
    auto valueTy = getLoweredTypeAndTypeInfo(IGF.IGM,
                                             substitutions[0].getReplacement());
    
    dest = IGF.Builder.CreateBitCast(dest,
                               valueTy.second.getStorageType()->getPointerTo());
    src = IGF.Builder.CreateBitCast(src,
                               valueTy.second.getStorageType()->getPointerTo());
    Address destArray = valueTy.second.getAddressForPointer(dest);
    Address srcArray = valueTy.second.getAddressForPointer(src);
    
    switch (Builtin.ID) {
    case BuiltinValueKind::CopyArray:
      valueTy.second.initializeArrayWithCopy(IGF, destArray, srcArray, count,
                                             valueTy.first);
      break;
    case BuiltinValueKind::TakeArrayFrontToBack:
      valueTy.second.initializeArrayWithTakeFrontToBack(IGF, destArray, srcArray,
                                                        count, valueTy.first);
      break;
    case BuiltinValueKind::TakeArrayBackToFront:
      valueTy.second.initializeArrayWithTakeBackToFront(IGF, destArray, srcArray,
                                                        count, valueTy.first);
      break;
    default:
      llvm_unreachable("out of sync with if condition");
    }    
    return;
  }
  
  if (Builtin.ID == BuiltinValueKind::CondUnreachable) {
    // conditionallyUnreachable is a no-op by itself. Since it's noreturn, there
    // should be a true unreachable terminator right after.
    return;
  }
  
  if (Builtin.ID == BuiltinValueKind::ZeroInitializer) {
    // Build a zero initializer of the result type.
    auto valueTy = getLoweredTypeAndTypeInfo(IGF.IGM,
                                             substitutions[0].getReplacement());
    auto schema = valueTy.second.getSchema();
    for (auto &elt : schema) {
      out.add(llvm::Constant::getNullValue(elt.getScalarType()));
    }
    return;
  }
  
  llvm_unreachable("IRGen unimplemented for this builtin!");
}


/// Emit the unsubstituted result of this call into the given explosion.
/// The unsubstituted result must be naturally returned directly.
void CallEmission::emitToUnmappedExplosion(Explosion &out) {
  assert(LastArgWritten == 0 && "emitting unnaturally to explosion");

  auto call = emitCallSite(false);

  // Bail out immediately on a void result.
  llvm::Value *result = call.getInstruction();
  if (result->getType()->isVoidTy()) return;

  // Get the natural IR type in the body of the function that makes
  // the call. This may be different than the IR type returned by the
  // call itself due to ABI type coercion.
  auto resultType = getCallee().getOrigFunctionType()->getSILResult();
  auto &resultTI = IGF.IGM.getTypeInfo(resultType);
  auto schema = resultTI.getSchema();
  auto *bodyType = schema.getScalarResultType(IGF.IGM);

  // Extract out the scalar results.
  extractScalarResults(IGF, bodyType, result, out);
}

/// Emit the unsubstituted result of this call to the given address.
/// The unsubstituted result must be naturally returned indirectly.
void CallEmission::emitToUnmappedMemory(Address result) {
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
  assert(LastArgWritten == 0);
  assert(!EmittedCall);
  EmittedCall = true;

  // Determine the calling convention.
  // FIXME: collect attributes in the CallEmission.
  auto cc = expandAbstractCC(IGF.IGM, getCallee().getAbstractCC());

  // Make the call and clear the arguments array.
  auto fnPtr = getCallee().getFunctionPointer();
  auto fnPtrTy = cast<llvm::PointerType>(fnPtr->getType());
  auto fnTy = cast<llvm::FunctionType>(fnPtrTy->getElementType());

  // Coerce argument types for those cases where the IR type required
  // by the ABI differs from the type used within the function body.
  assert(fnTy->getNumParams() == Args.size());
  for (int i = 0, e = fnTy->getNumParams(); i != e; ++i) {
    auto *paramTy = fnTy->getParamType(i);
    auto *argTy = Args[i]->getType();
    if (paramTy != argTy)
      Args[i] = IGF.coerceValue(Args[i], paramTy, IGF.IGM.DataLayout);
  }

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
  assert(LastArgWritten <= 1);

  // If the call is naturally to an explosion, emit it that way and
  // then initialize the temporary.
  if (LastArgWritten == 0) {
    Explosion result;
    emitToExplosion(result);
    cast<LoadableTypeInfo>(substResultTI).initialize(IGF, result, addr);
    return;
  }

  // Okay, we're naturally emitting to memory.
  Address origAddr = addr;

  auto origFnType = CurCallee.getOrigFunctionType();
  auto substFnType = CurCallee.getSubstFunctionType();
  assert(origFnType->hasIndirectResult() == substFnType->hasIndirectResult());

  CanType origResultType, substResultType;
  if (origFnType->hasIndirectResult()) {
    origResultType = origFnType->getIndirectResult().getType();
    substResultType = substFnType->getIndirectResult().getType();
  } else {
    origResultType = origFnType->getResult().getType();
    substResultType = substFnType->getResult().getType();
  }

  // Figure out how the substituted result differs from the original.
  auto resultDiff =
    computeResultDifference(IGF.IGM, origResultType, substResultType);
  switch (resultDiff) {

  // For aliasable types, just bitcast the output address.
  case ResultDifference::Aliasable: {
    auto origTy = IGF.IGM.getStoragePointerTypeForLowered(origResultType);
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
  assert(LastArgWritten <= 1);

  SILType substResultType =
    getCallee().getSubstFunctionType()->getSemanticResultSILType();

  auto &substResultTI =
    cast<LoadableTypeInfo>(IGF.getTypeInfo(substResultType));

  // If the call is naturally to memory, emit it that way and then
  // explode that temporary.
  if (LastArgWritten == 1) {
    // FIXME: we might still need to handle abstraction difference here?

    ContainedAddress ctemp = substResultTI.allocateStack(IGF, substResultType,
                                                         "call.aggresult");
    Address temp = ctemp.getAddress();
    emitToMemory(temp, substResultTI);
 
    // We can use a take.
    substResultTI.loadAsTake(IGF, temp, out);

    substResultTI.deallocateStack(IGF, ctemp.getContainer(), substResultType);
    return;
  }

  CanType origResultType =
    getCallee().getOrigFunctionType()->getResult().getType();
  if (origResultType->isDependentType())
    origResultType = IGF.IGM.getContextArchetypes()
      .substDependentType(origResultType)
      ->getCanonicalType();

  // Okay, we're naturally emitting to an explosion.
  // Figure out how the substituted result differs from the original.
  auto resultDiff = computeResultDifference(IGF.IGM, origResultType,
                                          substResultType.getSwiftRValueType());

  switch (resultDiff) {
  // If they don't differ at all, we're good. 
  case ResultDifference::Identical:
    emitToUnmappedExplosion(out);
    return;

  case ResultDifference::Aliasable: {
    Explosion temp;
    emitToUnmappedExplosion(temp);
    ExplosionSchema resultSchema = substResultTI.getSchema();
    assert(temp.size() == resultSchema.size());
    for (unsigned i = 0, e = temp.size(); i != e; ++i) {
      llvm::Type *expectedType = resultSchema.begin()[i].getScalarType();
      llvm::Value *value = temp.claimNext();
      if (value->getType() != expectedType)
        value = IGF.Builder.CreateBitCast(value, expectedType,
                                          value->getName() + ".asSubstituted");
      out.add(value);
    }
    return;
  }

  // If they do differ, we need to remap.
  case ResultDifference::Divergent:
    if (substResultType.is<MetatypeType>() &&
        isa<MetatypeType>(origResultType)) {
      // If we got here, it's because the substituted metatype is trivial.
      // Remapping is easy--the substituted type is empty, so we drop the
      // nontrivial representation of the original type.
      assert(substResultType.castTo<MetatypeType>()->getRepresentation()
               == MetatypeRepresentation::Thin
             && "remapping to non-thin metatype?!");
      
      Explosion temp;
      emitToUnmappedExplosion(temp);
      temp.claimAll();
      return;
    }
      
    if (auto origArchetype = dyn_cast<ArchetypeType>(origResultType)) {
      if (origArchetype->requiresClass()) {
        // Remap a class archetype to an instance.
        assert(substResultType.hasReferenceSemantics() &&
               "remapping class archetype to non-class?!");
        auto schema = substResultTI.getSchema();
        assert(schema.size() == 1 && schema.begin()->isScalar()
               && "remapping class archetype to non-single-scalar");
        Explosion temp;
        emitToUnmappedExplosion(temp);
        llvm::Value *pointer = temp.claimNext();
        pointer = IGF.Builder.CreateBitCast(pointer,
                                            schema.begin()->getScalarType());
        out.add(pointer);
        return;
      }
    }
      
    // There's a related FIXME in the Builtin.load/move code.
    IGF.unimplemented(SourceLoc(), "remapping explosion");
    IGF.emitFakeExplosion(substResultTI, out);
    return;
  }
    
  llvm_unreachable("bad difference kind");
}

CallEmission::CallEmission(CallEmission &&other)
  : IGF(other.IGF),
    Args(std::move(other.Args)),
    CurCallee(std::move(other.CurCallee)),
    LastArgWritten(other.LastArgWritten),
    EmittedCall(other.EmittedCall) {
  // Prevent other's destructor from asserting.
  other.invalidate();
}

CallEmission::~CallEmission() {
  assert(LastArgWritten == 0);
  assert(EmittedCall);
}

void CallEmission::invalidate() {
  LastArgWritten = 0;
  EmittedCall = true;
}


/// Set up this emitter afresh from the current callee specs.
void CallEmission::setFromCallee() {
  EmittedCall = false;

  unsigned numArgs = CurCallee.getLLVMFunctionType()->getNumParams();

  // Set up the args array.
  assert(Args.empty());
  Args.reserve(numArgs);
  Args.set_size(numArgs);
  LastArgWritten = numArgs;

  // Add the data pointer if we have one.
  // For blocks we emit this after all the arguments have been applied.
  if (CurCallee.getOrigFunctionType()->getRepresentation()
        != FunctionType::Representation::Block
      && CurCallee.hasDataPointer()) {
    assert(LastArgWritten > 0);
    Args[--LastArgWritten] = CurCallee.getDataPointer(IGF);
  }
}

/// Does an ObjC method or C function returning the given type require an
/// sret indirect result?
llvm::PointerType *
irgen::requiresExternalIndirectResult(IRGenModule &IGM,
                                      CanSILFunctionType fnType) {
  if (fnType->hasIndirectResult()) {
    return IGM.getStoragePointerType(
                             fnType->getIndirectResult().getSILType());
  }

  auto resultTy = fnType->getResult().getSILType();
  auto clangTy = IGM.getClangType(resultTy);
  assert(clangTy && "Unexpected failure in Clang type generation!");

  SmallVector<clang::CanQualType,1> args;
  auto extInfo = clang::FunctionType::ExtInfo();
  auto &FI = IGM.ABITypes->arrangeFreeFunctionCall(clangTy, args, extInfo,
                                             clang::CodeGen::RequiredArgs::All);

  auto &returnInfo = FI.getReturnInfo();
  if (!returnInfo.isIndirect())
    return nullptr;

  auto &ti = IGM.getTypeInfo(resultTy);
  return ti.getStorageType()->getPointerTo();
}

bool irgen::canCoerceToSchema(IRGenModule &IGM,
                              ArrayRef<llvm::Type*> expandedTys,
                              const ExplosionSchema &schema) {
  // If the schemas don't even match in number, we have to go
  // through memory.
  if (expandedTys.size() != schema.size())
    return false;

  // If there's just one element, we can always coerce as a scalar.
  if (expandedTys.size() == 1) return true;

  // If there are multiple elements, the pairs of types need to
  // match in size for the coercion to work.
  for (size_t i = 0, e = expandedTys.size(); i != e; ++i) {
    llvm::Type *inputTy = schema[i].getScalarType();
    llvm::Type *outputTy = expandedTys[i];
    if (inputTy != outputTy &&
        IGM.DataLayout.getTypeSizeInBits(inputTy) !=
        IGM.DataLayout.getTypeSizeInBits(outputTy))
      return false;
  }

  // Okay, everything is fine.
  return true;
}

static void emitDirectExternalArgument(IRGenFunction &IGF,
                                       SILType argType, llvm::Type *toTy,
                                       Explosion &in, Explosion &out) {
  // If we're supposed to pass directly as a struct type, that
  // really means expanding out as multiple arguments.
  ArrayRef<llvm::Type*> expandedTys;
  if (auto expansionTy = dyn_cast<llvm::StructType>(toTy)) {
    // Is there any good reason this isn't public API of llvm::StructType?
    expandedTys = makeArrayRef(expansionTy->element_begin(),
                               expansionTy->getNumElements());
  } else {
    expandedTys = toTy;
  }

  auto &argTI = cast<LoadableTypeInfo>(IGF.getTypeInfo(argType));
  auto inputSchema = argTI.getSchema();

  // Check to see if we can pairwise coerce Swift's exploded scalars
  // to Clang's expanded elements.
  if (canCoerceToSchema(IGF.IGM, expandedTys, inputSchema)) {
    for (auto outputTy : expandedTys) {
      llvm::Value *arg = in.claimNext();
      if (arg->getType() != outputTy)
        arg = IGF.coerceValue(arg, outputTy, IGF.IGM.DataLayout);
      out.add(arg);
    }
    return;
  }

  // Otherwise, we need to coerce through memory.

  // Store to a temporary.
  Address temporary = argTI.allocateStack(IGF, argType,
                                          "coerced-arg").getAddress();
  argTI.initializeFromParams(IGF, in, temporary, argType);

  // Bitcast the temporary to the expected type.
  Address coercedAddr =
    IGF.Builder.CreateBitCast(temporary, toTy->getPointerTo());

  // Project out individual elements if necessary.
  if (auto expansionTy = dyn_cast<llvm::StructType>(toTy)) {
    auto layout = IGF.IGM.DataLayout.getStructLayout(expansionTy);
    for (unsigned i = 0, e = expansionTy->getNumElements(); i != e; ++i) {
      auto fieldOffset = Size(layout->getElementOffset(i));
      auto fieldAddr = IGF.Builder.CreateStructGEP(coercedAddr, i, fieldOffset);
      out.add(IGF.Builder.CreateLoad(fieldAddr));
    }

  // Otherwise, collect the single scalar.
  } else {
    out.add(IGF.Builder.CreateLoad(coercedAddr));
  }

  argTI.deallocateStack(IGF, temporary, argType);
}

namespace {
  /// Load a clang argument expansion from a buffer.
  struct ClangExpandLoadEmitter :
    ClangExpandProjection<ClangExpandLoadEmitter> {

    Explosion &Out;
    ClangExpandLoadEmitter(IRGenFunction &IGF, Explosion &out)
      : ClangExpandProjection(IGF), Out(out) {}

    void visitScalar(llvm::Type *scalarTy, Address addr) {
      addr = IGF.Builder.CreateBitCast(addr, scalarTy->getPointerTo());
      auto value = IGF.Builder.CreateLoad(addr);
      Out.add(value);
    }
  };

  /// Store a clang argument expansion into a buffer.
  struct ClangExpandStoreEmitter :
    ClangExpandProjection<ClangExpandStoreEmitter> {

    Explosion &In;
    ClangExpandStoreEmitter(IRGenFunction &IGF, Explosion &in)
      : ClangExpandProjection(IGF), In(in) {}

    void visitScalar(llvm::Type *scalarTy, Address addr) {
      auto value = In.claimNext();

      addr = IGF.Builder.CreateBitCast(addr, scalarTy->getPointerTo());
      IGF.Builder.CreateStore(value, addr);
    }
  };
}

/// Given a Swift value explosion in 'in', produce a Clang expansion
/// (according to ABIArgInfo::Expand) in 'out'.
static void emitClangExpandedArgument(IRGenFunction &IGF,
                                      Explosion &in, Explosion &out,
                                      clang::CanQualType clangType,
                                      SILType swiftType,
                                      const LoadableTypeInfo &swiftTI) {
  // If Clang's expansion schema matches Swift's, great.
  auto swiftSchema = swiftTI.getSchema();
  if (doesClangExpansionMatchSchema(IGF.IGM, clangType, swiftSchema)) {
    return in.transferInto(out, swiftSchema.size());
  }

  // Otherwise, materialize to a temporary.
  Address temp = swiftTI.allocateStack(IGF, swiftType,
                                       "clang-expand-arg.temp").getAddress();
  swiftTI.initialize(IGF, in, temp);

  Address castTemp = IGF.Builder.CreateBitCast(temp, IGF.IGM.Int8PtrTy);
  ClangExpandLoadEmitter(IGF, out).visit(clangType, castTemp);
}

/// Given a Clang-expanded (according to ABIArgInfo::Expand) parameter
/// in 'in', produce a Swift value explosion in 'out'.
void irgen::emitClangExpandedParameter(IRGenFunction &IGF,
                                       Explosion &in, Explosion &out,
                                       clang::CanQualType clangType,
                                       SILType swiftType,
                                       const LoadableTypeInfo &swiftTI) {
  // If Clang's expansion schema matches Swift's, great.
  auto swiftSchema = swiftTI.getSchema();
  if (doesClangExpansionMatchSchema(IGF.IGM, clangType, swiftSchema)) {
    return in.transferInto(out, swiftSchema.size());
  }

  // Otherwise, materialize to a temporary.
  Address temp = swiftTI.allocateStack(IGF, swiftType,
                                       "clang-expand-param.temp").getAddress();
  Address castTemp = IGF.Builder.CreateBitCast(temp, IGF.IGM.Int8PtrTy);
  ClangExpandStoreEmitter(IGF, in).visit(clangType, castTemp);

  // Then load out.
  swiftTI.loadAsTake(IGF, temp, out);
}

static void externalizeArguments(IRGenFunction &IGF, const Callee &callee,
                                 Explosion &in, Explosion &out,
                     SmallVectorImpl<std::pair<unsigned, Alignment>> &newByvals,
                                 ArrayRef<SILParameterInfo> &params) {

  SmallVector<clang::CanQualType,4> paramTys;
  auto const &clangCtx = IGF.IGM.getClangASTContext();
  if (callee.getAbstractCC() == AbstractCC::ObjCMethod) {
    // The method will be uncurried to ((ArgsN...), ..., (Args1...),
    // Self). The self arg gets lowered to the first argument, and the
    // implicit _cmd argument goes in between it and the rest of the
    // args.
    // self
    auto &self = params.back();
    auto clangTy = IGF.IGM.getClangType(self.getSILType());
    paramTys.push_back(clangTy);
    paramTys.push_back(clangCtx.VoidPtrTy);
    params = params.slice(0, params.size() - 1);
  }

  for (auto param : params) {
    auto clangTy = IGF.IGM.getClangType(param.getSILType());
    paramTys.push_back(clangTy);
  }

  const auto &resultInfo = callee.getSubstFunctionType()->getResult();
  auto clangResultTy = IGF.IGM.getClangType(resultInfo.getSILType());

  // Generate function info for this set of arguments.
  auto extInfo = clang::FunctionType::ExtInfo();
  auto &FI = IGF.IGM.ABITypes->arrangeFreeFunctionCall(clangResultTy,
                                                       paramTys, extInfo,
                                             clang::CodeGen::RequiredArgs::All);

  assert(FI.arg_size() == paramTys.size() &&
         "Expected one ArgInfo for each parameter type!");

  // The index of the first "physical" parameter from paramTys/FI that
  // corresponds to a logical parameter from params.
  unsigned firstParam = 0;

  // Handle the ObjC prefix.
  if (callee.getAbstractCC() == AbstractCC::ObjCMethod) {
    // The first two parameters are pointers, and we make some
    // simplifying assumptions.
    assert(FI.arg_begin()[0].info.isDirect());
    assert(!FI.arg_begin()[0].info.getPaddingType());
    assert(FI.arg_begin()[1].info.isDirect());
    assert(!FI.arg_begin()[1].info.getPaddingType());

    // We do not have SILParameterInfo for the self and _cmd arguments,
    // but we expect these to be internally consistent in the compiler
    // so we shouldn't need to do any coercion.
    out.add(in.claimNext());
    out.add(in.claimNext());
    firstParam = 2;
  }

  for (auto i : indices(paramTys).slice(firstParam)) {
    auto &AI = FI.arg_begin()[i].info;

    // Add a padding argument if required.
    if (auto *padType = AI.getPaddingType())
      out.add(llvm::UndefValue::get(padType));

    SILType paramType = params[i - firstParam].getSILType();
    switch (AI.getKind()) {
    case clang::CodeGen::ABIArgInfo::Extend:
      // FIXME: Handle extension attribute.
      SWIFT_FALLTHROUGH;
    case clang::CodeGen::ABIArgInfo::Direct:
      emitDirectExternalArgument(IGF, paramType, AI.getCoerceToType(), in, out);
      break;
    case clang::CodeGen::ABIArgInfo::Indirect: {
      auto &ti = cast<LoadableTypeInfo>(IGF.getTypeInfo(paramType));
      Address addr = ti.allocateStack(IGF, paramType,
                                      "indirect-temporary").getAddress();
      ti.initialize(IGF, in, addr);

      if (AI.getIndirectByVal())
        newByvals.push_back({out.size(), addr.getAlignment()});
      out.add(addr.getAddress());
      break;
    }
    case clang::CodeGen::ABIArgInfo::Expand:
      emitClangExpandedArgument(IGF, in, out, paramTys[i], paramType,
                         cast<LoadableTypeInfo>(IGF.getTypeInfo(paramType)));
      break;
    case clang::CodeGen::ABIArgInfo::Ignore:
      break;
    case clang::CodeGen::ABIArgInfo::InAlloca:
      llvm_unreachable("Need to handle InAlloca when externalizing arguments");
      break;
    }
  }
}

/// Add a new set of arguments to the function.
void CallEmission::addArg(Explosion &arg) {
  SmallVector<std::pair<unsigned, Alignment>, 2> newByvals;

  auto origParams = getCallee().getOrigFunctionType()->getParameters();

  // Convert arguments to a representation appropriate to the calling
  // convention.
  switch (getCallee().getAbstractCC()) {
  case AbstractCC::C:
  case AbstractCC::ObjCMethod: {
    Explosion externalized;
    externalizeArguments(IGF, getCallee(), arg, externalized, newByvals,
                         origParams);
    arg = std::move(externalized);
    break;
  }

  case AbstractCC::Freestanding:
  case AbstractCC::Method:
  case AbstractCC::WitnessMethod:
    // Nothing to do.
    break;
  }

  // Add the given number of arguments.
  assert(LastArgWritten >= arg.size());

  size_t targetIndex = LastArgWritten - arg.size();
  assert(targetIndex <= 1);
  LastArgWritten = targetIndex;
  
  // If this is a block, add the block pointer before the written arguments.
  if (CurCallee.getOrigFunctionType()->getRepresentation()
        == FunctionType::Representation::Block) {
    assert(CurCallee.hasDataPointer());
    Args[--LastArgWritten] = CurCallee.getDataPointer(IGF);
  }
  
  // Add byval attributes.
  // FIXME: These should in theory be moved around with the arguments when
  // isLeftToRight, but luckily ObjC methods and C functions should only ever
  // have byvals in the last argument clause.
  // FIXME: these argument indexes are probably nonsense
  for (auto &byval : newByvals)
    addByvalArgumentAttributes(IGF.IGM, Attrs, byval.first+targetIndex,
                               byval.second);

  auto argIterator = Args.begin() + targetIndex;
  for (auto value : arg.claimAll()) {
    *argIterator++ = value;
  }
}

/// Initialize an Explosion with the parameters of the current
/// function.  All of the objects will be added unmanaged.  This is
/// really only useful when writing prologue code.
Explosion IRGenFunction::collectParameters() {
  Explosion params;
  for (auto i = CurFn->arg_begin(), e = CurFn->arg_end(); i != e; ++i)
    params.add(i);
  return params;
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

llvm::Value* IRGenFunction::coerceValue(llvm::Value *value, llvm::Type *toTy,
                                        const llvm::DataLayout &DL)
{
  llvm::Type *fromTy = value->getType();
  assert(fromTy != toTy && "Unexpected same types in type coercion!");
  assert(!fromTy->isVoidTy()
         && "Unexpected void source type in type coercion!");
  assert(!toTy->isVoidTy()
         && "Unexpected void destination type in type coercion!");

  // Use the pointer/pointer and pointer/int casts if we can.
  if (toTy->isPointerTy()) {
    if (fromTy->isPointerTy())
      return Builder.CreateBitCast(value, toTy);
    if (fromTy == IGM.IntPtrTy)
      return Builder.CreateIntToPtr(value, toTy);
  } else if (fromTy->isPointerTy()) {
    if (toTy == IGM.IntPtrTy) {
      return Builder.CreatePtrToInt(value, toTy);
    }
  }

  // Otherwise we need to store, bitcast, and load.
  assert(DL.getTypeSizeInBits(fromTy) >= DL.getTypeSizeInBits(toTy)
         && "Coerced type should not be smaller!");

  auto alignment = std::max(DL.getABITypeAlignment(fromTy),
                            DL.getABITypeAlignment(toTy));

  auto address = createAlloca(fromTy, Alignment(alignment),
                              value->getName() + ".coerced");
  Builder.CreateStore(value, address.getAddress());
  auto *coerced = Builder.CreateBitCast(address.getAddress(),
                                        toTy->getPointerTo());
  return Builder.CreateLoad(coerced);
}

void IRGenFunction::emitScalarReturn(llvm::Type *resultType,
                                     Explosion &result) {
  if (result.size() == 0) {
    Builder.CreateRetVoid();
    return;
  }

  auto *ABIType = CurFn->getReturnType();

  if (result.size() == 1) {
    auto *returned = result.claimNext();
    if (ABIType != returned->getType())
      returned = coerceValue(returned, ABIType, IGM.DataLayout);

    Builder.CreateRet(returned);
    return;
  }

  // Multiple return values are returned as a struct.
  assert(cast<llvm::StructType>(resultType)->getNumElements() == result.size());
  llvm::Value *resultAgg = llvm::UndefValue::get(resultType);
  for (unsigned i = 0, e = result.size(); i != e; ++i) {
    llvm::Value *elt = result.claimNext();
    resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
  }

  if (ABIType != resultType)
    resultAgg = coerceValue(resultAgg, ABIType, IGM.DataLayout);

  Builder.CreateRet(resultAgg);
}

void IRGenFunction::emitScalarReturn(SILType resultType, Explosion &result) {
  if (result.size() == 0) {
    Builder.CreateRetVoid();
    return;
  }

  auto *ABIType = CurFn->getReturnType();

  if (result.size() == 1) {
    auto *returned = result.claimNext();
    if (ABIType != returned->getType())
      returned = coerceValue(returned, ABIType, IGM.DataLayout);

    Builder.CreateRet(returned);
    return;
  }

  auto &resultTI = IGM.getTypeInfo(resultType);
  auto schema = resultTI.getSchema();
  auto *bodyType = schema.getScalarResultType(IGM);

  // Multiple return values are returned as a struct.
  assert(cast<llvm::StructType>(bodyType)->getNumElements() == result.size());
  llvm::Value *resultAgg = llvm::UndefValue::get(bodyType);
  for (unsigned i = 0, e = result.size(); i != e; ++i) {
    llvm::Value *elt = result.claimNext();
    resultAgg = Builder.CreateInsertValue(resultAgg, elt, i);
  }

  if (ABIType != bodyType)
    resultAgg = coerceValue(resultAgg, ABIType, IGM.DataLayout);

  Builder.CreateRet(resultAgg);
}

static void emitApplyArgument(IRGenFunction &IGF,
                              SILParameterInfo origParam,
                              SILParameterInfo substParam,
                              Explosion &in,
                              Explosion &out) {
  bool isSubstituted = (substParam.getSILType() != origParam.getSILType());
  
  // For indirect arguments, we just need to pass a pointer.
  if (origParam.isIndirect()) {
    // This address is of the substituted type.
    auto addr = in.claimNext();
    
    // If a substitution is in play, just bitcast the address.
    if (isSubstituted) {
      auto origType = IGF.IGM.getStoragePointerType(origParam.getSILType());
      addr = IGF.Builder.CreateBitCast(addr, origType);
    }
    
    out.add(addr);
    return;
  }
  
  // Otherwise, it's an explosion, which we may need to translate,
  // both in terms of explosion level and substitution levels.

  // Handle the last unsubstituted case.
  if (!isSubstituted) {
    auto &substArgTI
      = cast<LoadableTypeInfo>(IGF.getTypeInfo(substParam.getSILType()));
    substArgTI.reexplode(IGF, in, out);
    return;
  }
  
  reemitAsUnsubstituted(IGF, origParam.getSILType(),
                        substParam.getSILType(),
                        in, out);
}

/// Emit the forwarding stub function for a partial application.
static llvm::Function *emitPartialApplicationForwarder(IRGenModule &IGM,
                                   llvm::Function *staticFnPtr,
                                   llvm::Type *fnTy,
                                   CanSILFunctionType origType,
                                   CanSILFunctionType substType,
                                   CanSILFunctionType outType,
                                   ArrayRef<Substitution> subs,
                                   HeapLayout const &layout,
                                   ArrayRef<ParameterConvention> conventions) {
  llvm::AttributeSet attrs;
  ExtraData extraData
    = layout.isKnownEmpty() ? ExtraData::None : ExtraData::Retainable;
  llvm::FunctionType *fwdTy = IGM.getFunctionType(outType,
                                                  extraData,
                                                  attrs);
  // Build a name for the thunk. If we're thunking a static function reference,
  // include its symbol name in the thunk name.
  llvm::SmallString<20> thunkName;
  thunkName += "_TPA";
  if (staticFnPtr) {
    thunkName += '_';
    thunkName += staticFnPtr->getName();
  }
  
  // FIXME: Maybe cache the thunk by function and closure types?.
  llvm::Function *fwd =
    llvm::Function::Create(fwdTy, llvm::Function::InternalLinkage,
                           llvm::StringRef(thunkName), &IGM.Module);
  fwd->setAttributes(attrs);

  IRGenFunction subIGF(IGM, fwd);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(subIGF, fwd);
  
  Explosion origParams = subIGF.collectParameters();

  // Create a new explosion for potentially reabstracted parameters.
  Explosion params;

  {
    // Lower the forwarded arguments in the original function's generic context.
    GenericContextScope scope(IGM, origType->getGenericSignature());
    
    // Forward the indirect return value, if we have one.
    auto &resultTI = IGM.getTypeInfo(outType->getResult().getSILType());
    if (resultTI.getSchema().requiresIndirectResult(IGM))
      params.add(origParams.claimNext());
    
    // Reemit the parameters as unsubstituted.
    for (unsigned i = 0; i < outType->getParameters().size(); ++i) {
      emitApplyArgument(subIGF, origType->getParameters()[i],
                        outType->getParameters()[i],
                        origParams, params);
    }
  }

  struct AddressToDeallocate {
    SILType Type;
    const TypeInfo &TI;
    Address Addr;
  };
  SmallVector<AddressToDeallocate, 4> addressesToDeallocate;

  bool dependsOnContextLifetime = false;
  bool consumesContext;
  
  switch (outType->getCalleeConvention()) {
  case ParameterConvention::Direct_Owned:
    consumesContext = true;
    break;
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
    consumesContext = false;
    break;
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_Out:
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Guaranteed:
    llvm_unreachable("indirect callables not supported");
  }

  // If there's a data pointer required, grab it (it's always the
  // last parameter) and load out the extra, previously-curried
  // parameters.
  llvm::Value *rawData = nullptr;
  if (!layout.isKnownEmpty()) {
    // Lower the captured arguments in the original function's generic context.
    GenericContextScope scope(IGM, origType->getGenericSignature());
    
    rawData = origParams.takeLast();
    Address data = layout.emitCastTo(subIGF, rawData);

    unsigned origParamI = outType->getParameters().size();
    assert(layout.getElements().size() == conventions.size()
           && "conventions don't match context layout");
    
    unsigned i = 0;
    
    // Restore type metadata bindings, if we have them.
    if (layout.hasBindings()) {
      auto bindingLayout = layout.getElements()[i];
      // The bindings should be fixed-layout inside the object, so we can
      // pass None here. If they weren't, we'd have a chicken-egg problem.
      auto bindingsAddr = bindingLayout.project(subIGF, data, /*offsets*/ None);
      layout.getBindings().restore(subIGF, bindingsAddr);
      ++i;
    }

    // Calculate non-fixed field offsets.
    HeapNonFixedOffsets offsets(subIGF, layout);

    // Perform the loads.
    for (unsigned size = layout.getElements().size(); i < size; ++i) {
      auto &fieldLayout = layout.getElements()[i];
      auto &fieldTy = layout.getElementTypes()[i];
      auto fieldConvention = conventions[i];
      Address fieldAddr = fieldLayout.project(subIGF, data, offsets);
      auto &fieldTI = fieldLayout.getType();
      
      Explosion param;
      switch (fieldConvention) {
      case ParameterConvention::Indirect_In: {
        // The +1 argument is passed indirectly, so we need to copy into a
        // temporary.
        auto caddr = fieldTI.allocateStack(subIGF, fieldTy, "arg.temp");
        fieldTI.initializeWithCopy(subIGF, caddr.getAddress(), fieldAddr,
                                   fieldTy);
        param.add(caddr.getAddressPointer());
        
        // Remember to deallocate later.
        addressesToDeallocate.push_back(
                  AddressToDeallocate{fieldTy, fieldTI, caddr.getContainer()});

        break;
      }
      case ParameterConvention::Indirect_In_Guaranteed:
        // The argument is +0, so we can use the address of the param in
        // the context directly.
        param.add(fieldAddr.getAddress());
        dependsOnContextLifetime = true;
        break;
      case ParameterConvention::Indirect_Inout:
        // Load the add ress of the inout parameter.
        cast<LoadableTypeInfo>(fieldTI).loadAsCopy(subIGF, fieldAddr, param);
        break;
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Unowned:
        // Load these parameters directly. We can "take" since the parameter is
        // +0 and the context will keep the parameter alive for us. If the type
        // is nontrivial, keep the context alive.
        if (!fieldTI.isPOD(ResilienceScope::Local))
          dependsOnContextLifetime = true;
        cast<LoadableTypeInfo>(fieldTI).loadAsTake(subIGF, fieldAddr, param);
        break;
      case ParameterConvention::Direct_Owned:
        // Copy the value out at +1.
        cast<LoadableTypeInfo>(fieldTI).loadAsCopy(subIGF, fieldAddr, param);
        break;
      case ParameterConvention::Indirect_Out:
        llvm_unreachable("can't partially apply out params");
      }
      
      // Reemit the capture params as unsubstituted.
      if (origParamI < origType->getParameters().size()) {
        emitApplyArgument(subIGF,
                          origType->getParameters()[origParamI],
                          substType->getParameters()[origParamI],
                          param, params);
        ++origParamI;
      } else {
        params.add(param.claimAll());
      }
    }
    
    // If the parameters can live independent of the context, release it now
    // so we can tail call. The safety of this assumes that neither this release
    // nor any of the loads can throw.
    if (consumesContext && !dependsOnContextLifetime)
      subIGF.emitRelease(rawData);
  }
  
  // If we didn't receive a static function, dig the function pointer
  // out of the context.
  llvm::Value *fnPtr;
  if (staticFnPtr) {
    assert(staticFnPtr->getType() == fnTy && "static function type mismatch?!");
    fnPtr = staticFnPtr;
  } else {
    // The dynamic function pointer is packed "last" into the context.
    fnPtr = params.takeLast();
    // It comes out of the context as an i8*. Cast to the function type.
    fnPtr = subIGF.Builder.CreateBitCast(fnPtr, fnTy);
  }
  
  // Emit the polymorphic arguments.
  assert(subs.empty() != hasPolymorphicParameters(origType)
         && "should have substitutions iff original function is generic");
  if (hasPolymorphicParameters(origType))
    emitPolymorphicArguments(subIGF, origType, substType, subs, params);
  
  llvm::CallInst *call = subIGF.Builder.CreateCall(fnPtr, params.claimAll());
  
  // FIXME: Default Swift attributes for indirect calls?
  if (staticFnPtr) {
    call->setAttributes(staticFnPtr->getAttributes());
    call->setCallingConv(staticFnPtr->getCallingConv());
  }
  if (!consumesContext || !dependsOnContextLifetime)
    call->setTailCall();

  // Deallocate everything we allocated above.
  // FIXME: exceptions?
  for (auto &entry : addressesToDeallocate) {
    entry.TI.deallocateStack(subIGF, entry.Addr, entry.Type);
  }
  
  // If the parameters depended on the context, consume the context now.
  if (rawData && consumesContext && dependsOnContextLifetime)
    subIGF.emitRelease(rawData);
  
  // FIXME: Reabstract the result value as substituted.

  if (call->getType()->isVoidTy())
    subIGF.Builder.CreateRetVoid();
  else
    subIGF.Builder.CreateRet(call);
  
  return fwd;
}

/// Emit a partial application thunk for a function pointer applied to a partial
/// set of argument values.
void irgen::emitFunctionPartialApplication(IRGenFunction &IGF,
                                           llvm::Value *fnPtr,
                                           llvm::Value *fnContext,
                                           Explosion &args,
                                           ArrayRef<SILParameterInfo> params,
                                           ArrayRef<Substitution> subs,
                                           CanSILFunctionType origType,
                                           CanSILFunctionType substType,
                                           CanSILFunctionType outType,
                                           Explosion &out) {
  // If we have a single Swift-refcounted context value, we can adopt it
  // directly as our closure context without creating a box and thunk.
  enum HasSingleSwiftRefcountedContext { Maybe, Yes, No }
    hasSingleSwiftRefcountedContext = Maybe;
  Optional<ParameterConvention> singleRefcountedConvention;
  
  SmallVector<const TypeInfo *, 4> argTypeInfos;
  SmallVector<SILType, 4> argValTypes;
  SmallVector<ParameterConvention, 4> argConventions;

  // Reserve space for polymorphic bindings.
  auto bindings = NecessaryBindings::forFunctionInvocations(IGF.IGM,
                                                     origType, substType, subs);
  if (!bindings.empty()) {
    hasSingleSwiftRefcountedContext = No;
    auto bindingsSize = bindings.getBufferSize(IGF.IGM);
    auto &bindingsTI = IGF.IGM.getOpaqueStorageTypeInfo(bindingsSize,
                                                 IGF.IGM.getPointerAlignment());
    argValTypes.push_back(SILType());
    argTypeInfos.push_back(&bindingsTI);
    argConventions.push_back(ParameterConvention::Direct_Unowned);
  }

  // Collect the type infos for the context parameters.
  for (auto param : params) {
    SILType argType = param.getSILType();
    
    argValTypes.push_back(argType);
    argConventions.push_back(param.getConvention());
    
    CanType argLoweringTy;
    switch (param.getConvention()) {
    // Capture value parameters by value, consuming them.
    case ParameterConvention::Direct_Owned:
    case ParameterConvention::Direct_Unowned:
    case ParameterConvention::Direct_Guaranteed:
      argLoweringTy = argType.getSwiftRValueType();
      break;
      
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_In_Guaranteed:
      argLoweringTy = argType.getSwiftRValueType();
      break;
      
    // Capture inout parameters by pointer.
    case ParameterConvention::Indirect_Inout:
      argLoweringTy = argType.getSwiftType();
      break;
      
    case ParameterConvention::Indirect_Out:
      llvm_unreachable("can't partially apply out params");
    }
    
    auto &ti = IGF.getTypeInfoForLowered(argLoweringTy);
    argTypeInfos.push_back(&ti);

    // Update the single-swift-refcounted check, unless we already ruled that
    // out.
    if (hasSingleSwiftRefcountedContext == No)
      continue;
    
    // Empty values don't matter.
    auto schema = ti.getSchema();
    if (schema.size() == 0)
      continue;
    
    // Adding nonempty values when we already have a single refcounted pointer
    // means we don't have a single value anymore.
    if (hasSingleSwiftRefcountedContext == Yes) {
      hasSingleSwiftRefcountedContext = No;
      continue;
    }
      
    if (ti.isSingleSwiftRetainablePointer(ResilienceScope::Local)) {
      hasSingleSwiftRefcountedContext = Yes;
      singleRefcountedConvention = param.getConvention();
    } else {
      hasSingleSwiftRefcountedContext = No;
    }
  }
  
  // Include the context pointer, if any, in the function arguments.
  if (fnContext) {
    args.add(fnContext);
    argValTypes.push_back(SILType::getNativeObjectType(IGF.IGM.Context));
    argConventions.push_back(origType->getCalleeConvention());
    argTypeInfos.push_back(
         &IGF.getTypeInfoForLowered(IGF.IGM.Context.TheNativeObjectType));
    // If this is the only context argument we end up with, we can just share
    // it.
    if (args.size() == 1) {
      hasSingleSwiftRefcountedContext = Yes;
      singleRefcountedConvention = origType->getCalleeConvention();
    }
  }
  
  // If we have a single refcounted pointer context (and no polymorphic args
  // to capture), and the dest ownership semantics match the parameter's,
  // skip building the box and thunk and just take the pointer as
  // context.
  if (args.size() == 1 && hasSingleSwiftRefcountedContext == Yes
      && outType->getCalleeConvention() == *singleRefcountedConvention) {
    fnPtr = IGF.Builder.CreateBitCast(fnPtr, IGF.IGM.Int8PtrTy);
    out.add(fnPtr);
    llvm::Value *ctx = args.claimNext();
    ctx = IGF.Builder.CreateBitCast(ctx, IGF.IGM.RefCountedPtrTy);
    out.add(ctx);
    return;
  }
  
  // If the function pointer is dynamic, include it in the context.
  auto staticFn = dyn_cast<llvm::Function>(fnPtr);
  if (!staticFn) {
    llvm::Value *fnRawPtr = IGF.Builder.CreateBitCast(fnPtr, IGF.IGM.Int8PtrTy);
    args.add(fnRawPtr);
    argValTypes.push_back(SILType::getRawPointerType(IGF.IGM.Context));
    argTypeInfos.push_back(
         &IGF.getTypeInfoForLowered(IGF.IGM.Context.TheRawPointerType));
    argConventions.push_back(ParameterConvention::Direct_Unowned);
  }

  // Store the context arguments on the heap.
  assert(argValTypes.size() == argTypeInfos.size()
         && argTypeInfos.size() == argConventions.size()
         && "argument info lists out of sync");
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, argValTypes, argTypeInfos,
                    /*typeToFill*/ nullptr,
                    std::move(bindings));
  llvm::Value *data;
  if (layout.isKnownEmpty()) {
    data = IGF.IGM.RefCountedNull;
  } else {
    // Allocate a new object.
    HeapNonFixedOffsets offsets(IGF, layout);

    data = IGF.emitUnmanagedAlloc(layout, "closure", &offsets);
    Address dataAddr = layout.emitCastTo(IGF, data);

    
    unsigned i = 0;
    
    // Store necessary bindings, if we have them.
    if (layout.hasBindings()) {
      auto &bindingsLayout = layout.getElements()[i];
      Address bindingsAddr = bindingsLayout.project(IGF, dataAddr, offsets);
      layout.getBindings().save(IGF, bindingsAddr);
      ++i;
    }
    
    // Store the context arguments.
    for (unsigned end = layout.getElements().size(); i < end; ++i) {
      auto &fieldLayout = layout.getElements()[i];
      auto &fieldTy = layout.getElementTypes()[i];
      Address fieldAddr = fieldLayout.project(IGF, dataAddr, offsets);
      switch (argConventions[i]) {
      // Take indirect value arguments out of memory.
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed: {
        auto addr = fieldLayout.getType().getAddressForPointer(args.claimNext());
        fieldLayout.getType().initializeWithTake(IGF, fieldAddr, addr, fieldTy);
        break;
      }
      // Take direct value arguments and inout pointers by value.
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Indirect_Inout:
        cast<LoadableTypeInfo>(fieldLayout.getType())
          .initialize(IGF, args, fieldAddr);
        break;
      case ParameterConvention::Indirect_Out:
        llvm_unreachable("can't capture out params");
      }
    }
  }
  assert(args.empty() && "unused args in partial application?!");
  
  // Create the forwarding stub.
  llvm::AttributeSet attrs;
  auto fnPtrTy = IGF.IGM.getFunctionType(origType,
                                         fnContext ? ExtraData::Retainable
                                                   : ExtraData::None, attrs)
    ->getPointerTo();

  llvm::Function *forwarder = emitPartialApplicationForwarder(IGF.IGM,
                                                              staticFn,
                                                              fnPtrTy,
                                                              origType,
                                                              substType,
                                                              outType,
                                                              subs,
                                                              layout,
                                                              argConventions);
  llvm::Value *forwarderValue = IGF.Builder.CreateBitCast(forwarder,
                                                          IGF.IGM.Int8PtrTy);
  out.add(forwarderValue);
  out.add(data);
}

/// Emit the block copy helper for a block.
static llvm::Function *emitBlockCopyHelper(IRGenModule &IGM,
                                           CanSILBlockStorageType blockTy,
                                           const BlockStorageTypeInfo &blockTL){
  // See if we've produced a block copy helper for this type before.
  // TODO
  
  // Create the helper.
  llvm::Type *args[] = {
    blockTL.getStorageType()->getPointerTo(),
    blockTL.getStorageType()->getPointerTo(),
  };
  auto copyTy = llvm::FunctionType::get(IGM.VoidTy, args, /*vararg*/ false);
  // TODO: Give these predictable mangled names and shared linkage.
  auto func = llvm::Function::Create(copyTy, llvm::GlobalValue::InternalLinkage,
                                     "block_copy_helper",
                                     IGM.getModule());
  IRGenFunction IGF(IGM, func);
  
  // Copy the captures from the source to the destination.
  Explosion params = IGF.collectParameters();
  auto dest = Address(params.claimNext(), blockTL.getFixedAlignment());
  auto src = Address(params.claimNext(), blockTL.getFixedAlignment());
  
  auto destCapture = blockTL.projectCapture(IGF, dest);
  auto srcCapture = blockTL.projectCapture(IGF, src);
  auto &captureTL = IGM.getTypeInfoForLowered(blockTy->getCaptureType());
  captureTL.initializeWithCopy(IGF, destCapture, srcCapture,
                               blockTy->getCaptureAddressType());
  
  IGF.Builder.CreateRetVoid();
  
  return func;
}

/// Emit the block copy helper for a block.
static llvm::Function *emitBlockDisposeHelper(IRGenModule &IGM,
                                           CanSILBlockStorageType blockTy,
                                           const BlockStorageTypeInfo &blockTL){
  // See if we've produced a block destroy helper for this type before.
  // TODO
  
  // Create the helper.
  auto destroyTy = llvm::FunctionType::get(IGM.VoidTy,
                                       blockTL.getStorageType()->getPointerTo(),
                                       /*vararg*/ false);
  // TODO: Give these predictable mangled names and shared linkage.
  auto func = llvm::Function::Create(destroyTy,
                                     llvm::GlobalValue::InternalLinkage,
                                     "block_destroy_helper",
                                     IGM.getModule());
  IRGenFunction IGF(IGM, func);
  
  // Destroy the captures.
  Explosion params = IGF.collectParameters();
  auto storage = Address(params.claimNext(), blockTL.getFixedAlignment());
  auto capture = blockTL.projectCapture(IGF, storage);
  auto &captureTL = IGM.getTypeInfoForLowered(blockTy->getCaptureType());
  captureTL.destroy(IGF, capture, blockTy->getCaptureAddressType());
  IGF.Builder.CreateRetVoid();
  
  return func;
}

/// Emit the block header into a block storage slot.
void irgen::emitBlockHeader(IRGenFunction &IGF,
                            Address storage,
                            CanSILBlockStorageType blockTy,
                            llvm::Function *invokeFunction,
                            CanSILFunctionType invokeTy) {
  auto &storageTL
    = IGF.getTypeInfoForLowered(blockTy).as<BlockStorageTypeInfo>();
  
  Address headerAddr = storageTL.projectBlockHeader(IGF, storage);
  
  //
  // Initialize the "isa" pointer, which is _NSConcreteStackBlock.
  auto NSConcreteStackBlock
    = IGF.IGM.getModule()->getOrInsertGlobal("_NSConcreteStackBlock",
                                             IGF.IGM.ObjCClassStructTy);
  //
  // Set the flags.
  // - HAS_COPY_DISPOSE unless the capture type is POD
  uint32_t flags = 0;
  auto &captureTL
    = IGF.getTypeInfoForLowered(blockTy->getCaptureType());
  bool isPOD = captureTL.isPOD(ResilienceScope::Component);
  if (!isPOD)
    flags |= 1 << 25;
  
  // - HAS_STRET, if the invoke function is sret
  if (requiresExternalIndirectResult(IGF.IGM, invokeTy))
    flags |= 1 << 29;
  
  // - HAS_SIGNATURE
  flags |= 1 << 30;
  
  auto flagsVal = llvm::ConstantInt::get(IGF.IGM.Int32Ty, flags);
  
  //
  // Collect the reserved and invoke pointer fields.
  auto reserved = llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0);
  auto invokeVal = llvm::ConstantExpr::getBitCast(invokeFunction,
                                                  IGF.IGM.FunctionPtrTy);
  
  //
  // Build the block descriptor.
  SmallVector<llvm::Constant*, 5> descriptorFields;
  descriptorFields.push_back(llvm::ConstantInt::get(IGF.IGM.IntPtrTy, 0));
  descriptorFields.push_back(llvm::ConstantInt::get(IGF.IGM.IntPtrTy,
                                         storageTL.getFixedSize().getValue()));
  
  if (!isPOD) {
    // Define the copy and dispose helpers.
    descriptorFields.push_back(emitBlockCopyHelper(IGF.IGM, blockTy, storageTL));
    descriptorFields.push_back(emitBlockDisposeHelper(IGF.IGM, blockTy, storageTL));
  }
  
  //
  // Build the descriptor signature.
  // TODO
  descriptorFields.push_back(getBlockTypeExtendedEncoding(IGF.IGM, invokeTy));
  
  //
  // Create the descriptor.
  auto descriptorInit = llvm::ConstantStruct::getAnon(descriptorFields);
  auto descriptor = new llvm::GlobalVariable(*IGF.IGM.getModule(),
                                             descriptorInit->getType(),
                                             /*constant*/ true,
                                             llvm::GlobalValue::InternalLinkage,
                                             descriptorInit,
                                             "block_descriptor");
  auto descriptorVal = llvm::ConstantExpr::getBitCast(descriptor,
                                                      IGF.IGM.Int8PtrTy);
  
  //
  // Store the block header literal.
  llvm::Constant *blockFields[] = {
    NSConcreteStackBlock,
    flagsVal,
    reserved,
    invokeVal,
    descriptorVal,
  };
  auto blockHeader = llvm::ConstantStruct::get(IGF.IGM.ObjCBlockStructTy,
                                               blockFields);
  IGF.Builder.CreateStore(blockHeader, headerAddr);
}

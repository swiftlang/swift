//===--- GenValueWitness.cpp - IR generation for value witnesses ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for value witnesses in Swift.
//
//  Value witnesses are (predominantly) functions that implement the basic
//  operations for copying and destroying values.
//
//  In the comments throughout this file, three type names are used:
//    'B' is the type of a fixed-size buffer
//    'T' is the type which implements a protocol
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "ConstantBuilder.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "GenEnum.h"
#include "GenOpaque.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "StructLayout.h"
#include "TypeInfo.h"

#include "GenValueWitness.h"

using namespace swift;
using namespace irgen;

const char *irgen::getValueWitnessName(ValueWitness witness) {
  switch (witness) {
#define CASE(NAME) case ValueWitness::NAME: return #NAME;
  CASE(AllocateBuffer)
  CASE(AssignWithCopy)
  CASE(AssignWithTake)
  CASE(DeallocateBuffer)
  CASE(Destroy)
  CASE(DestroyBuffer)
  CASE(DestroyArray)
  CASE(InitializeBufferWithCopyOfBuffer)
  CASE(InitializeBufferWithCopy)
  CASE(InitializeWithCopy)
  CASE(InitializeBufferWithTake)
  CASE(InitializeWithTake)
  CASE(ProjectBuffer)
  CASE(InitializeBufferWithTakeOfBuffer)
  CASE(InitializeArrayWithCopy)
  CASE(InitializeArrayWithTakeFrontToBack)
  CASE(InitializeArrayWithTakeBackToFront)
  CASE(StoreExtraInhabitant)
  CASE(GetExtraInhabitantIndex)
  CASE(GetEnumTag)
  CASE(DestructiveProjectEnumData)
  CASE(DestructiveInjectEnumTag)
  CASE(Size)
  CASE(Flags)
  CASE(Stride)
  CASE(ExtraInhabitantFlags)
#undef CASE
  }
  llvm_unreachable("bad value witness kind");
}

static bool isNeverAllocated(FixedPacking packing) {
  switch (packing) {
  case FixedPacking::OffsetZero: return true;
  case FixedPacking::Allocate: return false;
  case FixedPacking::Dynamic: return false;
  }
  llvm_unreachable("bad FixedPacking value");
}

namespace {
  /// An operation to be performed for various kinds of packing.
  struct DynamicPackingOperation {
    virtual ~DynamicPackingOperation() = default;

    /// Emit the operation at a concrete packing kind.
    ///
    /// Immediately after this call, there will be an unconditional
    /// branch to the continuation block.
    virtual void emitForPacking(IRGenFunction &IGF,
                                SILType T,
                                const TypeInfo &type,
                                FixedPacking packing) = 0;

    /// Given that we are currently at the beginning of the
    /// continuation block, complete the operation.
    virtual void complete(IRGenFunction &IGF) = 0;
  };

  /// A class for merging a particular kind of value across control flow.
  template <class T> class DynamicPackingPHIMapping;

  /// An implementation of DynamicPackingPHIMapping for a single LLVM value.
  template <> class DynamicPackingPHIMapping<llvm::Value*> {
    llvm::PHINode *PHI = nullptr;
  public:
    void collect(IRGenFunction &IGF, llvm::Value *value) {
      // Add the result to the phi, creating it (unparented) if necessary.
      if (!PHI) PHI = llvm::PHINode::Create(value->getType(), 2,
                                            "dynamic-packing.result");
      PHI->addIncoming(value, IGF.Builder.GetInsertBlock());
    }
    void complete(IRGenFunction &IGF) {
      assert(PHI);
      IGF.Builder.Insert(PHI);
    }
    llvm::Value *get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {
      assert(PHI);
      return PHI;
    }
  };

  /// An implementation of DynamicPackingPHIMapping for Addresses.
  template <> class DynamicPackingPHIMapping<Address>
      : private DynamicPackingPHIMapping<llvm::Value*> {
    typedef DynamicPackingPHIMapping<llvm::Value*> super;
  public:
    void collect(IRGenFunction &IGF, Address value) {
      super::collect(IGF, value.getAddress());
    }
    void complete(IRGenFunction &IGF) {
      super::complete(IGF);
    }
    Address get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {
      return type.getAddressForPointer(super::get(IGF, T, type));
    }
  };

  /// An implementation of packing operations based around a lambda.
  template <class ResultTy, class FnTy>
  class LambdaDynamicPackingOperation : public DynamicPackingOperation {
    FnTy Fn;
    DynamicPackingPHIMapping<ResultTy> Mapping;
  public:
    explicit LambdaDynamicPackingOperation(FnTy &&fn) : Fn(fn) {}
    void emitForPacking(IRGenFunction &IGF, SILType T, const TypeInfo &type,
                        FixedPacking packing) override {
      Mapping.collect(IGF, Fn(IGF, T, type, packing));
    }

    void complete(IRGenFunction &IGF) override {
      Mapping.complete(IGF);
    }

    ResultTy get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {
      return Mapping.get(IGF, T, type);
    }
  };

  /// A partial specialization for lambda-based packing operations
  /// that return 'void'.
  template <class FnTy>
  class LambdaDynamicPackingOperation<void, FnTy>
      : public DynamicPackingOperation {
    FnTy Fn;
  public:
    explicit LambdaDynamicPackingOperation(FnTy &&fn) : Fn(fn) {}
    void emitForPacking(IRGenFunction &IGF, SILType T, const TypeInfo &type,
                        FixedPacking packing) override {
      Fn(IGF, T, type, packing);
    }
    void complete(IRGenFunction &IGF) override {}
    void get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {}
  };
} // end anonymous namespace

/// Dynamic check for the enabling conditions of different kinds of
/// packing into a fixed-size buffer, and perform an operation at each
/// of them.
static void emitDynamicPackingOperation(IRGenFunction &IGF,
                                        SILType T,
                                        const TypeInfo &type,
                                        DynamicPackingOperation &operation) {
  auto indirectBB = IGF.createBasicBlock("dynamic-packing.indirect");
  auto directBB = IGF.createBasicBlock("dynamic-packing.direct");
  auto contBB = IGF.createBasicBlock("dynamic-packing.cont");

  // Branch.
  auto isInline = type.isDynamicallyPackedInline(IGF, T);
  IGF.Builder.CreateCondBr(isInline, directBB, indirectBB);

  // Emit the indirect path.
  IGF.Builder.emitBlock(indirectBB); {
    ConditionalDominanceScope condition(IGF);
    operation.emitForPacking(IGF, T, type, FixedPacking::Allocate);
    IGF.Builder.CreateBr(contBB);
  }

  // Emit the direct path.
  IGF.Builder.emitBlock(directBB); {
    ConditionalDominanceScope condition(IGF);
    operation.emitForPacking(IGF, T, type, FixedPacking::OffsetZero);
    IGF.Builder.CreateBr(contBB);
  }

  // Enter the continuation block and add the PHI if required.
  IGF.Builder.emitBlock(contBB);
  operation.complete(IGF);
}

/// A helper function for creating a lambda-based DynamicPackingOperation.
template <class ResultTy, class FnTy>
LambdaDynamicPackingOperation<ResultTy, FnTy>
makeLambdaDynamicPackingOperation(FnTy &&fn) {
  return LambdaDynamicPackingOperation<ResultTy, FnTy>(std::move(fn));
}

/// Perform an operation on a type that requires dynamic packing.
template <class ResultTy, class... ArgTys, class... ParamTys>
static ResultTy emitForDynamicPacking(IRGenFunction &IGF,
                                      ResultTy (*fn)(ParamTys...),
                                      SILType T,
                                      const TypeInfo &type,
                                      ArgTys... args) {
  auto operation = makeLambdaDynamicPackingOperation<ResultTy>(
    [&](IRGenFunction &IGF, SILType T, const TypeInfo &type,
        FixedPacking packing) {
      return fn(IGF, args..., T, type, packing);
    });
  emitDynamicPackingOperation(IGF, T, type, operation);
  return operation.get(IGF, T, type);
}

/// Emit a 'projectBuffer' operation.  Always returns a T*.
static Address emitDefaultProjectBuffer(IRGenFunction &IGF, Address buffer,
                                        SILType T, const TypeInfo &type,
                                        FixedPacking packing) {
  llvm::PointerType *resultTy = type.getStorageType()->getPointerTo();
  switch (packing) {
  case FixedPacking::Allocate: {

    // Use copy-on-write existentials?
    auto &IGM = IGF.IGM;
    auto &Builder = IGF.Builder;
    if (IGM.getSILModule().getOptions().UseCOWExistentials) {
      Address boxAddress(
          Builder.CreateBitCast(buffer.getAddress(),
                                IGM.RefCountedPtrTy->getPointerTo()),
          buffer.getAlignment());
      auto *boxStart = IGF.Builder.CreateLoad(boxAddress);
      auto *alignmentMask = type.getAlignmentMask(IGF, T);
      auto *heapHeaderSize =
          llvm::ConstantInt::get(IGM.SizeTy, getHeapHeaderSize(IGM).getValue());
      auto *startOffset =
        Builder.CreateAnd(Builder.CreateAdd(heapHeaderSize, alignmentMask),
                              Builder.CreateNot(alignmentMask));
      auto *addressInBox =
          IGF.emitByteOffsetGEP(boxStart, startOffset, IGM.OpaqueTy);

      addressInBox = Builder.CreateBitCast(addressInBox, resultTy);
      return type.getAddressForPointer(addressInBox);
    }

    Address slot =
        Builder.CreateBitCast(buffer, resultTy->getPointerTo(), "storage-slot");
    llvm::Value *address = Builder.CreateLoad(slot);
    return type.getAddressForPointer(address);
  }

  case FixedPacking::OffsetZero: {
    return IGF.Builder.CreateBitCast(buffer, resultTy, "object");
  }

  case FixedPacking::Dynamic:
    return emitForDynamicPacking(IGF, &emitDefaultProjectBuffer,
                                 T, type, buffer);

  }
  llvm_unreachable("bad packing!");

}

/// Emit an 'allocateBuffer' operation.  Always returns a T*.
static Address emitDefaultAllocateBuffer(IRGenFunction &IGF, Address buffer,
                                         SILType T, const TypeInfo &type,
                                         FixedPacking packing) {
  switch (packing) {
  case FixedPacking::Allocate: {

    // Use copy-on-write existentials?
    auto &IGM = IGF.IGM;
    if (IGM.getSILModule().getOptions().UseCOWExistentials) {
      /* This would be faster but what do we pass as genericEnv?
      if (isa<FixedTypeInfo>(T)) {
        assert(T->getFixedPacking() == FixedPacking::Allocate);
        auto *genericEnv = nullptr; //???;
          // Otherwise, allocate a box with enough storage.
        Address addr = emitAllocateExistentialBoxInBuffer(
          IGF, valueType, buffer, genericEnv, "exist.box.addr");
        return type.getAddressForPointer(addr);
      }
      */

      llvm::Value *box, *address;
      auto *metadata = IGF.emitTypeMetadataRefForLayout(T);
      IGF.emitAllocBoxCall(metadata, box, address);
      IGF.Builder.CreateStore(
          box, Address(IGF.Builder.CreateBitCast(
                           buffer.getAddress(), box->getType()->getPointerTo()),
                       buffer.getAlignment()));

      llvm::PointerType *resultTy = type.getStorageType()->getPointerTo();
      address = IGF.Builder.CreateBitCast(address, resultTy);
      return type.getAddressForPointer(address);
    }

    auto sizeAndAlign = type.getSizeAndAlignmentMask(IGF, T);
    llvm::Value *addr =
      IGF.emitAllocRawCall(sizeAndAlign.first, sizeAndAlign.second);
    buffer = IGF.Builder.CreateBitCast(buffer, IGM.Int8PtrPtrTy);
    IGF.Builder.CreateStore(addr, buffer);

    addr = IGF.Builder.CreateBitCast(addr,
                                     type.getStorageType()->getPointerTo());
    return type.getAddressForPointer(addr);
  }

  case FixedPacking::OffsetZero:
    return emitDefaultProjectBuffer(IGF, buffer, T, type, packing);

  case FixedPacking::Dynamic:
    return emitForDynamicPacking(IGF, &emitDefaultAllocateBuffer,
                                 T, type, buffer);
  }
  llvm_unreachable("bad packing!");
}

/// Emit a 'deallocateBuffer' operation.
static void emitDefaultDeallocateBuffer(IRGenFunction &IGF,
                                        Address buffer,
                                        SILType T,
                                        const TypeInfo &type,
                                        FixedPacking packing) {
  switch (packing) {
  case FixedPacking::Allocate: {
    Address slot =
      IGF.Builder.CreateBitCast(buffer, IGF.IGM.Int8PtrPtrTy);
    llvm::Value *addr = IGF.Builder.CreateLoad(slot, "storage");
    auto sizeAndAlignMask = type.getSizeAndAlignmentMask(IGF, T);
    IGF.emitDeallocRawCall(addr, sizeAndAlignMask.first,
                           sizeAndAlignMask.second);
    return;
  }

  case FixedPacking::OffsetZero:
    return;

  case FixedPacking::Dynamic:
    return emitForDynamicPacking(IGF, &emitDefaultDeallocateBuffer,
                                 T, type, buffer);
  }
  llvm_unreachable("bad packing!");
}

/// Emit a 'destroyBuffer' operation.
static void emitDefaultDestroyBuffer(IRGenFunction &IGF, Address buffer,
                                     SILType T, const TypeInfo &type,
                                     FixedPacking packing) {
  // Special-case dynamic packing in order to thread the jumps.
  if (packing == FixedPacking::Dynamic)
    return emitForDynamicPacking(IGF, &emitDefaultDestroyBuffer,
                                 T, type, buffer);

  Address object = emitDefaultProjectBuffer(IGF, buffer, T, type, packing);
  type.destroy(IGF, object, T);
  emitDefaultDeallocateBuffer(IGF, buffer, T, type, packing);
}

/// Emit an 'initializeBufferWithCopyOfBuffer' operation.
/// Returns the address of the destination object.
static Address
emitDefaultInitializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                            Address destBuffer,
                                            Address srcBuffer,
                                            SILType T,
                                            const TypeInfo &type,
                                            FixedPacking packing) {
  // Special-case dynamic packing in order to thread the jumps.
  if (packing == FixedPacking::Dynamic)
    return emitForDynamicPacking(IGF,
                                 &emitDefaultInitializeBufferWithCopyOfBuffer,
                                 T, type, destBuffer, srcBuffer);

  if (IGF.IGM.getSILModule().getOptions().UseCOWExistentials) {
    if (packing == FixedPacking::OffsetZero) {
      Address destObject =
        emitDefaultAllocateBuffer(IGF, destBuffer, T, type, packing);
      Address srcObject =
        emitDefaultProjectBuffer(IGF, srcBuffer, T, type, packing);
      type.initializeWithCopy(IGF, destObject, srcObject, T);
      return destObject;
    } else {
      assert(packing == FixedPacking::Allocate);
      auto *destReferenceAddr = IGF.Builder.CreateBitCast(
          destBuffer.getAddress(), IGF.IGM.RefCountedPtrTy->getPointerTo());
      auto *srcReferenceAddr = IGF.Builder.CreateBitCast(
          srcBuffer.getAddress(), IGF.IGM.RefCountedPtrTy->getPointerTo());
      auto *srcReference =
        IGF.Builder.CreateLoad(srcReferenceAddr, srcBuffer.getAlignment());
      IGF.emitNativeStrongRetain(srcReference, IGF.getDefaultAtomicity());
      IGF.Builder.CreateStore(
        srcReference,
        Address(destReferenceAddr, destBuffer.getAlignment()));
      return emitDefaultProjectBuffer(IGF, destBuffer, T, type, packing);
    }
  }

  Address destObject =
    emitDefaultAllocateBuffer(IGF, destBuffer, T, type, packing);
  Address srcObject =
    emitDefaultProjectBuffer(IGF, srcBuffer, T, type, packing);
  type.initializeWithCopy(IGF, destObject, srcObject, T);
  return destObject;
}

/// Emit an 'initializeBufferWithTakeOfBuffer' operation.
/// Returns the address of the destination object.
static Address
emitDefaultInitializeBufferWithTakeOfBuffer(IRGenFunction &IGF,
                                            Address destBuffer,
                                            Address srcBuffer,
                                            SILType T,
                                            const TypeInfo &type,
                                            FixedPacking packing) {
  switch (packing) {

  case FixedPacking::Dynamic:
    // Special-case dynamic packing in order to thread the jumps.
    return emitForDynamicPacking(IGF,
                                 &emitDefaultInitializeBufferWithTakeOfBuffer,
                                 T, type, destBuffer, srcBuffer);

  case FixedPacking::OffsetZero: {
    // Both of these allocations/projections should be no-ops.
    Address destObject =
      emitDefaultAllocateBuffer(IGF, destBuffer, T, type, packing);
    Address srcObject =
      emitDefaultProjectBuffer(IGF, srcBuffer, T, type, packing);
    type.initializeWithTake(IGF, destObject, srcObject, T);
    return destObject;
  }

  case FixedPacking::Allocate: {
    // Just copy the out-of-line storage pointers.
    srcBuffer = IGF.Builder.CreateBitCast(
        srcBuffer, IGF.IGM.RefCountedPtrTy->getPointerTo());
    llvm::Value *addr = IGF.Builder.CreateLoad(srcBuffer);
    destBuffer = IGF.Builder.CreateBitCast(
        destBuffer, IGF.IGM.RefCountedPtrTy->getPointerTo());
    IGF.Builder.CreateStore(addr, destBuffer);
    return emitDefaultProjectBuffer(IGF, destBuffer, T, type, packing);
  }
  }
  llvm_unreachable("bad fixed packing");
}

static Address emitDefaultInitializeBufferWithCopy(IRGenFunction &IGF,
                                                   Address destBuffer,
                                                   Address srcObject,
                                                   SILType T,
                                                   const TypeInfo &type,
                                                   FixedPacking packing) {
  Address destObject =
    emitDefaultAllocateBuffer(IGF, destBuffer, T, type, packing);
  type.initializeWithCopy(IGF, destObject, srcObject, T);
  return destObject;
}

static Address emitDefaultInitializeBufferWithTake(IRGenFunction &IGF,
                                                   Address destBuffer,
                                                   Address srcObject,
                                                   SILType T,
                                                   const TypeInfo &type,
                                                   FixedPacking packing) {
  Address destObject =
    emitDefaultAllocateBuffer(IGF, destBuffer, T, type, packing);
  type.initializeWithTake(IGF, destObject, srcObject, T);
  return destObject;
}

// Metaprogram some of the common boilerplate here:
//   - the default implementation in TypeInfo
//   - the value-witness emitter which tries to avoid some dynamic
//     dispatch and the recomputation of the fixed packing

#define DEFINE_BINARY_BUFFER_OP(LOWER, TITLE)                             \
Address TypeInfo::LOWER(IRGenFunction &IGF, Address dest, Address src,    \
                        SILType T) const {                                \
  return emitDefault##TITLE(IGF, dest, src, T, *this,                     \
                            getFixedPacking(IGF.IGM));                    \
}                                                                         \
static Address emit##TITLE(IRGenFunction &IGF, Address dest, Address src, \
                           SILType T, const TypeInfo &type,               \
                           FixedPacking packing) {                        \
  if (packing == FixedPacking::Dynamic)                                   \
    return type.LOWER(IGF, dest, src, T);                                 \
  return emitDefault##TITLE(IGF, dest, src, T, type, packing);            \
}
DEFINE_BINARY_BUFFER_OP(initializeBufferWithCopy,
                        InitializeBufferWithCopy)
DEFINE_BINARY_BUFFER_OP(initializeBufferWithTake,
                        InitializeBufferWithTake)
DEFINE_BINARY_BUFFER_OP(initializeBufferWithCopyOfBuffer,
                        InitializeBufferWithCopyOfBuffer)
DEFINE_BINARY_BUFFER_OP(initializeBufferWithTakeOfBuffer,
                        InitializeBufferWithTakeOfBuffer)
#undef DEFINE_BINARY_BUFFER_OP

#define DEFINE_UNARY_BUFFER_OP(RESULT, LOWER, TITLE)                          \
RESULT TypeInfo::LOWER(IRGenFunction &IGF, Address buffer, SILType T) const { \
  return emitDefault##TITLE(IGF, buffer, T, *this, getFixedPacking(IGF.IGM)); \
}                                                                             \
static RESULT emit##TITLE(IRGenFunction &IGF, Address buffer, SILType T,      \
                          const TypeInfo &type, FixedPacking packing) {       \
  if (packing == FixedPacking::Dynamic)                                       \
    return type.LOWER(IGF, buffer, T);                                        \
  return emitDefault##TITLE(IGF, buffer, T, type, packing);                   \
}
DEFINE_UNARY_BUFFER_OP(Address, allocateBuffer, AllocateBuffer)
DEFINE_UNARY_BUFFER_OP(Address, projectBuffer, ProjectBuffer)
DEFINE_UNARY_BUFFER_OP(void, destroyBuffer, DestroyBuffer)
DEFINE_UNARY_BUFFER_OP(void, deallocateBuffer, DeallocateBuffer)
#undef DEFINE_UNARY_BUFFER_OP

static llvm::Value *getArg(llvm::Function::arg_iterator &it,
                           StringRef name) {
  llvm::Value *arg = &*(it++);
  arg->setName(name);
  return arg;
}

/// Get the next argument as a pointer to the given storage type.
static Address getArgAs(IRGenFunction &IGF,
                        llvm::Function::arg_iterator &it,
                        const TypeInfo &type,
                        StringRef name) {
  llvm::Value *arg = getArg(it, name);
  llvm::Value *result =
    IGF.Builder.CreateBitCast(arg, type.getStorageType()->getPointerTo());
  return type.getAddressForPointer(result);
}

/// Get the next argument as a pointer to the given storage type.
static Address getArgAsBuffer(IRGenFunction &IGF,
                              llvm::Function::arg_iterator &it,
                              StringRef name) {
  llvm::Value *arg = getArg(it, name);
  return Address(arg, getFixedBufferAlignment(IGF.IGM));
}

/// Given an abstract type --- a type possibly expressed in terms of
/// unbound generic types --- return the formal type within the type's
/// primary defining context.
static CanType getFormalTypeInContext(CanType abstractType) {
  // Map the parent of any non-generic nominal type.
  if (auto nominalType = dyn_cast<NominalType>(abstractType)) {
    // If it doesn't have a parent, or the parent doesn't need remapping,
    // do nothing.
    auto abstractParentType = nominalType.getParent();
    if (!abstractParentType) return abstractType;
    auto parentType = getFormalTypeInContext(abstractParentType);
    if (abstractParentType == parentType) return abstractType;

    // Otherwise, rebuild the type.
    return CanType(NominalType::get(nominalType->getDecl(), parentType,
                                    nominalType->getDecl()->getASTContext()));

  // Map unbound types into their defining context.
  } else if (auto ugt = dyn_cast<UnboundGenericType>(abstractType)) {
    return ugt->getDecl()->getDeclaredTypeInContext()->getCanonicalType();

  // Everything else stays the same.
  } else {
    return abstractType;
  }
}

/// Get the next argument and use it as the 'self' type metadata.
static void getArgAsLocalSelfTypeMetadata(IRGenFunction &IGF,
                                          llvm::Function::arg_iterator &it,
                                          CanType abstractType) {
  llvm::Value *arg = &*it++;
  assert(arg->getType() == IGF.IGM.TypeMetadataPtrTy &&
         "Self argument is not a type?!");

  auto formalType = getFormalTypeInContext(abstractType);
  IGF.bindLocalTypeDataFromTypeMetadata(formalType, IsExact, arg);
}

/// Build a value witness that initializes an array front-to-back.
static void emitInitializeArrayFrontToBackWitness(IRGenFunction &IGF,
                                           llvm::Function::arg_iterator argv,
                                           CanType abstractType,
                                           SILType concreteType,
                                           const TypeInfo &type,
                                           IsTake_t take) {
  Address destArray = getArgAs(IGF, argv, type, "dest");
  Address srcArray = getArgAs(IGF, argv, type, "src");
  llvm::Value *count = getArg(argv, "count");
  getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

  emitInitializeArrayFrontToBack(IGF, type, destArray, srcArray, count,
                                 concreteType, take);

  destArray = IGF.Builder.CreateBitCast(destArray, IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateRet(destArray.getAddress());
}

/// Build a value witness that initializes an array back-to-front.
static void emitInitializeArrayBackToFrontWitness(IRGenFunction &IGF,
                                           llvm::Function::arg_iterator argv,
                                           CanType abstractType,
                                           SILType concreteType,
                                           const TypeInfo &type,
                                           IsTake_t take) {
  Address destArray = getArgAs(IGF, argv, type, "dest");
  Address srcArray = getArgAs(IGF, argv, type, "src");
  llvm::Value *count = getArg(argv, "count");
  getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

  emitInitializeArrayBackToFront(IGF, type, destArray, srcArray, count,
                                 concreteType, take);

  destArray = IGF.Builder.CreateBitCast(destArray, IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateRet(destArray.getAddress());
}

/// Build a specific value-witness function.
static void buildValueWitnessFunction(IRGenModule &IGM,
                                      llvm::Function *fn,
                                      ValueWitness index,
                                      FixedPacking packing,
                                      CanType abstractType,
                                      SILType concreteType,
                                      const TypeInfo &type) {
  assert(isValueWitnessFunction(index));

  IRGenFunction IGF(IGM, fn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, fn);

  auto argv = fn->arg_begin();
  switch (index) {
  case ValueWitness::AllocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    Address result =
      emitAllocateBuffer(IGF, buffer, concreteType, type, packing);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::AssignWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    type.assignWithCopy(IGF, dest, src, concreteType);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::AssignWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    type.assignWithTake(IGF, dest, src, concreteType);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::DeallocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    emitDeallocateBuffer(IGF, buffer, concreteType, type, packing);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::Destroy: {
    Address object = getArgAs(IGF, argv, type, "object");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    type.destroy(IGF, object, concreteType);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::DestroyArray: {
    Address array = getArgAs(IGF, argv, type, "array");
    llvm::Value *count = getArg(argv, "count");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    auto entry = IGF.Builder.GetInsertBlock();
    auto iter = IGF.createBasicBlock("iter");
    auto loop = IGF.createBasicBlock("loop");
    auto exit = IGF.createBasicBlock("exit");
    IGF.Builder.CreateBr(iter);
    IGF.Builder.emitBlock(iter);

    auto counter = IGF.Builder.CreatePHI(IGM.SizeTy, 2);
    counter->addIncoming(count, entry);
    auto elementVal = IGF.Builder.CreatePHI(array.getType(), 2);
    elementVal->addIncoming(array.getAddress(), entry);
    Address element(elementVal, array.getAlignment());

    auto done = IGF.Builder.CreateICmpEQ(counter,
                                         llvm::ConstantInt::get(IGM.SizeTy, 0));
    IGF.Builder.CreateCondBr(done, exit, loop);

    IGF.Builder.emitBlock(loop);
    ConditionalDominanceScope condition(IGF);
    type.destroy(IGF, element, concreteType);
    auto nextCounter = IGF.Builder.CreateSub(counter,
                                     llvm::ConstantInt::get(IGM.SizeTy, 1));
    auto nextElement = type.indexArray(IGF, element,
                                       llvm::ConstantInt::get(IGM.SizeTy, 1),
                                       concreteType);
    auto loopEnd = IGF.Builder.GetInsertBlock();
    counter->addIncoming(nextCounter, loopEnd);
    elementVal->addIncoming(nextElement.getAddress(), loopEnd);
    IGF.Builder.CreateBr(iter);

    IGF.Builder.emitBlock(exit);
    IGF.Builder.CreateRetVoid();

    return;
  }

  case ValueWitness::DestroyBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    emitDestroyBuffer(IGF, buffer, concreteType, type, packing);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAsBuffer(IGF, argv, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithCopyOfBuffer(IGF, dest, src, concreteType,
                                           type, packing);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTakeOfBuffer: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAsBuffer(IGF, argv, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithTakeOfBuffer(IGF, dest, src, concreteType,
                                           type, packing);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithCopy: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithCopy(IGF, dest, src, concreteType, type, packing);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTake: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithTake(IGF, dest, src, concreteType, type, packing);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    type.initializeWithCopy(IGF, dest, src, concreteType);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::InitializeArrayWithCopy: {
    emitInitializeArrayFrontToBackWitness(IGF, argv, abstractType, concreteType,
                                          type, IsNotTake);
    return;
  }

  case ValueWitness::InitializeWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    type.initializeWithTake(IGF, dest, src, concreteType);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::InitializeArrayWithTakeFrontToBack: {
    emitInitializeArrayFrontToBackWitness(IGF, argv, abstractType, concreteType,
                                          type, IsTake);
    return;
  }

  case ValueWitness::InitializeArrayWithTakeBackToFront: {
    emitInitializeArrayBackToFrontWitness(IGF, argv, abstractType, concreteType,
                                          type, IsTake);
    return;
  }

  case ValueWitness::ProjectBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result = emitProjectBuffer(IGF, buffer, concreteType, type, packing);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::StoreExtraInhabitant: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    llvm::Value *index = getArg(argv, "index");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    type.storeExtraInhabitant(IGF, index, dest, concreteType);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::GetExtraInhabitantIndex: {
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    llvm::Value *idx = type.getExtraInhabitantIndex(IGF, src, concreteType);
    IGF.Builder.CreateRet(idx);
    return;
  }

  case ValueWitness::GetEnumTag: {
    auto &strategy = getEnumImplStrategy(IGM, concreteType);

    llvm::Value *value = getArg(argv, "value");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    auto enumTy = type.getStorageType()->getPointerTo();
    value = IGF.Builder.CreateBitCast(value, enumTy);
    auto enumAddr = type.getAddressForPointer(value);

    llvm::Value *result = strategy.emitGetEnumTag(IGF, concreteType, enumAddr);
    IGF.Builder.CreateRet(result);
    return;
  }

  case ValueWitness::DestructiveProjectEnumData: {
    auto &strategy = getEnumImplStrategy(IGM, concreteType);

    llvm::Value *value = getArg(argv, "value");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    if (strategy.getElementsWithPayload().size() > 0) {
      strategy.destructiveProjectDataForLoad(
          IGF, concreteType,
          Address(value, type.getBestKnownAlignment()));
    }

    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::DestructiveInjectEnumTag: {
    auto &strategy = getEnumImplStrategy(IGM, concreteType);

    llvm::Value *value = getArg(argv, "value");

    auto enumTy = type.getStorageType()->getPointerTo();
    value = IGF.Builder.CreateBitCast(value, enumTy);

    llvm::Value *tag = getArg(argv, "tag");

    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    strategy.emitStoreTag(IGF, concreteType,
                          Address(value, type.getBestKnownAlignment()),
                          tag);

    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::Size:
  case ValueWitness::Flags:
  case ValueWitness::Stride:
  case ValueWitness::ExtraInhabitantFlags:
    llvm_unreachable("these value witnesses aren't functions");
  }
  llvm_unreachable("bad value witness kind!");
}

/// Return a function which takes two pointer arguments and returns
/// void immediately.
static llvm::Constant *getNoOpVoidFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.TypeMetadataPtrTy };
  return IGM.getOrCreateHelperFunction("__swift_noop_void_return",
                                       IGM.VoidTy, argTys,
                                       [&](IRGenFunction &IGF) {
    IGF.Builder.CreateRetVoid();
  });
}

/// Return a function which takes two pointer arguments and returns
/// the first one immediately.
static llvm::Constant *getReturnSelfFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.TypeMetadataPtrTy };
  return IGM.getOrCreateHelperFunction(
      "__swift_noop_self_return", IGM.Int8PtrTy, argTys,
      [&](IRGenFunction &IGF) {
        IGF.Builder.CreateRet(&*IGF.CurFn->arg_begin());
      });
}

/// Return a function which takes three pointer arguments and does a
/// retaining assignWithCopy on the first two: it loads a pointer from
/// the second, retains it, loads a pointer from the first, stores the
/// new pointer in the first, and releases the old pointer.
static llvm::Constant *getAssignWithCopyStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  return IGM.getOrCreateHelperFunction("__swift_assignWithCopy_strong",
                                       ptrPtrTy, argTys,
                                       [&](IRGenFunction &IGF) {
    auto it = IGF.CurFn->arg_begin();
    Address dest(&*(it++), IGM.getPointerAlignment());
    Address src(&*(it++), IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    IGF.emitNativeStrongRetain(newValue, IGF.getDefaultAtomicity());
    llvm::Value *oldValue = IGF.Builder.CreateLoad(dest, "old");
    IGF.Builder.CreateStore(newValue, dest);
    IGF.emitNativeStrongRelease(oldValue, IGF.getDefaultAtomicity());

    IGF.Builder.CreateRet(dest.getAddress());
  });
}

/// Return a function which takes three pointer arguments and does a
/// retaining assignWithTake on the first two: it loads a pointer from
/// the second, retains it, loads a pointer from the first, stores the
/// new pointer in the first, and releases the old pointer.
static llvm::Constant *getAssignWithTakeStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  return IGM.getOrCreateHelperFunction("__swift_assignWithTake_strong",
                                       ptrPtrTy, argTys,
                                       [&](IRGenFunction &IGF) {
    auto it = IGF.CurFn->arg_begin();
    Address dest(&*(it++), IGM.getPointerAlignment());
    Address src(&*(it++), IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    llvm::Value *oldValue = IGF.Builder.CreateLoad(dest, "old");
    IGF.Builder.CreateStore(newValue, dest);
    IGF.emitNativeStrongRelease(oldValue, IGF.getDefaultAtomicity());

    IGF.Builder.CreateRet(dest.getAddress());
  });
}

/// Return a function which takes three pointer arguments and does a
/// retaining initWithCopy on the first two: it loads a pointer from
/// the second, retains it, and stores that in the first.
static llvm::Constant *getInitWithCopyStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  return IGM.getOrCreateHelperFunction("__swift_initWithCopy_strong",
                                       ptrPtrTy, argTys,
                                       [&](IRGenFunction &IGF) {
    auto it = IGF.CurFn->arg_begin();
    Address dest(&*(it++), IGM.getPointerAlignment());
    Address src(&*(it++), IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    IGF.emitNativeStrongRetain(newValue, IGF.getDefaultAtomicity());
    IGF.Builder.CreateStore(newValue, dest);

    IGF.Builder.CreateRet(dest.getAddress());
  });
}

/// Return a function which takes two pointer arguments, loads a
/// pointer from the first, and calls swift_release on it immediately.
static llvm::Constant *getDestroyStrongFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrPtrTy, IGM.WitnessTablePtrTy };
  return IGM.getOrCreateHelperFunction("__swift_destroy_strong",
                                       IGM.VoidTy, argTys,
                                       [&](IRGenFunction &IGF) {
    Address arg(&*IGF.CurFn->arg_begin(), IGM.getPointerAlignment());
    IGF.emitNativeStrongRelease(IGF.Builder.CreateLoad(arg), IGF.getDefaultAtomicity());
    IGF.Builder.CreateRetVoid();
  });
}

/// Return a function which takes two pointer arguments, memcpys
/// from the second to the first, and returns the first argument.
static llvm::Constant *getMemCpyFunction(IRGenModule &IGM,
                                         const TypeInfo &objectTI) {
  // If we don't have a fixed type, use the standard copy-opaque-POD
  // routine.  It's not quite clear how in practice we'll be able to
  // conclude that something is known-POD without knowing its size,
  // but it's (1) conceivable and (2) needed as a general export anyway.
  auto *fixedTI = dyn_cast<FixedTypeInfo>(&objectTI);
  if (!fixedTI) return IGM.getCopyPODFn();

  // We need to unique by both size and alignment.  Note that we're
  // assuming that it's safe to call a function that returns a pointer
  // at a site that assumes the function returns void.
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    nameStream << "__swift_memcpy";
    nameStream << fixedTI->getFixedSize().getValue();
    nameStream << '_';
    nameStream << fixedTI->getFixedAlignment().getValue();
  }

  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy, IGM.TypeMetadataPtrTy };
  return IGM.getOrCreateHelperFunction(name, IGM.Int8PtrTy, argTys,
                                       [&](IRGenFunction &IGF) {
    auto it = IGF.CurFn->arg_begin();
    Address dest(&*it++, fixedTI->getFixedAlignment());
    Address src(&*it++, fixedTI->getFixedAlignment());
    IGF.emitMemCpy(dest, src, fixedTI->getFixedSize());
    IGF.Builder.CreateRet(dest.getAddress());
  });
}

/// Return a function which takes two buffer arguments, copies
/// a pointer from the second to the first, and returns the pointer.
static llvm::Constant *getCopyOutOfLinePointerFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrPtrTy, IGM.Int8PtrPtrTy,
                           IGM.TypeMetadataPtrTy };

  return IGM.getOrCreateHelperFunction("__swift_copy_outline_pointer",
                                       IGM.Int8PtrTy, argTys,
                                       [&](IRGenFunction &IGF) {
    auto it = IGF.CurFn->arg_begin();
    Address dest(&*it++, IGM.getPointerAlignment());
    Address src(&*it++, IGM.getPointerAlignment());
    auto ptr = IGF.Builder.CreateLoad(src);
    IGF.Builder.CreateStore(ptr, dest);
    IGF.Builder.CreateRet(ptr);
  });
}
/// Return a function which takes two buffer arguments, copies
/// a pointer from the second to the first, and returns the pointer.
static llvm::Constant *
getCopyOutOfLineBoxPointerFunction(IRGenModule &IGM,
                                   const FixedTypeInfo &fixedTI) {
  llvm::Type *argTys[] = { IGM.Int8PtrPtrTy, IGM.Int8PtrPtrTy,
                           IGM.TypeMetadataPtrTy };
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    nameStream << "__swift_copy_outline_existential_box_pointer";
    nameStream << fixedTI.getFixedAlignment().getValue();
  }
  return IGM.getOrCreateHelperFunction(
      name, IGM.Int8PtrTy, argTys, [&](IRGenFunction &IGF) {
        auto it = IGF.CurFn->arg_begin();
        Address dest(&*it++, IGM.getPointerAlignment());
        Address src(&*it++, IGM.getPointerAlignment());
        auto *ptr = IGF.Builder.CreateLoad(src);
        IGF.Builder.CreateStore(ptr, dest);
        auto *alignmentMask = fixedTI.getStaticAlignmentMask(IGM);
        auto *heapHeaderSize = llvm::ConstantInt::get(
            IGM.SizeTy, getHeapHeaderSize(IGM).getValue());
        auto *startOffset = IGF.Builder.CreateAnd(
            IGF.Builder.CreateAdd(heapHeaderSize, alignmentMask),
            IGF.Builder.CreateNot(alignmentMask));
        auto *objectAddr =
            IGF.emitByteOffsetGEP(ptr, startOffset, IGM.Int8Ty);
        IGF.Builder.CreateRet(objectAddr);
      });
}

namespace {
  enum class MemMoveOrCpy { MemMove, MemCpy };
} // end anonymous namespace

/// Return a function which takes two pointer arguments and a count, memmoves
/// or memcpys from the second to the first, and returns the first argument.
static llvm::Constant *getMemOpArrayFunction(IRGenModule &IGM,
                                             const TypeInfo &objectTI,
                                             MemMoveOrCpy kind) {
  llvm::Type *argTys[] = {
    IGM.Int8PtrTy, IGM.Int8PtrTy, IGM.SizeTy,
    IGM.TypeMetadataPtrTy
  };

  // TODO: Add a copyPODArray runtime entry point for bitwise-takable but non-
  // fixed-size types. Currently only fixed-layout types should be known
  // bitwise-takable.
  auto &fixedTI = cast<FixedTypeInfo>(objectTI);

  // We need to unique by both size and alignment.  Note that we're
  // assuming that it's safe to call a function that returns a pointer
  // at a site that assumes the function returns void.
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    switch (kind) {
    case MemMoveOrCpy::MemCpy:
      nameStream << "__swift_memcpy_array";
      break;
    case MemMoveOrCpy::MemMove:
      nameStream << "__swift_memmove_array";
      break;
    }
    nameStream << fixedTI.getFixedStride().getValue();
    nameStream << '_';
    nameStream << fixedTI.getFixedAlignment().getValue();
  }

  return IGM.getOrCreateHelperFunction(name, IGM.Int8PtrTy, argTys,
                                       [&](IRGenFunction &IGF) {
    auto it = IGF.CurFn->arg_begin();
    Address dest(&*it++, fixedTI.getFixedAlignment());
    Address src(&*it++, fixedTI.getFixedAlignment());
    llvm::Value *count = &*(it++);
    llvm::Value *stride
      = llvm::ConstantInt::get(IGM.SizeTy, fixedTI.getFixedStride().getValue());
    llvm::Value *totalCount = IGF.Builder.CreateNUWMul(count, stride);
    switch (kind) {
    case MemMoveOrCpy::MemMove:
      IGF.Builder.CreateMemMove(dest.getAddress(), src.getAddress(), totalCount,
                                fixedTI.getFixedAlignment().getValue());
      break;
    case MemMoveOrCpy::MemCpy:
      IGF.Builder.CreateMemCpy(dest.getAddress(), src.getAddress(), totalCount,
                               fixedTI.getFixedAlignment().getValue());
      break;
    }
    IGF.Builder.CreateRet(dest.getAddress());
  });
}

static llvm::Constant *getMemMoveArrayFunction(IRGenModule &IGM,
                                               const TypeInfo &objectTI) {
  return getMemOpArrayFunction(IGM, objectTI, MemMoveOrCpy::MemMove);
}
static llvm::Constant *getMemCpyArrayFunction(IRGenModule &IGM,
                                               const TypeInfo &objectTI) {
  return getMemOpArrayFunction(IGM, objectTI, MemMoveOrCpy::MemCpy);
}

/// Find a witness to the fact that a type is a value type.
/// Always adds an i8*.
static void addValueWitness(IRGenModule &IGM,
                            ConstantArrayBuilder &B,
                            ValueWitness index,
                            FixedPacking packing,
                            CanType abstractType,
                            SILType concreteType,
                            const TypeInfo &concreteTI) {
  auto addFunction = [&](llvm::Constant *fn) {
    B.addBitCast(fn, IGM.Int8PtrTy);
  };

  // Try to use a standard function.
  switch (index) {
  case ValueWitness::DeallocateBuffer:
    if (isNeverAllocated(packing))
      return addFunction(getNoOpVoidFunction(IGM));
    goto standard;

  case ValueWitness::DestroyBuffer:
    if (concreteTI.isPOD(ResilienceExpansion::Maximal)) {
      if (isNeverAllocated(packing))
        return addFunction(getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceExpansion::Maximal)) {
      assert(isNeverAllocated(packing));
      return addFunction(getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::Destroy:
    if (concreteTI.isPOD(ResilienceExpansion::Maximal)) {
      return addFunction(getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceExpansion::Maximal)) {
      return addFunction(getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::DestroyArray:
    if (concreteTI.isPOD(ResilienceExpansion::Maximal)) {
      return addFunction(getNoOpVoidFunction(IGM));
    }
    // TODO: A standard "destroy strong array" entrypoint for arrays of single
    // refcounted pointer types.
    goto standard;

  case ValueWitness::InitializeBufferWithCopyOfBuffer:
  case ValueWitness::InitializeBufferWithCopy:
    if (packing == FixedPacking::OffsetZero) {
      if (concreteTI.isPOD(ResilienceExpansion::Maximal)) {
        return addFunction(getMemCpyFunction(IGM, concreteTI));
      } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceExpansion::Maximal)) {
        return addFunction(getInitWithCopyStrongFunction(IGM));
      }
    }
    goto standard;

  case ValueWitness::InitializeBufferWithTakeOfBuffer:
    if (packing == FixedPacking::Allocate) {
      if (IGM.getSILModule().getOptions().UseCOWExistentials) {
        return addFunction(getCopyOutOfLineBoxPointerFunction(
            IGM, cast<FixedTypeInfo>(concreteTI)));
      }
      // Copy-on-write existentials would have to do a projection in the buffer
      // to get the values starting address.
      return addFunction(getCopyOutOfLinePointerFunction(IGM));
    } else
      if (packing == FixedPacking::OffsetZero &&
               concreteTI.isBitwiseTakable(ResilienceExpansion::Maximal)) {
      return addFunction(getMemCpyFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeBufferWithTake:
    if (concreteTI.isBitwiseTakable(ResilienceExpansion::Maximal)
        && packing == FixedPacking::OffsetZero)
      return addFunction(getMemCpyFunction(IGM, concreteTI));
    goto standard;

  case ValueWitness::InitializeWithTake:
    if (concreteTI.isBitwiseTakable(ResilienceExpansion::Maximal)) {
      return addFunction(getMemCpyFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithTakeFrontToBack:
    if (concreteTI.isBitwiseTakable(ResilienceExpansion::Maximal)) {
      return addFunction(getMemMoveArrayFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithTakeBackToFront:
    if (concreteTI.isBitwiseTakable(ResilienceExpansion::Maximal)) {
      return addFunction(getMemMoveArrayFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::AssignWithCopy:
    if (concreteTI.isPOD(ResilienceExpansion::Maximal)) {
      return addFunction(getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceExpansion::Maximal)) {
      return addFunction(getAssignWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::AssignWithTake:
    if (concreteTI.isPOD(ResilienceExpansion::Maximal)) {
      return addFunction(getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceExpansion::Maximal)) {
      return addFunction(getAssignWithTakeStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeWithCopy:
    if (concreteTI.isPOD(ResilienceExpansion::Maximal)) {
      return addFunction(getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceExpansion::Maximal)) {
      return addFunction(getInitWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithCopy:
    if (concreteTI.isPOD(ResilienceExpansion::Maximal)) {
      return addFunction(getMemCpyArrayFunction(IGM, concreteTI));
    }
    // TODO: A standard "copy strong array" entrypoint for arrays of single
    // refcounted pointer types.
    goto standard;

  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer:
    if (packing == FixedPacking::OffsetZero)
      return addFunction(getReturnSelfFunction(IGM));
    goto standard;

  case ValueWitness::Size: {
    if (auto value = concreteTI.getStaticSize(IGM))
      return B.add(llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy));

    // Just fill in null here if the type can't be statically laid out.
    return B.add(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
  }

  case ValueWitness::Flags: {
    uint64_t flags = 0;

    // If we locally know that the type has fixed layout, we can emit
    // meaningful flags for it.
    if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&concreteTI)) {
      flags |= fixedTI->getFixedAlignment().getValue() - 1;
      if (!fixedTI->isPOD(ResilienceExpansion::Maximal))
        flags |= ValueWitnessFlags::IsNonPOD;
      assert(packing == FixedPacking::OffsetZero ||
             packing == FixedPacking::Allocate);
      if (packing != FixedPacking::OffsetZero)
        flags |= ValueWitnessFlags::IsNonInline;

      if (fixedTI->getFixedExtraInhabitantCount(IGM) > 0)
        flags |= ValueWitnessFlags::Enum_HasExtraInhabitants;

      if (!fixedTI->isBitwiseTakable(ResilienceExpansion::Maximal))
        flags |= ValueWitnessFlags::IsNonBitwiseTakable;
    }

    if (concreteType.getEnumOrBoundGenericEnum())
      flags |= ValueWitnessFlags::HasEnumWitnesses;

    auto value = IGM.getSize(Size(flags));
    return B.add(llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy));
  }

  case ValueWitness::Stride: {
    if (auto value = concreteTI.getStaticStride(IGM))
      return B.add(llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy));

    // Just fill in null here if the type can't be statically laid out.
    return B.add(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
  }

  case ValueWitness::StoreExtraInhabitant:
  case ValueWitness::GetExtraInhabitantIndex: {
    if (!concreteTI.mayHaveExtraInhabitants(IGM)) {
      assert(concreteType.getEnumOrBoundGenericEnum());
      return B.addNullPointer(IGM.Int8PtrTy);
    }

    goto standard;
  }

  case ValueWitness::ExtraInhabitantFlags: {
    if (!concreteTI.mayHaveExtraInhabitants(IGM)) {
      assert(concreteType.getEnumOrBoundGenericEnum());
      return B.add(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
    }

    // If we locally know that the type has fixed layout, we can emit
    // meaningful flags for it.
    if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&concreteTI)) {
      uint64_t numExtraInhabitants = fixedTI->getFixedExtraInhabitantCount(IGM);
      assert(numExtraInhabitants <= ExtraInhabitantFlags::NumExtraInhabitantsMask);
      auto value = IGM.getSize(Size(numExtraInhabitants));
      return B.add(llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy));
    }

    // Otherwise, just fill in null here if the type can't be statically
    // queried for extra inhabitants.
    return B.add(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
  }

  case ValueWitness::GetEnumTag:
  case ValueWitness::DestructiveProjectEnumData:
  case ValueWitness::DestructiveInjectEnumTag:
    assert(concreteType.getEnumOrBoundGenericEnum());
    goto standard;
  }
  llvm_unreachable("bad value witness kind");

 standard:
  llvm::Function *fn =
    IGM.getAddrOfValueWitness(abstractType, index, ForDefinition);
  if (fn->empty())
    buildValueWitnessFunction(IGM, fn, index, packing, abstractType,
                              concreteType, concreteTI);
  addFunction(fn);
}

/// Collect the value witnesses for a particular type.
static void addValueWitnesses(IRGenModule &IGM,
                              ConstantArrayBuilder &B,
                              FixedPacking packing,
                              CanType abstractType,
                              SILType concreteType,
                              const TypeInfo &concreteTI) {
  for (unsigned i = 0; i != NumRequiredValueWitnesses; ++i) {
    addValueWitness(IGM, B, ValueWitness(i), packing,
                    abstractType, concreteType, concreteTI);
  }
  if (concreteType.getEnumOrBoundGenericEnum() ||
      concreteTI.mayHaveExtraInhabitants(IGM)) {
    for (auto i = unsigned(ValueWitness::First_ExtraInhabitantValueWitness);
         i <= unsigned(ValueWitness::Last_ExtraInhabitantValueWitness);
         ++i) {
      addValueWitness(IGM, B, ValueWitness(i), packing,
                      abstractType, concreteType, concreteTI);
    }
  }
  if (concreteType.getEnumOrBoundGenericEnum()) {
    for (auto i = unsigned(ValueWitness::First_EnumValueWitness);
         i <= unsigned(ValueWitness::Last_EnumValueWitness);
         ++i) {
      addValueWitness(IGM, B, ValueWitness(i), packing,
                      abstractType, concreteType, concreteTI);
    }
  }
}

/// True if a type has a generic-parameter-dependent value witness table.
/// Currently, this is true if the size and/or alignment of the type is
/// dependent on its generic parameters.
bool irgen::hasDependentValueWitnessTable(IRGenModule &IGM, CanType ty) {
  return !IGM.getTypeInfoForUnlowered(getFormalTypeInContext(ty)).isFixedSize();
}

static void addValueWitnessesForAbstractType(IRGenModule &IGM,
                                             ConstantArrayBuilder &B,
                                             CanType abstractType,
                                             bool &canBeConstant) {
  CanType concreteFormalType = getFormalTypeInContext(abstractType);

  auto concreteLoweredType = IGM.getLoweredType(concreteFormalType);
  auto &concreteTI = IGM.getTypeInfo(concreteLoweredType);
  FixedPacking packing = concreteTI.getFixedPacking(IGM);

  // For now, assume that we never have any interest in dynamically
  // changing the value witnesses for something that's fixed-layout.
  canBeConstant = concreteTI.isFixedSize();

  addValueWitnesses(IGM, B, packing, abstractType,
                    concreteLoweredType, concreteTI);
}

/// Emit a value-witness table for the given type, which is assumed to
/// be non-dependent.
llvm::Constant *irgen::emitValueWitnessTable(IRGenModule &IGM,
                                             CanType abstractType) {
  // We shouldn't emit global value witness tables for generic type instances.
  assert(!isa<BoundGenericType>(abstractType) &&
         "emitting VWT for generic instance");

  ConstantInitBuilder builder(IGM);
  auto witnesses = builder.beginArray(IGM.Int8PtrTy);

  bool canBeConstant = false;
  addValueWitnessesForAbstractType(IGM, witnesses, abstractType, canBeConstant);

  auto addr = IGM.getAddrOfValueWitnessTable(abstractType,
                                             witnesses.finishAndCreateFuture());
  auto global = cast<llvm::GlobalVariable>(addr);
  global->setConstant(canBeConstant);

  return llvm::ConstantExpr::getBitCast(global, IGM.WitnessTablePtrTy);
}

llvm::Constant *IRGenModule::emitFixedTypeLayout(CanType t,
                                                 const FixedTypeInfo &ti) {
  auto silTy = SILType::getPrimitiveAddressType(t);
  // Collect the interesting information that gets encoded in a type layout
  // record, to see if there's one we can reuse.
  unsigned size = ti.getFixedSize().getValue();
  unsigned align = ti.getFixedAlignment().getValue();

  bool pod = ti.isPOD(ResilienceExpansion::Maximal);
  bool bt = ti.isBitwiseTakable(ResilienceExpansion::Maximal);
  unsigned numExtraInhabitants = ti.getFixedExtraInhabitantCount(*this);

  // Try to use common type layouts exported by the runtime.
  llvm::Constant *commonValueWitnessTable = nullptr;
  if (pod && bt && numExtraInhabitants == 0) {
    if (size == 0)
      commonValueWitnessTable =
        getAddrOfValueWitnessTable(Context.TheEmptyTupleType);
    if (   (size ==  1 && align ==  1)
        || (size ==  2 && align ==  2)
        || (size ==  4 && align ==  4)
        || (size ==  8 && align ==  8)
        || (size == 16 && align == 16)
        || (size == 32 && align == 32))
      commonValueWitnessTable =
        getAddrOfValueWitnessTable(BuiltinIntegerType::get(size * 8, Context)
                                     ->getCanonicalType());
  }

  if (commonValueWitnessTable) {
    auto index = llvm::ConstantInt::get(Int32Ty,
                               (unsigned)ValueWitness::First_TypeLayoutWitness);
    return llvm::ConstantExpr::getGetElementPtr(Int8PtrTy,
                                                commonValueWitnessTable,
                                                index);
  }

  // Otherwise, see if a layout has been emitted with these characteristics
  // already.
  FixedLayoutKey key{size, numExtraInhabitants, align, pod, bt};

  auto found = PrivateFixedLayouts.find(key);
  if (found != PrivateFixedLayouts.end())
    return found->second;

  // Emit the layout values.
  ConstantInitBuilder builder(*this);
  auto witnesses = builder.beginArray(Int8PtrTy);
  FixedPacking packing = ti.getFixedPacking(*this);
  for (auto witness = ValueWitness::First_TypeLayoutWitness;
       witness <= ValueWitness::Last_RequiredTypeLayoutWitness;
       witness = ValueWitness(unsigned(witness) + 1)) {
    addValueWitness(*this, witnesses, witness, packing, t, silTy, ti);
  }

  if (ti.mayHaveExtraInhabitants(*this))
    for (auto witness = ValueWitness::First_ExtraInhabitantValueWitness;
         witness <= ValueWitness::Last_TypeLayoutWitness;
         witness = ValueWitness(unsigned(witness) + 1))
      addValueWitness(*this, witnesses, witness, packing, t, silTy, ti);

  auto layoutVar
    = witnesses.finishAndCreateGlobal(
        "type_layout_" + llvm::Twine(size)
                       + "_" + llvm::Twine(align)
                       + "_" + llvm::Twine::utohexstr(numExtraInhabitants)
                       + (pod ? "_pod" :
                          bt  ? "_bt"  : ""),
                                      getPointerAlignment(),
                                      /*constant*/ true,
                                      llvm::GlobalValue::PrivateLinkage);

  auto zero = llvm::ConstantInt::get(Int32Ty, 0);
  llvm::Constant *indices[] = {zero, zero};
  auto layout = llvm::ConstantExpr::getGetElementPtr(layoutVar->getValueType(),
                                                     layoutVar, indices);

  PrivateFixedLayouts.insert({key, layout});
  return layout;
}

/// Emit the elements of a dependent value witness table template into a
/// vector.
void irgen::emitDependentValueWitnessTablePattern(IRGenModule &IGM,
                                                  ConstantStructBuilder &B,
                                                  CanType abstractType) {
  // We shouldn't emit global value witness tables for generic type instances.
  assert(!isa<BoundGenericType>(abstractType) &&
         "emitting VWT for generic instance");

  // We shouldn't emit global value witness tables for fixed-layout types.
  assert(hasDependentValueWitnessTable(IGM, abstractType) &&
         "emitting VWT pattern for fixed-layout type");

  bool canBeConstant = false;
  auto witnesses = B.beginArray(IGM.Int8PtrTy);
  addValueWitnessesForAbstractType(IGM, witnesses, abstractType, canBeConstant);
  witnesses.finishAndAddTo(B);
}

FixedPacking TypeInfo::getFixedPacking(IRGenModule &IGM) const {
  auto fixedTI = dyn_cast<FixedTypeInfo>(this);

  // If the type isn't fixed, we have to do something dynamic.
  // FIXME: some types are provably too big (or aligned) to be
  // allocated inline.
  if (!fixedTI)
    return FixedPacking::Dynamic;

  Size bufferSize = getFixedBufferSize(IGM);
  Size requiredSize = fixedTI->getFixedSize();

  // Flat out, if we need more space than the buffer provides,
  // we always have to allocate.
  // FIXME: there might be some interesting cases where this
  // is suboptimal for enums.
  if (requiredSize > bufferSize)
    return FixedPacking::Allocate;

  Alignment bufferAlign = getFixedBufferAlignment(IGM);
  Alignment requiredAlign = fixedTI->getFixedAlignment();

  // If the buffer alignment is good enough for the type, great.
  if (bufferAlign >= requiredAlign)
    return FixedPacking::OffsetZero;

  // TODO: consider using a slower mode that dynamically checks
  // whether the buffer size is small enough.

  // Otherwise we're stuck and have to separately allocate.
  return FixedPacking::Allocate;
}

Address TypeInfo::indexArray(IRGenFunction &IGF, Address base,
                             llvm::Value *index, SILType T) const {
  // The stride of a Swift type may not match its LLVM size. If we know we have
  // a fixed stride different from our size, or we have a dynamic size,
  // do a byte-level GEP with the proper stride.
  const FixedTypeInfo *fixedTI = dyn_cast<FixedTypeInfo>(this);

  llvm::Value *destValue = nullptr;
  Size stride(1);
  
  // TODO: Arrays currently lower-bound the stride to 1.
  if (!fixedTI || fixedTI->getFixedStride() != fixedTI->getFixedSize()) {
    llvm::Value *byteAddr = IGF.Builder.CreateBitCast(base.getAddress(),
                                                      IGF.IGM.Int8PtrTy);
    llvm::Value *size = getStride(IGF, T);
    if (size->getType() != index->getType())
      size = IGF.Builder.CreateZExtOrTrunc(size, index->getType());
    llvm::Value *distance = IGF.Builder.CreateNSWMul(index, size);
    destValue = IGF.Builder.CreateInBoundsGEP(byteAddr, distance);
    destValue = IGF.Builder.CreateBitCast(destValue, base.getType());
  } else {
    // We don't expose a non-inbounds GEP operation.
    destValue = IGF.Builder.CreateInBoundsGEP(base.getAddress(), index);
    stride = fixedTI->getFixedStride();
  }
  if (auto *IndexConst = dyn_cast<llvm::ConstantInt>(index)) {
    // If we know the indexing value, we can get a better guess on the
    // alignment.
    // This even works if the stride is not known (and assumed to be 1).
    stride *= IndexConst->getValue().getZExtValue();
  }
  Alignment Align = base.getAlignment().alignmentAtOffset(stride);
  return Address(destValue, Align);
}

Address TypeInfo::roundUpToTypeAlignment(IRGenFunction &IGF, Address base,
                                         SILType T) const {
  Alignment Align = base.getAlignment();
  llvm::Value *TyAlignMask = getAlignmentMask(IGF, T);
  if (auto *TyAlignMaskConst = dyn_cast<llvm::ConstantInt>(TyAlignMask)) {
    Alignment TyAlign(TyAlignMaskConst->getZExtValue() + 1);

    // No need to align if the base is already aligned.
    if (TyAlign <= Align)
      return base;
  }
  llvm::Value *Addr = base.getAddress();
  Addr = IGF.Builder.CreatePtrToInt(Addr, IGF.IGM.IntPtrTy);
  Addr = IGF.Builder.CreateNUWAdd(Addr, TyAlignMask);
  llvm::Value *InvertedMask = IGF.Builder.CreateNot(TyAlignMask);
  Addr = IGF.Builder.CreateAnd(Addr, InvertedMask);
  Addr = IGF.Builder.CreateIntToPtr(Addr, base.getAddress()->getType());
  return Address(Addr, Align);
}

void TypeInfo::destroyArray(IRGenFunction &IGF, Address array,
                            llvm::Value *count, SILType T) const {
  if (isPOD(ResilienceExpansion::Maximal))
    return;

  auto entry = IGF.Builder.GetInsertBlock();
  auto iter = IGF.createBasicBlock("iter");
  auto loop = IGF.createBasicBlock("loop");
  auto exit = IGF.createBasicBlock("exit");
  IGF.Builder.CreateBr(iter);
  IGF.Builder.emitBlock(iter);

  auto counter = IGF.Builder.CreatePHI(IGF.IGM.SizeTy, 2);
  counter->addIncoming(count, entry);
  auto elementVal = IGF.Builder.CreatePHI(array.getType(), 2);
  elementVal->addIncoming(array.getAddress(), entry);
  Address element(elementVal, array.getAlignment());

  auto done = IGF.Builder.CreateICmpEQ(counter,
                                     llvm::ConstantInt::get(IGF.IGM.SizeTy, 0));
  IGF.Builder.CreateCondBr(done, exit, loop);

  IGF.Builder.emitBlock(loop);
  ConditionalDominanceScope condition(IGF);

  destroy(IGF, element, T);
  auto nextCounter = IGF.Builder.CreateSub(counter,
                                   llvm::ConstantInt::get(IGF.IGM.SizeTy, 1));
  auto nextElement = indexArray(IGF, element,
                                llvm::ConstantInt::get(IGF.IGM.SizeTy, 1), T);
  auto loopEnd = IGF.Builder.GetInsertBlock();
  counter->addIncoming(nextCounter, loopEnd);
  elementVal->addIncoming(nextElement.getAddress(), loopEnd);
  IGF.Builder.CreateBr(iter);

  IGF.Builder.emitBlock(exit);
}

/// Build a value witness that initializes an array front-to-back.
void irgen::emitInitializeArrayFrontToBack(IRGenFunction &IGF,
                                           const TypeInfo &type,
                                           Address destArray,
                                           Address srcArray,
                                           llvm::Value *count,
                                           SILType T,
                                           IsTake_t take) {
  auto &IGM = IGF.IGM;

  auto entry = IGF.Builder.GetInsertBlock();
  auto iter = IGF.createBasicBlock("iter");
  auto loop = IGF.createBasicBlock("loop");
  auto exit = IGF.createBasicBlock("exit");
  IGF.Builder.CreateBr(iter);
  IGF.Builder.emitBlock(iter);

  auto counter = IGF.Builder.CreatePHI(IGM.SizeTy, 2);
  counter->addIncoming(count, entry);
  auto destVal = IGF.Builder.CreatePHI(destArray.getType(), 2);
  destVal->addIncoming(destArray.getAddress(), entry);
  auto srcVal = IGF.Builder.CreatePHI(srcArray.getType(), 2);
  srcVal->addIncoming(srcArray.getAddress(), entry);
  Address dest(destVal, destArray.getAlignment());
  Address src(srcVal, srcArray.getAlignment());

  auto done = IGF.Builder.CreateICmpEQ(counter,
                                       llvm::ConstantInt::get(IGM.SizeTy, 0));
  IGF.Builder.CreateCondBr(done, exit, loop);

  IGF.Builder.emitBlock(loop);
  ConditionalDominanceScope condition(IGF);
  type.initialize(IGF, dest, src, take, T);

  auto nextCounter = IGF.Builder.CreateSub(counter,
                                   llvm::ConstantInt::get(IGM.SizeTy, 1));
  auto nextDest = type.indexArray(IGF, dest,
                                  llvm::ConstantInt::get(IGM.SizeTy, 1), T);
  auto nextSrc = type.indexArray(IGF, src,
                                 llvm::ConstantInt::get(IGM.SizeTy, 1), T);
  auto loopEnd = IGF.Builder.GetInsertBlock();
  counter->addIncoming(nextCounter, loopEnd);
  destVal->addIncoming(nextDest.getAddress(), loopEnd);
  srcVal->addIncoming(nextSrc.getAddress(), loopEnd);
  IGF.Builder.CreateBr(iter);

  IGF.Builder.emitBlock(exit);
}

/// Build a value witness that initializes an array back-to-front.
void irgen::emitInitializeArrayBackToFront(IRGenFunction &IGF,
                                           const TypeInfo &type,
                                           Address destArray,
                                           Address srcArray,
                                           llvm::Value *count,
                                           SILType T,
                                           IsTake_t take) {
  auto &IGM = IGF.IGM;

  auto destEnd = type.indexArray(IGF, destArray, count, T);
  auto srcEnd = type.indexArray(IGF, srcArray, count, T);

  auto entry = IGF.Builder.GetInsertBlock();
  auto iter = IGF.createBasicBlock("iter");
  auto loop = IGF.createBasicBlock("loop");
  auto exit = IGF.createBasicBlock("exit");
  IGF.Builder.CreateBr(iter);
  IGF.Builder.emitBlock(iter);

  auto counter = IGF.Builder.CreatePHI(IGM.SizeTy, 2);
  counter->addIncoming(count, entry);
  auto destVal = IGF.Builder.CreatePHI(destEnd.getType(), 2);
  destVal->addIncoming(destEnd.getAddress(), entry);
  auto srcVal = IGF.Builder.CreatePHI(srcEnd.getType(), 2);
  srcVal->addIncoming(srcEnd.getAddress(), entry);
  Address dest(destVal, destArray.getAlignment());
  Address src(srcVal, srcArray.getAlignment());

  auto done = IGF.Builder.CreateICmpEQ(counter,
                                       llvm::ConstantInt::get(IGM.SizeTy, 0));
  IGF.Builder.CreateCondBr(done, exit, loop);

  IGF.Builder.emitBlock(loop);
  ConditionalDominanceScope condition(IGF);
  auto prevDest = type.indexArray(IGF, dest,
                              llvm::ConstantInt::getSigned(IGM.SizeTy, -1), T);
  auto prevSrc = type.indexArray(IGF, src,
                              llvm::ConstantInt::getSigned(IGM.SizeTy, -1), T);

  type.initialize(IGF, prevDest, prevSrc, take, T);

  auto nextCounter = IGF.Builder.CreateSub(counter,
                                   llvm::ConstantInt::get(IGM.SizeTy, 1));
  auto loopEnd = IGF.Builder.GetInsertBlock();
  counter->addIncoming(nextCounter, loopEnd);
  destVal->addIncoming(prevDest.getAddress(), loopEnd);
  srcVal->addIncoming(prevSrc.getAddress(), loopEnd);
  IGF.Builder.CreateBr(iter);

  IGF.Builder.emitBlock(exit);
}

void TypeInfo::initializeArrayWithCopy(IRGenFunction &IGF,
                                       Address dest, Address src,
                                       llvm::Value *count, SILType T) const {
  if (isPOD(ResilienceExpansion::Maximal)) {
    llvm::Value *stride = getStride(IGF, T);
    llvm::Value *byteCount = IGF.Builder.CreateNUWMul(stride, count);
    IGF.Builder.CreateMemCpy(dest.getAddress(), src.getAddress(),
                             byteCount, dest.getAlignment().getValue());
    return;
  }

  emitInitializeArrayFrontToBack(IGF, *this, dest, src, count, T, IsNotTake);
}

void TypeInfo::initializeArrayWithTakeFrontToBack(IRGenFunction &IGF,
                                                  Address dest, Address src,
                                                  llvm::Value *count, SILType T)
const {
  if (isBitwiseTakable(ResilienceExpansion::Maximal)) {
    llvm::Value *stride = getStride(IGF, T);
    llvm::Value *byteCount = IGF.Builder.CreateNUWMul(stride, count);
    IGF.Builder.CreateMemMove(dest.getAddress(), src.getAddress(),
                              byteCount, dest.getAlignment().getValue());
    return;
  }

  emitInitializeArrayFrontToBack(IGF, *this, dest, src, count, T, IsTake);
}

void TypeInfo::initializeArrayWithTakeBackToFront(IRGenFunction &IGF,
                                                  Address dest, Address src,
                                                  llvm::Value *count, SILType T)
const {
  if (isBitwiseTakable(ResilienceExpansion::Maximal)) {
    llvm::Value *stride = getStride(IGF, T);
    llvm::Value *byteCount = IGF.Builder.CreateNUWMul(stride, count);
    IGF.Builder.CreateMemMove(dest.getAddress(), src.getAddress(),
                              byteCount, dest.getAlignment().getValue());
    return;
  }

  emitInitializeArrayBackToFront(IGF, *this, dest, src, count, T, IsTake);
}

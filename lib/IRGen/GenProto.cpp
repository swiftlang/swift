//===--- GenProto.cpp - Swift IR Generation for Protocols -----------------===//
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
//  This file implements IR generation for protocols in Swift.
//
//  Protocols serve two masters: generic algorithms and existential
//  types.  In either case, the size and structure of a type is opaque
//  to the code manipulating a value.  Local values of the type must
//  be stored in fixed-size buffers (which can overflow to use heap
//  allocation), and basic operations on the type must be dynamically
//  delegated to a collection of information that "witnesses" the
//  truth that a particular type implements the protocol.
//
//  In the comments throughout this file, three type names are used:
//    'B' is the type of a fixed-size buffer
//    'T' is the type which implements a protocol
//    'W' is the type of a witness to the protocol
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/MetadataValues.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILWitnessVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

#include "CallEmission.h"
#include "EnumPayload.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "GenArchetype.h"
#include "GenClass.h"
#include "GenEnum.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenOpaque.h"
#include "GenPoly.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "MetadataPath.h"
#include "NecessaryBindings.h"
#include "ProtocolInfo.h"
#include "TypeInfo.h"

#include "GenProto.h"

using namespace swift;
using namespace irgen;


namespace {
  /// A concrete witness table, together with its known layout.
  class WitnessTable {
    llvm::Value *Table;
    const ProtocolInfo &Info;
  public:
    WitnessTable(llvm::Value *wtable, const ProtocolInfo &info)
      : Table(wtable), Info(info) {}

    llvm::Value *getTable() const { return Table; }
    const ProtocolInfo &getInfo() const { return Info; }
  };

  /// A class which lays out a witness table in the abstract.
  class WitnessTableLayout : public SILWitnessVisitor<WitnessTableLayout> {
    unsigned NumWitnesses = 0;
    SmallVector<WitnessTableEntry, 16> Entries;

    WitnessIndex getNextIndex() {
      return WitnessIndex(NumWitnesses++, /*isPrefix=*/false);
    }

  public:
    /// The next witness is an out-of-line base protocol.
    void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
      Entries.push_back(
             WitnessTableEntry::forOutOfLineBase(baseProto, getNextIndex()));
    }

    void addMethod(FuncDecl *func) {
      Entries.push_back(WitnessTableEntry::forFunction(func, getNextIndex()));
    }

    void addConstructor(ConstructorDecl *ctor) {
      Entries.push_back(WitnessTableEntry::forFunction(ctor, getNextIndex()));
    }

    void addAssociatedType(AssociatedTypeDecl *ty,
                           ArrayRef<ProtocolDecl *> protos) {
      // An associated type takes up a spot for the type metadata and for the
      // witnesses to all its conformances.
      Entries.push_back(
                      WitnessTableEntry::forAssociatedType(ty, getNextIndex()));
      for (auto *proto : protos)
        if (Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
          ++NumWitnesses;
    }

    unsigned getNumWitnesses() const { return NumWitnesses; }
    ArrayRef<WitnessTableEntry> getEntries() const { return Entries; }
  };

  /// A path through a protocol hierarchy.
  class ProtocolPath {
    IRGenModule &IGM;

    /// The destination protocol.
    ProtocolDecl *Dest;

    /// The path from the selected origin down to the destination
    /// protocol.
    SmallVector<WitnessIndex, 8> ReversePath;

    /// The origin index to use.
    unsigned OriginIndex;

    /// The best path length we found.
    unsigned BestPathLength;

  public:
    /// Find a path from the given set of origins to the destination
    /// protocol.
    ///
    /// T needs to provide a couple of member functions:
    ///   ProtocolDecl *getProtocol() const;
    ///   const ProtocolInfo &getInfo() const;
    template <class T>
    ProtocolPath(IRGenModule &IGM, ArrayRef<T> origins, ProtocolDecl *dest)
      : IGM(IGM), Dest(dest), BestPathLength(~0U) {

      // Consider each of the origins in turn, breaking out if any of
      // them yields a zero-length path.
      for (unsigned i = 0, e = origins.size(); i != e; ++i) {
        auto &origin = origins[i];
        if (considerOrigin(origin.getProtocol(), origin.getInfo(), i))
          break;
      }

      // Sanity check that we actually found a path at all.
      assert(BestPathLength != ~0U);
      assert(BestPathLength == ReversePath.size());
    }

    /// Returns the index of the origin protocol we chose.
    unsigned getOriginIndex() const { return OriginIndex; }

    /// Apply the path to the given witness table.
    llvm::Value *apply(IRGenFunction &IGF, llvm::Value *wtable) const {
      for (unsigned i = ReversePath.size(); i != 0; --i) {
        wtable = emitInvariantLoadOfOpaqueWitness(IGF, wtable,
                                                  ReversePath[i-1]);
        wtable = IGF.Builder.CreateBitCast(wtable, IGF.IGM.WitnessTablePtrTy);
      }
      return wtable;
    }

  private:
    /// Consider paths starting from a new origin protocol.
    /// Returns true if there's no point in considering other origins.
    bool considerOrigin(ProtocolDecl *origin, const ProtocolInfo &originInfo,
                        unsigned originIndex) {
      assert(BestPathLength != 0);

      // If the origin *is* the destination, we can stop here.
      if (origin == Dest) {
        OriginIndex = originIndex;
        BestPathLength = 0;
        ReversePath.clear();
        return true;
      }

      // Otherwise, if the origin gives rise to a better path, that's
      // also cool.
      if (findBetterPath(origin, originInfo, 0)) {
        OriginIndex = originIndex;
        return BestPathLength == 0;
      }

      return false;
    }

    /// Consider paths starting at the given protocol.
    bool findBetterPath(ProtocolDecl *proto, const ProtocolInfo &protoInfo,
                        unsigned lengthSoFar) {
      assert(lengthSoFar < BestPathLength);
      assert(proto != Dest);

      // Keep track of whether we found a better path than the
      // previous best.
      bool foundBetter = false;
      for (auto base : proto->getInheritedProtocols(nullptr)) {
        // ObjC protocols do not have witnesses.
        if (!Lowering::TypeConverter::protocolRequiresWitnessTable(base))
          continue;

        auto &baseEntry = protoInfo.getWitnessEntry(base);
        assert(baseEntry.isBase());

        // Compute the length down to this base.
        unsigned lengthToBase = lengthSoFar;
        if (baseEntry.isOutOfLineBase()) {
          lengthToBase++;

          // Don't consider this path if we reach a length that can't
          // possibly be better than the best so far.
          if (lengthToBase == BestPathLength) continue;
        }
        assert(lengthToBase < BestPathLength);

        // If this base *is* the destination, go ahead and start
        // building the path into ReversePath.
        if (base == Dest) {
          // Reset the collected best-path information.
          BestPathLength = lengthToBase;
          ReversePath.clear();

        // Otherwise, if there isn't a better path through this base,
        // don't accumulate anything in the path.
        } else if (!findBetterPath(base, IGM.getProtocolInfo(base),
                                   lengthToBase)) {
          continue;
        }

        // Okay, we've found a better path, and ReversePath contains a
        // path leading from base to Dest.
        assert(BestPathLength >= lengthToBase);
        foundBetter = true;

        // Add the link from proto to base if necessary.
        if (baseEntry.isOutOfLineBase()) {
          ReversePath.push_back(baseEntry.getOutOfLineBaseIndex());

        // If it isn't necessary, then we might be able to
        // short-circuit considering the bases of this protocol.
        } else {
          if (lengthSoFar == BestPathLength)
            return true;
        }
      }

      return foundBetter;
    }
  };

} // end anonymous namespace

/// Is there anything about the given conformance that requires witness
/// tables to be dependently-generated?
static bool isDependentConformance(IRGenModule &IGM,
                             const NormalProtocolConformance *conformance,
                                   ResilienceScope resilienceScope) {
  // If the conforming type isn't dependent, this is never true.
  if (!conformance->getDeclContext()->isGenericContext())
    return false;

  // Check whether any of the associated types are dependent.
  if (conformance->forEachTypeWitness(nullptr,
        [&](AssociatedTypeDecl *requirement, const Substitution &sub,
            TypeDecl *explicitDecl) -> bool {
          // RESILIENCE: this could be an opaque conformance
          return sub.getReplacement()->hasArchetype();
         })) {
    return true;
  }

  // Check whether any of the associated types are dependent.
  for (auto &entry : conformance->getInheritedConformances()) {
    if (isDependentConformance(IGM, entry.second->getRootNormalConformance(),
                               resilienceScope)) {
      return true;
    }
  }

  return false;
}

/// Detail about how an object conforms to a protocol.
class irgen::ConformanceInfo {
  friend class ProtocolInfo;
public:
  virtual ~ConformanceInfo() {}
  virtual llvm::Value *getTable(IRGenFunction &IGF,
                                CanType conformingType) const = 0;
  /// Try to get this table as a constant pointer.  This might just
  /// not be supportable at all.
  virtual llvm::Constant *tryGetConstantTable(IRGenModule &IGM,
                                              CanType conformingType) const = 0;
};

static llvm::Value *
emitWitnessTableAccessorCall(IRGenFunction &IGF,
                             const NormalProtocolConformance *conformance,
                             CanType conformingType) {
  auto accessor =
    IGF.IGM.getAddrOfWitnessTableAccessFunction(conformance, NotForDefinition);

  // If the conforming type is generic, the accessor takes the metatype
  // as an argument.
  llvm::CallInst *call;
  if (conformance->getDeclContext()->isGenericContext()) {
    auto metadata = IGF.emitTypeMetadataRef(conformingType);
    call = IGF.Builder.CreateCall(accessor, {metadata});
  } else {
    call = IGF.Builder.CreateCall(accessor, {});
  }

  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotAccessMemory();
  call->setDoesNotThrow();

  return call;
}

/// Fetch the lazy access function for the given conformance of the
/// given type.
static llvm::Function *
getWitnessTableLazyAccessFunction(IRGenModule &IGM,
                                  const NormalProtocolConformance *conformance,
                                  CanType conformingType) {
  assert(!conformingType->hasArchetype());
  llvm::Function *accessor =
    IGM.getAddrOfWitnessTableLazyAccessFunction(conformance, conformingType,
                                                ForDefinition);

  // If we're not supposed to define the accessor, or if we already
  // have defined it, just return the pointer.
  if (!accessor->empty())
    return accessor;

  // Okay, define the accessor.
  auto cacheVariable = cast<llvm::GlobalVariable>(
    IGM.getAddrOfWitnessTableLazyCacheVariable(conformance, conformingType,
                                               ForDefinition));
  emitLazyCacheAccessFunction(IGM, accessor, cacheVariable,
                              [&](IRGenFunction &IGF) -> llvm::Value* {
    return emitWitnessTableAccessorCall(IGF, conformance, conformingType);
  });

  return accessor;
}

namespace {

/// Conformance info for a witness table that can be directly generated.
class DirectConformanceInfo : public ConformanceInfo {
  friend class ProtocolInfo;

  const NormalProtocolConformance *RootConformance;
public:
  DirectConformanceInfo(const NormalProtocolConformance *C)
    : RootConformance(C) {}

  llvm::Value *getTable(IRGenFunction &IGF,
                        CanType conformingType) const override {
    return IGF.IGM.getAddrOfWitnessTable(RootConformance);
  }

  llvm::Constant *tryGetConstantTable(IRGenModule &IGM,
                                      CanType conformingType) const override {
    return IGM.getAddrOfWitnessTable(RootConformance);
  }
};

/// Conformance info for a witness table that is (or may be) dependent.
class AccessorConformanceInfo : public ConformanceInfo {
  friend class ProtocolInfo;

  const NormalProtocolConformance *Conformance;
public:
  AccessorConformanceInfo(const NormalProtocolConformance *C)
    : Conformance(C) {}

  llvm::Value *getTable(IRGenFunction &IGF, CanType type) const override {
    // If the conformance isn't generic, or we're looking up a dependent
    // type, we don't want to / can't cache the result.
    if (!Conformance->getDeclContext()->isGenericContext() ||
        type->hasArchetype()) {
      return emitWitnessTableAccessorCall(IGF, Conformance, type);
    }

    // Otherwise, call a lazy-cache function.
    auto accessor =
      getWitnessTableLazyAccessFunction(IGF.IGM, Conformance, type);
    llvm::CallInst *call = IGF.Builder.CreateCall(accessor, {});
    call->setCallingConv(IGF.IGM.RuntimeCC);
    call->setDoesNotAccessMemory();
    call->setDoesNotThrow();

    return call;
  }

  llvm::Constant *tryGetConstantTable(IRGenModule &IGM,
                                      CanType conformingType) const override {
    return nullptr;
  }
};

} //end anonymous namespace

static bool isNeverAllocated(FixedPacking packing) {
  switch (packing) {
  case FixedPacking::OffsetZero: return true;
  case FixedPacking::Allocate: return false;
  case FixedPacking::Dynamic: return false;
  }
  llvm_unreachable("bad FixedPacking value");
}

namespace {
  /// An operation to be peformed for various kinds of packing.
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
    virtual void complete(IRGenFunction &IGF,
                          SILType T,
                          const TypeInfo &type) = 0;
  };

  /// A class for merging a particular kind of value across control flow.
  template <class T> class DynamicPackingPHIMapping;

  /// An implementation of DynamicPackingPHIMapping for a single LLVM value.
  template <> class DynamicPackingPHIMapping<llvm::Value*> {
    llvm::PHINode *PHI = nullptr;
  public:
    void collect(IRGenFunction &IGF, SILType T,
                 const TypeInfo &type, llvm::Value *value) {
      // Add the result to the phi, creating it (unparented) if necessary.
      if (!PHI) PHI = llvm::PHINode::Create(value->getType(), 2,
                                            "dynamic-packing.result");
      PHI->addIncoming(value, IGF.Builder.GetInsertBlock());
    }
    void complete(IRGenFunction &IGF, SILType T, const TypeInfo &type) {
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
    void collect(IRGenFunction &IGF, SILType T,
                 const TypeInfo &type, Address value) {
      super::collect(IGF, T, type, value.getAddress());
    }
    void complete(IRGenFunction &IGF, SILType T,
                  const TypeInfo &type) {
      super::complete(IGF, T, type);
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
      Mapping.collect(IGF, T, type, Fn(IGF, T, type, packing));
    }

    void complete(IRGenFunction &IGF, SILType T,
                  const TypeInfo &type) override {
      Mapping.complete(IGF, T, type);
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
    void complete(IRGenFunction &IGF, SILType T,
                  const TypeInfo &type) override {}
    void get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {}
  };
}

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
  IGF.Builder.emitBlock(indirectBB);
  operation.emitForPacking(IGF, T, type, FixedPacking::Allocate);
  IGF.Builder.CreateBr(contBB);

  // Emit the direct path.
  IGF.Builder.emitBlock(directBB);
  operation.emitForPacking(IGF, T, type, FixedPacking::OffsetZero);
  IGF.Builder.CreateBr(contBB);

  // Enter the continuation block and add the PHI if required.
  IGF.Builder.emitBlock(contBB);
  operation.complete(IGF, T, type);
}

/// A helper function for creating a lambda-based DynamicPackingOperation.
template <class ResultTy, class FnTy>
LambdaDynamicPackingOperation<ResultTy, FnTy>
makeLambdaDynamicPackingOperation(FnTy &&fn) {
  return LambdaDynamicPackingOperation<ResultTy, FnTy>(std::move(fn));
}

/// Perform an operation on a type that requires dynamic packing.
template <class ResultTy, class... ArgTys>
static ResultTy emitForDynamicPacking(IRGenFunction &IGF,
                                      ResultTy (*fn)(IRGenFunction &IGF,
                                                     SILType T,
                                                     const TypeInfo &type,
                                                     FixedPacking packing,
                                                     ArgTys... args),
                                      SILType T,
                                      const TypeInfo &type,
                        // using enable_if to block template argument deduction
                        typename std::enable_if<true,ArgTys>::type... args) {
  auto operation = makeLambdaDynamicPackingOperation<ResultTy>(
    [&](IRGenFunction &IGF, SILType T, const TypeInfo &type, FixedPacking packing) {
      return fn(IGF, T, type, packing, args...);
    });
  emitDynamicPackingOperation(IGF, T, type, operation);
  return operation.get(IGF, T, type);
}

/// Emit a 'projectBuffer' operation.  Always returns a T*.
static Address emitProjectBuffer(IRGenFunction &IGF,
                                 SILType T,
                                 const TypeInfo &type,
                                 FixedPacking packing,
                                 Address buffer) {
  llvm::PointerType *resultTy = type.getStorageType()->getPointerTo();
  switch (packing) {
  case FixedPacking::Allocate: {
    Address slot = IGF.Builder.CreateBitCast(buffer, resultTy->getPointerTo(),
                                             "storage-slot");
    llvm::Value *address = IGF.Builder.CreateLoad(slot);
    return type.getAddressForPointer(address);
  }

  case FixedPacking::OffsetZero: {
    return IGF.Builder.CreateBitCast(buffer, resultTy, "object");
  }

  case FixedPacking::Dynamic:
    return emitForDynamicPacking(IGF, &emitProjectBuffer, T, type, buffer);

  }
  llvm_unreachable("bad packing!");

}
namespace swift { namespace irgen { using ::emitProjectBuffer; } }

/// Project to the address of a value in a value buffer.
Address irgen::emitProjectBuffer(IRGenFunction &IGF, SILType valueType,
                                 Address buffer) {
  const TypeInfo &valueTI = IGF.getTypeInfo(valueType);
  FixedPacking packing = valueTI.getFixedPacking(IGF.IGM);
  return ::emitProjectBuffer(IGF, valueType, valueTI, packing, buffer);
}

/// Emit an 'allocateBuffer' operation.  Always returns a T*.
static Address emitAllocateBuffer(IRGenFunction &IGF,
                                  SILType T,
                                  const TypeInfo &type,
                                  FixedPacking packing,
                                  Address buffer) {
  switch (packing) {
  case FixedPacking::Allocate: {
    auto sizeAndAlign = type.getSizeAndAlignmentMask(IGF, T);
    llvm::Value *addr =
      IGF.emitAllocRawCall(sizeAndAlign.first, sizeAndAlign.second);
    buffer = IGF.Builder.CreateBitCast(buffer, IGF.IGM.Int8PtrPtrTy);
    IGF.Builder.CreateStore(addr, buffer);

    addr = IGF.Builder.CreateBitCast(addr,
                                     type.getStorageType()->getPointerTo());
    return type.getAddressForPointer(addr);
  }

  case FixedPacking::OffsetZero:
    return emitProjectBuffer(IGF, T, type, packing, buffer);

  case FixedPacking::Dynamic:
    return emitForDynamicPacking(IGF, &emitAllocateBuffer, T, type, buffer);
  }
  llvm_unreachable("bad packing!");
}
namespace swift { namespace irgen { using ::emitAllocateBuffer; } }

/// Allocate space for a value in a value buffer.
Address irgen::emitAllocateBuffer(IRGenFunction &IGF, SILType valueType,
                                  Address buffer) {
  const TypeInfo &valueTI = IGF.getTypeInfo(valueType);
  FixedPacking packing = valueTI.getFixedPacking(IGF.IGM);
  return emitAllocateBuffer(IGF, valueType, valueTI, packing, buffer);
}

/// Emit a 'deallocateBuffer' operation.
static void emitDeallocateBuffer(IRGenFunction &IGF,
                                 SILType T,
                                 const TypeInfo &type,
                                 FixedPacking packing,
                                 Address buffer) {
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
    return emitForDynamicPacking(IGF, &emitDeallocateBuffer, T, type, buffer);
  }
  llvm_unreachable("bad packing!");
}
namespace swift { namespace irgen { using ::emitDeallocateBuffer; } }

/// Deallocate space for a value in a value buffer.
void irgen::emitDeallocateBuffer(IRGenFunction &IGF, SILType valueType,
                                 Address buffer) {
  const TypeInfo &valueTI = IGF.getTypeInfo(valueType);
  FixedPacking packing = valueTI.getFixedPacking(IGF.IGM);
  emitDeallocateBuffer(IGF, valueType, valueTI, packing, buffer);
}

/// Emit a 'destroyBuffer' operation.
static void emitDestroyBuffer(IRGenFunction &IGF,
                              SILType T,
                              const TypeInfo &type,
                              FixedPacking packing,
                              Address buffer) {
  // Special-case dynamic packing in order to thread the jumps.
  if (packing == FixedPacking::Dynamic)
    return emitForDynamicPacking(IGF, &emitDestroyBuffer, T, type, buffer);

  Address object = emitProjectBuffer(IGF, T, type, packing, buffer);
  type.destroy(IGF, object, T);
  emitDeallocateBuffer(IGF, T, type, packing, buffer);
}

/// Emit an 'initializeWithCopy' operation.
static void emitInitializeWithCopy(IRGenFunction &IGF,
                                   SILType T,
                                   const TypeInfo &type,
                                   Address dest, Address src) {
  type.initializeWithCopy(IGF, dest, src, T);
}

/// Emit an 'initializeWithTake' operation.
static void emitInitializeWithTake(IRGenFunction &IGF,
                                   SILType T,
                                   const TypeInfo &type,
                                   Address dest, Address src) {
  type.initializeWithTake(IGF, dest, src, T);
}

/// Emit an 'initializeBufferWithCopyOfBuffer' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                                    SILType T,
                                                    const TypeInfo &type,
                                                    FixedPacking packing,
                                                    Address dest,
                                                    Address src) {
  // Special-case dynamic packing in order to thread the jumps.
  if (packing == FixedPacking::Dynamic)
    return emitForDynamicPacking(IGF, &emitInitializeBufferWithCopyOfBuffer,
                                 T, type, dest, src);

  Address destObject = emitAllocateBuffer(IGF, T, type, packing, dest);
  Address srcObject = emitProjectBuffer(IGF, T, type, packing, src);
  emitInitializeWithCopy(IGF, T, type, destObject, srcObject);
  return destObject;
}

/// Emit an 'initializeBufferWithTakeOfBuffer' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithTakeOfBuffer(IRGenFunction &IGF,
                                                    SILType T,
                                                    const TypeInfo &type,
                                                    FixedPacking packing,
                                                    Address dest,
                                                    Address src) {
  switch (packing) {

  case FixedPacking::Dynamic:
    // Special-case dynamic packing in order to thread the jumps.
    return emitForDynamicPacking(IGF, &emitInitializeBufferWithTakeOfBuffer,
                                 T, type, dest, src);

  case FixedPacking::OffsetZero: {
    // Both of these allocations/projections should be no-ops.
    Address destObject = emitAllocateBuffer(IGF, T, type, packing, dest);
    Address srcObject = emitProjectBuffer(IGF, T, type, packing, src);
    emitInitializeWithTake(IGF, T, type, destObject, srcObject);
    return destObject;
  }

  case FixedPacking::Allocate: {
    // Just copy the out-of-line storage pointers.
    llvm::Type *ptrTy = type.getStorageType()->getPointerTo()->getPointerTo();
    src = IGF.Builder.CreateBitCast(src, ptrTy);
    llvm::Value *addr = IGF.Builder.CreateLoad(src);
    dest = IGF.Builder.CreateBitCast(dest, ptrTy);
    IGF.Builder.CreateStore(addr, dest);
    return type.getAddressForPointer(addr);
  }
  }
  llvm_unreachable("bad fixed packing");
}

/// Emit an 'initializeBufferWithCopy' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithCopy(IRGenFunction &IGF,
                                            SILType T,
                                            const TypeInfo &type,
                                            FixedPacking packing,
                                            Address dest,
                                            Address srcObject) {
  Address destObject = emitAllocateBuffer(IGF, T, type, packing, dest);
  emitInitializeWithCopy(IGF, T, type, destObject, srcObject);
  return destObject;
}

/// Emit an 'initializeBufferWithTake' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithTake(IRGenFunction &IGF,
                                            SILType T,
                                            const TypeInfo &type,
                                            FixedPacking packing,
                                            Address dest,
                                            Address srcObject) {
  Address destObject = emitAllocateBuffer(IGF, T, type, packing, dest);
  emitInitializeWithTake(IGF, T, type, destObject, srcObject);
  return destObject;
}

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

/// Get the next argument and use it as the 'self' type metadata.
static void getArgAsLocalSelfTypeMetadata(IRGenFunction &IGF,
                                          llvm::Function::arg_iterator &it,
                                          CanType abstractType);

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
    Address result = emitAllocateBuffer(IGF, concreteType, type, packing, buffer);
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
    emitDeallocateBuffer(IGF, concreteType, type, packing, buffer);
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
    emitDestroyBuffer(IGF, concreteType, type, packing, buffer);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAsBuffer(IGF, argv, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithCopyOfBuffer(IGF, concreteType,
                                           type, packing, dest, src);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTakeOfBuffer: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAsBuffer(IGF, argv, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithTakeOfBuffer(IGF, concreteType,
                                           type, packing, dest, src);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithCopy: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithCopy(IGF, concreteType, type, packing, dest, src);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTake: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithTake(IGF, concreteType, type, packing, dest, src);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    emitInitializeWithCopy(IGF, concreteType, type, dest, src);
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

    emitInitializeWithTake(IGF, concreteType, type, dest, src);
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

    Address result = emitProjectBuffer(IGF, concreteType, type, packing, buffer);
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
    result = IGF.Builder.CreateZExtOrTrunc(result, IGF.IGM.Int32Ty);

    IGF.Builder.CreateRet(result);
    return;
  }

  case ValueWitness::DestructiveProjectEnumData: {
    llvm::Value *value = getArg(argv, "value");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    auto &strategy = getEnumImplStrategy(IGM, concreteType);
    if (strategy.getElementsWithPayload().size() > 0) {
      strategy.destructiveProjectDataForLoad(
          IGF, concreteType,
          Address(value, type.getBestKnownAlignment()));
    }

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

static llvm::Constant *asOpaquePtr(IRGenModule &IGM, llvm::Constant *in) {
  return llvm::ConstantExpr::getBitCast(in, IGM.Int8PtrTy);
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
    IGF.emitRetainCall(newValue);
    llvm::Value *oldValue = IGF.Builder.CreateLoad(dest, "old");
    IGF.Builder.CreateStore(newValue, dest);
    IGF.emitRelease(oldValue);

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
    IGF.emitRelease(oldValue);

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
    IGF.emitRetainCall(newValue);
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
    Address arg(IGF.CurFn->arg_begin(), IGM.getPointerAlignment());
    IGF.emitRelease(IGF.Builder.CreateLoad(arg));
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
    Address dest(it++, fixedTI->getFixedAlignment());
    Address src(it++, fixedTI->getFixedAlignment());
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
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());
    auto ptr = IGF.Builder.CreateLoad(src);
    IGF.Builder.CreateStore(ptr, dest);
    IGF.Builder.CreateRet(ptr);
  });
}

namespace {
  enum class MemMoveOrCpy { MemMove, MemCpy };
}

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
    Address dest(it++, fixedTI.getFixedAlignment());
    Address src(it++, fixedTI.getFixedAlignment());
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
/// Always returns an i8*.
static llvm::Constant *getValueWitness(IRGenModule &IGM,
                                       ValueWitness index,
                                       FixedPacking packing,
                                       CanType abstractType,
                                       SILType concreteType,
                                       const TypeInfo &concreteTI) {
  // Try to use a standard function.
  switch (index) {
  case ValueWitness::DeallocateBuffer:
    if (isNeverAllocated(packing))
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    goto standard;

  case ValueWitness::DestroyBuffer:
    if (concreteTI.isPOD(ResilienceScope::Component)) {
      if (isNeverAllocated(packing))
        return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Component)) {
      assert(isNeverAllocated(packing));
      return asOpaquePtr(IGM, getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::Destroy:
    if (concreteTI.isPOD(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::DestroyArray:
    if (concreteTI.isPOD(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    }
    // TODO: A standard "destroy strong array" entrypoint for arrays of single
    // refcounted pointer types.
    goto standard;

  case ValueWitness::InitializeBufferWithCopyOfBuffer:
  case ValueWitness::InitializeBufferWithCopy:
    if (packing == FixedPacking::OffsetZero) {
      if (concreteTI.isPOD(ResilienceScope::Component)) {
        return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
      } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Component)) {
        return asOpaquePtr(IGM, getInitWithCopyStrongFunction(IGM));
      }
    }
    goto standard;

  case ValueWitness::InitializeBufferWithTakeOfBuffer:
    if (packing == FixedPacking::Allocate) {
      return asOpaquePtr(IGM, getCopyOutOfLinePointerFunction(IGM));
    } else if (packing == FixedPacking::OffsetZero &&
               concreteTI.isBitwiseTakable(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeBufferWithTake:
    if (concreteTI.isBitwiseTakable(ResilienceScope::Component)
        && packing == FixedPacking::OffsetZero)
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    goto standard;

  case ValueWitness::InitializeWithTake:
    if (concreteTI.isBitwiseTakable(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithTakeFrontToBack:
    if (concreteTI.isBitwiseTakable(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getMemMoveArrayFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithTakeBackToFront:
    if (concreteTI.isBitwiseTakable(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getMemMoveArrayFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::AssignWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getAssignWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::AssignWithTake:
    if (concreteTI.isPOD(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getAssignWithTakeStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getInitWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Component)) {
      return asOpaquePtr(IGM, getMemCpyArrayFunction(IGM, concreteTI));
    }
    // TODO: A standard "copy strong array" entrypoint for arrays of single
    // refcounted pointer types.
    goto standard;

  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer:
    if (packing == FixedPacking::OffsetZero)
      return asOpaquePtr(IGM, getReturnSelfFunction(IGM));
    goto standard;

  case ValueWitness::Size: {
    if (auto value = concreteTI.getStaticSize(IGM))
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);

    // Just fill in null here if the type can't be statically laid out.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  case ValueWitness::Flags: {
    uint64_t flags = 0;

    // If we locally know that the type has fixed layout, we can emit
    // meaningful flags for it.
    if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&concreteTI)) {
      flags |= fixedTI->getFixedAlignment().getValue() - 1;
      if (!fixedTI->isPOD(ResilienceScope::Component))
        flags |= ValueWitnessFlags::IsNonPOD;
      assert(packing == FixedPacking::OffsetZero ||
             packing == FixedPacking::Allocate);
      if (packing != FixedPacking::OffsetZero)
        flags |= ValueWitnessFlags::IsNonInline;

      if (fixedTI->getFixedExtraInhabitantCount(IGM) > 0)
        flags |= ValueWitnessFlags::Enum_HasExtraInhabitants;

      if (!fixedTI->isBitwiseTakable(ResilienceScope::Component))
        flags |= ValueWitnessFlags::IsNonBitwiseTakable;
    }

    if (concreteType.getEnumOrBoundGenericEnum())
      flags |= ValueWitnessFlags::HasEnumWitnesses;

    auto value = IGM.getSize(Size(flags));
    return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);
  }

  case ValueWitness::Stride: {
    if (auto value = concreteTI.getStaticStride(IGM))
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);

    // Just fill in null here if the type can't be statically laid out.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  case ValueWitness::StoreExtraInhabitant:
  case ValueWitness::GetExtraInhabitantIndex: {
    if (!concreteTI.mayHaveExtraInhabitants(IGM)) {
      assert(concreteType.getEnumOrBoundGenericEnum());
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }

    goto standard;
  }

  case ValueWitness::ExtraInhabitantFlags: {
    if (!concreteTI.mayHaveExtraInhabitants(IGM)) {
      assert(concreteType.getEnumOrBoundGenericEnum());
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }

    // If we locally know that the type has fixed layout, we can emit
    // meaningful flags for it.
    if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&concreteTI)) {
      uint64_t numExtraInhabitants = fixedTI->getFixedExtraInhabitantCount(IGM);
      assert(numExtraInhabitants <= ExtraInhabitantFlags::NumExtraInhabitantsMask);
      auto value = IGM.getSize(Size(numExtraInhabitants));
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);
    }

    // Otherwise, just fill in null here if the type can't be statically
    // queried for extra inhabitants.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  case ValueWitness::GetEnumTag:
  case ValueWitness::DestructiveProjectEnumData:
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
  return asOpaquePtr(IGM, fn);
}

namespace {
  /// A class which lays out a specific conformance to a protocol.
  class WitnessTableBuilder : public SILWitnessVisitor<WitnessTableBuilder> {
    IRGenModule &IGM;
    SmallVectorImpl<llvm::Constant*> &Table;
    CanType ConcreteType;
    GenericParamList *ConcreteGenerics = nullptr;
    const TypeInfo &ConcreteTI;
    const ProtocolConformance &Conformance;
    ArrayRef<Substitution> Substitutions;
    ArrayRef<SILWitnessTable::Entry> SILEntries;
#ifndef NDEBUG
    const ProtocolInfo &PI;
#endif

    void computeSubstitutionsForType() {
      // FIXME: This is a bit of a hack; the AST doesn't directly encode
      // substitutions for the conformance of a generic type to a
      // protocol, so we have to dig them out.
      Type ty = ConcreteType;
      while (ty) {
        if (auto nomTy = ty->getAs<NominalType>())
          ty = nomTy->getParent();
        else
          break;
      }
      if (ty) {
        if (auto boundTy = ty->getAs<BoundGenericType>()) {
          ConcreteGenerics = boundTy->getDecl()->getGenericParams();
          Substitutions = boundTy->getSubstitutions(/*FIXME:*/nullptr, nullptr);
        } else {
          assert(!ty || !ty->isSpecialized());
        }
      }
    }

  public:
    WitnessTableBuilder(IRGenModule &IGM,
                        SmallVectorImpl<llvm::Constant*> &table,
                        SILWitnessTable *SILWT)
      : IGM(IGM), Table(table),
        ConcreteType(SILWT->getConformance()->getType()->getCanonicalType()),
        ConcreteTI(
               IGM.getTypeInfoForUnlowered(SILWT->getConformance()->getType())),
        Conformance(*SILWT->getConformance()),
        SILEntries(SILWT->getEntries())
#ifndef NDEBUG
        , PI(IGM.getProtocolInfo(SILWT->getConformance()->getProtocol()))
#endif
    {
      computeSubstitutionsForType();
    }

    /// A base protocol is witnessed by a pointer to the conformance
    /// of this type to that protocol.
    void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
#ifndef NDEBUG
      auto &entry = SILEntries.front();
      assert(entry.getKind() == SILWitnessTable::BaseProtocol
             && "sil witness table does not match protocol");
      assert(entry.getBaseProtocolWitness().Requirement == baseProto
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(baseProto);
      assert(piEntry.getOutOfLineBaseIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif
      
      SILEntries = SILEntries.slice(1);

      // TODO: Use the witness entry instead of falling through here.

      // Look for a protocol type info.
      const ProtocolInfo &basePI = IGM.getProtocolInfo(baseProto);
      const ProtocolConformance *astConf
        = Conformance.getInheritedConformance(baseProto);
      const ConformanceInfo &conf =
        basePI.getConformance(IGM, baseProto, astConf);

      llvm::Constant *baseWitness = conf.tryGetConstantTable(IGM, ConcreteType);
      assert(baseWitness && "couldn't get a constant table!");
      Table.push_back(asOpaquePtr(IGM, baseWitness));
    }

    void addMethodFromSILWitnessTable(AbstractFunctionDecl *iface) {
      auto &entry = SILEntries.front();
      SILEntries = SILEntries.slice(1);

      // Handle missing optional requirements.
      if (entry.getKind() == SILWitnessTable::MissingOptional) {
        Table.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
        return;
      }

#ifndef NDEBUG
      assert(entry.getKind() == SILWitnessTable::Method
             && "sil witness table does not match protocol");
      assert(entry.getMethodWitness().Requirement.getDecl() == iface
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(iface);
      assert(piEntry.getFunctionIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif

      SILFunction *Func = entry.getMethodWitness().Witness;
      llvm::Constant *witness = nullptr;
      if (Func) {
        witness = IGM.getAddrOfSILFunction(Func, NotForDefinition);
        witness = llvm::ConstantExpr::getBitCast(witness, IGM.Int8PtrTy);
      } else {
        // The method is removed by dead method elimination.
        // It should be never called. We add a pointer to an error function.
        witness = llvm::ConstantExpr::getBitCast(IGM.getDeadMethodErrorFn(),
                                                 IGM.Int8PtrTy);
      }
      Table.push_back(witness);
      return;
    }

    void addMethod(FuncDecl *iface) {
      return addMethodFromSILWitnessTable(iface);
    }

    void addConstructor(ConstructorDecl *iface) {
      return addMethodFromSILWitnessTable(iface);
    }

    void addAssociatedType(AssociatedTypeDecl *ty,
                           ArrayRef<ProtocolDecl *> protos) {
#ifndef NDEBUG
      auto &entry = SILEntries.front();
      assert(entry.getKind() == SILWitnessTable::AssociatedType
             && "sil witness table does not match protocol");
      assert(entry.getAssociatedTypeWitness().Requirement == ty
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(ty);
      assert(piEntry.getAssociatedTypeIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif

      SILEntries = SILEntries.slice(1);

      // FIXME: Use info from SILWitnessTable instead of falling through.

      // Determine whether the associated type has static metadata. If it
      // doesn't, then this witness table is a template that requires runtime
      // instantiation.

      // FIXME: Add static type metadata.
      Table.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));

      // FIXME: Add static witness tables for type conformances.
      for (auto protocol : protos) {
        if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
          continue;

        auto &entry = SILEntries.front();
        (void)entry;
        assert(entry.getKind() == SILWitnessTable::AssociatedTypeProtocol
               && "sil witness table does not match protocol");
        assert(entry.getAssociatedTypeProtocolWitness().Requirement == ty
               && "sil witness table does not match protocol");
        assert(entry.getAssociatedTypeProtocolWitness().Protocol == protocol
               && "sil witness table does not match protocol");

        SILEntries = SILEntries.slice(1);

        // FIXME: Use info from SILWitnessTable instead of falling through.
        // FIXME: Add static witness table reference.
        Table.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
      }
    }
  };
}

/// Collect the value witnesses for a particular type.
static void addValueWitnesses(IRGenModule &IGM, FixedPacking packing,
                              CanType abstractType,
                              SILType concreteType, const TypeInfo &concreteTI,
                              SmallVectorImpl<llvm::Constant*> &table) {
  for (unsigned i = 0; i != NumRequiredValueWitnesses; ++i) {
    table.push_back(getValueWitness(IGM, ValueWitness(i),
                                    packing, abstractType, concreteType,
                                    concreteTI));
  }
  if (concreteType.getEnumOrBoundGenericEnum() ||
      concreteTI.mayHaveExtraInhabitants(IGM)) {
    for (auto i = unsigned(ValueWitness::First_ExtraInhabitantValueWitness);
         i <= unsigned(ValueWitness::Last_ExtraInhabitantValueWitness);
         ++i) {
      table.push_back(getValueWitness(IGM, ValueWitness(i), packing,
                                      abstractType, concreteType, concreteTI));
    }
  }
  if (concreteType.getEnumOrBoundGenericEnum()) {
    for (auto i = unsigned(ValueWitness::First_EnumValueWitness);
         i <= unsigned(ValueWitness::Last_EnumValueWitness);
         ++i) {
      table.push_back(getValueWitness(IGM, ValueWitness(i), packing,
                                      abstractType, concreteType, concreteTI));
    }
  }
}

/// True if a type has a generic-parameter-dependent value witness table.
/// Currently, this is true if the size and/or alignment of the type is
/// dependent on its generic parameters.
bool irgen::hasDependentValueWitnessTable(IRGenModule &IGM, CanType ty) {
  if (auto ugt = dyn_cast<UnboundGenericType>(ty))
    ty = ugt->getDecl()->getDeclaredTypeInContext()->getCanonicalType();

  return !IGM.getTypeInfoForUnlowered(ty).isFixedSize();
}

static void addValueWitnessesForAbstractType(IRGenModule &IGM,
                                 CanType abstractType,
                                 SmallVectorImpl<llvm::Constant*> &witnesses) {
  // Instantiate unbound generic types on their context archetypes.
  CanType concreteFormalType = abstractType;
  if (auto ugt = dyn_cast<UnboundGenericType>(abstractType)) {
    concreteFormalType = ugt->getDecl()->getDeclaredTypeInContext()->getCanonicalType();
  }

  auto concreteLoweredType = IGM.SILMod->Types.getLoweredType(concreteFormalType);
  auto &concreteTI = IGM.getTypeInfo(concreteLoweredType);
  FixedPacking packing = concreteTI.getFixedPacking(IGM);

  addValueWitnesses(IGM, packing, abstractType,
                    concreteLoweredType, concreteTI, witnesses);
}

/// Emit a value-witness table for the given type, which is assumed to
/// be non-dependent.
llvm::Constant *irgen::emitValueWitnessTable(IRGenModule &IGM,
                                             CanType abstractType) {
  // We shouldn't emit global value witness tables for generic type instances.
  assert(!isa<BoundGenericType>(abstractType) &&
         "emitting VWT for generic instance");

  // We shouldn't emit global value witness tables for non-fixed-layout types.
  assert(!hasDependentValueWitnessTable(IGM, abstractType) &&
         "emitting global VWT for dynamic-layout type");

  SmallVector<llvm::Constant*, MaxNumValueWitnesses> witnesses;
  addValueWitnessesForAbstractType(IGM, abstractType, witnesses);

  auto tableTy = llvm::ArrayType::get(IGM.Int8PtrTy, witnesses.size());
  auto table = llvm::ConstantArray::get(tableTy, witnesses);

  auto addr = IGM.getAddrOfValueWitnessTable(abstractType, table->getType());
  auto global = cast<llvm::GlobalVariable>(addr);
  global->setConstant(true);
  global->setInitializer(table);

  return llvm::ConstantExpr::getBitCast(global, IGM.WitnessTablePtrTy);
}

llvm::Constant *IRGenModule::emitFixedTypeLayout(CanType t,
                                                 const FixedTypeInfo &ti) {
  auto silTy = SILType::getPrimitiveAddressType(t);
  // Collect the interesting information that gets encoded in a type layout
  // record, to see if there's one we can reuse.
  unsigned size = ti.getFixedSize().getValue();
  unsigned align = ti.getFixedAlignment().getValue();

  bool pod = ti.isPOD(ResilienceScope::Component);
  bool bt = ti.isBitwiseTakable(ResilienceScope::Component);
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
  SmallVector<llvm::Constant *, MaxNumTypeLayoutWitnesses> witnesses;
  FixedPacking packing = ti.getFixedPacking(*this);
  for (auto witness = ValueWitness::First_TypeLayoutWitness;
       witness <= ValueWitness::Last_RequiredTypeLayoutWitness;
       witness = ValueWitness(unsigned(witness) + 1)) {
    witnesses.push_back(getValueWitness(*this, witness,
                                        packing, t, silTy, ti));
  }

  if (ti.mayHaveExtraInhabitants(*this))
    for (auto witness = ValueWitness::First_ExtraInhabitantValueWitness;
         witness <= ValueWitness::Last_TypeLayoutWitness;
         witness = ValueWitness(unsigned(witness) + 1))
      witnesses.push_back(getValueWitness(*this, witness,
                                          packing, t, silTy, ti));

  auto layoutTy = llvm::ArrayType::get(Int8PtrTy, witnesses.size());
  auto layoutVal = llvm::ConstantArray::get(layoutTy, witnesses);

  llvm::Constant *layoutVar
    = new llvm::GlobalVariable(Module, layoutTy, /*constant*/ true,
        llvm::GlobalValue::PrivateLinkage, layoutVal,
        "type_layout_" + llvm::Twine(size)
                       + "_" + llvm::Twine(align)
                       + "_" + llvm::Twine::utohexstr(numExtraInhabitants)
                       + (pod ? "_pod" :
                          bt  ? "_bt"  : ""));

  auto zero = llvm::ConstantInt::get(Int32Ty, 0);
  llvm::Constant *indices[] = {zero, zero};
  layoutVar = llvm::ConstantExpr::getGetElementPtr(layoutTy, layoutVar,
                                                   indices);

  PrivateFixedLayouts.insert({key, layoutVar});
  return layoutVar;
}

/// Emit the elements of a dependent value witness table template into a
/// vector.
void irgen::emitDependentValueWitnessTablePattern(IRGenModule &IGM,
                                    CanType abstractType,
                                    SmallVectorImpl<llvm::Constant*> &fields) {
  // We shouldn't emit global value witness tables for generic type instances.
  assert(!isa<BoundGenericType>(abstractType) &&
         "emitting VWT for generic instance");

  // We shouldn't emit global value witness tables for fixed-layout types.
  assert(hasDependentValueWitnessTable(IGM, abstractType) &&
         "emitting VWT pattern for fixed-layout type");

  addValueWitnessesForAbstractType(IGM, abstractType, fields);
}

/// Do a memoized witness-table layout for a protocol.
const ProtocolInfo &IRGenModule::getProtocolInfo(ProtocolDecl *protocol) {
  return Types.getProtocolInfo(protocol);
}

/// Do a memoized witness-table layout for a protocol.
const ProtocolInfo &TypeConverter::getProtocolInfo(ProtocolDecl *protocol) {
  // Check whether we've already translated this protocol.
  auto it = Protocols.find(protocol);
  if (it != Protocols.end()) return *it->second;

  // If not, lay out the protocol's witness table, if it needs one.
  WitnessTableLayout layout;
  if (Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
    layout.visitProtocolDecl(protocol);

  // Create a ProtocolInfo object from the layout.
  ProtocolInfo *info = ProtocolInfo::create(layout.getNumWitnesses(),
                                            layout.getEntries());
  info->NextConverted = FirstProtocol;
  FirstProtocol = info;

  // Memoize.
  Protocols.insert(std::make_pair(protocol, info));

  // Done.
  return *info;
}

/// Allocate a new ProtocolInfo.
ProtocolInfo *ProtocolInfo::create(unsigned numWitnesses,
                                   ArrayRef<WitnessTableEntry> table) {
  unsigned numEntries = table.size();
  size_t bufferSize =
    sizeof(ProtocolInfo) + numEntries * sizeof(WitnessTableEntry);
  void *buffer = ::operator new(bufferSize);
  return new(buffer) ProtocolInfo(numWitnesses, table);
}

ProtocolInfo::~ProtocolInfo() {
  for (auto &conf : Conformances) {
    delete conf.second;
  }
}

/// Find the conformance information for a protocol.
const ConformanceInfo &
ProtocolInfo::getConformance(IRGenModule &IGM, ProtocolDecl *protocol,
                             const ProtocolConformance *conformance) const {
  // Drill down to the root normal conformance.
  auto normalConformance = conformance->getRootNormalConformance();

  // Check whether we've already cached this.
  auto it = Conformances.find(normalConformance);
  if (it != Conformances.end()) return *it->second;

  ConformanceInfo *info;

  // If the conformance is dependent in any way, we need to unique it.
  // TODO: maybe this should apply whenever it's out of the module?
  // TODO: actually enable this
  if ((false) &&
      isDependentConformance(IGM, normalConformance,
                             ResilienceScope::Component)) {
    info = new AccessorConformanceInfo(normalConformance);

  // Otherwise, we can use a direct-referencing conformance.
  } else {
    info = new DirectConformanceInfo(normalConformance);
  }

  Conformances.insert({normalConformance, info});
  return *info;
}

void IRGenModule::emitSILWitnessTable(SILWitnessTable *wt) {
  // Don't emit a witness table if it is a declaration.
  if (wt->isDeclaration())
    return;
  // Don't emit a witness table that is available externally if we are emitting
  // code for the JIT. We do not do any optimization for the JIT and it has
  // problems with external symbols that get merged with non-external symbols.
  if (Opts.UseJIT && isAvailableExternally(wt->getLinkage()))
    return;

  // Build the witnesses.
  SmallVector<llvm::Constant*, 32> witnesses;
  WitnessTableBuilder(*this, witnesses, wt)
    .visitProtocolDecl(wt->getConformance()->getProtocol());
  
  assert(getProtocolInfo(wt->getConformance()->getProtocol())
           .getNumWitnesses() == witnesses.size()
         && "witness table size doesn't match ProtocolInfo");

  // Produce the initializer value.
  auto tableTy = llvm::ArrayType::get(FunctionPtrTy, witnesses.size());
  auto initializer = llvm::ConstantArray::get(tableTy, witnesses);

  auto global = cast<llvm::GlobalVariable>(
                         getAddrOfWitnessTable(wt->getConformance(), tableTy));
  global->setConstant(true);
  global->setInitializer(initializer);
  global->setAlignment(getWitnessTableAlignment().getValue());

  // Build the conformance record, if it lives in this TU.
  if (isAvailableExternally(wt->getLinkage()))
    return;

  addProtocolConformanceRecord(wt->getConformance());
}


/// True if a function's signature in LLVM carries polymorphic parameters.
/// Generic functions and protocol witnesses carry polymorphic parameters.
bool irgen::hasPolymorphicParameters(CanSILFunctionType ty) {
  switch (ty->getRepresentation()) {
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
    // Should never be polymorphic.
    assert(!ty->isPolymorphic() && "polymorphic C function?!");
    return false;

  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::ObjCMethod:
    return ty->isPolymorphic();

  case SILFunctionTypeRepresentation::WitnessMethod:
    // Always carries polymorphic parameters for the Self type.
    return true;
  }
}

namespace {
  struct Fulfillment {
    Fulfillment() = default;
    Fulfillment(unsigned sourceIndex, MetadataPath &&path)
      : SourceIndex(sourceIndex), Path(std::move(path)) {}

    /// The source index.
    unsigned SourceIndex;

    /// The path from the source metadata.
    MetadataPath Path;
  };
  typedef std::pair<Type, ProtocolDecl*> FulfillmentKey;

  /// A class for computing how to pass arguments to a polymorphic
  /// function.  The subclasses of this are the places which need to
  /// be updated if the convention changes.
  class PolymorphicConvention {
  public:
    enum class SourceKind {
      /// The polymorphic arguments are derived from a source class
      /// pointer.
      ClassPointer,

      /// The polymorphic arguments are derived from a type metadata
      /// pointer.
      Metadata,

      /// The polymorphic arguments are passed from generic type
      /// metadata for the origin type.
      GenericLValueMetadata,

      /// The polymorphic arguments are derived from a Self type binding
      /// passed via the WitnessMethod convention.
      WitnessSelf,

      /// The polymorphic arguments are derived from a Self type binding
      /// embedded in a thick WitnessMethod function value.
      WitnessExtraData,
    };

    static bool requiresSourceIndex(SourceKind kind) {
      return (kind == SourceKind::ClassPointer ||
              kind == SourceKind::Metadata ||
              kind == SourceKind::GenericLValueMetadata);
    }

    enum : unsigned { InvalidSourceIndex = ~0U };

    class Source {
      /// The kind of source this is.
      SourceKind Kind;

      /// The parameter index, for ClassPointer and Metadata sources.
      unsigned Index;

    public:
      CanType Type;

      Source(SourceKind kind, unsigned index, CanType type)
         : Kind(kind), Index(index), Type(type) {
        assert(index != InvalidSourceIndex || !requiresSourceIndex(kind));
      }

      SourceKind getKind() const { return Kind; }
      unsigned getParamIndex() const {
        assert(requiresSourceIndex(getKind()));
        return Index;
      }
    };

  protected:
    ModuleDecl &M;
    CanSILFunctionType FnType;

    /// This is the canonical "mangling" signature of the function type, which
    /// is minimized in a way such that getAllDependentTypes() excludes
    /// types with equality constraints to concrete types.
    CanGenericSignature Generics;

    std::vector<Source> Sources;
    bool DidUseLastSource = false;

    llvm::DenseMap<FulfillmentKey, Fulfillment> Fulfillments;

    auto getConformsTo(Type t) -> decltype(Generics->getConformsTo(t, M)) {
      return Generics->getConformsTo(t, M);
    }

  public:
    PolymorphicConvention(CanSILFunctionType fnType, Module &M)
        : M(M), FnType(fnType) {
      initGenerics();

      auto params = fnType->getParameters();
      unsigned selfIndex = ~0U;

      auto rep = fnType->getRepresentation();

      if (rep == SILFunctionTypeRepresentation::WitnessMethod) {
        // Protocol witnesses always derive all polymorphic parameter
        // information from the Self argument. We also *cannot* consider other
        // arguments; doing so would potentially make the signature
        // incompatible with other witnesses for the same method.
        selfIndex = params.size() - 1;
        Sources.emplace_back(SourceKind::WitnessSelf, InvalidSourceIndex,
                             CanType());
        considerWitnessSelf(params[selfIndex], selfIndex);
      } else if (rep == SILFunctionTypeRepresentation::ObjCMethod) {
        // Objective-C methods also always derive all polymorphic parameter
        // information from the Self argument.
        selfIndex = params.size() - 1;
        Sources.emplace_back(SourceKind::ClassPointer, selfIndex, CanType());
        considerWitnessSelf(params[selfIndex], selfIndex);
      } else {
        // We don't need to pass anything extra as long as all of the
        // archetypes (and their requirements) are producible from
        // arguments.

        // Consider 'self' first.
        if (fnType->hasSelfParam()) {
          selfIndex = params.size() - 1;
          considerParameter(params[selfIndex], selfIndex, true);
        }

        // Now consider the rest of the parameters.
        for (auto index : indices(params)) {
          if (index != selfIndex)
            considerParameter(params[index], index, false);
        }
      }
    }

    /// Extract dependent type metadata for a value witness function of the given
    /// type.
    PolymorphicConvention(NominalTypeDecl *ntd, Module &M)
      : M(M), FnType(getNotionalFunctionType(ntd))
    {
      initGenerics();

      auto paramType = FnType->getParameters()[0].getType();
      Sources.emplace_back(SourceKind::Metadata, 0, paramType);

      considerBoundGenericType(cast<BoundGenericType>(paramType),
                              MetadataPath());
    }

    ArrayRef<Source> getSources() const { return Sources; }

    GenericSignatureWitnessIterator getAllDependentTypes() const {
      return Generics ? Generics->getAllDependentTypes()
                      : GenericSignatureWitnessIterator::emptyRange();
    }

    /// Given that we have metadata for a type, is it exactly of the
    /// specified type, or might it be a subtype?
    enum IsExact_t : bool { IsInexact = false, IsExact = true };

  private:
    void initGenerics() {
      assert(hasPolymorphicParameters(FnType));

      // The canonical mangling signature removes dependent types that are
      // equal to concrete types, but isn't necessarily parallel with
      // substitutions.
      Generics = FnType->getGenericSignature();
    }

    static CanSILFunctionType getNotionalFunctionType(NominalTypeDecl *D) {
      ASTContext &ctx = D->getASTContext();
      SILFunctionType::ExtInfo extInfo(SILFunctionType::Representation::Method,
                                       /*noreturn*/ false);
      SILResultInfo result(TupleType::getEmpty(ctx),
                           ResultConvention::Unowned);
      SILParameterInfo param(D->getDeclaredInterfaceType()->getCanonicalType(),
                              ParameterConvention::Direct_Owned);

      CanGenericSignature sig = D->getGenericSignatureOfContext()
        ? D->getGenericSignatureOfContext()->getCanonicalSignature()
        : nullptr;

      return SILFunctionType::get(sig, extInfo,
                                  ParameterConvention::Direct_Unowned,
                                  param, result, None, ctx);
    }

    /// Is the given type interesting for fulfillments?
    static bool isInterestingTypeForFulfillments(CanType type) {
      return type->hasTypeParameter();
    }

    void considerNewTypeSource(SourceKind kind, unsigned paramIndex,
                               CanType type, IsExact_t isExact) {
      if (!isInterestingTypeForFulfillments(type)) return;

      // Prospectively add a source.
      Sources.emplace_back(kind, paramIndex, type);
      DidUseLastSource = false;

      // Consider the source.
      considerType(type, MetadataPath(), isExact);

      // If the last source was not used in any fulfillments, remove it.
      if (!DidUseLastSource)
        Sources.pop_back();
    }

    /// Testify to generic parameters in the Self type.
    void considerWitnessSelf(SILParameterInfo param, unsigned paramIndex) {
      CanType selfTy = param.getType();
      if (auto metaTy = dyn_cast<AnyMetatypeType>(selfTy))
        selfTy = metaTy.getInstanceType();
      Sources.back().Type = selfTy;

      if (auto nomTy = dyn_cast<NominalType>(selfTy))
        considerNominalType(nomTy, MetadataPath());
      else if (auto bgTy = dyn_cast<BoundGenericType>(selfTy))
        considerBoundGenericType(bgTy, MetadataPath());
      else if (auto paramTy = dyn_cast<GenericTypeParamType>(selfTy))
        considerWitnessParamType(paramTy);
      else
        llvm_unreachable("witness for non-nominal type?!");
    }

    void considerParameter(SILParameterInfo param, unsigned paramIndex,
                           bool isSelfParameter) {
      auto type = param.getType();
      switch (param.getConvention()) {
      // Out-parameters don't give us a value we can use.
      case ParameterConvention::Indirect_Out:
        return;

      // In-parameters do, but right now we don't bother, for no good
      // reason. But if this is 'self', consider passing an extra
      // metatype.
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_Inout:
        if (!isSelfParameter) return;
        if (type->getNominalOrBoundGenericNominal()) {
          considerNewTypeSource(SourceKind::GenericLValueMetadata,
                                paramIndex, type, IsExact);
        }
        return;

      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Deallocating:
        // Classes are sources of metadata.
        if (type->getClassOrBoundGenericClass()) {
          considerNewTypeSource(SourceKind::ClassPointer, paramIndex, type,
                                IsInexact);
          return;
        }

        // Thick metatypes are sources of metadata.
        if (auto metatypeTy = dyn_cast<MetatypeType>(type)) {
          if (metatypeTy->getRepresentation() != MetatypeRepresentation::Thick)
            return;

          CanType objTy = metatypeTy.getInstanceType();
          considerNewTypeSource(SourceKind::Metadata, paramIndex, objTy,
                                IsInexact);
          return;
        }

        return;
      }
      llvm_unreachable("bad parameter convention");
    }

    /// Given that we have a source for metadata of the given type, check
    /// to see if it fulfills anything.
    ///
    /// \param isExact - true if the metadata is known to be exactly the
    ///   metadata for the given type, false if it might be a subtype
    void considerType(CanType type, MetadataPath &&path, IsExact_t isExact) {

      // Type parameters.  Inexact metadata are useless here.
      if (isExact && type->isTypeParameter()) {
        return considerTypeParameter(type, std::move(path));
      }

      // Inexact metadata will be a problem if we ever try to use this
      // to remember that we already have the metadata for something.
      if (auto nomTy = dyn_cast<NominalType>(type)) {
        return considerNominalType(nomTy, std::move(path));
      }
      if (auto boundTy = dyn_cast<BoundGenericType>(type)) {
        return considerBoundGenericType(boundTy, std::move(path));
      }

      // TODO: tuples
      // TODO: functions
      // TODO: metatypes
    }

    void considerParentType(CanType parent, MetadataPath &&path) {
      // We might not have a parent type.
      if (!parent) return;

      // If we do, it has to be nominal one way or another.
      path.addNominalParentComponent();
      considerType(parent, std::move(path), IsExact);
    }

    void considerNominalType(CanNominalType type, MetadataPath &&path) {
      // Nominal types add no generic arguments themselves, but they
      // may have the arguments of their parents.
      considerParentType(type.getParent(), std::move(path));
    }

    void considerBoundGenericType(CanBoundGenericType type,
                                  MetadataPath &&path) {
      auto params = type->getDecl()->getGenericParams()->getAllArchetypes();
      auto substitutions = type->getSubstitutions(&M, nullptr);
      assert(params.size() >= substitutions.size() &&
             "generic decl archetypes should parallel generic type subs");

      for (unsigned i = 0, e = substitutions.size(); i != e; ++i) {
        auto sub = substitutions[i];
        CanType arg = sub.getReplacement()->getCanonicalType();

        if (!isInterestingTypeForFulfillments(arg))
          continue;

        // If the argument is a type parameter, fulfill conformances for it.
        if (arg->isTypeParameter()) {
          considerTypeArgConformances(arg, params[i], path, i);
        }

        // Refine the path.
        MetadataPath argPath = path;
        argPath.addNominalTypeArgumentComponent(i);
        considerType(arg, std::move(argPath), IsExact);
      }

      // Also match against the parent.  The polymorphic type
      // will start with any arguments from the parent.
      considerParentType(CanType(type->getParent()), std::move(path));
    }

    void considerTypeArgConformances(CanType arg, ArchetypeType *param,
                                     const MetadataPath &path,
                                     unsigned argIndex) {
      // Our sources are the protocol conformances that are recorded in
      // the generic metadata.
      auto storedConformances = param->getConformsTo();
      if (storedConformances.empty()) return;

      // Our targets are the conformances required for the type argument.
      auto requiredConformances = getConformsTo(arg);
      if (requiredConformances.empty()) return;

      for (auto target : requiredConformances) {
        // Ignore trivial protocols.
        if (!Lowering::TypeConverter::protocolRequiresWitnessTable(target))
          continue;

        // Check each of the stored conformances.
        for (size_t confIndex : indices(storedConformances)) {
          // TODO: maybe this should consider indirect conformance.
          // But that should be part of the metadata path.
          if (target == storedConformances[confIndex]) {
            MetadataPath confPath = path;
            confPath.addNominalTypeArgumentConformanceComponent(argIndex,
                                                                confIndex);
            addFulfillment(arg, target, std::move(confPath));
          }
        }
      }
    }

    /// We found a reference to the dependent arg type at the given path.
    /// Add any fulfillments this gives us.
    void considerTypeParameter(CanType arg, MetadataPath &&path) {
      addFulfillment(arg, nullptr, std::move(path));
    }

    /// We're binding an archetype for a protocol witness.
    void considerWitnessParamType(CanGenericTypeParamType arg) {
      assert(arg->getDepth() == 0 && arg->getIndex() == 0);

      // First of all, the archetype or concrete type fulfills its own
      // requirements.
      addSelfFulfillment(arg, MetadataPath());

      // FIXME: We can't pass associated types of Self through the witness
      // CC, so as a hack, fake up impossible fulfillments for the associated
      // types. For now all conformances are concrete, so the associated types
      // can be recovered by substitution on the implementation side. For
      // default implementations, we will need to get associated types from
      // witness tables anyway.
      for (auto depTy : getAllDependentTypes()) {
        // Is this a dependent member?
        auto depMemTy = dyn_cast<DependentMemberType>(CanType(depTy));
        if (!depMemTy)
          continue;

        // Is it rooted in a generic parameter?
        CanType rootTy;
        do {
          rootTy = depMemTy.getBase();
        } while ((depMemTy = dyn_cast<DependentMemberType>(rootTy)));

        auto rootParamTy = dyn_cast<GenericTypeParamType>(rootTy);
        if (!rootParamTy)
          continue;

        // If so, suppress providing metadata for the type by making up a bogus
        // fulfillment.
        if (rootParamTy == arg) {
          MetadataPath path;
          path.addImpossibleComponent();
          addSelfFulfillment(CanType(depTy), std::move(path));
        }
      }
    }

    void addSelfFulfillment(CanType arg, MetadataPath &&path) {
      for (auto protocol : getConformsTo(arg)) {
        addFulfillment(arg, protocol, MetadataPath(path));
      }
      addFulfillment(arg, nullptr, std::move(path));
    }

    /// Testify that there's a fulfillment at the given path.
    void addFulfillment(CanType arg, ProtocolDecl *proto,
                        MetadataPath &&path) {
      assert(!Sources.empty() && "adding fulfillment without source?");
      auto sourceIndex = Sources.size() - 1;

      // Only add a fulfillment if we don't have any previous
      // fulfillment for that value or if it 's cheaper than the existing
      // fulfillment.
      assert(arg->isTypeParameter() && "fulfilling non-dependent type?!");
      auto key = FulfillmentKey(arg, proto);

      auto it = Fulfillments.find(key);
      if (it != Fulfillments.end()) {
        if (path.cost() < it->second.Path.cost()) {
          it->second.SourceIndex = sourceIndex;
          it->second.Path = std::move(path);
          DidUseLastSource = true;
        }
      } else {
        Fulfillments.insert(std::make_pair(key, 
                                   Fulfillment(sourceIndex, std::move(path))));
        DidUseLastSource = true;
      }
    }
  };

  /// A class for binding type parameters of a generic function.
  class EmitPolymorphicParameters : public PolymorphicConvention {
    IRGenFunction &IGF;
    GenericParamList *ContextParams;

    struct SourceValue {
      llvm::Value *Value = nullptr;
      MetadataPath::Map<llvm::Value*> Cache;
    };

    std::vector<SourceValue> SourceValues;

  public:
    EmitPolymorphicParameters(IRGenFunction &IGF,
                              SILFunction &Fn)
      : PolymorphicConvention(Fn.getLoweredFunctionType(),
                              *IGF.IGM.SILMod->getSwiftModule()),
        IGF(IGF), ContextParams(Fn.getContextGenericParams()) {}

    void emit(Explosion &in, WitnessMetadata *witnessMetadata,
              const GetParameterFn &getParameter);

    /// Emit polymorphic parameters for a generic value witness.
    EmitPolymorphicParameters(IRGenFunction &IGF, NominalTypeDecl *ntd)
      : PolymorphicConvention(ntd, *IGF.IGM.SILMod->getSwiftModule()),
        IGF(IGF), ContextParams(ntd->getGenericParams()) {}

    void emitForGenericValueWitness(llvm::Value *selfMeta);

  private:
    // Emit metadata bindings after the source, if any, has been bound.
    void emitWithSourcesBound(Explosion &in);

    CanType getArgTypeInContext(unsigned paramIndex) const {
      return ArchetypeBuilder::mapTypeIntoContext(
                            IGF.IGM.SILMod->getSwiftModule(), ContextParams,
                            FnType->getParameters()[paramIndex].getType())
        ->getCanonicalType();
    }

    /// Emit the source value for parameters.
    llvm::Value *emitSourceForParameters(const Source &source,
                                         Explosion &in,
                                         WitnessMetadata *witnessMetadata,
                                         const GetParameterFn &getParameter) {
      switch (source.getKind()) {
      case SourceKind::Metadata:
        return getParameter(source.getParamIndex());

      case SourceKind::ClassPointer: {
        unsigned paramIndex = source.getParamIndex();
        llvm::Value *instanceRef = getParameter(paramIndex);
        SILType instanceType =
          SILType::getPrimitiveObjectType(getArgTypeInContext(paramIndex));
        return emitDynamicTypeOfHeapObject(IGF, instanceRef, instanceType);
      }

      case SourceKind::GenericLValueMetadata: {
        llvm::Value *metatype = in.claimNext();
        metatype->setName("Self");

        // Mark this as the cached metatype for the l-value's object type.
        CanType argTy = getArgTypeInContext(source.getParamIndex());
        IGF.setUnscopedLocalTypeData(argTy, LocalTypeData::forMetatype(),
                                     metatype);
        return metatype;
      }

      case SourceKind::WitnessSelf: {
        assert(witnessMetadata && "no metadata for witness method");
        llvm::Value *metatype = witnessMetadata->SelfMetadata;
        assert(metatype && "no Self metadata for witness method");
        
        // Mark this as the cached metatype for Self.
        CanType argTy = getArgTypeInContext(FnType->getParameters().size() - 1);
        IGF.setUnscopedLocalTypeData(argTy,
                                     LocalTypeData::forMetatype(), metatype);
        return metatype;
      }
          
      case SourceKind::WitnessExtraData: {
        // The 'Self' parameter is provided last.
        // TODO: For default implementations, the witness table pointer for
        // the 'Self : P' conformance must be provided last along with the
        // metatype.
        llvm::Value *metatype = in.takeLast();
        metatype->setName("Self");
        return metatype;
      }
      }
      llvm_unreachable("bad source kind!");
    }

    /// Produce the metadata value for the given depth, using the
    /// given cache.
    llvm::Value *getMetadataForFulfillment(const Fulfillment &fulfillment) {
      unsigned sourceIndex = fulfillment.SourceIndex;
      auto &source = getSources()[sourceIndex];
      auto &sourceValue = SourceValues[sourceIndex];

      return fulfillment.Path.followFromTypeMetadata(IGF, source.Type,
                                                     sourceValue.Value,
                                                     &sourceValue.Cache);
    }
  };
};

/// Emit a polymorphic parameters clause, binding all the metadata necessary.
void EmitPolymorphicParameters::emit(Explosion &in,
                                     WitnessMetadata *witnessMetadata,
                                     const GetParameterFn &getParameter) {
  SourceValues.reserve(getSources().size());
  for (const Source &source : getSources()) {
    llvm::Value *value =
      emitSourceForParameters(source, in, witnessMetadata, getParameter);
    SourceValues.emplace_back();
    SourceValues.back().Value = value;
  }

  emitWithSourcesBound(in);
}

/// Emit a polymorphic parameters clause for a generic value witness, binding
/// all the metadata necessary.
void
EmitPolymorphicParameters::emitForGenericValueWitness(llvm::Value *selfMeta) {
  // We get the source metadata verbatim from the value witness signature.
  assert(getSources().size() == 1);
  SourceValues.emplace_back();
  SourceValues.back().Value = selfMeta;

  // All our archetypes should be satisfiable from the source.
  Explosion empty;
  emitWithSourcesBound(empty);
}

void
EmitPolymorphicParameters::emitWithSourcesBound(Explosion &in) {
  for (auto depTy : getAllDependentTypes()) {
    // Get the corresponding context archetype.
    auto contextTy
      = ArchetypeBuilder::mapTypeIntoContext(IGF.IGM.SILMod->getSwiftModule(),
                                             ContextParams, depTy)
        ->getAs<ArchetypeType>();
    assert(contextTy);

    // Derive the appropriate metadata reference.
    llvm::Value *metadata;

    // If the reference is fulfilled by the source, go for it.
    auto it = Fulfillments.find(FulfillmentKey(depTy, nullptr));
    if (it != Fulfillments.end()) {
      metadata = getMetadataForFulfillment(it->second);

    // Otherwise, it's just next in line.
    } else {
      metadata = in.claimNext();
    }

    // Collect all the witness tables.
    SmallVector<llvm::Value *, 8> wtables;
    assert(contextTy->getConformsTo() == makeArrayRef(getConformsTo(depTy)));
    for (auto protocol : contextTy->getConformsTo()) {
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;

      llvm::Value *wtable;

      // If the protocol witness table is fulfilled by the source, go for it.
      auto it = Fulfillments.find(FulfillmentKey(depTy, protocol));
      if (it != Fulfillments.end()) {
        wtable = getMetadataForFulfillment(it->second);

      // Otherwise, it's just next in line.
      } else {
        wtable = in.claimNext();
      }
      wtables.push_back(wtable);
    }
    IGF.bindArchetype(contextTy, metadata, wtables);
  }
}

llvm::Value *
MetadataPath::followFromTypeMetadata(IRGenFunction &IGF,
                                     CanType sourceType,
                                     llvm::Value *source,
                                     Map<llvm::Value*> *cache) const {
  return follow(IGF, sourceType, nullptr, source,
                Path.begin(), Path.end(), cache);
}

llvm::Value *
MetadataPath::followFromWitnessTable(IRGenFunction &IGF,
                                     ProtocolDecl *sourceDecl,
                                     llvm::Value *source,
                                     Map<llvm::Value*> *cache) const {
  return follow(IGF, CanType(), sourceDecl, source,
                Path.begin(), Path.end(), cache);
}

llvm::Value *MetadataPath::follow(IRGenFunction &IGF,
                                  CanType sourceType, Decl *sourceDecl,
                                  llvm::Value *source,
                                  iterator begin, iterator end,
                                  Map<llvm::Value*> *cache) {
  assert(source && "no source metadata value!");
  iterator i = begin;

  // If there's a cache, look for the entry matching the longest prefix
  // of this path.
  if (cache) {
    auto result = cache->findPrefix(begin, end);
    if (result.first) {
      source = *result.first;

      // If that was the end, there's no more work to do; don't bother
      // adjusting the source decl/type.
      if (result.second == end)
        return source;

      // Advance sourceDecl/sourceType past the cached prefix.
      while (i != result.second) {
        Component component = *i++;
        (void)followComponent(IGF, sourceType, sourceDecl,
                              /*source*/ nullptr, component);
      }
    }
  }

  // Drill in on the actual source value.
  while (i != end) {
    auto component = *i++;
    source = followComponent(IGF, sourceType, sourceDecl, source, component);

    // Remember this in the cache at the next position.
    if (cache) {
      cache->insertNew(begin, i, source);
    }
  }

  return source;
}

/// Drill down on a single stage of component.
///
/// sourceType and sourceDecl will be adjusted to refer to the new
/// component.  Source can be null, in which case this will be the only
/// thing done.
llvm::Value *MetadataPath::followComponent(IRGenFunction &IGF,
                                           CanType &sourceType,
                                           Decl *&sourceDecl,
                                           llvm::Value *source,
                                           Component component) {
  switch (component.getKind()) {
  case Component::Kind::NominalTypeArgument: {
    auto generic = cast<BoundGenericType>(sourceType);
    auto index = component.getPrimaryIndex();
    if (source) {
      source = emitArgumentMetadataRef(IGF, generic->getDecl(), index, source);
    }

    auto subs = generic->getSubstitutions(IGF.IGM.SILMod->getSwiftModule(),
                                          nullptr);
    sourceType = subs[index].getReplacement()->getCanonicalType();
    return source;
  }

  /// Generic type argument protocol conformance.
  case Component::Kind::NominalTypeArgumentConformance: {
    auto generic = cast<BoundGenericType>(sourceType);
    auto argIndex = component.getPrimaryIndex();
    auto confIndex = component.getSecondaryIndex();

    ProtocolDecl *protocol =
      generic->getDecl()->getGenericParams()->getAllArchetypes()[argIndex]
                                            ->getConformsTo()[confIndex];

    if (source) {
      source = emitArgumentWitnessTableRef(IGF, generic->getDecl(), argIndex,
                                           protocol, source);
    }

    sourceType = CanType();
    sourceDecl = protocol;
    return source;
  }

  case Component::Kind::NominalParent: {
    NominalTypeDecl *nominalDecl;
    if (auto nominal = dyn_cast<NominalType>(sourceType)) {
      nominalDecl = nominal->getDecl();
      sourceType = nominal.getParent();
    } else {
      auto generic = cast<BoundGenericType>(sourceType);
      nominalDecl = generic->getDecl();
      sourceType = generic.getParent();
    }

    if (source) {
      source = emitParentMetadataRef(IGF, nominalDecl, source);
    }
    return source;
  }

  case Component::Kind::Impossible:
    llvm_unreachable("following an impossible path!");

  } 
  llvm_unreachable("bad metata path component");
}

/// Collect any required metadata for a witness method from the end of
/// the given parameter list.
void irgen::collectTrailingWitnessMetadata(IRGenFunction &IGF,
                                           SILFunction &fn,
                                           Explosion &params,
                                           WitnessMetadata &witnessMetadata) {
  assert(fn.getLoweredFunctionType()->getRepresentation()
           == SILFunctionTypeRepresentation::WitnessMethod);

  llvm::Value *metatype = params.takeLast();
  assert(metatype->getType() == IGF.IGM.TypeMetadataPtrTy &&
         "parameter signature mismatch: witness metadata didn't "
         "end in metatype?");
  metatype->setName("Self");
  witnessMetadata.SelfMetadata = metatype;
}

/// Perform all the bindings necessary to emit the given declaration.
void irgen::emitPolymorphicParameters(IRGenFunction &IGF,
                                      SILFunction &Fn,
                                      Explosion &in,
                                      WitnessMetadata *witnessMetadata,
                                      const GetParameterFn &getParameter) {
  EmitPolymorphicParameters(IGF, Fn).emit(in, witnessMetadata, getParameter);
}

/// Perform the metadata bindings necessary to emit a generic value witness.
void irgen::emitPolymorphicParametersForGenericValueWitness(IRGenFunction &IGF,
                                                        NominalTypeDecl *ntd,
                                                        llvm::Value *selfMeta) {
  // Nothing to do if the type isn't generic.
  if (!ntd->getGenericParamsOfContext())
    return;

  EmitPolymorphicParameters(IGF, ntd).emitForGenericValueWitness(selfMeta);
  // Register the 'Self' argument as generic metadata for the type.
  IGF.setUnscopedLocalTypeData(ntd->getDeclaredTypeInContext()->getCanonicalType(),
                               LocalTypeData::forMetatype(), selfMeta);
}

/// Get the next argument and use it as the 'self' type metadata.
static void getArgAsLocalSelfTypeMetadata(IRGenFunction &IGF,
                                          llvm::Function::arg_iterator &it,
                                          CanType abstractType) {
  llvm::Value *arg = getArg(it, "Self");
  assert(arg->getType() == IGF.IGM.TypeMetadataPtrTy &&
         "Self argument is not a type?!");
  if (auto ugt = dyn_cast<UnboundGenericType>(abstractType)) {
    emitPolymorphicParametersForGenericValueWitness(IGF, ugt->getDecl(), arg);
  }
}

namespace {
  /// A CRTP class for finding the archetypes we need to bind in order
  /// to perform value operations on the given type.
  struct FindArchetypesForValueOperations
    : CanTypeVisitor<FindArchetypesForValueOperations>
  {
    NecessaryBindings &Bindings;
  public:
    FindArchetypesForValueOperations(NecessaryBindings &bindings)
      : Bindings(bindings) {}

    // We're collecting archetypes.
    void visitArchetypeType(CanArchetypeType type) {
      Bindings.addArchetype(type);
    }

    // We need to walk into tuples.
    void visitTupleType(CanTupleType tuple) {
      for (auto eltType : tuple.getElementTypes()) {
        visit(eltType);
      }
    }

    // Walk into on-stack block storage.
    void visitSILBlockStorageType(CanSILBlockStorageType t) {
      visit(t->getCaptureType());
    }

    // We do not need to walk into any of these types, because their
    // value operations do not depend on the specifics of their
    // sub-structure (or they have none).
    void visitAnyFunctionType(CanAnyFunctionType fn) {}
    void visitSILFunctionType(CanSILFunctionType fn) {}
    void visitBuiltinType(CanBuiltinType type) {}
    void visitAnyMetatypeType(CanAnyMetatypeType type) {}
    void visitModuleType(CanModuleType type) {}
    void visitDynamicSelfType(CanDynamicSelfType type) {}
    void visitProtocolCompositionType(CanProtocolCompositionType type) {}
    void visitReferenceStorageType(CanReferenceStorageType type) {}
    void visitSILBoxType(CanSILBoxType t) {}

    // L-values are impossible.
    void visitLValueType(CanLValueType type) {
      llvm_unreachable("cannot store l-value type directly");
    }
    void visitInOutType(CanInOutType type) {
      llvm_unreachable("cannot store inout type directly");
    }

    // Bind archetypes from the parent of nominal types.
    void visitNominalType(CanNominalType type) {
      if (auto parent = CanType(type->getParent()))
        visit(parent);
    }
    // Bind archetypes from bound generic types and their parents.
    void visitBoundGenericType(CanBoundGenericType type) {
      if (auto parent = CanType(type->getParent()))
        visit(parent);
      for (auto arg : type->getGenericArgs())
        visit(CanType(arg));
    }

    // FIXME: Will need to bind the archetype that this eventually refers to.
    void visitGenericTypeParamType(CanGenericTypeParamType type) { }

    // FIXME: Will need to bind the archetype that this eventually refers to.
    void visitDependentMemberType(CanDependentMemberType type) { }
  };
}

NecessaryBindings
NecessaryBindings::forFunctionInvocations(IRGenModule &IGM,
                                          CanSILFunctionType origType,
                                          CanSILFunctionType substType,
                                          ArrayRef<Substitution> subs) {
  NecessaryBindings bindings;
  // Collect bindings required by the polymorphic parameters to the function.
  for (auto &sub : subs) {
    sub.getReplacement().findIf([&](Type t) -> bool {
      if (auto archetype = dyn_cast<ArchetypeType>(t->getCanonicalType())) {
        bindings.addArchetype(archetype);
      }
      return false;
    });
  }
  return bindings;
}

NecessaryBindings
NecessaryBindings::forValueOperations(IRGenModule &IGM, CanType type) {
  NecessaryBindings bindings;
  FindArchetypesForValueOperations(bindings).visit(type);
  return bindings;
}

Size NecessaryBindings::getBufferSize(IRGenModule &IGM) const {
  unsigned numPointers = 0;

  // We need one pointer for each archetype and witness table.
  for (auto type : Types) {
    numPointers++;
    for (auto proto : type->getConformsTo())
      if (Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
        numPointers++;
  }

  return IGM.getPointerSize() * numPointers;
}

void NecessaryBindings::restore(IRGenFunction &IGF, Address buffer) const {
  if (Types.empty()) return;

  // Cast the buffer to %type**.
  auto metatypePtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
  buffer = IGF.Builder.CreateBitCast(buffer, metatypePtrPtrTy);

  for (unsigned archetypeI = 0, e = Types.size(), metadataI = 0;
       archetypeI != e; ++archetypeI) {
    auto archetype = Types[archetypeI];

    // GEP to the appropriate slot.
    Address slot = buffer;
    if (metadataI) slot = IGF.Builder.CreateConstArrayGEP(slot, metadataI,
                                                  IGF.IGM.getPointerSize());
    ++metadataI;

    // Load the archetype's metatype.
    llvm::Value *metatype = IGF.Builder.CreateLoad(slot);

    // Load the witness tables for the archetype's protocol constraints.
    SmallVector<llvm::Value*, 4> witnesses;
    for (unsigned protocolI : indices(archetype->getConformsTo())) {
      auto protocol = archetype->getConformsTo()[protocolI];
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;
      Address witnessSlot = IGF.Builder.CreateConstArrayGEP(buffer, metadataI,
                                                      IGF.IGM.getPointerSize());
      witnessSlot = IGF.Builder.CreateBitCast(witnessSlot,
                                    IGF.IGM.WitnessTablePtrTy->getPointerTo());
      ++metadataI;
      llvm::Value *witness = IGF.Builder.CreateLoad(witnessSlot);
      witnesses.push_back(witness);
    }

    IGF.bindArchetype(archetype, metatype, witnesses);
  }
}

void NecessaryBindings::save(IRGenFunction &IGF, Address buffer) const {
  if (Types.empty()) return;

  // Cast the buffer to %type**.
  auto metatypePtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
  buffer = IGF.Builder.CreateBitCast(buffer, metatypePtrPtrTy);

  for (unsigned typeI = 0, typeE = Types.size(),
                metadataI = 0; typeI != typeE; ++typeI) {
    auto archetype = Types[typeI];

    // GEP to the appropriate slot.
    Address slot = buffer;
    if (metadataI) slot = IGF.Builder.CreateConstArrayGEP(slot, metadataI,
                                                  IGF.IGM.getPointerSize());
    ++metadataI;

    // Find the metatype for the appropriate archetype and store it in
    // the slot.
    llvm::Value *metatype =
      IGF.getLocalTypeData(CanType(archetype), LocalTypeData::forMetatype());
    IGF.Builder.CreateStore(metatype, slot);

    // Find the witness tables for the archetype's protocol constraints and
    // store them in the slot.
    for (unsigned protocolI : indices(archetype->getConformsTo())) {
      auto protocol = archetype->getConformsTo()[protocolI];
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;
      Address witnessSlot = IGF.Builder.CreateConstArrayGEP(buffer, metadataI,
                                                      IGF.IGM.getPointerSize());
      witnessSlot = IGF.Builder.CreateBitCast(witnessSlot,
                                    IGF.IGM.WitnessTablePtrTy->getPointerTo());
      ++metadataI;
      llvm::Value *witness =
        IGF.getLocalTypeData(CanType(archetype),
                         LocalTypeData::forArchetypeProtocolWitness(protocolI));
      IGF.Builder.CreateStore(witness, witnessSlot);
    }
  }
}

void NecessaryBindings::addArchetype(CanArchetypeType type) {
  if (Types.insert(type))
    // Collect the associated archetypes.
    for (auto nested : type->getNestedTypes())
      if (auto assocArchetype = nested.second.getAsArchetype())
        addArchetype(CanArchetypeType(assocArchetype));
}

llvm::Value *irgen::emitImpliedWitnessTableRef(IRGenFunction &IGF,
                                               ArrayRef<ProtocolEntry> entries,
                                               ProtocolDecl *target,
                                     const GetWitnessTableFn &getWitnessTable) {
  ProtocolPath path(IGF.IGM, entries, target);
  auto wtable = getWitnessTable(path.getOriginIndex());
  wtable = path.apply(IGF, wtable);
  return wtable;
}

/// Emit a protocol witness table for a conformance.
llvm::Value *irgen::emitWitnessTableRef(IRGenFunction &IGF,
                                        CanType srcType,
                                        const TypeInfo &srcTI,
                                        ProtocolDecl *proto,
                                        const ProtocolInfo &protoI,
                                        ProtocolConformance *conformance) {
  assert(Lowering::TypeConverter::protocolRequiresWitnessTable(proto)
         && "protocol does not have witness tables?!");

  // If the source type is an archetype and we don't have concrete conformance
  // info, the conformance must be via one of the protocol requirements of the
  // archetype. Look at what's locally bound.
  if (!conformance) {
    auto archetype = cast<ArchetypeType>(srcType);
    return emitWitnessTableRef(IGF, archetype, proto);
  }

  // All other source types should be concrete enough that we have conformance
  // info for them.
  auto &conformanceI = protoI.getConformance(IGF.IGM, proto, conformance);
  return conformanceI.getTable(IGF, srcType);
}

/// Emit the witness table references required for the given type
/// substitution.
void irgen::emitWitnessTableRefs(IRGenFunction &IGF,
                                 const Substitution &sub,
                                 SmallVectorImpl<llvm::Value*> &out) {
  auto conformances = sub.getConformances();

  // We don't need to do anything if we have no protocols to conform to.
  auto archetypeProtos = sub.getArchetype()->getConformsTo();
  assert(!conformances.size() || archetypeProtos.size() == conformances.size());

  if (archetypeProtos.empty()) return;

  // Look at the replacement type.
  CanType replType = sub.getReplacement()->getCanonicalType();
  auto &replTI = IGF.getTypeInfoForUnlowered(replType);

  for (unsigned j = 0, je = archetypeProtos.size(); j != je; ++j) {
    auto proto = archetypeProtos[j];
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
      continue;

    auto conformance = conformances.size() ? conformances[j] : nullptr;
    auto wtable = emitWitnessTableRef(IGF, replType, replTI, proto,
                                      IGF.IGM.getProtocolInfo(proto),
                                      conformance);

    out.push_back(wtable);
  }
}

namespace {
  class EmitPolymorphicArguments : public PolymorphicConvention {
    IRGenFunction &IGF;
  public:
    EmitPolymorphicArguments(IRGenFunction &IGF,
                             CanSILFunctionType polyFn)
      : PolymorphicConvention(polyFn, *IGF.IGM.SILMod->getSwiftModule()),
        IGF(IGF) {}

    void emit(CanType substInputType, ArrayRef<Substitution> subs,
              WitnessMetadata *witnessMetadata, Explosion &out);

  private:
    void emitEarlySources(CanType substInputType, Explosion &out) {
      for (auto &source : getSources()) {
        switch (source.getKind()) {
        // Already accounted for in the parameters.
        case SourceKind::ClassPointer:
        case SourceKind::Metadata:
          continue;

        // Needs a special argument.
        case SourceKind::GenericLValueMetadata: {
          out.add(IGF.emitTypeMetadataRef(substInputType));
          continue;
        }

        // Witness 'Self' argument(s) are added as a special case in
        // EmitPolymorphicArguments::emit.
        case SourceKind::WitnessSelf:
          continue;

        // The 'Self' argument(s) are added implicitly from ExtraData
        // of the function value.
        case SourceKind::WitnessExtraData:
          continue;
        }
        llvm_unreachable("bad source kind!");
      }
    }
  };
}

void irgen::emitTrailingWitnessArguments(IRGenFunction &IGF,
                                         WitnessMetadata &witnessMetadata,
                                         Explosion &args) {
  llvm::Value *self = witnessMetadata.SelfMetadata;
  assert(self && "no Self value bound");
  args.add(self);
}

/// Pass all the arguments necessary for the given function.
void irgen::emitPolymorphicArguments(IRGenFunction &IGF,
                                     CanSILFunctionType origFnType,
                                     CanSILFunctionType substFnType,
                                     ArrayRef<Substitution> subs,
                                     WitnessMetadata *witnessMetadata,
                                     Explosion &out) {
  // Grab the apparent 'self' type.  If there isn't a 'self' type,
  // we're not going to try to access this anyway.
  CanType substInputType;
  if (!substFnType->getParameters().empty()) {
    auto selfParam = substFnType->getParameters().back();
    substInputType = selfParam.getType();
    // If the parameter is a direct metatype parameter, this is a static method
    // of the instance type. We can assume this because:
    // - metatypes cannot directly conform to protocols
    // - even if they could, they would conform as a value type 'self' and thus
    //   be passed indirectly as an @in or @inout parameter.
    if (auto meta = dyn_cast<MetatypeType>(substInputType)) {
      if (!selfParam.isIndirect())
        substInputType = meta.getInstanceType();
    }
  }

  EmitPolymorphicArguments(IGF, origFnType).emit(substInputType, subs,
                                                 witnessMetadata, out);
}

void EmitPolymorphicArguments::emit(CanType substInputType,
                                    ArrayRef<Substitution> subs,
                                    WitnessMetadata *witnessMetadata,
                                    Explosion &out) {
  // Add all the early sources.
  emitEarlySources(substInputType, out);

  // For now, treat all archetypes independently.
  // FIXME: Later, we'll want to emit only the minimal set of archetypes,
  // because non-primary archetypes (which correspond to associated types)
  // will have their witness tables embedded in the witness table corresponding
  // to their parent.
  for (auto depTy : getAllDependentTypes()) {
    // The substitutions should be in the same order.
    const Substitution &sub = subs.front();
    subs = subs.slice(1);

    CanType argType = sub.getReplacement()->getCanonicalType();

    // If same-type constraints have eliminated the genericity of this
    // parameter, it doesn't need an independent metadata parameter.
    if (Generics->isConcreteType(depTy, M))
      continue;

    // Add the metadata reference unless it's fulfilled.
    if (!Fulfillments.count(FulfillmentKey(depTy, nullptr))) {
      out.add(IGF.emitTypeMetadataRef(argType));
    }

    // Nothing else to do if there aren't any protocols to witness.
    auto protocols = getConformsTo(depTy);
    auto conformances = sub.getConformances();
    assert(!conformances.size() || protocols.size() == conformances.size());

    if (protocols.empty()) continue;

    auto &argTI = IGF.getTypeInfoForUnlowered(argType);

    // Add witness tables for each of the required protocols.
    for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
      auto protocol = protocols[i];

      // Skip this if the protocol doesn't require a witness table.
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;

      // Skip this if it's fulfilled by the source.
      if (Fulfillments.count(FulfillmentKey(depTy, protocol)))
        continue;

      auto conformance = conformances.size() ? conformances[i] : nullptr;
      auto wtable = emitWitnessTableRef(IGF,
                                        argType, argTI,
                                        protocol,
                                        IGF.IGM.getProtocolInfo(protocol),
                                        conformance);
      out.add(wtable);
    }
  }
  assert(subs.empty()
         && "did not use all substitutions?!");

  // For a witness call, add the Self argument metadata arguments last.
  for (auto &source : getSources()) {
    switch (source.getKind()) {
    case SourceKind::Metadata:
    case SourceKind::ClassPointer:
      // Already accounted for in the arguments.
      continue;

    case SourceKind::GenericLValueMetadata:
      // Added in the early phase.
      continue;

    case SourceKind::WitnessSelf: {
      assert(witnessMetadata && "no metadata structure for witness method");
      auto self = IGF.emitTypeMetadataRef(substInputType);
      witnessMetadata->SelfMetadata = self;
      continue;
    }

    case SourceKind::WitnessExtraData:
      // The 'Self' argument(s) are added implicitly from ExtraData of the
      // function value.
      continue;
    }
    llvm_unreachable("bad source kind");
  }
}

namespace {
  /// A class for expanding a polymorphic signature.
  class ExpandPolymorphicSignature : public PolymorphicConvention {
    IRGenModule &IGM;
  public:
    ExpandPolymorphicSignature(IRGenModule &IGM, CanSILFunctionType fn)
      : PolymorphicConvention(fn, *IGM.SILMod->getSwiftModule()), IGM(IGM) {}

    void expand(SmallVectorImpl<llvm::Type*> &out) {
      for (auto &source : getSources())
        addEarlySource(source, out);

      for (auto depTy : getAllDependentTypes()) {
        // Only emit parameters for independent parameters that haven't been
        // constrained to concrete types.
        if (Generics->isConcreteType(depTy, M))
          continue;

        // Pass the type argument if not fulfilled.
        if (!Fulfillments.count(FulfillmentKey(depTy, nullptr))) {
          out.push_back(IGM.TypeMetadataPtrTy);
        }

        // Pass each signature requirement that needs a witness table
        // separately (unless fulfilled).
        for (auto protocol : getConformsTo(depTy)) {
          if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
            continue;

          if (!Fulfillments.count(FulfillmentKey(depTy, protocol))) {
            out.push_back(IGM.WitnessTablePtrTy);
          }
        }
      }
    }

  private:
    /// Add signature elements for the source metadata.
    void addEarlySource(const Source &source,
                        SmallVectorImpl<llvm::Type*> &out) {
      switch (source.getKind()) {
      case SourceKind::ClassPointer: return; // already accounted for
      case SourceKind::Metadata: return; // already accounted for
      case SourceKind::GenericLValueMetadata:
        return out.push_back(IGM.TypeMetadataPtrTy);
      case SourceKind::WitnessSelf:
        return; // handled as a special case in expand()
      case SourceKind::WitnessExtraData:
        return; // added implicitly as ExtraData
      }
      llvm_unreachable("bad source kind");
    }
  };
}

/// Given a generic signature, add the argument types required in order to call it.
void irgen::expandPolymorphicSignature(IRGenModule &IGM,
                                       CanSILFunctionType polyFn,
                                       SmallVectorImpl<llvm::Type*> &out) {
  ExpandPolymorphicSignature(IGM, polyFn).expand(out);
}

void irgen::expandTrailingWitnessSignature(IRGenModule &IGM,
                                           CanSILFunctionType polyFn,
                                           SmallVectorImpl<llvm::Type*> &out) {
  assert(polyFn->getRepresentation()
          == SILFunctionTypeRepresentation::WitnessMethod);

  assert(getTrailingWitnessSignatureLength(IGM, polyFn) == 1);

  // A witness method always provides Self.
  out.push_back(IGM.TypeMetadataPtrTy);

  // TODO: Should also provide the protocol witness table,
  // for default implementations.
}

void
irgen::emitWitnessMethodValue(IRGenFunction &IGF,
                              CanType baseTy,
                              SILDeclRef member,
                              ProtocolConformance *conformance,
                              Explosion &out) {
  auto fn = cast<AbstractFunctionDecl>(member.getDecl());

  // The protocol we're calling on.
  ProtocolDecl *fnProto = cast<ProtocolDecl>(fn->getDeclContext());

  // Find the witness table.
  // FIXME conformance for concrete type
  auto &baseTI = IGF.getTypeInfoForUnlowered(baseTy);
  auto &fnProtoInfo = IGF.IGM.getProtocolInfo(fnProto);
  llvm::Value *wtable = emitWitnessTableRef(IGF, baseTy, baseTI,
                                            fnProto,
                                            fnProtoInfo,
                                            conformance);

  // Find the witness we're interested in.
  auto index = fnProtoInfo.getWitnessEntry(fn).getFunctionIndex();
  llvm::Value *witness = emitInvariantLoadOfOpaqueWitness(IGF, wtable, index);
  
  // Cast the witness pointer to i8*.
  witness = IGF.Builder.CreateBitCast(witness, IGF.IGM.Int8PtrTy);
  
  // Build the value.
  out.add(witness);
}

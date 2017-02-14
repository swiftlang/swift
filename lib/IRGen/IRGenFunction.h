//===--- IRGenFunction.h - IR Generation for Swift Functions ----*- C++ -*-===//
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
// This file defines the structure used to generate the IR body of a
// function.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENFUNCTION_H
#define SWIFT_IRGEN_IRGENFUNCTION_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CallingConv.h"
#include "IRBuilder.h"
#include "LocalTypeDataKind.h"
#include "DominancePoint.h"

namespace llvm {
  class AllocaInst;
  class CallSite;
  class Constant;
  class Function;
}

namespace swift {
  class ArchetypeType;
  class AssociatedTypeDecl;
  class ClassDecl;
  class ConstructorDecl;
  class Decl;
  class ExtensionDecl;
  class FuncDecl;
  class EnumElementDecl;
  class EnumType;
  class Pattern;
  class PatternBindingDecl;
  class SILDebugScope;
  class SILType;
  class SourceLoc;
  class StructType;
  class Substitution;
  class ValueDecl;
  class VarDecl;

namespace Lowering {
  class TypeConverter;
}
  
namespace irgen {
  class Explosion;
  class FunctionRef;
  class HeapLayout;
  class HeapNonFixedOffsets;
  class IRGenModule;
  class LinkEntity;
  class LocalTypeDataCache;
  class Scope;
  class TypeInfo;
  enum class ValueWitness : unsigned;
  enum class ReferenceCounting : unsigned char;

/// IRGenFunction - Primary class for emitting LLVM instructions for a
/// specific function.
class IRGenFunction {
public:
  IRGenModule &IGM;
  IRBuilder Builder;

  llvm::Function *CurFn;
  ModuleDecl *getSwiftModule() const;
  SILModule &getSILModule() const;
  Lowering::TypeConverter &getSILTypes() const;

  IRGenFunction(IRGenModule &IGM, llvm::Function *fn,
                const SILDebugScope *DbgScope = nullptr,
                Optional<SILLocation> DbgLoc = None);
  ~IRGenFunction();

  void unimplemented(SourceLoc Loc, StringRef Message);

  friend class Scope;

//--- Function prologue and epilogue -------------------------------------------
public:
  Explosion collectParameters();
  void emitScalarReturn(SILType resultTy, Explosion &scalars);
  void emitScalarReturn(llvm::Type *resultTy, Explosion &scalars);
  
  void emitBBForReturn();
  bool emitBranchToReturnBB();

  /// Return the error result slot, given an error type.  There's
  /// always only one error type.
  Address getErrorResultSlot(SILType errorType);

  /// Return the error result slot provided by the caller.
  Address getCallerErrorResultSlot();

  /// Set the error result slot.
  void setErrorResultSlot(llvm::Value *address);
  
private:
  void emitPrologue();
  void emitEpilogue();

  Address ReturnSlot;
  llvm::BasicBlock *ReturnBB;
  llvm::Value *ErrorResultSlot = nullptr;

//--- Helper methods -----------------------------------------------------------
public:
  Address createAlloca(llvm::Type *ty, Alignment align,
                       const llvm::Twine &name);
  Address createAlloca(llvm::Type *ty, llvm::Value *ArraySize, Alignment align,
                       const llvm::Twine &name);
  Address createFixedSizeBufferAlloca(const llvm::Twine &name);

  llvm::BasicBlock *createBasicBlock(const llvm::Twine &Name);
  const TypeInfo &getTypeInfoForUnlowered(Type subst);
  const TypeInfo &getTypeInfoForUnlowered(AbstractionPattern orig, Type subst);
  const TypeInfo &getTypeInfoForUnlowered(AbstractionPattern orig,
                                          CanType subst);
  const TypeInfo &getTypeInfoForLowered(CanType T);
  const TypeInfo &getTypeInfo(SILType T);
  void emitMemCpy(llvm::Value *dest, llvm::Value *src,
                  Size size, Alignment align);
  void emitMemCpy(llvm::Value *dest, llvm::Value *src,
                  llvm::Value *size, Alignment align);
  void emitMemCpy(Address dest, Address src, Size size);
  void emitMemCpy(Address dest, Address src, llvm::Value *size);

  llvm::Value *emitByteOffsetGEP(llvm::Value *base, llvm::Value *offset,
                                 llvm::Type *objectType,
                                 const llvm::Twine &name = "");
  Address emitByteOffsetGEP(llvm::Value *base, llvm::Value *offset,
                            const TypeInfo &type,
                            const llvm::Twine &name = "");

  void emitStoreOfRelativeIndirectablePointer(llvm::Value *value,
                                              Address addr,
                                              bool isFar);

  llvm::Value *
  emitLoadOfRelativeIndirectablePointer(Address addr, bool isFar,
                                        llvm::PointerType *expectedType,
                                        const llvm::Twine &name = "");


  llvm::Value *emitAllocObjectCall(llvm::Value *metadata, llvm::Value *size,
                                   llvm::Value *alignMask,
                                   const llvm::Twine &name = "");
  llvm::Value *emitInitStackObjectCall(llvm::Value *metadata,
                                       llvm::Value *object,
                                       const llvm::Twine &name = "");
  llvm::Value *emitVerifyEndOfLifetimeCall(llvm::Value *object,
                                           const llvm::Twine &name = "");
  llvm::Value *emitAllocRawCall(llvm::Value *size, llvm::Value *alignMask,
                                const llvm::Twine &name ="");
  void emitDeallocRawCall(llvm::Value *pointer, llvm::Value *size,
                          llvm::Value *alignMask);
  
  void emitAllocBoxCall(llvm::Value *typeMetadata,
                         llvm::Value *&box,
                         llvm::Value *&valueAddress);

  void emitDeallocBoxCall(llvm::Value *box, llvm::Value *typeMetadata);

  llvm::Value *emitProjectBoxCall(llvm::Value *box, llvm::Value *typeMetadata);

  // Emit a reference to the canonical type metadata record for the given AST
  // type. This can be used to identify the type at runtime. For types with
  // abstraction difference, the metadata contains the layout information for
  // values in the maximally-abstracted representation of the type; this is
  // correct for all uses of reabstractable values in opaque contexts.
  llvm::Value *emitTypeMetadataRef(CanType type);

  // Emit a reference to a type layout record for the given type. The referenced
  // data is enough to lay out an aggregate containing a value of the type, but
  // can't uniquely represent the type or perform value witness operations on
  // it.
  llvm::Value *emitTypeLayoutRef(SILType type);
  
  // Emit a reference to a metadata object that can be used for layout, but
  // cannot be used to identify a type. This will produce a layout appropriate
  // to the abstraction level of the given type. It may be able to avoid runtime
  // calls if there is a standard metadata object with the correct layout for
  // the type.
  //
  // TODO: It might be better to return just a value witness table reference
  // here, since for some types it's easier to get a shared reference to one
  // than a metadata reference, and it would be more type-safe.
  llvm::Value *emitTypeMetadataRefForLayout(SILType type);
  
  llvm::Value *emitValueWitnessTableRef(CanType type);
  llvm::Value *emitValueWitnessTableRefForLayout(SILType type);
  llvm::Value *emitValueWitnessTableRefForMetadata(llvm::Value *metadata);
  
  llvm::Value *emitValueWitness(CanType type, ValueWitness index);
  llvm::Value *emitValueWitnessForLayout(SILType type, ValueWitness index);

  /// Emit a load of a reference to the given Objective-C selector.
  llvm::Value *emitObjCSelectorRefLoad(StringRef selector);

  /// Return the SILDebugScope for this function.
  const SILDebugScope *getDebugScope() const { return DbgScope; }
  llvm::Value *coerceValue(llvm::Value *value, llvm::Type *toTy,
                           const llvm::DataLayout &);

  /// Mark a load as invariant.
  void setInvariantLoad(llvm::LoadInst *load);
  /// Mark a load as dereferenceable to `size` bytes.
  void setDereferenceableLoad(llvm::LoadInst *load, unsigned size);

private:
  llvm::Instruction *AllocaIP;
  const SILDebugScope *DbgScope;

//--- Reference-counting methods -----------------------------------------------
public:
  // Returns the default atomicity of the module.
  Atomicity getDefaultAtomicity();

  llvm::Value *emitUnmanagedAlloc(const HeapLayout &layout,
                                  const llvm::Twine &name,
                                  llvm::Constant *captureDescriptor,
                                  const HeapNonFixedOffsets *offsets = 0);

  // Functions that don't care about the reference-counting style.
  void emitFixLifetime(llvm::Value *value);

  // Routines that are generic over the reference-counting style:
  //   - strong references
  void emitStrongRetain(llvm::Value *value, ReferenceCounting refcounting,
                        Atomicity atomicity);
  void emitStrongRelease(llvm::Value *value, ReferenceCounting refcounting,
                         Atomicity atomicity);
  llvm::Value *emitLoadRefcountedPtr(Address addr, ReferenceCounting style);

  //   - unowned references
  void emitUnownedRetain(llvm::Value *value, ReferenceCounting style,
                         Atomicity atomicity);
  void emitUnownedRelease(llvm::Value *value, ReferenceCounting style,
                          Atomicity atomicity);
  void emitStrongRetainUnowned(llvm::Value *value, ReferenceCounting style,
                               Atomicity atomicity);
  void emitStrongRetainAndUnownedRelease(llvm::Value *value,
                                         ReferenceCounting style,
                                         Atomicity atomicity);
  void emitUnownedInit(llvm::Value *val, Address dest, ReferenceCounting style);
  void emitUnownedAssign(llvm::Value *value, Address dest,
                         ReferenceCounting style);
  void emitUnownedCopyInit(Address destAddr, Address srcAddr,
                           ReferenceCounting style);
  void emitUnownedTakeInit(Address destAddr, Address srcAddr,
                           ReferenceCounting style);
  void emitUnownedCopyAssign(Address destAddr, Address srcAddr,
                             ReferenceCounting style);
  void emitUnownedTakeAssign(Address destAddr, Address srcAddr,
                             ReferenceCounting style);
  llvm::Value *emitUnownedLoadStrong(Address src, llvm::Type *resultType,
                                     ReferenceCounting style);
  llvm::Value *emitUnownedTakeStrong(Address src, llvm::Type *resultType,
                                     ReferenceCounting style);
  void emitUnownedDestroy(Address addr, ReferenceCounting style);
  llvm::Value *getUnownedExtraInhabitantIndex(Address src,
                                              ReferenceCounting style);
  void storeUnownedExtraInhabitant(llvm::Value *index, Address dest,
                                   ReferenceCounting style);

  //   - weak references
  void emitWeakInit(llvm::Value *ref, Address dest, ReferenceCounting style);
  void emitWeakAssign(llvm::Value *ref, Address dest, ReferenceCounting style);
  void emitWeakCopyInit(Address destAddr, Address srcAddr,
                        ReferenceCounting style);
  void emitWeakTakeInit(Address destAddr, Address srcAddr,
                        ReferenceCounting style);
  void emitWeakCopyAssign(Address destAddr, Address srcAddr,
                          ReferenceCounting style);
  void emitWeakTakeAssign(Address destAddr, Address srcAddr,
                          ReferenceCounting style);
  llvm::Value *emitWeakLoadStrong(Address src, llvm::Type *resultType,
                                  ReferenceCounting style);
  llvm::Value *emitWeakTakeStrong(Address src, llvm::Type *resultType,
                                  ReferenceCounting style);
  void emitWeakDestroy(Address addr, ReferenceCounting style);

  // Routines for the Swift native reference-counting style.
  //   - strong references
  void emitNativeStrongAssign(llvm::Value *value, Address addr);
  void emitNativeStrongInit(llvm::Value *value, Address addr);
  void emitNativeStrongRetain(llvm::Value *value, Atomicity atomicity);
  void emitNativeStrongRelease(llvm::Value *value, Atomicity atomicity);
  void emitNativeSetDeallocating(llvm::Value *value);
  //   - unowned references
  void emitNativeUnownedRetain(llvm::Value *value, Atomicity atomicity);
  void emitNativeUnownedRelease(llvm::Value *value, Atomicity atomicity);
  void emitNativeStrongRetainUnowned(llvm::Value *value, Atomicity atomicity);
  void emitNativeStrongRetainAndUnownedRelease(llvm::Value *value,
                                               Atomicity atomicity);
  void emitNativeUnownedInit(llvm::Value *val, Address dest);
  void emitNativeUnownedAssign(llvm::Value *value, Address dest);
  void emitNativeUnownedCopyInit(Address destAddr, Address srcAddr);
  void emitNativeUnownedTakeInit(Address destAddr, Address srcAddr);
  void emitNativeUnownedCopyAssign(Address destAddr, Address srcAddr);
  void emitNativeUnownedTakeAssign(Address destAddr, Address srcAddr);
  llvm::Value *emitNativeUnownedLoadStrong(Address src, llvm::Type *resultType);
  llvm::Value *emitNativeUnownedTakeStrong(Address src, llvm::Type *resultType);
  void emitNativeUnownedDestroy(Address addr);

  //   - weak references
  void emitNativeWeakInit(llvm::Value *value, Address dest);
  void emitNativeWeakAssign(llvm::Value *value, Address dest);
  llvm::Value *emitNativeWeakLoadStrong(Address src, llvm::Type *type);
  llvm::Value *emitNativeWeakTakeStrong(Address src, llvm::Type *type);
  void emitNativeWeakDestroy(Address addr);
  void emitNativeWeakCopyInit(Address destAddr, Address srcAddr);
  void emitNativeWeakTakeInit(Address destAddr, Address srcAddr);
  void emitNativeWeakCopyAssign(Address destAddr, Address srcAddr);
  void emitNativeWeakTakeAssign(Address destAddr, Address srcAddr);
  //   - other operations
  llvm::Value *emitNativeTryPin(llvm::Value *object, Atomicity atomicity);
  void emitNativeUnpin(llvm::Value *handle, Atomicity atomicity);

  // Routines for the ObjC reference-counting style.
  void emitObjCStrongRetain(llvm::Value *value);
  llvm::Value *emitObjCRetainCall(llvm::Value *value);
  llvm::Value *emitObjCAutoreleaseCall(llvm::Value *value);
  void emitObjCStrongRelease(llvm::Value *value);

  llvm::Value *emitBlockCopyCall(llvm::Value *value);
  void emitBlockRelease(llvm::Value *value);

  // Routines for an unknown reference-counting style (meaning,
  // dynamically something compatible with either the ObjC or Swift styles).
  //   - strong references
  void emitUnknownStrongRetain(llvm::Value *value, Atomicity atomicity);
  void emitUnknownStrongRelease(llvm::Value *value, Atomicity atomicity);
  //   - unowned references
  void emitUnknownUnownedInit(llvm::Value *val, Address dest);
  void emitUnknownUnownedAssign(llvm::Value *value, Address dest);
  void emitUnknownUnownedCopyInit(Address destAddr, Address srcAddr);
  void emitUnknownUnownedTakeInit(Address destAddr, Address srcAddr);
  void emitUnknownUnownedCopyAssign(Address destAddr, Address srcAddr);
  void emitUnknownUnownedTakeAssign(Address destAddr, Address srcAddr);
  llvm::Value *emitUnknownUnownedLoadStrong(Address src, llvm::Type *resultTy);
  llvm::Value *emitUnknownUnownedTakeStrong(Address src, llvm::Type *resultTy);
  void emitUnknownUnownedDestroy(Address addr);
  //   - weak references
  void emitUnknownWeakDestroy(Address addr);
  void emitUnknownWeakCopyInit(Address destAddr, Address srcAddr);
  void emitUnknownWeakTakeInit(Address destAddr, Address srcAddr);
  void emitUnknownWeakCopyAssign(Address destAddr, Address srcAddr);
  void emitUnknownWeakTakeAssign(Address destAddr, Address srcAddr);
  void emitUnknownWeakInit(llvm::Value *value, Address dest);
  void emitUnknownWeakAssign(llvm::Value *value, Address dest);
  llvm::Value *emitUnknownWeakLoadStrong(Address src, llvm::Type *type);
  llvm::Value *emitUnknownWeakTakeStrong(Address src, llvm::Type *type);

  // Routines for the Builtin.NativeObject reference-counting style.
  void emitBridgeStrongRetain(llvm::Value *value, Atomicity atomicity);
  void emitBridgeStrongRelease(llvm::Value *value, Atomicity atomicity);

  // Routines for the ErrorType reference-counting style.
  void emitErrorStrongRetain(llvm::Value *value);
  void emitErrorStrongRelease(llvm::Value *value);

  llvm::Value *emitIsUniqueCall(llvm::Value *value, SourceLoc loc,
                                bool isNonNull, bool checkPinned);

//--- Expression emission ------------------------------------------------------
public:
  void emitFakeExplosion(const TypeInfo &type, Explosion &explosion);

//--- Declaration emission -----------------------------------------------------
public:

  void bindArchetype(ArchetypeType *type,
                     llvm::Value *metadata,
                     ArrayRef<llvm::Value*> wtables);

  struct ArchetypeAccessPath {
    CanArchetypeType BaseType;
    AssociatedTypeDecl *Association;
  };

  /// Register an additional access path to the given archetype besides
  /// (if applicable) just drilling down from its parent.
  ///
  /// This is necessary when an archetype gains conformances from an
  /// associated type that it's been constrained to be equal to
  /// but which is not simply its parent.
  void addArchetypeAccessPath(CanArchetypeType targetArchetype,
                              ArchetypeAccessPath accessPath);

  ArrayRef<ArchetypeAccessPath>
  getArchetypeAccessPaths(CanArchetypeType targetArchetype);

//--- Type emission ------------------------------------------------------------
public:
  /// Look up a local type data reference, returning null if no entry was
  /// found.  This will emit code to materialize the reference if an
  /// "abstract" entry is present.
  llvm::Value *tryGetLocalTypeData(CanType type, LocalTypeDataKind kind) {
    return tryGetLocalTypeData(LocalTypeDataKey{type, kind});
  }
  llvm::Value *tryGetLocalTypeData(LocalTypeDataKey key);

  /// Look up a local type data reference, returning null if no entry was
  /// found or if the only viable entries are abstract.  This will never
  /// emit code.
  llvm::Value *tryGetConcreteLocalTypeData(LocalTypeDataKey key);

  /// Retrieve a local type data reference which is known to exist.
  llvm::Value *getLocalTypeData(CanType type, LocalTypeDataKind kind);

  /// Add a local type-metadata reference at a point which definitely
  /// dominates all of its uses.
  void setUnscopedLocalTypeData(CanType type, LocalTypeDataKind kind,
                                llvm::Value *data) {
    setUnscopedLocalTypeData(LocalTypeDataKey{type, kind}, data);
  }
  void setUnscopedLocalTypeData(LocalTypeDataKey key, llvm::Value *data);
  
  /// Add a local type-metadata reference, valid at the current insertion
  /// point.
  void setScopedLocalTypeData(CanType type, LocalTypeDataKind kind,
                              llvm::Value *data) {
    setScopedLocalTypeData(LocalTypeDataKey{type, kind}, data);
  }
  void setScopedLocalTypeData(LocalTypeDataKey key, llvm::Value *data);

  /// The same as tryGetLocalTypeData, just for the Layout metadata.
  ///
  /// We use a separate function name for this to clarify that you should
  /// only ever be looking type metadata for a lowered SILType for the
  /// purposes of local layout (e.g. of a tuple).
  llvm::Value *tryGetLocalTypeDataForLayout(SILType type,
                                            LocalTypeDataKind kind) {
    return tryGetLocalTypeData(type.getSwiftRValueType(), kind);
  }

  /// Add a local type-metadata reference, which is valid for the containing
  /// block.
  void setScopedLocalTypeDataForLayout(SILType type, LocalTypeDataKind kind,
                                       llvm::Value *data) {
    setScopedLocalTypeData(type.getSwiftRValueType(), kind, data);
  }

  /// Given a concrete type metadata node, add all the local type data
  /// that we can reach from it.
  void bindLocalTypeDataFromTypeMetadata(CanType type, IsExact_t isExact,
                                         llvm::Value *metadata);

  void setDominanceResolver(DominanceResolverFunction resolver) {
    assert(DominanceResolver == nullptr);
    DominanceResolver = resolver;
  }

  bool isActiveDominancePointDominatedBy(DominancePoint point) {
    // If the point is universal, it dominates.
    if (point.isUniversal()) return true;

    assert(!ActiveDominancePoint.isUniversal() &&
           "active dominance point is universal but there exists a"
           "non-universal point?");

    // If we don't have a resolver, we're emitting a simple helper
    // function; just assume dominance.
    if (!DominanceResolver) return true;

    // Otherwise, ask the resolver.
    return DominanceResolver(*this, ActiveDominancePoint, point);
  }

  /// Is the current dominance point conditional in some way not
  /// tracked by the active dominance point?
  ///
  /// This should only be used by the local type data cache code.
  bool isConditionalDominancePoint() const {
    return ConditionalDominance != nullptr;
  }

  void registerConditionalLocalTypeDataKey(LocalTypeDataKey key) {
    assert(ConditionalDominance != nullptr &&
           "not in a conditional dominance scope");
    ConditionalDominance->registerConditionalLocalTypeDataKey(key);
  }

  /// Return the currently-active dominance point.
  DominancePoint getActiveDominancePoint() const {
    return ActiveDominancePoint;
  }

  /// A RAII object for temporarily changing the dominance of the active
  /// definition point.
  class DominanceScope {
    IRGenFunction &IGF;
    DominancePoint OldDominancePoint;
  public:
    explicit DominanceScope(IRGenFunction &IGF, DominancePoint newPoint)
        : IGF(IGF), OldDominancePoint(IGF.ActiveDominancePoint) {
      IGF.ActiveDominancePoint = newPoint;
      assert(!newPoint.isOrdinary() || IGF.DominanceResolver);
    }

    DominanceScope(const DominanceScope &other) = delete;
    DominanceScope &operator=(const DominanceScope &other) = delete;

    ~DominanceScope() {
      IGF.ActiveDominancePoint = OldDominancePoint;
    }
  };

  /// A RAII object for temporarily suppressing type-data caching at the
  /// active definition point.  Do this if you're adding local control flow
  /// that isn't modeled by the dominance system.
  class ConditionalDominanceScope {
    IRGenFunction &IGF;
    ConditionalDominanceScope *OldScope;
    SmallVector<LocalTypeDataKey, 2> RegisteredKeys;

  public:
    explicit ConditionalDominanceScope(IRGenFunction &IGF)
        : IGF(IGF), OldScope(IGF.ConditionalDominance) {
      IGF.ConditionalDominance = this;
    }

    ConditionalDominanceScope(const ConditionalDominanceScope &other) = delete;
    ConditionalDominanceScope &operator=(const ConditionalDominanceScope &other)
      = delete;

    void registerConditionalLocalTypeDataKey(LocalTypeDataKey key) {
      RegisteredKeys.push_back(key);
    }

    ~ConditionalDominanceScope();
  };

  /// The kind of value LocalSelf is.
  enum LocalSelfKind {
    /// An object reference.
    ObjectReference,
    /// A Swift metatype.
    SwiftMetatype,
    /// An ObjC metatype.
    ObjCMetatype,
  };

  llvm::Value *getLocalSelfMetadata();
  void setLocalSelfMetadata(llvm::Value *value, LocalSelfKind kind);
  
private:
  LocalTypeDataCache &getOrCreateLocalTypeData();
  void destroyLocalTypeData();

  LocalTypeDataCache *LocalTypeData = nullptr;

  /// The dominance resolver.  This can be set at most once; when it's not
  /// set, this emission must never have a non-null active definition point.
  DominanceResolverFunction DominanceResolver = nullptr;
  DominancePoint ActiveDominancePoint = DominancePoint::universal();
  ConditionalDominanceScope *ConditionalDominance = nullptr;
  
  /// The value that satisfies metadata lookups for dynamic Self.
  llvm::Value *LocalSelf = nullptr;
  
  LocalSelfKind SelfKind;

  llvm::DenseMap<CanType, std::vector<ArchetypeAccessPath>>
  ArchetypeAccessPaths;
};

using ConditionalDominanceScope = IRGenFunction::ConditionalDominanceScope;

} // end namespace irgen
} // end namespace swift

#endif

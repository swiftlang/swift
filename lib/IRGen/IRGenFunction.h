//===--- IRGenFunction.h - IR Generation for Swift Functions ---*- C++ -*-===//
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
// This file defines the structure used to generate the IR body of a
// function.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRGENFUNCTION_H
#define SWIFT_IRGEN_IRGENFUNCTION_H

#include "swift/Basic/LLVM.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CallingConv.h"
#include "IRBuilder.h"


namespace llvm {
  class AllocaInst;
  class CallSite;
  class Constant;
  class Function;
}

namespace swift {
  class ArchetypeType;
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
  
namespace irgen {
  class Explosion;
  class FunctionRef;
  class HeapLayout;
  class HeapNonFixedOffsets;
  class IRGenModule;
  class LinkEntity;
  class Scope;
  class TypeInfo;

/// LocalTypeData - A nonce value for storing some sort of
/// locally-known information about a type.
/// 
/// The enumerated values are all in the "negative" range and so do
/// not collide with reasonable index values.
enum class LocalTypeData : unsigned {
  /// A reference to a metatype.
  Metatype = ~0U
};
  
/// IRGenFunction - Primary class for emitting LLVM instructions for a
/// specific function.
class IRGenFunction {
public:
  IRGenModule &IGM;
  IRBuilder Builder;

  llvm::Function *CurFn;

  IRGenFunction(IRGenModule &IGM,
                llvm::Function *fn,
                SILDebugScope* DbgScope = nullptr,
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

  llvm::Value *emitAllocObjectCall(llvm::Value *metadata, llvm::Value *size,
                                   llvm::Value *alignMask,
                                   const llvm::Twine &name = "");
  llvm::Value *emitAllocRawCall(llvm::Value *size, llvm::Value *alignMask,
                                const llvm::Twine &name ="");
  void emitDeallocRawCall(llvm::Value *pointer, llvm::Value *size,
                          llvm::Value *alignMask);
  
  void emitAllocBoxCall(llvm::Value *typeMetadata,
                        llvm::Value *&box,
                        llvm::Value *&valueAddress);
  
  void emitDeallocBoxCall(llvm::Value *box, llvm::Value *typeMetadata);

  // Emit a reference to the canonical type metadata record for the given AST
  // type. This can be used to identify the type at runtime. For types with
  // abstraction difference, the metadata contains the layout information for
  // values in the maximally-abstracted representation of the type; this is
  // correct for all uses of reabstractable values in opaque contexts.
  llvm::Value *emitTypeMetadataRef(CanType type);
  
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
  
  llvm::Value *emitValueWitnessTableRefForMetadata(llvm::Value *metadata);

  /// Emit a load of a reference to the given Objective-C selector.
  llvm::Value *emitObjCSelectorRefLoad(StringRef selector);

  /// Return the SILDebugScope for this function.
  SILDebugScope* getDebugScope() const { return DbgScope; }
  llvm::Value *coerceValue(llvm::Value *value, llvm::Type *toTy,
                           const llvm::DataLayout &);

private:
  llvm::Instruction *AllocaIP;
  SILDebugScope* DbgScope;

//--- Reference-counting methods -----------------------------------------------
public:
  llvm::Value *emitUnmanagedAlloc(const HeapLayout &layout,
                                  const llvm::Twine &name,
                                  const HeapNonFixedOffsets *offsets = 0);
  void emitLoadAndRetain(Address addr, Explosion &explosion);
  void emitAssignRetained(llvm::Value *value, Address addr);
  void emitInitializeRetained(llvm::Value *value, Address addr);
  void emitRetain(llvm::Value *value, Explosion &explosion);
  void emitRetainCall(llvm::Value *value);
  void emitRelease(llvm::Value *value);
  void emitRetainUnowned(llvm::Value *value);
  llvm::Value *emitTryPin(llvm::Value *object);
  void emitUnpin(llvm::Value *handle);
  void emitUnownedRetain(llvm::Value *value);
  void emitUnownedRelease(llvm::Value *value);
  void emitFixLifetime(llvm::Value *value);
  void emitWeakInit(llvm::Value *value, Address dest);
  void emitWeakAssign(llvm::Value *value, Address dest);
  llvm::Value *emitWeakLoadStrong(Address src, llvm::Type *type);
  llvm::Value *emitWeakTakeStrong(Address src, llvm::Type *type);
  void emitWeakDestroy(Address addr);
  void emitWeakCopyInit(Address destAddr, Address srcAddr);
  void emitWeakTakeInit(Address destAddr, Address srcAddr);
  void emitWeakCopyAssign(Address destAddr, Address srcAddr);
  void emitWeakTakeAssign(Address destAddr, Address srcAddr);
  void emitObjCRetain(llvm::Value *value, Explosion &explosion);
  llvm::Value *emitObjCRetainCall(llvm::Value *value);
  llvm::Value *emitObjCAutoreleaseCall(llvm::Value *value);
  void emitObjCRelease(llvm::Value *value);
  llvm::Value *emitBlockCopyCall(llvm::Value *value);
  void emitBlockRelease(llvm::Value *value);
  
  /// Emit a retain of a class instance with unknown retain semantics.
  void emitUnknownRetain(llvm::Value *value, Explosion &explosion);
  /// Emit a retain of a class instance with unknown retain semantics, and
  /// return the retained value.
  llvm::Value *emitUnknownRetainCall(llvm::Value *value);
  /// Emit a release of a class instance with unknown retain semantics.
  void emitUnknownRelease(llvm::Value *value);
  void emitUnknownUnownedRetain(llvm::Value *value);
  void emitUnknownUnownedRelease(llvm::Value *value);
  void emitUnknownRetainUnowned(llvm::Value *value);
  /// Emit a retain of a class instance with bridge retain semantics.
  void emitBridgeRetain(llvm::Value *value, Explosion &explosion);
  /// Emit a retain of a class instance with bridge retain semantics, and
  /// return the retained value.
  llvm::Value *emitBridgeRetainCall(llvm::Value *value);
  /// Emit a release of a class instance with bridge retain semantics.
  void emitBridgeRelease(llvm::Value *value);
  void emitBridgeRetainUnowned(llvm::Value *value);
  void emitErrorRetain(llvm::Value *value);
  llvm::Value *emitErrorRetainCall(llvm::Value *value);
  void emitErrorRelease(llvm::Value *value);
  void emitUnknownWeakDestroy(Address addr);
  void emitUnknownWeakCopyInit(Address destAddr, Address srcAddr);
  void emitUnknownWeakTakeInit(Address destAddr, Address srcAddr);
  void emitUnknownWeakCopyAssign(Address destAddr, Address srcAddr);
  void emitUnknownWeakTakeAssign(Address destAddr, Address srcAddr);
  void emitUnknownWeakInit(llvm::Value *value, Address dest);
  void emitUnknownWeakAssign(llvm::Value *value, Address dest);
  llvm::Value *emitUnknownWeakLoadStrong(Address src, llvm::Type *type);
  llvm::Value *emitUnknownWeakTakeStrong(Address src, llvm::Type *type);
  llvm::Value *emitLoadNativeRefcountedPtr(Address addr);
  llvm::Value *emitLoadUnknownRefcountedPtr(Address addr);
  llvm::Value *emitLoadBridgeRefcountedPtr(Address addr);
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

//--- Type emission ------------------------------------------------------------
public:
  /// Look for a mapping for a local type-metadata reference.
  /// The lookup is done for the current block which is the Builder's
  /// insert-block.
  llvm::Value *tryGetLocalTypeData(CanType type, LocalTypeData index) {
    return lookupTypeDataMap(type, index, ScopedTypeDataMap);
  }

  /// The same as tryGetLocalTypeData, just for the Layout metadata.
  llvm::Value *tryGetLocalTypeDataForLayout(CanType type, LocalTypeData index) {
    return lookupTypeDataMap(type, index, ScopedTypeDataMapForLayout);
  }

  /// Retrieve a local type-metadata reference which is known to exist.
  llvm::Value *getLocalTypeData(CanType type, LocalTypeData index) {
    auto key = getLocalTypeDataKey(type, index);
    assert(LocalTypeDataMap.count(key) && "no mapping for local type data");
    return LocalTypeDataMap.find(key)->second;
  }

  /// Add a local type-metadata reference at a point which dominates
  /// the entire function.
  void setUnscopedLocalTypeData(CanType type, LocalTypeData index,
                                llvm::Value *data) {
    assert(data && "setting a null value for type data!");

    auto key = getLocalTypeDataKey(type, index);
    assert(!LocalTypeDataMap.count(key) &&
           "existing mapping for local type data");
    LocalTypeDataMap.insert(std::make_pair(key, data));
  }
  
  /// Add a local type-metadata reference, which is valid for the containing
  /// block.
  void setScopedLocalTypeData(CanType type, llvm::Instruction *data) {
    assert(data->getParent() == Builder.GetInsertBlock() &&
           "metadata instruction not inserted into the Builder's insert-block");
    ScopedTypeDataMap[type] = data;
  }

  /// Add a local type-metadata reference, which is valid for the containing
  /// block.
  void setScopedLocalTypeDataForLayout(CanType type, llvm::Instruction *data) {
    assert(data->getParent() == Builder.GetInsertBlock() &&
           "metadata instruction not inserted into the Builder's insert-block");
    ScopedTypeDataMapForLayout[type] = data;
  }

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
  typedef unsigned LocalTypeDataDepth;
  typedef std::pair<TypeBase*,unsigned> LocalTypeDataPair;
  LocalTypeDataPair getLocalTypeDataKey(CanType type, LocalTypeData index) {
    return LocalTypeDataPair(type.getPointer(), unsigned(index));
  }

  typedef llvm::DenseMap<CanType, llvm::Instruction*> TypeDataMap;

  llvm::Value *lookupTypeDataMap(CanType type, LocalTypeData index,
                                 const TypeDataMap &scopedMap);

  llvm::DenseMap<LocalTypeDataPair, llvm::Value*> LocalTypeDataMap;

  TypeDataMap ScopedTypeDataMap;

  TypeDataMap ScopedTypeDataMapForLayout;
  
  /// The value that satisfies metadata lookups for dynamic Self.
  llvm::Value *LocalSelf = nullptr;
  
  LocalSelfKind SelfKind;
};

} // end namespace irgen
} // end namespace swift

#endif

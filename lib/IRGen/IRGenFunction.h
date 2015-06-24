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
#include "swift/SIL/SILType.h"
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
  enum class ValueWitness : unsigned;
  enum class ReferenceCounting : unsigned char;

/// A nonce value for storing some sort of locally-known information about a type.
class LocalTypeData {
  unsigned Value;
  
  explicit LocalTypeData(unsigned Value) : Value(Value) {}
  
  /// Magic values for special kinds of index.
  enum : unsigned {
    Metatype = ~0U,
    ValueWitnessTable = ~1U,

    ValueWitnessBase = 0xFFFFFF00U,
  };
  
public:
  LocalTypeData() = default;
  
  // The magic values are all in the "negative" range and so do
  // not collide with reasonable index values.
  
  /// A reference to the type metadata.
  static LocalTypeData forMetatype() { return LocalTypeData(Metatype); }
  /// A reference to the value witness table.
  static LocalTypeData forValueWitnessTable() {
    return LocalTypeData(ValueWitnessTable);
  }

  /// A reference to a specific value witness.
  static LocalTypeData forValueWitness(ValueWitness witness) {
    return LocalTypeData((unsigned)witness + ValueWitnessBase);
  }
  
  /// A reference to a protocol witness table for an archetype.
  static LocalTypeData forArchetypeProtocolWitness(unsigned index) {
    return LocalTypeData(index);
  }
  
  unsigned getValue() const {
    return Value;
  }
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
  void emitAllocBox2Call(llvm::Value *typeMetadata,
                         llvm::Value *&box,
                         llvm::Value *&valueAddress);

  void emitDeallocBoxCall(llvm::Value *box, llvm::Value *typeMetadata);
  void emitDeallocBox2Call(llvm::Value *box, llvm::Value *typeMetadata);

  llvm::Value *emitProjectBox2Call(llvm::Value *box, llvm::Value *typeMetadata);

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
  
  llvm::Value *emitValueWitnessTableRef(CanType type);
  llvm::Value *emitValueWitnessTableRefForLayout(SILType type);
  llvm::Value *emitValueWitnessTableRefForMetadata(llvm::Value *metadata);
  
  llvm::Value *emitValueWitness(CanType type, ValueWitness index);
  llvm::Value *emitValueWitnessForLayout(SILType type, ValueWitness index);

  /// Emit a load of a reference to the given Objective-C selector.
  llvm::Value *emitObjCSelectorRefLoad(StringRef selector);

  /// Return the SILDebugScope for this function.
  SILDebugScope* getDebugScope() const { return DbgScope; }
  llvm::Value *coerceValue(llvm::Value *value, llvm::Type *toTy,
                           const llvm::DataLayout &);

  /// Mark a load as invariant.
  void setInvariantLoad(llvm::LoadInst *load);
  /// Mark a load as dereferenceable to `size` bytes.
  void setDereferenceableLoad(llvm::LoadInst *load, unsigned size);

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
  void emitScalarRetainCall(llvm::Value *value, ReferenceCounting refcounting);
  void emitScalarRelease(llvm::Value *value, ReferenceCounting refcounting);
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
  llvm::Value *tryGetLocalTypeDataForLayout(SILType type, LocalTypeData index) {
    return lookupTypeDataMap(type.getSwiftRValueType(), index,
                             ScopedTypeDataMapForLayout);
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
    LocalTypeDataMap.insert({key, data});
  }
  
  /// Add a local type-metadata reference, which is valid for the containing
  /// block.
  void setScopedLocalTypeData(CanType type, LocalTypeData index,
                              llvm::Value *data) {
    assert(_isValidScopedLocalTypeData(data) &&
           "metadata instruction not inserted into the Builder's insert-block");
    ScopedTypeDataMap[getLocalTypeDataKey(type, index)] = data;
  }

  /// Add a local type-metadata reference, which is valid for the containing
  /// block.
  void setScopedLocalTypeDataForLayout(SILType type, LocalTypeData index,
                                       llvm::Value *data) {
    assert(_isValidScopedLocalTypeData(data) &&
           "metadata instruction not inserted into the Builder's insert-block");
    ScopedTypeDataMapForLayout[
                  getLocalTypeDataKey(type.getSwiftRValueType(), index)] = data;
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
#ifndef NDEBUG
  bool _isValidScopedLocalTypeData(llvm::Value *v) {
    // Constants are valid anywhere.
    if (isa<llvm::Constant>(v))
      return true;
    // Instructions are valid only in the current insert block.
    if (auto inst = dyn_cast<llvm::Instruction>(v))
      return inst->getParent() == Builder.GetInsertBlock();
    // TODO: Other kinds of value?
    return false;
  }
#endif

  typedef unsigned LocalTypeDataDepth;
  typedef std::pair<TypeBase*,unsigned> LocalTypeDataPair;
  LocalTypeDataPair getLocalTypeDataKey(CanType type, LocalTypeData index) {
    return LocalTypeDataPair(type.getPointer(), index.getValue());
  }

  typedef llvm::DenseMap<LocalTypeDataPair, llvm::Value*> TypeDataMap;

  llvm::Value *lookupTypeDataMap(CanType type, LocalTypeData index,
                                 const TypeDataMap &scopedMap);

  TypeDataMap LocalTypeDataMap;

  TypeDataMap ScopedTypeDataMap;

  TypeDataMap ScopedTypeDataMapForLayout;
  
  /// The value that satisfies metadata lookups for dynamic Self.
  llvm::Value *LocalSelf = nullptr;
  
  LocalSelfKind SelfKind;
};

} // end namespace irgen
} // end namespace swift

#endif

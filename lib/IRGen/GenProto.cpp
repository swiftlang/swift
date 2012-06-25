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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Module.h"

#include "Cleanup.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "GenFunc.h"
#include "GenType.h"
#include "IndirectTypeInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "ProtocolInfo.h"
#include "TypeInfo.h"

#include "GenProto.h"

using namespace swift;
using namespace irgen;

/// A fixed-size buffer is always 16 bytes and pointer-aligned.
/// If we align them more, we'll need to introduce padding to
/// make protocol types work.
static Size getFixedBufferSize(IRGenModule &IGM) {
  return 3 * IGM.getPointerSize();
}
static Alignment getFixedBufferAlignment(IRGenModule &IGM) {
  return IGM.getPointerAlignment();
}

/// Lazily create the standard fixed-buffer type.
llvm::Type *IRGenModule::getFixedBufferTy() {
  if (FixedBufferTy) return FixedBufferTy;

  auto size = getFixedBufferSize(*this).getValue();
  FixedBufferTy = llvm::ArrayType::get(Int8Ty, size);
  return FixedBufferTy;
}

/// A witness table is an i8**.
static llvm::PointerType *getWitnessTableTy(IRGenModule &IGM) {
  return IGM.Int8PtrTy->getPointerTo(0);
}

static llvm::StructType *getSizeAndAlignmentResultType(IRGenModule &IGM) {
  llvm::Type *resultElts[] = { IGM.SizeTy, IGM.SizeTy };
  return llvm::StructType::get(IGM.getLLVMContext(), resultElts, false);
}

static llvm::FunctionType *createWitnessFunctionType(IRGenModule &IGM,
                                                     ValueWitness index) {
  switch (index) {

  // void (*deallocateBuffer)(B *buffer, W *self);
  // void (*destroyBuffer)(B *buffer, W *self);
  case ValueWitness::DeallocateBuffer:
  case ValueWitness::DestroyBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // void (*destroy)(T *object, witness_t *self);
  case ValueWitness::Destroy: {
    llvm::Type *args[] = { IGM.Int8PtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, W *self);
  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, bufPtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.Int8PtrTy, args, false);
  }

  // T *(*allocateBuffer)(B *buffer, W *self);
  // T *(*projectBuffer)(B *buffer, W *self);
  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.Int8PtrTy, args, false);
  }

  // T *(*initializeBufferWithCopy)(B *dest, T *src, W *self);
  // T *(*initializeBufferWithTake)(B *dest, T *src, W *self);
  case ValueWitness::InitializeBufferWithCopy:
  case ValueWitness::InitializeBufferWithTake: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.Int8PtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.Int8PtrTy, args, false);
  }

  // T *(*assignWithCopy)(T *dest, T *src, W *self);
  // T *(*assignWithTake)(T *dest, T *src, W *self);
  // T *(*initializeWithCopy)(T *dest, T *src, W *self);
  // T *(*initializeWithTake)(T *dest, T *src, W *self);
  case ValueWitness::AssignWithCopy:
  case ValueWitness::AssignWithTake:
  case ValueWitness::InitializeWithCopy:
  case ValueWitness::InitializeWithTake: {
    llvm::Type *ptrTy = IGM.Int8PtrTy;
    llvm::Type *args[] = { ptrTy, ptrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(ptrTy, args, false);
  }

  // typedef struct { size_t Size; size_t Align } layout_t;
  // layout_t (*sizeAndAlignment)(W *self);
  case ValueWitness::SizeAndAlignment: {
    llvm::StructType *resultTy = getSizeAndAlignmentResultType(IGM);
    return llvm::FunctionType::get(resultTy, getWitnessTableTy(IGM), false);
  }

  }
  llvm_unreachable("bad value witness!");
}

/// Return the cached pointer-to-function type for the given value
/// witness index.
llvm::PointerType *IRGenModule::getValueWitnessTy(ValueWitness index) {
  static_assert(IRGenModule::NumValueWitnesses == ::NumValueWitnesses,
                "array on IGM has the wrong size");

  auto &ty = ValueWitnessTys[unsigned(index)];
  if (ty) return ty;

  ty = createWitnessFunctionType(*this, index)->getPointerTo(0);
  return ty;
}

static llvm::StringRef getValueWitnessLabel(ValueWitness index) {
  switch (index) {
  case ValueWitness::DeallocateBuffer:
    return "deallocateBuffer";
  case ValueWitness::DestroyBuffer:
    return "destroyBuffer";
  case ValueWitness::Destroy:
    return "destroy";
  case ValueWitness::InitializeBufferWithCopyOfBuffer:
    return "initializeBufferWithCopyOfBuffer";
  case ValueWitness::AllocateBuffer:
    return "allocateBuffer";
  case ValueWitness::ProjectBuffer:
    return "projectBuffer";
  case ValueWitness::InitializeBufferWithCopy:
    return "initializeBufferWithCopy";
  case ValueWitness::InitializeBufferWithTake:
    return "initializeBufferWithTake";
  case ValueWitness::AssignWithCopy:
    return "assignWithCopy";
  case ValueWitness::AssignWithTake:
    return "assignWithTake";
  case ValueWitness::InitializeWithCopy:
    return "initializeWithCopy";
  case ValueWitness::InitializeWithTake:
    return "initializeWithTake";
  case ValueWitness::SizeAndAlignment:
    return "sizeAndAlignment";
  }
  llvm_unreachable("bad value witness index");
}

namespace {
}

/// Given the offset of the buffer within an existential type.
static Size getBufferOffset(IRGenModule &IGM) {
  return IGM.getPointerSize();
}

/// Given the address of an existential object, drill down to the
/// buffer.
static Address projectExistentialBuffer(IRGenFunction &IGF, Address addr) {
  return IGF.Builder.CreateStructGEP(addr, 1, getBufferOffset(IGF.IGM));
}

/// Given the address of an existential object, drill down to the
/// witness-table field.
static Address projectWitnessTable(IRGenFunction &IGF, Address addr) {
  return IGF.Builder.CreateStructGEP(addr, 0, Size(0));
}

/// Given the address of an existential object, load its witness table.
static llvm::Value *loadWitnessTable(IRGenFunction &IGF, Address addr) {
  return IGF.Builder.CreateLoad(projectWitnessTable(IGF, addr),
                                "witness-table");
}

/// Load a specific witness from a known table.  The result is
/// always an i8*.
static llvm::Value *loadOpaqueWitness(IRGenFunction &IGF,
                                      llvm::Value *table,
                                      WitnessIndex index) {
  assert(table->getType() == IGF.IGM.Int8PtrTy->getPointerTo());

  // GEP to the appropriate index, avoiding spurious IR in the trivial case.
  llvm::Value *slot = table;
  if (index.getValue() != 0)
    slot = IGF.Builder.CreateConstInBoundsGEP1_32(table, index.getValue());

  llvm::Value *witness =
    IGF.Builder.CreateLoad(Address(slot, IGF.IGM.getPointerAlignment()));
  return witness;
}

/// Given a witness table, load one of the value witnesses.
/// The result has the appropriate type for the witness.
static llvm::Value *loadValueWitness(IRGenFunction &IGF,
                                     llvm::Value *table,
                                     ValueWitness index) {
  llvm::Value *witness = loadOpaqueWitness(IGF, table, index);
  return IGF.Builder.CreateBitCast(witness, IGF.IGM.getValueWitnessTy(index),
                                   getValueWitnessLabel(index));
}

/// Given a call to a helper function that produces a result
/// into its first argument, set attributes appropriately.
static void setHelperAttributesForAggResult(llvm::CallInst *call,
                                            bool isFormalResult = true) {
  llvm::SmallVector<llvm::AttributeWithIndex, 2> attrs;

  // Don't set 'sret' unless this is also the formal result.
  auto resultAttrs = llvm::Attribute::NoAlias;
  if (isFormalResult) resultAttrs = resultAttrs | llvm::Attribute::StructRet;
  attrs.push_back(llvm::AttributeWithIndex::get(1, resultAttrs));

  // Set as nounwind.
  attrs.push_back(llvm::AttributeWithIndex::get(~0,
                                llvm::Attribute::NoUnwind));

  call->setAttributes(llvm::AttrListPtr::get(attrs));
}

/// Given a call to a helper function, set attributes appropriately.
static void setHelperAttributes(llvm::CallInst *call) {
  // Set as nounwind.
  llvm::SmallVector<llvm::AttributeWithIndex, 1> attrs;
  attrs.push_back(llvm::AttributeWithIndex::get(~0,
                                llvm::Attribute::NoUnwind));

  call->setAttributes(llvm::AttrListPtr::get(attrs));
}

/// Emit a call to do an 'initializeBufferWithCopyOfBuffer' operation.
static llvm::Value *emitInitializeBufferWithCopyOfBufferCall(IRGenFunction &IGF,
                                                     llvm::Value *witnessTable,
                                                     Address destBuffer,
                                                     Address srcBuffer) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                             ValueWitness::InitializeBufferWithCopyOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destBuffer.getAddress(),
                            srcBuffer.getAddress(), witnessTable);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributesForAggResult(call, false);

  return call;
}

/// Emit a call to do an 'allocateBuffer' operation.
static llvm::Value *emitAllocateBufferCall(IRGenFunction &IGF,
                                           llvm::Value *witnessTable,
                                           Address buffer) {
  llvm::Value *allocateFn = loadValueWitness(IGF, witnessTable,
                                             ValueWitness::AllocateBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall2(allocateFn, buffer.getAddress(), witnessTable);
  result->setCallingConv(IGF.IGM.RuntimeCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do a 'projectBuffer' operation.
static llvm::Value *emitProjectBufferCall(IRGenFunction &IGF,
                                          llvm::Value *witnessTable,
                                          Address buffer) {
  llvm::Value *projectFn = loadValueWitness(IGF, witnessTable,
                                            ValueWitness::ProjectBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall2(projectFn, buffer.getAddress(), witnessTable);
  result->setCallingConv(IGF.IGM.RuntimeCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do an 'initializeWithCopy' operation.
static void emitInitializeWithCopyCall(IRGenFunction &IGF,
                                       llvm::Value *witnessTable,
                                       llvm::Value *destObject,
                                       llvm::Value *srcObject) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                                         ValueWitness::InitializeWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, witnessTable);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithCopy' operation.
static void emitAssignWithCopyCall(IRGenFunction &IGF,
                                   llvm::Value *witnessTable,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                                         ValueWitness::AssignWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, witnessTable);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do a 'destroy' operation.
static void emitDestroyCall(IRGenFunction &IGF,
                            llvm::Value *witnessTable,
                            llvm::Value *object) {
  llvm::Value *fn = loadValueWitness(IGF, witnessTable, ValueWitness::Destroy);
  llvm::CallInst *call = IGF.Builder.CreateCall2(fn, object, witnessTable);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'destroyBuffer' operation.
static void emitDestroyBufferCall(IRGenFunction &IGF,
                                  llvm::Value *witnessTable,
                                  Address buffer) {
  llvm::Value *fn = loadValueWitness(IGF, witnessTable,
                                     ValueWitness::DestroyBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), witnessTable);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'deallocateBuffer' operation.
static void emitDeallocateBufferCall(IRGenFunction &IGF,
                                     llvm::Value *witnessTable,
                                     Address buffer) {
  llvm::Value *fn = loadValueWitness(IGF, witnessTable,
                                     ValueWitness::DeallocateBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), witnessTable);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Given the address of an existential object, destroy it.
static void emitDestroyExistential(IRGenFunction &IGF, Address addr) {
  llvm::Value *table = loadWitnessTable(IGF, addr);
  emitDestroyBufferCall(IGF, table, projectExistentialBuffer(IGF, addr));
}

static llvm::Constant *getAssignExistentialsFunction(IRGenModule &IGM,
                                                     llvm::Type *objectPtrTy);

namespace {
  struct DestroyBuffer : Cleanup {
    Address Buffer;
    llvm::Value *Table;
    DestroyBuffer(Address buffer, llvm::Value *table)
      : Buffer(buffer), Table(table) {}

    void emit(IRGenFunction &IGF) const {
      emitDestroyBufferCall(IGF, Table, Buffer);
    }
  };

  struct DeallocateBuffer : Cleanup {
    Address Buffer;
    llvm::Value *Table;
    DeallocateBuffer(Address buffer, llvm::Value *table)
      : Buffer(buffer), Table(table) {}

    void emit(IRGenFunction &IGF) const {
      emitDeallocateBufferCall(IGF, Table, Buffer);
    }
  };

  struct DestroyExistential : Cleanup {
    Address Addr;
    DestroyExistential(Address addr) : Addr(addr) {}

    void emit(IRGenFunction &IGF) const {
      emitDestroyExistential(IGF, Addr);
    }
  };

  /// A CRTP class for visiting the witnesses of a protocol.
  ///
  /// The design here is that each entry (or small group of entries)
  /// gets turned into a call to the implementation class describing
  /// the exact variant of witness.  For example, for member
  /// variables, there should be separate callbacks for adding a
  /// getter/setter pair, for just adding a getter, and for adding a
  /// physical projection (if we decide to support that).
  template <class T> class WitnessVisitor {
  protected:
    IRGenModule &IGM;

    WitnessVisitor(IRGenModule &IGM) : IGM(IGM) {}

  public:
    void visit(ProtocolDecl *protocol) {
      visitInherited(protocol->getInherited());
      visitMembers(protocol->getMembers());
    }

  private:
    T &asDerived() { return *static_cast<T*>(this); }

    void visitInherited(ArrayRef<Type> inherited) {
      if (inherited.empty()) return;

      // TODO: We need to figure out all the guarantees we want here.
      // It would be abstractly good to allow conversion to a base
      // protocol to be trivial, but it's not clear that there's
      // really a structural guarantee we can rely on here.
      for (Type baseType : inherited) {
        SmallVector<ProtocolDecl *, 4> baseProtos;
        baseType->isExistentialType(baseProtos);
        for (auto baseProto : baseProtos) {
          asDerived().addOutOfLineBaseProtocol(baseType, baseProto);
        }
      }
    }

    /// Visit the witnesses for the direct members of a protocol.
    void visitMembers(ArrayRef<Decl*> members) {
      for (Decl *member : members) {
        visitMember(member);
      }
    }

    void visitMember(Decl *member) {
      switch (member->getKind()) {
      case DeclKind::Import:
      case DeclKind::Extension:
      case DeclKind::PatternBinding:
      case DeclKind::TopLevelCode:
      case DeclKind::OneOf:
      case DeclKind::Struct:
      case DeclKind::Class:
      case DeclKind::Protocol:
      case DeclKind::OneOfElement:
      case DeclKind::Constructor:
        llvm_unreachable("declaration not legal as a protocol member");

      case DeclKind::Func:
        return visitFunc(cast<FuncDecl>(member));

      case DeclKind::Subscript:
        IGM.unimplemented(member->getLoc(),
                          "subscript declaration in protocol");
        return;

      case DeclKind::Var:
        IGM.unimplemented(member->getLoc(), "var declaration in protocol");
        return;

      case DeclKind::TypeAlias:
        // Nothing to do for associated types.
        // FIXME: Is this always true? We might want a type descriptor.
        return;
      }
      llvm_unreachable("bad decl kind");
    }

    void visitFunc(FuncDecl *func) {
      if (func->isStatic()) {
        asDerived().addStaticMethod(func);
      } else {
        asDerived().addInstanceMethod(func);
      }
    }
  };

  /// A class which lays out a witness table in the abstract.
  class WitnessTableLayout : public WitnessVisitor<WitnessTableLayout> {
    unsigned NumWitnesses;
    SmallVector<WitnessTableEntry, 16> Entries;

    WitnessIndex getNextIndex() {
      return WitnessIndex(NumWitnesses++);
    }

  public:
    WitnessTableLayout(IRGenModule &IGM)
      : WitnessVisitor(IGM), NumWitnesses(NumValueWitnesses) {}

    /// The next witness is an out-of-line base protocol.
    void addOutOfLineBaseProtocol(Type baseType, ProtocolDecl *baseProto) {
      Entries.push_back(
             WitnessTableEntry::forOutOfLineBase(baseProto, getNextIndex()));
    }

    void addStaticMethod(FuncDecl *func) {
      Entries.push_back(WitnessTableEntry::forFunction(func, getNextIndex()));
    }

    void addInstanceMethod(FuncDecl *func) {
      Entries.push_back(WitnessTableEntry::forFunction(func, getNextIndex()));
    }

    unsigned getNumWitnesses() const { return NumWitnesses; }
    ArrayRef<WitnessTableEntry> getEntries() const { return Entries; }
  };

  /// A TypeInfo implementation for "protocol types", i.e. types like:
  ///   Printable
  /// with the semantic translation:
  ///   \exists t : Printable . t
  /// t here is an ArchetypeType, and a more generic protocol<> type
  /// is a ProtocolCompositionType.
  class ProtocolTypeInfo :
      public IndirectTypeInfo<ProtocolTypeInfo, FixedTypeInfo> {
    const ProtocolInfo &Protocol;

    ProtocolTypeInfo(llvm::Type *ty, Size size, Alignment align,
                     const ProtocolInfo &protocol)
      : IndirectTypeInfo(ty, size, align, IsNotPOD), Protocol(protocol) {}

  public:
    static const ProtocolTypeInfo *create(llvm::Type *ty, Size size,
                                          Alignment align,
                                          const ProtocolInfo &protocol) {
      return new ProtocolTypeInfo(ty, size, align, protocol);
    }

    const ProtocolInfo &getProtocol() const { return Protocol; }

    using FixedTypeInfo::allocate;
    Address allocate(IRGenFunction &IGF,
                     const Twine &name = "protocol.temporary") const {
      return IGF.createAlloca(getStorageType(), StorageAlignment, name);
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      auto objPtrTy = dest.getAddress()->getType();
      auto fn = getAssignExistentialsFunction(IGF.IGM, objPtrTy);
      auto call = IGF.Builder.CreateCall2(fn, dest.getAddress(),
                                          src.getAddress());
      call->setCallingConv(IGF.IGM.RuntimeCC);
      call->setDoesNotThrow();
    }

    void initializeWithCopy(IRGenFunction &IGF,
                            Address dest, Address src) const {
      // Load the witness table and copy it into the new object.
      llvm::Value *table = loadWitnessTable(IGF, src);
      IGF.Builder.CreateStore(table, projectWitnessTable(IGF, dest));

      // Project down to the buffers and ask the witnesses to do a
      // copy-initialize.
      Address srcBuffer = projectExistentialBuffer(IGF, src);
      Address destBuffer = projectExistentialBuffer(IGF, dest);
      emitInitializeBufferWithCopyOfBufferCall(IGF, table,
                                               destBuffer, srcBuffer);
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      emitDestroyExistential(IGF, addr);
    }
  };

  /// A type implementation for an ArchetypeType, otherwise known as a
  /// type variable: for example, This in a protocol declaration, or T
  /// in a generic declaration like foo<T>(x : T) -> T.  The critical
  /// thing here is that performing an operation involving archetypes
  /// is dependent on the witness binding we can see.
  class ArchetypeTypeInfo :
      public IndirectTypeInfo<ArchetypeTypeInfo, TypeInfo> {

    /// The current witness-table pointer registered with this type.
    llvm::Value *WTable = nullptr;
    
  public:
    ArchetypeTypeInfo(llvm::Type *type)
      : IndirectTypeInfo(type, Size(1), Alignment(1), IsNotPOD) {}

    /// Return the witness table that's been set for this type.
    llvm::Value *getWitnessTable(IRGenFunction &IGF) const{
      assert(WTable && "witness table not set!");
      return WTable;
    }

    /// Create an uninitialized archetype object.
    OwnedAddress allocate(IRGenFunction &IGF, Initialization &init,
                          InitializedObject object, OnHeap_t onHeap,
                          const llvm::Twine &name) const {
      if (onHeap) {
        IGF.IGM.unimplemented(SourceLoc(),
                              "on-heap emission of archetype object");
        // Just fall into the NotOnHeap path.
      }

      // Make a fixed-size buffer.
      Address buffer = IGF.createAlloca(IGF.IGM.getFixedBufferTy(),
                                        getFixedBufferAlignment(IGF.IGM),
                                        name);

      // Allocate an object of the appropriate type within it.
      llvm::Value *wtable = getWitnessTable(IGF);
      Address allocated(emitAllocateBufferCall(IGF, wtable, buffer),
                        Alignment(1));
      OwnedAddress ownedAddr(allocated, IGF.IGM.RefCountedNull);

      // Push a cleanup to dealloc it.
      IGF.pushCleanup<DeallocateBuffer>(buffer, wtable);
      CleanupsDepth dealloc = IGF.getCleanupsDepth();
      init.markAllocated(IGF, object, ownedAddr, dealloc);
      return ownedAddr;
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      emitAssignWithCopyCall(IGF, getWitnessTable(IGF),
                             dest.getAddress(), src.getAddress());
    }

    void initializeWithCopy(IRGenFunction &IGF,
                            Address dest, Address src) const {
      emitInitializeWithCopyCall(IGF, getWitnessTable(IGF),
                                 dest.getAddress(), src.getAddress());
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      emitDestroyCall(IGF, getWitnessTable(IGF), addr.getAddress());
    }    
  };

  /// Ways in which an object can fit into a fixed-size buffer.
  enum class FixedPacking {
    /// It fits at offset zero.
    OffsetZero,

    /// It doesn't fit and needs to be side-allocated.
    Allocate

    // Resilience: it needs to be checked dynamically.
  };
}

/// Detail about how an object conforms to a protocol.
class irgen::ConformanceInfo {
  friend class ProtocolInfo;

  /// The pointer to the table.  In practice, it's not really
  /// reasonable for this to always be a constant!  It probably
  /// needs to be managed by the runtime, and the information stored
  /// here would just indicate how to find the actual thing.
  llvm::Constant *Table;

  FixedPacking Packing;

public:
  llvm::Value *getTable(IRGenFunction &IGF) const {
    return Table;
  }

  /// Try to get this table as a constant pointer.  This might just
  /// not be supportable at all.
  llvm::Constant *tryGetConstantTable() const {
    return Table;
  }

  FixedPacking getPacking() const {
    return Packing;
  }
};

static FixedPacking computePacking(IRGenModule &IGM,
                                   const TypeInfo &concreteTI) {
  Size bufferSize = getFixedBufferSize(IGM);
  Size requiredSize = concreteTI.StorageSize;

  // Flat out, if we need more space than the buffer provides,
  // we always have to allocate.
  // FIXME: there might be some interesting cases where this
  // is suboptimal for oneofs.
  if (requiredSize > bufferSize)
    return FixedPacking::Allocate;

  Alignment bufferAlign = getFixedBufferAlignment(IGM);
  Alignment requiredAlign = concreteTI.StorageAlignment;

  // If the buffer alignment is good enough for the type, great.
  if (bufferAlign >= requiredAlign)
    return FixedPacking::OffsetZero;

  // TODO: consider using a slower mode that dynamically checks
  // whether the buffer size is small enough.

  // Otherwise we're stuck and have to separately allocate.
  return FixedPacking::Allocate;
}

static bool isNeverAllocated(FixedPacking packing) {
  switch (packing) {
  case FixedPacking::OffsetZero: return true;
  case FixedPacking::Allocate: return false;
  }
  llvm_unreachable("bad FixedPacking value");
}

/// Emit a 'projectBuffer' operation.  Always returns a T*.
static Address emitProjectBuffer(IRGenFunction &IGF,
                                 Address buffer,
                                 FixedPacking packing,
                                 const TypeInfo &type) {
  llvm::PointerType *resultTy = type.getStorageType()->getPointerTo();
  switch (packing) {
  case FixedPacking::Allocate: {
    Address slot = IGF.Builder.CreateBitCast(buffer, resultTy->getPointerTo(),
                                             "storage-slot");
    llvm::Value *address = IGF.Builder.CreateLoad(slot);
    return Address(address, type.StorageAlignment);
  }

  case FixedPacking::OffsetZero: {
    return IGF.Builder.CreateBitCast(buffer, resultTy, "object");
  }
    
  }
  llvm_unreachable("bad packing!");
  
}

/// Emit an 'allocateBuffer' operation.  Always returns a T*.
static Address emitAllocateBuffer(IRGenFunction &IGF,
                                  Address buffer,
                                  FixedPacking packing,
                                  const TypeInfo &type) {
  switch (packing) {
  case FixedPacking::Allocate: {
    auto sizeAndAlign = type.getSizeAndAlignment(IGF);
    llvm::Value *addr =
      IGF.emitAllocRawCall(sizeAndAlign.first, sizeAndAlign.second);
    buffer = IGF.Builder.CreateBitCast(buffer,
                                       IGF.IGM.Int8PtrTy->getPointerTo());
    IGF.Builder.CreateStore(addr, buffer);
    return IGF.Builder.CreateBitCast(Address(addr, type.StorageAlignment),
                                     type.getStorageType()->getPointerTo());
  }

  case FixedPacking::OffsetZero:
    return emitProjectBuffer(IGF, buffer, packing, type);
  }
  llvm_unreachable("bad packing!");
}

/// Emit an 'assignWithCopy' operation.
static void emitAssignWithCopy(IRGenFunction &IGF,
                               Address src, Address dest,
                               const TypeInfo &type) {
  Explosion value(ExplosionKind::Maximal);
  type.load(IGF, src, value);
  type.assign(IGF, value, dest);
}

/// Emit an 'assignWithTake' operation.
static void emitAssignWithTake(IRGenFunction &IGF,
                               Address src, Address dest,
                               const TypeInfo &type) {
  Explosion value(ExplosionKind::Maximal);
  type.loadAsTake(IGF, src, value);
  type.assign(IGF, value, dest);
}

/// Emit a 'deallocateBuffer' operation.
static void emitDeallocateBuffer(IRGenFunction &IGF,
                                 Address buffer,
                                 FixedPacking packing,
                                 const TypeInfo &type) {
  switch (packing) {
  case FixedPacking::Allocate: {
    Address slot =
      IGF.Builder.CreateBitCast(buffer, IGF.IGM.Int8PtrTy->getPointerTo());
    llvm::Value *addr = IGF.Builder.CreateLoad(slot, "storage");
    IGF.emitDeallocRawCall(addr, type.getSizeOnly(IGF));
    return;
  }

  case FixedPacking::OffsetZero:
    return;
  }
  llvm_unreachable("bad packing!");
}

namespace {
  /// A cleanup for deallocating a buffer when the concrete type
  /// stored there is known.
  class DeallocateConcreteBuffer : public Cleanup {
    Address Buffer;
    FixedPacking Packing;
    const TypeInfo &ConcreteTI;

  public:
    DeallocateConcreteBuffer(Address buffer, FixedPacking packing,
                             const TypeInfo &concreteTI)
      : Buffer(buffer), Packing(packing), ConcreteTI(concreteTI) {}

    void emit(IRGenFunction &IGF) const {
      emitDeallocateBuffer(IGF, Buffer, Packing, ConcreteTI);
    }
  };
}

/// Emit a 'destroyObject' operation.
static void emitDestroyObject(IRGenFunction &IGF,
                              Address object,
                              const TypeInfo &type) {
  if (!type.isPOD(ResilienceScope::Local))
    type.destroy(IGF, object);
}

/// Emit a 'destroyBuffer' operation.
static void emitDestroyBuffer(IRGenFunction &IGF,
                              Address buffer,
                              FixedPacking packing,
                              const TypeInfo &type) {
  Address object = emitProjectBuffer(IGF, buffer, packing, type);
  emitDestroyObject(IGF, object, type);
  emitDeallocateBuffer(IGF, buffer, packing, type);
}

/// Emit an 'initializeWithCopy' operation.
static void emitInitializeWithCopy(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   const TypeInfo &type) {
  type.initializeWithCopy(IGF, dest, src);
}

/// Emit an 'initializeWithTake' operation.
static void emitInitializeWithTake(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   const TypeInfo &type) {
  type.initializeWithTake(IGF, dest, src);
}

/// Emit an 'initializeBufferWithCopyOfBuffer' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                                    Address dest,
                                                    Address src,
                                                    FixedPacking packing,
                                                    const TypeInfo &type) {
  Address destObject = emitAllocateBuffer(IGF, dest, packing, type);
  Address srcObject = emitProjectBuffer(IGF, src, packing, type);
  emitInitializeWithCopy(IGF, destObject, srcObject, type);
  return destObject;
}

/// Emit an 'initializeBufferWithCopy' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithCopy(IRGenFunction &IGF,
                                            Address dest,
                                            Address srcObject,
                                            FixedPacking packing,
                                            const TypeInfo &type) {
  Address destObject = emitAllocateBuffer(IGF, dest, packing, type);
  emitInitializeWithCopy(IGF, destObject, srcObject, type);
  return destObject;
}

/// Emit an 'initializeBufferWithTake' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithTake(IRGenFunction &IGF,
                                            Address dest,
                                            Address srcObject,
                                            FixedPacking packing,
                                            const TypeInfo &type) {
  Address destObject = emitAllocateBuffer(IGF, dest, packing, type);
  emitInitializeWithTake(IGF, destObject, srcObject, type);
  return destObject;
}

static llvm::Value *getArg(llvm::Function::arg_iterator &it,
                           StringRef name) {
  llvm::Value *arg = it++;
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
  return Address(result, type.StorageAlignment);
}

/// Get the next argument as a pointer to the given storage type.
static Address getArgAsBuffer(IRGenFunction &IGF,
                              llvm::Function::arg_iterator &it,
                              StringRef name) {
  llvm::Value *arg = getArg(it, name);
  return Address(arg, getFixedBufferAlignment(IGF.IGM));
}

/// Build a specific value-witness function.
static void buildValueWitness(IRGenModule &IGM,
                              llvm::Function *fn,
                              ValueWitness index,
                              FixedPacking packing,
                              const TypeInfo &type) {
  IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  auto argv = fn->arg_begin();
  switch (index) {
  case ValueWitness::AllocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    Address result = emitAllocateBuffer(IGF, buffer, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::AssignWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitAssignWithCopy(IGF, src, dest, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::AssignWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitAssignWithTake(IGF, src, dest, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::DeallocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    emitDeallocateBuffer(IGF, buffer, packing, type);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::Destroy: {
    Address object = getArgAs(IGF, argv, type, "object");
    emitDestroyObject(IGF, object, type);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::DestroyBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    emitDestroyBuffer(IGF, buffer, packing, type);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAsBuffer(IGF, argv, "src");
    Address result =
      emitInitializeBufferWithCopyOfBuffer(IGF, dest, src, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithCopy: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    Address result =
      emitInitializeBufferWithCopy(IGF, dest, src, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTake: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    Address result =
      emitInitializeBufferWithTake(IGF, dest, src, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitInitializeWithCopy(IGF, dest, src, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::InitializeWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitInitializeWithTake(IGF, dest, src, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::ProjectBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    Address result = emitProjectBuffer(IGF, buffer, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::SizeAndAlignment: {
    auto sizeAndAlign = type.getSizeAndAlignment(IGF);

    llvm::Type *pairTy = getSizeAndAlignmentResultType(IGF.IGM);
    llvm::Value *result = llvm::UndefValue::get(pairTy);
    result = IGF.Builder.CreateInsertValue(result, sizeAndAlign.first, 0);
    result = IGF.Builder.CreateInsertValue(result, sizeAndAlign.second, 1);
    IGF.Builder.CreateRet(result);
    return;
  }

  }
  llvm_unreachable("bad value witness kind!");
}

static llvm::Constant *asOpaquePtr(IRGenModule &IGM, llvm::Constant *in) {
  return llvm::ConstantExpr::getBitCast(in, IGM.Int8PtrTy);
}

/// Should we be defining the given helper function?
static llvm::Function *shouldDefineHelper(IRGenModule &IGM,
                                          llvm::Constant *fn) {
  llvm::Function *def = dyn_cast<llvm::Function>(fn);
  if (!def) return nullptr;
  if (!def->empty()) return nullptr;

  def->setLinkage(llvm::Function::LinkOnceODRLinkage);
  def->setVisibility(llvm::Function::HiddenVisibility);
  def->setDoesNotThrow();
  def->setCallingConv(IGM.RuntimeCC);
  return def;
}

/// Return a function which performs an assignment operation on two
/// existentials.
///
/// Existential types are nominal, so we potentially need to cast the
/// function to the appropriate object-pointer type.
static llvm::Constant *getAssignExistentialsFunction(IRGenModule &IGM,
                                                     llvm::Type *objectPtrTy) {
  llvm::Type *argTys[] = { objectPtrTy, objectPtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assign_existentials", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, getFixedBufferAlignment(IGM));
    Address src(it++, getFixedBufferAlignment(IGM));

    // If doing a self-assignment, we're done.
    llvm::BasicBlock *doneBB = IGF.createBasicBlock("done");
    llvm::BasicBlock *contBB = IGF.createBasicBlock("cont");
    llvm::Value *isSelfAssign =
      IGF.Builder.CreateICmpEQ(dest.getAddress(), src.getAddress(),
                               "isSelfAssign");
    IGF.Builder.CreateCondBr(isSelfAssign, doneBB, contBB);

    // Project down to the buffers.
    IGF.Builder.emitBlock(contBB);
    Address destBuffer = projectExistentialBuffer(IGF, dest);
    Address srcBuffer = projectExistentialBuffer(IGF, src);

    // Load the dest and source tables.
    Address destTableSlot = projectWitnessTable(IGF, dest);
    llvm::Value *destTable = IGF.Builder.CreateLoad(destTableSlot);
    llvm::Value *srcTable = loadWitnessTable(IGF, src);

    llvm::BasicBlock *matchBB = IGF.createBasicBlock("match");
    llvm::BasicBlock *noMatchBB = IGF.createBasicBlock("no-match");

    // Check whether the tables match.
    llvm::Value *sameTable =
      IGF.Builder.CreateICmpEQ(destTable, srcTable, "sameTable");
    IGF.Builder.CreateCondBr(sameTable, matchBB, noMatchBB);

    // If so, do a direct assignment.
    IGF.Builder.emitBlock(matchBB);
    llvm::Value *destObject =
      emitProjectBufferCall(IGF, destTable, destBuffer);
    llvm::Value *srcObject =
      emitProjectBufferCall(IGF, destTable, srcBuffer);
    emitAssignWithCopyCall(IGF, destTable, destObject, srcObject);
    IGF.Builder.CreateBr(doneBB);

    // Otherwise, destroy and copy-initialize.
    IGF.Builder.emitBlock(noMatchBB);
    IGF.Builder.CreateStore(srcTable, destTableSlot);
    emitDestroyBufferCall(IGF, destTable, destBuffer);
    emitInitializeBufferWithCopyOfBufferCall(IGF, srcTable,
                                             destBuffer, srcBuffer);
    IGF.Builder.CreateBr(doneBB);

    // All done.
    IGF.Builder.emitBlock(doneBB);
    IGF.Builder.CreateRetVoid();
  }
  return fn;
}

/// Return a function which takes two pointer arguments and returns
/// void immediately.
static llvm::Constant *getNoOpVoidFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_noop_void_return", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(IGM.getLLVMContext(), "entry", def);
    llvm::ReturnInst::Create(IGM.getLLVMContext(), entry);
  }
  return fn;
}

/// Return a function which takes two pointer arguments and returns
/// the first one immediately.
static llvm::Constant *getReturnSelfFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.Int8PtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_noop_self_return", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(IGM.getLLVMContext(), "entry", def);
    llvm::ReturnInst::Create(IGM.getLLVMContext(),
                             def->arg_begin(), entry);
  }
  return fn;
}

/// Return a function which takes three pointer arguments and does a
/// retaining assignWithCopy on the first two: it loads a pointer from
/// the second, retains it, loads a pointer from the first, stores the
/// new pointer in the first, and releases the old pointer.
static llvm::Constant *getAssignWithCopyStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assignWithCopy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    IGF.emitRetainCall(newValue);
    llvm::Value *oldValue = IGF.Builder.CreateLoad(dest, "old");
    IGF.Builder.CreateStore(newValue, dest);
    IGF.emitRelease(oldValue);

    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes three pointer arguments and does a
/// retaining assignWithTake on the first two: it loads a pointer from
/// the second, retains it, loads a pointer from the first, stores the
/// new pointer in the first, and releases the old pointer.
static llvm::Constant *getAssignWithTakeStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assignWithTake_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    llvm::Value *oldValue = IGF.Builder.CreateLoad(dest, "old");
    IGF.Builder.CreateStore(newValue, dest);
    IGF.emitRelease(oldValue);

    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes three pointer arguments and does a
/// retaining initWithCopy on the first two: it loads a pointer from
/// the second, retains it, and stores that in the first.
static llvm::Constant *getInitWithCopyStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_initWithCopy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    IGF.emitRetainCall(newValue);
    IGF.Builder.CreateStore(newValue, dest);

    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes two pointer arguments, loads a
/// pointer from the first, and calls swift_release on it immediately.
static llvm::Constant *getDestroyStrongFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy->getPointerTo(), IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_destroy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    Address arg(def->arg_begin(), IGM.getPointerAlignment());
    IGF.emitRelease(IGF.Builder.CreateLoad(arg));
    IGF.Builder.CreateRetVoid();
  }
  return fn;
}

/// Return a function which takes three pointer arguments, memcpys
/// from the second to the first, and returns the first argument.
static llvm::Constant *getMemCpyFunction(IRGenModule &IGM,
                                         const TypeInfo &type) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.Int8PtrTy, argTys, false);

  // We need to unique by both size and alignment.  Note that we're
  // assuming that it's safe to call a function that returns a pointer
  // at a site that assumes the function returns void.
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    nameStream << "__swift_memcpy";
    nameStream << type.StorageSize.getValue();
    nameStream << '_';
    nameStream << type.StorageAlignment.getValue();
  }

  llvm::Constant *fn = IGM.Module.getOrInsertFunction(name, fnTy);
  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, type.StorageAlignment);
    Address src(it++, type.StorageAlignment);
    IGF.emitMemCpy(dest, src, type.StorageSize);
    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes one pointer argument and returns a
/// constant size and alignment.
static llvm::Constant *getSizeAndAlignmentFunction(IRGenModule &IGM,
                                                   const TypeInfo &type) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy };
  llvm::Type *resultTy = getSizeAndAlignmentResultType(IGM);
  llvm::FunctionType *fnTy = llvm::FunctionType::get(resultTy, argTys, false);

  // We need to unique by both size and alignment.  Note that we're
  // assuming that it's safe to call a function that returns a pointer
  // at a site that assumes the function returns void.
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    nameStream << "__swift_sizeAndAlignment_";
    nameStream << type.StorageSize.getValue();
    nameStream << '_';
    nameStream << type.StorageAlignment.getValue();
  }

  llvm::Constant *fn = IGM.Module.getOrInsertFunction(name, fnTy);
  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto sizeAndAlign = type.getSizeAndAlignment(IGF);
    assert(isa<llvm::ConstantInt>(sizeAndAlign.first));
    assert(isa<llvm::ConstantInt>(sizeAndAlign.second));

    llvm::Value *result = llvm::UndefValue::get(resultTy);
    result = IGF.Builder.CreateInsertValue(result, sizeAndAlign.first, 0);
    result = IGF.Builder.CreateInsertValue(result, sizeAndAlign.second, 1);
    IGF.Builder.CreateRet(result);
  }
  return fn;
}


/// Find a witness to the fact that a type is a value type.
/// Always returns an i8*.
static llvm::Constant *getValueWitness(IRGenModule &IGM,
                                       ValueWitness index,
                                       FixedPacking packing,
                                       Type concreteType,
                                       const TypeInfo &concreteTI) {
  // Try to use a standard function.
  switch (index) {
  case ValueWitness::DeallocateBuffer:
    if (isNeverAllocated(packing))
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    goto standard;

  case ValueWitness::DestroyBuffer:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      if (isNeverAllocated(packing))
        return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      assert(isNeverAllocated(packing));
      return asOpaquePtr(IGM, getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::Destroy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeBufferWithCopyOfBuffer:
  case ValueWitness::InitializeBufferWithCopy:
    if (packing == FixedPacking::OffsetZero) {
      if (concreteTI.isPOD(ResilienceScope::Local)) {
        return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
      } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
        return asOpaquePtr(IGM, getInitWithCopyStrongFunction(IGM));
      }
    }
    goto standard;

  case ValueWitness::InitializeBufferWithTake:
    if (packing == FixedPacking::OffsetZero)    
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    goto standard;

  case ValueWitness::InitializeWithTake:
    return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));

  case ValueWitness::AssignWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getAssignWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::AssignWithTake:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getAssignWithTakeStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getInitWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer:
    if (packing == FixedPacking::OffsetZero)
      return asOpaquePtr(IGM, getReturnSelfFunction(IGM));
    goto standard;

  case ValueWitness::SizeAndAlignment:
    return asOpaquePtr(IGM, getSizeAndAlignmentFunction(IGM, concreteTI));
  }
  llvm_unreachable("bad value witness kind");

 standard:
  llvm::Function *fn =
    IGM.getAddrOfValueWitness(concreteType, index);
  if (fn->empty())
    buildValueWitness(IGM, fn, index, packing, concreteTI);
  return asOpaquePtr(IGM, fn);
}

namespace {
  /// A class which builds a single witness.
  class WitnessBuilder {
    IRGenModule &IGM;
    llvm::Constant *ImplPtr;
    Type ImplTy;
    Type SignatureTy;
    Type SignatureResultTy;
    Type ImplResultTy;
    unsigned UncurryLevel;

    /// The return value needs to be returned via a fixed-size buffer.
    bool HasAbstractedResult;

    /// At least one of the arguments is abstracted.
    bool HasAbstractedArg;

    /// An argument of the implementation function, and how to
    /// construct it from the witness parameters.  There's an entry
    /// here for each possible "abstraction point", which includes
    /// non-singleton tuples.
    class Arg {
    public:
      enum Kind {
        /// This argument is a placeholder for a tuple that could have
        /// been abstracted, but is not.
        DecomposedTuple,

        /// This argument is nested within a type that's been abstracted.
        Nested,

        /// This argument is passed normally.
        Direct,

        /// This argument requires a bitcast but is otherwise passed
        /// normally.
        Bitcast,

        /// This argument is passed to the witness abstractly and
        /// needs to be exploded.
        IndirectToDirect,

        /// This argument is passed to the witness as an l-value and
        /// needs to be loaded.
        LValueToRValue
      };

    private:
      Kind TheKind;
      union {
        const TypeInfo *TI;
        llvm::PointerType *PtrTy;
      };

      static bool isTypeInfoKind(Kind kind) {
        return (kind == Direct ||
                kind == IndirectToDirect ||
                kind == LValueToRValue);
      }

      static bool isPointerTypeKind(Kind kind) {
        return (kind == Bitcast);
      }

      static bool isNonTriviallyAbstractedKind(Kind kind) {
        return (kind == IndirectToDirect || kind == LValueToRValue);
      }

    public:
      Arg(Kind kind) : TheKind(kind) {
        assert(kind == DecomposedTuple || kind == Nested);
      }
      Arg(Kind kind, const TypeInfo &type) : TheKind(kind), TI(&type) {
        assert(isTypeInfoKind(kind));
      }
      Arg(Kind kind, llvm::PointerType *type) : TheKind(kind), PtrTy(type) {
        assert(isPointerTypeKind(kind));
      }

      Kind getKind() const { return TheKind; }
      bool isNonTriviallyAbstracted() const {
        return isNonTriviallyAbstractedKind(getKind());
      }

      const TypeInfo &getTypeInfo() const {
        assert(isTypeInfoKind(getKind()));
        return *TI;
      }

      llvm::PointerType *getPointerType() const {
        assert(isPointerTypeKind(getKind()));
        return PtrTy;
      }
    };

    SmallVector<Arg, 16> Args;
    SmallVector<unsigned, 4> UncurryClauseBegin;

    unsigned NextParam;

  public:
    WitnessBuilder(IRGenModule &IGM, llvm::Constant *impl,
                   Type implTy, Type sigTy, unsigned uncurryLevel)
      : IGM(IGM), ImplPtr(impl), UncurryLevel(uncurryLevel) {

      implTy = implTy->getUnlabeledType(IGM.Context)->getCanonicalType();
      ImplTy = implTy;

      sigTy = sigTy->getUnlabeledType(IGM.Context)->getCanonicalType();
      SignatureTy = sigTy;

      // Find abstract parameters.
      HasAbstractedArg = false;
      for (unsigned i = 0; i != uncurryLevel + 1; ++i) {
        FunctionType *sigFnTy = cast<FunctionType>(sigTy);
        FunctionType *implFnTy = cast<FunctionType>(implTy);
        sigTy = sigFnTy->getResult();
        implTy = implFnTy->getResult();

        UncurryClauseBegin.push_back(Args.size());
        findAbstractParameters(implFnTy->getInput(), sigFnTy->getInput());
      }

      ImplResultTy = implTy;
      SignatureResultTy = sigTy;
      HasAbstractedResult = hasAbstractResult(sigTy);
    }

    /// Add entries to Args for all the abstraction points in this type.
    void collectNestedArgs(Type param) {
      Args.push_back(Arg(Arg::Nested));
      if (TupleType *tuple = dyn_cast<TupleType>(param)) {
        for (auto &field : tuple->getFields())
          collectNestedArgs(field.getType());
      }
    }

    /// Find the abstract parameters.
    void findAbstractParameters(Type impl, Type sig) {
      // Walk recursively into tuples.
      if (auto sigTuple = dyn_cast<TupleType>(sig)) {
        // The tuple itself is a potential point of abstraction.
        Args.push_back(Arg(Arg::DecomposedTuple));

        auto sigFields = sigTuple->getFields();
        auto implFields = cast<TupleType>(impl)->getFields();
        assert(sigFields.size() == implFields.size());

        for (unsigned i = 0, e = sigFields.size(); i != e; ++i)
          findAbstractParameters(implFields[i].getType(),
                                 sigFields[i].getType());
        return;
      }

      // Check whether we're directly matching an archetype.
      if (isa<ArchetypeType>(sig)) {
        // Count all the nested types.
        collectNestedArgs(impl);

        // That will include an entry for this type; remove that.
        Args.pop_back();

        // If the implementation type is passed indirectly anyway, we
        // don't need any abstraction.
        const TypeInfo &implTI = IGM.getFragileTypeInfo(impl);
        ExplosionSchema schema = implTI.getSchema(ExplosionKind::Minimal);
        if (schema.isSingleAggregate()) {
          auto ptrTy = schema.begin()->getAggregateType()->getPointerTo();
          Args.push_back(Arg(Arg::Bitcast, ptrTy));

        // Otherwise, we have to do indirect-to-direct lowering.
        } else {
          Args.push_back(Arg(Arg::IndirectToDirect, implTI));
          HasAbstractedArg = true;
        }

        return;
      }

      // Check for an l-value.  In some cases, we need an
      // lvalue-to-rvalue abstraction.
      if (isa<LValueType>(sig)) {
        const TypeInfo &implTI = IGM.getFragileTypeInfo(impl);

        // If we've got l-values on both sides, we can just directly convert.
        if (isa<LValueType>(impl)) {
          auto ptrTy = cast<llvm::PointerType>(implTI.getStorageType());
          Args.push_back(Arg(Arg::Bitcast, ptrTy));

        // Otherwise, assume this is 'this'-conversion, where the
        // protocol user passes 'this' by reference but the
        // implementation takes it by value.
        } else {
          Args.push_back(Arg(Arg::LValueToRValue, implTI));
          HasAbstractedArg = true;
        }
        return;
      }

      const TypeInfo &implTI = IGM.getFragileTypeInfo(impl);

      // The basic function representation doesn't change just because
      // you involved an archetype, but we might need to translate
      // function values to make the types work.
      if (isa<FunctionType>(sig)) {
        if (!sig->isEqual(impl)) {
          IGM.unimplemented(SourceLoc(), "can't rewrite function values!");
        }

        Args.push_back(Arg(Arg::Direct, implTI));
        return;
      }

      // That's all the structural types, so the types really ought to
      // match perfectly now.
      assert(sig->isEqual(impl));
      Args.push_back(Arg(Arg::Direct, implTI));
    }

    /// If the given type includes an archetype, we need an abstracted
    /// result because we have to return into a buffer.
    bool hasAbstractResult(Type sig) {
      if (auto tuple = sig->getAs<TupleType>())
        for (auto &field : tuple->getFields())
          if (hasAbstractResult(field.getType()))
            return true;
      return sig->is<ArchetypeType>();
    }

    llvm::Constant *get() {
      // If we don't need any abstractions, we're golden.
      if (!HasAbstractedResult && !HasAbstractedArg)
        return asOpaquePtr(IGM, ImplPtr);

      // Okay, mangle a name.
      llvm::SmallString<128> name;
      mangleThunk(name);

      // If a function with that name exists, use it.  We don't care
      // about the type.
      llvm::Function *fn = IGM.Module.getFunction(name);
      if (fn) return asOpaquePtr(IGM, fn);

      // Create the function.
      auto fnTy = IGM.getFunctionType(SignatureTy, ExplosionKind::Minimal,
                                      UncurryLevel, /*withData*/ false);
      fn = llvm::Function::Create(fnTy, llvm::Function::LinkOnceODRLinkage,
                                  name.str(), &IGM.Module);
      fn->setVisibility(llvm::Function::HiddenVisibility);

      // Start building it.
      IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                        ExplosionKind::Minimal, UncurryLevel, fn,
                        Prologue::Bare);
      emitThunk(IGF);

      return asOpaquePtr(IGM, fn);
    }

    void emitThunk(IRGenFunction &IGF) {
      Explosion outerArgs = IGF.collectParameters();
      Explosion innerArgs(ExplosionKind::Minimal);

      const TypeInfo &innerResultTI = IGM.getFragileTypeInfo(ImplResultTy);
      bool innerHasIndirectResult =
        innerResultTI.getSchema(innerArgs.getKind()).requiresIndirectResult();

      Address innerResult;
      Address outerResult;

      // Pull off the outer result.
      FixedPacking innerResultPacking;
      CleanupsDepth outerResultCleanup = CleanupsDepth::invalid();
      if (HasAbstractedResult) {
        outerResult = Address(outerArgs.claimUnmanagedNext(),
                              getFixedBufferAlignment(IGM));
        innerResultPacking = computePacking(IGM, innerResultTI);

        // Allocate space for the output.
        innerResult = emitAllocateBuffer(IGF, outerResult,
                                         innerResultPacking, innerResultTI);

        // Enter a deallocate cleanup if necessary.
        if (!isNeverAllocated(innerResultPacking)) {
          IGF.pushFullExprCleanup<DeallocateConcreteBuffer>(outerResult,
                                                            innerResultPacking,
                                                            innerResultTI);
          outerResultCleanup = IGF.getCleanupsDepth();
        }
      } else if (innerHasIndirectResult) {
        innerResult = Address(outerArgs.claimUnmanagedNext(),
                              innerResultTI.StorageAlignment);
      }

      // Add the inner result.
      if (innerHasIndirectResult) {
        assert(innerResult.isValid());
        innerArgs.addUnmanaged(innerResult.getAddress());
      }

      // Okay, walk backwards through the parameter clauses,
      // forward the arguments.
      for (unsigned i = UncurryLevel + 1; i != 0; ) {
        unsigned clauseBegin = UncurryClauseBegin[--i];
        unsigned clauseEnd =
          (i == UncurryLevel ? Args.size() : UncurryClauseBegin[i+1]);

        forwardArgs(IGF, outerArgs, innerArgs,
                    ArrayRef<Arg>(&Args[clauseBegin], clauseEnd - clauseBegin));
      }

      // Do the call.
      SmallVector<llvm::Value*, 16> finalArgs;
      innerArgs.forward(IGF, innerArgs.size(), finalArgs);

      // FIXME: exceptions!  Or better yet, get the basic call
      // machinery to do all this for us.
      llvm::CallInst *call = IGF.Builder.CreateCall(ImplPtr, finalArgs);

      // In an abstracted result, we build into the buffer given us,
      // but we also return the result pointer.
      if (HasAbstractedResult) {
        if (outerResultCleanup.isValid())
          IGF.setCleanupState(outerResultCleanup, CleanupState::Dead);

        llvm::Value *addr = innerResult.getAddress();
        addr = IGF.Builder.CreateBitCast(addr, IGM.Int8PtrTy);
        IGF.Builder.CreateRet(addr);

      // Otherwise we're just using the call return.
      } else if (call->getType()->isVoidTy()) {
        IGF.Builder.CreateRetVoid();
      } else {
        IGF.Builder.CreateRet(call);
      }
    }

    void forwardArgs(IRGenFunction &IGF, Explosion &outerArgs,
                     Explosion &innerArgs, ArrayRef<Arg> techniques) {
      for (auto &technique : techniques) {
        switch (technique.getKind()) {
        case Arg::DecomposedTuple: continue;
        case Arg::Nested: continue;

        case Arg::Direct:
          technique.getTypeInfo().reexplode(IGF, outerArgs, innerArgs);
          continue;

        case Arg::Bitcast: {
          llvm::Value *value = outerArgs.claimUnmanagedNext();
          value = IGF.Builder.CreateBitCast(value, technique.getPointerType());
          innerArgs.addUnmanaged(value);
          continue;
        }

        case Arg::IndirectToDirect: {
          const TypeInfo &innerTI = technique.getTypeInfo();
          llvm::Value *rawAddr = outerArgs.claimUnmanagedNext();
          rawAddr = IGF.Builder.CreateBitCast(rawAddr,
                                   innerTI.getStorageType()->getPointerTo());

          innerTI.loadAsTake(IGF, Address(rawAddr, innerTI.StorageAlignment),
                             innerArgs);
          continue;
        }

        case Arg::LValueToRValue: {
          const TypeInfo &innerTI = technique.getTypeInfo();
          llvm::Value *rawAddr = outerArgs.claimUnmanagedNext();
          rawAddr = IGF.Builder.CreateBitCast(rawAddr,
                                   innerTI.getStorageType()->getPointerTo());

          innerTI.load(IGF, Address(rawAddr, innerTI.StorageAlignment),
                       innerArgs);
          continue;
        }
        }
        llvm_unreachable("bad Arg technique kind");
      }
    }

    /// Mangle the name of the thunk this requires.
    void mangleThunk(SmallVectorImpl<char> &buffer) {
      llvm::raw_svector_ostream str(buffer);

      StringRef fnName =
        cast<llvm::Function>(ImplPtr->stripPointerCasts())->getName();
      str << "_T";
      str << (HasAbstractedResult ? "nr" : "na");

      // Mangle the nontrivially-abstracted args as spans between the
      // slots with nontrivial abstraction.
      if (HasAbstractedArg) {
        unsigned last = ~0U;
        for (unsigned i = 0, e = Args.size(); i != e; ++i) {
          if (!Args[i].isNonTriviallyAbstracted()) continue;

          unsigned n = (last == ~0U ? i : i - last - 1);
          last = i;

          mangleCount(str, n, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                              "abcdefghijklmnopqrstuvwxyz");
        }
      }

      str << '_';
      if (fnName.startswith("_T")) {
        str << fnName.substr(2);
      } else {
        str << '_' << fnName;
      }
    }

    /// Mangle a count as a sequence of characters from the given alphabet.
    /// The last character in the alphabet is the 'continuation' character:
    /// it means "add one less than the size of the alphabet to the total"
    /// and then consider another character.  All the other characters have
    /// the value of their sequence in the alphabet.
    ///
    /// Thus, a count sequence never ends with the last character in the
    /// alphabet, and its value is the sum of the ordinal values minus
    /// the number of continuation characters.
    ///
    /// For example, if the alphabet were "12345", we would have
    ///   "3"        ==                   0 + 3 == 3
    ///   "51"       == 5               - 1 + 1 == 5
    ///   "5554"     == 5+5+5           - 3 + 4 == 16
    ///   "55555552" == 5+5+5+5+5+5+5+5 - 7 + 2 == 30
    ///
    /// This is a reasonable encoding given that most functions are not
    /// going to have an absolutely enormous number of formal arguments.
    template <unsigned AlphabetSize>
    static void mangleCount(llvm::raw_svector_ostream &str, unsigned count,
                            const char (&alphabet)[AlphabetSize]) {
      // -1 for continuation character, -1 for '\0'.
      const unsigned numTerminalChars = AlphabetSize - 2;
      do {
        unsigned index = std::min(count, numTerminalChars);
        count -= index;
        str << alphabet[index];
      } while (count != 0);
    }
  };

  /// A class which lays out a specific conformance to a protocol.
  class WitnessTableBuilder : public WitnessVisitor<WitnessTableBuilder> {
    SmallVectorImpl<llvm::Constant*> &Table;
    FixedPacking Packing;
    Type ConcreteType;
    const TypeInfo &ConcreteTI;
    const ProtocolConformance &Conformance;
    
  public:
    WitnessTableBuilder(IRGenModule &IGM,
                        SmallVectorImpl<llvm::Constant*> &table,
                        FixedPacking packing,
                        Type concreteType, const TypeInfo &concreteTI,
                        const ProtocolConformance &conformance)
      : WitnessVisitor(IGM), Table(table), Packing(packing),
        ConcreteType(concreteType), ConcreteTI(concreteTI),
        Conformance(conformance) {}

    /// A base protocol is witnessed by a pointer to the conformance
    /// of this type to that protocol.
    void addOutOfLineBaseProtocol(Type baseType, ProtocolDecl *baseProto) {
      // Look for a protocol type info.
      const ProtocolTypeInfo &baseTI =
        IGM.getFragileTypeInfo(baseType).as<ProtocolTypeInfo>();
      const ProtocolConformance *astConf =
        Conformance.InheritedMapping.find(baseProto)->second;
      assert(astConf && "couldn't find base conformance!");
      const ConformanceInfo &conf =
        baseTI.getProtocol().getConformance(IGM, ConcreteType, ConcreteTI,
                                            baseProto, *astConf);

      llvm::Constant *baseWitness = conf.tryGetConstantTable();
      assert(baseWitness && "couldn't get a constant table!");
      Table.push_back(asOpaquePtr(IGM, baseWitness));
    }

    /// A static method is just witnessed by the 
    void addStaticMethod(FuncDecl *func) {
      // FIXME
      Table.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
    }

    void addInstanceMethod(FuncDecl *iface) {
      FuncDecl *impl = cast<FuncDecl>(Conformance.Mapping.find(iface)->second);
      Table.push_back(getInstanceMethodWitness(impl, iface->getType()));
    }

    /// Returns a function which calls the given implementation under
    /// the given interface.
    llvm::Constant *getInstanceMethodWitness(FuncDecl *impl, Type ifaceType) {
      llvm::Constant *implPtr =
        IGM.getAddrOfFunction(impl, ExplosionKind::Minimal, 1, /*data*/ false);
      return getWitness(implPtr, impl->getType(), ifaceType, 1);
    }

    llvm::Constant *getWitness(llvm::Constant *fn, Type fnTy, Type ifaceTy,
                               unsigned uncurryLevel) {
      return WitnessBuilder(IGM, fn, fnTy, ifaceTy, uncurryLevel).get();
    }
  };
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

  // If not, layout the protocol's witness table.
  WitnessTableLayout layout(IGM);
  layout.visit(protocol);

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
ProtocolInfo::getConformance(IRGenModule &IGM, Type concreteType,
                             const TypeInfo &concreteTI,
                             ProtocolDecl *protocol,
                             const ProtocolConformance &conformance) const {
  // Check whether we've already cached this.
  auto it = Conformances.find(&conformance);
  if (it != Conformances.end()) return *it->second;

  // We haven't.  First things first:  compute the packing.
  FixedPacking packing = computePacking(IGM, concreteTI);

  // Build the witnesses:
  SmallVector<llvm::Constant*, 32> witnesses;

  // First, build the value witnesses.
  for (unsigned i = 0; i != NumValueWitnesses; ++i) {
    witnesses.push_back(getValueWitness(IGM, ValueWitness(i),
                                        packing, concreteType,
                                        concreteTI));
  }

  // Next, build the protocol witnesses.
  WitnessTableBuilder(IGM, witnesses, packing, concreteType, concreteTI,
                      conformance).visit(protocol);

  // We've got our global initializer.
  llvm::ArrayType *tableTy =
    llvm::ArrayType::get(IGM.Int8PtrTy, witnesses.size());
  llvm::Constant *initializer =
    llvm::ConstantArray::get(tableTy, witnesses);

  // Construct a variable for that.
  // FIXME: linkage, better name, agreement across t-units,
  // interaction with runtime, etc.
  llvm::GlobalVariable *var =
    new llvm::GlobalVariable(IGM.Module, tableTy, /*constant*/ true,
                             llvm::GlobalVariable::InternalLinkage,
                             initializer, "witness_table");

  // Abstract away the length.
  llvm::ConstantInt *zero = llvm::ConstantInt::get(IGM.SizeTy, 0);
  llvm::Constant *indices[] = { zero, zero };
  llvm::Constant *table =
    llvm::ConstantExpr::getInBoundsGetElementPtr(var, indices);

  ConformanceInfo *info = new ConformanceInfo;
  info->Table = table;
  info->Packing = packing;

  auto res = Conformances.insert(std::make_pair(&conformance, info));
  return *res.first->second;
}

const TypeInfo *TypeConverter::convertProtocolType(ProtocolType *T) {
  // Protocol types are nominal.
  llvm::StructType *type = IGM.createNominalType(T->getDecl());
  llvm::Type *fields[] = {
    IGM.Int8PtrTy->getPointerTo(0),  // witness table
    IGM.getFixedBufferTy()           // value buffer
  };
  type->setBody(fields);

  const ProtocolInfo &protocol = getProtocolInfo(T->getDecl());

  Alignment align = getFixedBufferAlignment(IGM);

  Size size = getBufferOffset(IGM);
  assert(size.roundUpToAlignment(align) == size);
  size += getFixedBufferSize(IGM);

  return ProtocolTypeInfo::create(type, size, align, protocol);
}

const TypeInfo *TypeConverter::convertArchetypeType(ArchetypeType *T) {
  // For now, just always use the same type.
  // TODO: also store something about the protocols we conform to?
  llvm::Type *storageType = IGM.getOpaqueStructTy();
  return new ArchetypeTypeInfo(storageType);
}

/// Emit an erasure expression as an initializer for the given memory.
void irgen::emitErasureAsInit(IRGenFunction &IGF, ErasureExpr *E,
                              Address dest, const TypeInfo &rawTI) {
  if (E->getConformances().size() != 1) {
    IGF.unimplemented(E->getLoc(), "erasure to a protocol composition type");
    return;
  }

  if (E->getSubExpr()->getType()->is<ArchetypeType>()) {
    IGF.unimplemented(E->getLoc(), "erasure from an archetype");
    return;
  }

  ProtocolDecl *protocol = E->getType()->castTo<ProtocolType>()->getDecl();

  if (!E->getConformances()[0]) {
    // Special case: we're converting from an existential type to another
    // existential type in some trivial manner (e.g., to an inherited protocol).
    Type srcType = E->getSubExpr()->getType();
    const ProtocolTypeInfo &srcProtoTI =
      IGF.getFragileTypeInfo(srcType).as<ProtocolTypeInfo>();
    
    // Compute the path to the destination protocol.
    auto &witnessEntry = srcProtoTI.getProtocol().getWitnessEntry(protocol);
    assert(witnessEntry.isBase());
    
    // Emit the sub-expression in-place.  Bitcasting the destination is
    // safe because every protocol type has identical representation.
    // Regardless of the other decisions we make, we won't need to touch
    // the buffer.
    auto derivedPtrTy = srcProtoTI.getStorageType()->getPointerTo();
    dest = IGF.Builder.CreateBitCast(dest, derivedPtrTy);
    IGF.emitRValueAsInit(E->getSubExpr(), dest, srcProtoTI);
    
    // If it's a prefix base, that's good enough.
    if (!witnessEntry.isOutOfLineBase()) return;
    
    // Otherwise, we need to map the witness down.  Find the old table.
    Address tableSlot = projectWitnessTable(IGF, dest);
    llvm::Value *oldTable = IGF.Builder.CreateLoad(tableSlot, "derived-table");
    
    // Drill down to get the new table.
    llvm::Value *newTable =
    loadOpaqueWitness(IGF, oldTable, witnessEntry.getOutOfLineBaseIndex());
    newTable = IGF.Builder.CreateBitCast(newTable, getWitnessTableTy(IGF.IGM));
    
    // Drop it in place and we're done.
    IGF.Builder.CreateStore(newTable, tableSlot);

    return;
  }

  // We're converting from a concrete type to an existential type.
  auto &protoI = rawTI.as<ProtocolTypeInfo>().getProtocol();

  // Find the concrete type.
  Type concreteType = E->getSubExpr()->getType();
  const TypeInfo &concreteTI = IGF.getFragileTypeInfo(concreteType);

  // Compute the conformance information.
  assert(E->getConformances()[0]);
  const ConformanceInfo &conformance =
    protoI.getConformance(IGF.IGM, concreteType, concreteTI,
                          protocol, *E->getConformances()[0]);

  // First things first: compute the table and store that into the
  // destination.

  llvm::Value *table = conformance.getTable(IGF);
  Address tableSlot = projectWitnessTable(IGF, dest);
  IGF.Builder.CreateStore(table, tableSlot);

  FixedPacking packing = conformance.getPacking();

  // If the type is provably empty, just emit the operand as ignored.
  if (concreteTI.isEmpty(ResilienceScope::Local)) {
    assert(packing == FixedPacking::OffsetZero);
    return IGF.emitIgnored(E->getSubExpr());
  }

  // Otherwise, allocate if necessary.
  Address buffer = projectExistentialBuffer(IGF, dest);
  Address object = emitAllocateBuffer(IGF, buffer, packing, concreteTI);

  // Push a cleanup to destroy that.
  CleanupsDepth deallocCleanup;
  bool needsDeallocCleanup = !isNeverAllocated(packing);
  if (needsDeallocCleanup) {
    IGF.pushFullExprCleanup<DeallocateConcreteBuffer>(buffer, packing, concreteTI);
    deallocCleanup = IGF.getCleanupsDepth();
  }

  // Emit the object in-place.
  IGF.emitRValueAsInit(E->getSubExpr(), object, concreteTI);

  // Deactivate the dealloc cleanup.
  if (needsDeallocCleanup) {
    IGF.setCleanupState(deallocCleanup, CleanupState::Dead);
  }
}

/// Emit an expression which erases the concrete type of its value and
/// replaces it with a generic type.
void irgen::emitErasure(IRGenFunction &IGF, ErasureExpr *E, Explosion &out) {
  const ProtocolTypeInfo &protoTI =
    IGF.getFragileTypeInfo(E->getType()).as<ProtocolTypeInfo>();

  // Create a temporary of the appropriate type.
  Address temp = protoTI.allocate(IGF);

  // Initialize the temporary.
  emitErasureAsInit(IGF, E, temp, protoTI);

  // Add that as something to destroy.
  IGF.enterDestroyCleanup(temp, protoTI, out);
}

/// Emit an expression which accesses a member out of an existential type.
void irgen::emitExistentialMemberRef(IRGenFunction &IGF,
                                     ExistentialMemberRefExpr *E,
                                     Explosion &out) {
  // The l-value case should have been weeded out.
  assert(!E->getType()->is<LValueType>());

  // The remaining case is to construct an implicit closure.
  // Just refuse to do this for now.
  assert(E->getType()->is<FunctionType>());
  IGF.unimplemented(E->getLoc(),
              "forming implicit closure over existential member reference");
  IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), out);
}

/// Emit an expression which accesses a member out of an existential type.
LValue irgen::emitExistentialMemberRefLValue(IRGenFunction &IGF,
                                             ExistentialMemberRefExpr *E) {

  ValueDecl *decl = E->getDecl();
  assert(isa<VarDecl>(decl) || isa<SubscriptDecl>(decl));

  IGF.unimplemented(E->getLoc(),
                    "using existential member reference as l-value");
  return IGF.emitFakeLValue(IGF.getFragileTypeInfo(E->getType()));
}

/// Find the path of base protocols necessary to reach the given
/// function.
/// TODO: try to find an optimal path, not just the first path.
static bool findBasePath(SmallVectorImpl<ProtocolDecl*> &path,
                         ProtocolDecl *from, ProtocolDecl *to) {
  if (from == to) return true;
  for (Type ty : from->getInherited()) {
    ProtocolDecl *next = ty->castTo<ProtocolType>()->getDecl();
    path.push_back(next);
    if (findBasePath(path, next, to))
      return true;
    path.pop_back();
  }
  return false;
}
                         

/// Emit an existential member reference as a callee.
Callee irgen::emitExistentialMemberRefCallee(IRGenFunction &IGF,
                                             ExistentialMemberRefExpr *E,
                                             SmallVectorImpl<Arg> &calleeArgs,
                                             ExplosionKind maxExplosionLevel,
                                             unsigned maxUncurry) {
  // The base of an existential member reference is always an l-value.
  LValue lvalue = IGF.emitLValue(E->getBase());
  Address existAddr = IGF.emitMaterializeWithWriteback(std::move(lvalue),
                                                       NotOnHeap);

  FuncDecl *fn = cast<FuncDecl>(E->getDecl());

  Type baseTy = E->getBase()->getType();
  baseTy = baseTy->castTo<LValueType>()->getObjectType();
  assert(baseTy->is<ProtocolType>() && "base is not a protocol lvalue");
  ProtocolDecl *proto = baseTy->castTo<ProtocolType>()->getDecl();

  // Find a path to the protocol which declares the method.
  SmallVector<ProtocolDecl*, 4> basePath;
  bool foundPath =
    findBasePath(basePath, proto, cast<ProtocolDecl>(fn->getDeclContext()));
  assert(foundPath && "no path to protocol declaring method!");
  (void) foundPath;

  // Load the witness table.
  llvm::Value *table = loadWitnessTable(IGF, existAddr);

  auto &ownerPI =
    IGF.IGM.getProtocolInfo(cast<ProtocolDecl>(fn->getDeclContext()));

  if (!fn->isStatic()) {
    // Use the first-level witness table to copy into a new buffer.
    // This is important for safety.
    // TODO: just do a project when the source object is obviously not
    // aliased.
    Address copyBuffer = IGF.createAlloca(IGF.IGM.getFixedBufferTy(),
                                          getFixedBufferAlignment(IGF.IGM),
                                          "copy-for-call");
    Address srcBuffer = projectExistentialBuffer(IGF, existAddr);

    // The result of the copy is an i8* which points at the new object.
    llvm::Value *copy =
      emitInitializeBufferWithCopyOfBufferCall(IGF, table, copyBuffer,
                                               srcBuffer);

    // Enter a cleanup for the temporary.  
    IGF.pushFullExprCleanup<DestroyBuffer>(copyBuffer, table);

    // Add the temporary address as a callee arg.
    // This argument is bitcast to the type of the 'this' argument (a
    // [byref] to the archetype This), although the underlying function actually
    // takes the underlying object pointer.
    Type ThisParamTy = fn->getType()->castTo<FunctionType>()->getInput();
    copy = IGF.Builder.CreateBitCast(copy,
                                     IGF.IGM.getFragileType(ThisParamTy),
                                     "object-pointer");
    Explosion *arg = new Explosion(ExplosionKind::Minimal);
    arg->addUnmanaged(copy);
    calleeArgs.push_back(Arg::forOwned(arg));
  }

  for (unsigned i = 0, e = basePath.size(); i != e; ++i) {
    ProtocolDecl *derived = (i == 0 ? proto : basePath[i-1]);
    ProtocolDecl *base = basePath[i];

    // Find the witness table index for the base protocol.
    auto &derivedPI = IGF.IGM.getProtocolInfo(derived);
    auto &witnessEntry = derivedPI.getWitnessEntry(base);
    assert(witnessEntry.isBase());

    // If it's an inline base, the conversion is trivial.
    if (!witnessEntry.isOutOfLineBase()) continue;

    // If it's out-of-line, we have to drill down.
    table = loadOpaqueWitness(IGF, table, witnessEntry.getOutOfLineBaseIndex());
    table = IGF.Builder.CreateBitCast(table, getWitnessTableTy(IGF.IGM));
  }

  // Pull out the function witness.
  auto &witnessEntry = ownerPI.getWitnessEntry(fn);
  llvm::Value *witness =
    loadOpaqueWitness(IGF, table, witnessEntry.getFunctionIndex());

  if (fn->isStatic())
    return Callee::forKnownFunction(AbstractCC::Method, fn->getType(), witness,
                                    ManagedValue(nullptr),
                                    ExplosionKind::Minimal, 0);

  // FIXME: writeback
  return Callee::forKnownFunction(AbstractCC::Method, fn->getType(), witness,
                                  ManagedValue(nullptr),
                                  ExplosionKind::Minimal, 1);
}

/// Determine the natural limits on how we can call the given protocol
/// member function.
AbstractCallee irgen::getAbstractProtocolCallee(IRGenFunction &IGF,
                                                FuncDecl *fn) {
  // TODO: consider adding non-minimal or curried entrypoints.
  if (fn->isStatic())
    return AbstractCallee(AbstractCC::Freestanding, ExplosionKind::Minimal,
                          0, 0, false);
  return AbstractCallee(AbstractCC::Method, ExplosionKind::Minimal,
                        1, 1, false);
}

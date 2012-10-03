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

#include "CallEmission.h"
#include "Cleanup.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "FunctionRef.h"
#include "GenHeap.h"
#include "GenMeta.h"
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

/// A special index into the IGF-stored witness tables list for an
/// archetype for the metadata pointer.
const unsigned MetadataWitnessKey = ~0U;

namespace swift {
namespace irgen {
  /// A little helper to give this file some privileged access.
  /// The amount of code in this class is intentionally very small.
  class GenProto {
  public:
    static llvm::DenseMap<TypeBase*, llvm::Constant*> &
    getTrivialWitnessTablesCache(IRGenModule &IGM) {
      return IGM.Types.TrivialWitnessTables;
    }

    static llvm::Value *getArchetypeValueWitness(IRGenFunction &IGF,
                                                 const void *archetypeTI,
                                                 unsigned which) {
      auto &map = IGF.ArchetypeValueWitnessMap;
      auto key = std::make_pair(archetypeTI, which);
      assert(map.count(key) && "IGF has no wtable set for archetype");
      return map.find(key)->second;
    }

    static void setArchetypeValueWitness(IRGenFunction &IGF,
                                         const void *archetypeTI,
                                         unsigned which,
                                         llvm::Value *wtable) {
      auto &map = IGF.ArchetypeValueWitnessMap;
      auto key = std::make_pair(archetypeTI, which);
      assert(!map.count(key) && "IGF already has wtable set for archetype");
      map.insert(std::make_pair(key, wtable));
    }
  };
}
}

/// Given a type metadata pointer, load its value witness table.
static llvm::Value *emitValueWitnessTableRefForMetadata(IRGenFunction &IGF,
                                                        llvm::Value *metadata) {
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);
  auto wtableSlot = IGF.Builder.CreateStructGEP(metadata, 1);
  return IGF.Builder.CreateLoad(Address(wtableSlot,
                                        IGF.IGM.getPointerAlignment()));
}

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

static llvm::FunctionType *createWitnessFunctionType(IRGenModule &IGM,
                                                     ValueWitness index) {
  switch (index) {

  // void (*deallocateBuffer)(B *buffer, W *self);
  // void (*destroyBuffer)(B *buffer, W *self);
  case ValueWitness::DeallocateBuffer:
  case ValueWitness::DestroyBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.WitnessTablePtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // void (*destroy)(T *object, witness_t *self);
  case ValueWitness::Destroy: {
    llvm::Type *args[] = { IGM.OpaquePtrTy, IGM.WitnessTablePtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, W *self);
  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, bufPtrTy, IGM.WitnessTablePtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
  }

  // T *(*allocateBuffer)(B *buffer, W *self);
  // T *(*projectBuffer)(B *buffer, W *self);
  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.WitnessTablePtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
  }

  // T *(*initializeBufferWithCopy)(B *dest, T *src, W *self);
  // T *(*initializeBufferWithTake)(B *dest, T *src, W *self);
  case ValueWitness::InitializeBufferWithCopy:
  case ValueWitness::InitializeBufferWithTake: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.OpaquePtrTy, IGM.WitnessTablePtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
  }

  // T *(*assignWithCopy)(T *dest, T *src, W *self);
  // T *(*assignWithTake)(T *dest, T *src, W *self);
  // T *(*initializeWithCopy)(T *dest, T *src, W *self);
  // T *(*initializeWithTake)(T *dest, T *src, W *self);
  case ValueWitness::AssignWithCopy:
  case ValueWitness::AssignWithTake:
  case ValueWitness::InitializeWithCopy:
  case ValueWitness::InitializeWithTake: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *args[] = { ptrTy, ptrTy, IGM.WitnessTablePtrTy };
    return llvm::FunctionType::get(ptrTy, args, false);
  }

  case ValueWitness::Size:
  case ValueWitness::Alignment:
  case ValueWitness::Stride:
    llvm_unreachable("these witnesses aren't value witnesses!");
  }
  llvm_unreachable("bad value witness!");
}

/// Return the cached pointer-to-function type for the given value
/// witness index.
llvm::Type *IRGenModule::getValueWitnessTy(ValueWitness index) {
  static_assert(IRGenModule::NumValueWitnessFunctions
                  == ::NumValueWitnessFunctions,
                "array on IGM has the wrong size");

  /// All the non-function values are size_t's.
  if (!isValueWitnessFunction(index))
    return SizeTy;

  assert(unsigned(index) < NumValueWitnessFunctions);
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
  case ValueWitness::Size:
    return "size";
  case ValueWitness::Alignment:
    return "alignment";
  case ValueWitness::Stride:
    return "stride";
  }
  llvm_unreachable("bad value witness index");
}

namespace {
  /// The layout of an existential buffer.  This is intended to be a
  /// small, easily-computed type that can be passed around by value.
  class ExistentialLayout {
  private:
    unsigned NumTables;
    // If you add anything to the layout computation, you might need
    // to update certain uses;  check the external uses of getNumTables().
    // For example, getAssignExistentialsFunction relies on being uniqued
    // for different layout kinds.

  public:
    explicit ExistentialLayout(unsigned numTables) : NumTables(numTables) {}

    unsigned getNumTables() const { return NumTables; }

    friend bool operator==(ExistentialLayout a, ExistentialLayout b) {
      return a.NumTables == b.NumTables;
    }

    /// Given the offset of the buffer within an existential type.
    Size getBufferOffset(IRGenModule &IGM) const {
      return IGM.getPointerSize() * NumTables;
    }

    /// Given the address of an existential object, drill down to the
    /// buffer.
    Address projectExistentialBuffer(IRGenFunction &IGF, Address addr) const {
      return IGF.Builder.CreateStructGEP(addr, getNumTables(),
                                         getBufferOffset(IGF.IGM));
    }

    /// Given the address of an existential object, drill down to the
    /// witness-table field.
    Address projectWitnessTable(IRGenFunction &IGF, Address addr,
                                unsigned which) const {
      assert(which < getNumTables());
      return IGF.Builder.CreateStructGEP(addr, which,
                                         IGF.IGM.getPointerSize() * which);
    }

    /// Given the address of an existential object, load its witness table.
    llvm::Value *loadWitnessTable(IRGenFunction &IGF, Address addr,
                                  unsigned which) const {
      return IGF.Builder.CreateLoad(projectWitnessTable(IGF, addr, which),
                                    "witness-table");
    }

    /// Given the address of an existential object, find a witness
    /// table; it doesn't matter which.
    llvm::Value *loadAnyWitnessTable(IRGenFunction &IGF, Address addr) const {
      return loadWitnessTable(IGF, addr, 0);
    }
  };

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
}

/// Load a specific witness from a known table.  The result is
/// always an i8*.
static llvm::Value *loadOpaqueWitness(IRGenFunction &IGF,
                                      llvm::Value *table,
                                      WitnessIndex index) {
  assert(table->getType() == IGF.IGM.WitnessTablePtrTy);

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
  auto label = getValueWitnessLabel(index);
  auto type = IGF.IGM.getValueWitnessTy(index);
  if (isValueWitnessFunction(index)) {
    return IGF.Builder.CreateBitCast(witness, type, label);
  } else {
    return IGF.Builder.CreatePtrToInt(witness, type, label);
  }
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

/// Emit a call to do an 'initializeWithTake' operation.
static void emitInitializeWithTakeCall(IRGenFunction &IGF,
                                       llvm::Value *witnessTable,
                                       llvm::Value *destObject,
                                       llvm::Value *srcObject) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                                         ValueWitness::InitializeWithTake);
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

/// Emit a call to do an 'assignWithTake' operation.
static void emitAssignWithTakeCall(IRGenFunction &IGF,
                                   llvm::Value *witnessTable,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                                         ValueWitness::AssignWithTake);
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
static void emitDestroyExistential(IRGenFunction &IGF, Address addr,
                                   ExistentialLayout layout) {
  llvm::Value *table = layout.loadAnyWitnessTable(IGF, addr);
  emitDestroyBufferCall(IGF, table, layout.projectExistentialBuffer(IGF, addr));
}

static llvm::Constant *getAssignExistentialsFunction(IRGenModule &IGM,
                                                     llvm::Type *objectPtrTy,
                                                     ExistentialLayout layout);

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
    ExistentialLayout Layout;
    Address Addr;
    DestroyExistential(ExistentialLayout layout, Address addr)
      : Layout(layout), Addr(addr) {}

    void emit(IRGenFunction &IGF) const {
      emitDestroyExistential(IGF, Addr, Layout);
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

    void visitInherited(ArrayRef<TypeLoc> inherited) {
      if (inherited.empty()) return;

      // TODO: We need to figure out all the guarantees we want here.
      // It would be abstractly good to allow conversion to a base
      // protocol to be trivial, but it's not clear that there's
      // really a structural guarantee we can rely on here.
      for (TypeLoc baseType : inherited) {
        SmallVector<ProtocolDecl *, 4> baseProtos;
        baseType.getType()->isExistentialType(baseProtos);
        for (auto baseProto : baseProtos) {
          asDerived().addOutOfLineBaseProtocol(baseProto);
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
      case DeclKind::Destructor:
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
    void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
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
        wtable = loadOpaqueWitness(IGF, wtable, ReversePath[i-1]);
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
      for (TypeLoc inherited : proto->getInherited()) {
        ProtocolDecl *base =
            inherited.getType()->castTo<ProtocolType>()->getDecl();
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

  /// An entry in an existential type's list of known protocols.
  class ProtocolEntry {
    ProtocolDecl *Protocol;
    const ProtocolInfo &Impl;

  public:
    explicit ProtocolEntry(ProtocolDecl *proto, const ProtocolInfo &impl)
      : Protocol(proto), Impl(impl) {}

    ProtocolDecl *getProtocol() const { return Protocol; }
    const ProtocolInfo &getInfo() const { return Impl; }
  };

  /// A TypeInfo implementation for existential types, i.e. types like:
  ///   Printable
  ///   protocol<Printable, Serializable>
  /// with the semantic translation:
  ///   \exists t : Printable . t
  /// t here is an ArchetypeType.
  ///
  /// This is used for both ProtocolTypes and ProtocolCompositionTypes.
  class ExistentialTypeInfo :
      public IndirectTypeInfo<ExistentialTypeInfo, FixedTypeInfo> {
    unsigned NumProtocols;

    ProtocolEntry *getProtocolsBuffer() {
      return reinterpret_cast<ProtocolEntry *>(this + 1);
    }
    const ProtocolEntry *getProtocolsBuffer() const {
      return reinterpret_cast<const ProtocolEntry *>(this + 1);
    }

    ExistentialTypeInfo(llvm::Type *ty, Size size, Alignment align,
                        ArrayRef<ProtocolEntry> protocols)
      : IndirectTypeInfo(ty, size, align, IsNotPOD), NumProtocols(protocols.size()) {

      for (unsigned i = 0; i != NumProtocols; ++i) {
        new (&getProtocolsBuffer()[i]) ProtocolEntry(protocols[i]);
      }
    }

  public:
    ExistentialLayout getLayout() const {
      return ExistentialLayout(std::max(NumProtocols, 1U));
    }

    static const ExistentialTypeInfo *create(llvm::Type *ty, Size size,
                                          Alignment align,
                                          ArrayRef<ProtocolEntry> protocols) {
      void *buffer = operator new(sizeof(ExistentialTypeInfo) +
                                  protocols.size() * sizeof(ProtocolEntry));
      return new(buffer) ExistentialTypeInfo(ty, size, align, protocols);
    }

    /// Returns the protocols that values of this type are known to
    /// implement.  This can be empty, meaning that values of this
    /// type are not know to implement any protocols, although we do
    /// still know how to manipulate them.
    ArrayRef<ProtocolEntry> getProtocols() const {
      return ArrayRef<ProtocolEntry>(getProtocolsBuffer(), NumProtocols);
    }

    /// Given an existential object, find the witness table
    /// corresponding to the given protocol.
    llvm::Value *findWitnessTable(IRGenFunction &IGF, Address obj,
                                  ProtocolDecl *protocol) const {
      assert(NumProtocols != 0 &&
             "finding a witness table in a trivial existential");

      ProtocolPath path(IGF.IGM, getProtocols(), protocol);
      llvm::Value *originTable =
        getLayout().loadWitnessTable(IGF, obj, path.getOriginIndex());
      return path.apply(IGF, originTable);
    }

    using FixedTypeInfo::allocate;
    Address allocate(IRGenFunction &IGF,
                     const Twine &name = "protocol.temporary") const {
      return IGF.createAlloca(getStorageType(), StorageAlignment, name);
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      auto objPtrTy = dest.getAddress()->getType();
      auto fn = getAssignExistentialsFunction(IGF.IGM, objPtrTy, getLayout());
      auto call = IGF.Builder.CreateCall2(fn, dest.getAddress(),
                                          src.getAddress());
      call->setCallingConv(IGF.IGM.RuntimeCC);
      call->setDoesNotThrow();
    }

    void initializeWithCopy(IRGenFunction &IGF,
                            Address dest, Address src) const {
      auto layout = getLayout();

      // Load the witness tables and copy them into the new object.
      // Remember one of them for the copy later;  it doesn't matter which.
      llvm::Value *wtable = nullptr;
      for (unsigned i = 0, e = layout.getNumTables(); i != e; ++i) {
        llvm::Value *table = layout.loadWitnessTable(IGF, src, i);
        Address destSlot = layout.projectWitnessTable(IGF, dest, i);
        IGF.Builder.CreateStore(table, destSlot);

        if (i == 0) wtable = table;
      }
      assert(wtable != nullptr);

      // Project down to the buffers and ask the witnesses to do a
      // copy-initialize.
      Address srcBuffer = layout.projectExistentialBuffer(IGF, src);
      Address destBuffer = layout.projectExistentialBuffer(IGF, dest);
      emitInitializeBufferWithCopyOfBufferCall(IGF, wtable,
                                               destBuffer, srcBuffer);
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      emitDestroyExistential(IGF, addr, getLayout());
    }
  };

  /// A type implementation for an ArchetypeType, otherwise known as a
  /// type variable: for example, This in a protocol declaration, or T
  /// in a generic declaration like foo<T>(x : T) -> T.  The critical
  /// thing here is that performing an operation involving archetypes
  /// is dependent on the witness binding we can see.
  class ArchetypeTypeInfo :
      public IndirectTypeInfo<ArchetypeTypeInfo, TypeInfo> {

    /// The number of protocols that this archetype ascribes to.
    unsigned NumProtocols;

    ProtocolEntry *getProtocolsBuffer() {
      return reinterpret_cast<ProtocolEntry*>(this + 1);
    }
    const ProtocolEntry *getProtocolsBuffer() const {
      return reinterpret_cast<const ProtocolEntry*>(this + 1);
    }

    ArchetypeTypeInfo(llvm::Type *type, ArrayRef<ProtocolEntry> protocols)
      : IndirectTypeInfo(type, Size(1), Alignment(1), IsNotPOD),
        NumProtocols(protocols.size()) {
      for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
        new (&getProtocolsBuffer()[i]) ProtocolEntry(protocols[i]);
      }
    }

  public:
    static const ArchetypeTypeInfo *create(llvm::Type *type,
                                           ArrayRef<ProtocolEntry> protocols) {
      void *buffer =
        operator new(sizeof(ArchetypeTypeInfo) +
                     protocols.size() * sizeof(ProtocolEntry));
      return new (buffer) ArchetypeTypeInfo(type, protocols);
    }

    ArrayRef<ProtocolEntry> getProtocols() const {
      return llvm::makeArrayRef(getProtocolsBuffer(), NumProtocols);
    }

    llvm::Value *getMetadataRef(IRGenFunction &IGF) const {
      return GenProto::getArchetypeValueWitness(IGF, this, MetadataWitnessKey);
    }

    void setMetadataRef(IRGenFunction &IGF, llvm::Value *metadata) const {
      GenProto::setArchetypeValueWitness(IGF, this, MetadataWitnessKey, metadata);
    }

    /// Return the witness table that's been set for this type.
    llvm::Value *getWitnessTable(IRGenFunction &IGF, unsigned which) const {
      assert(which < NumProtocols);
      return GenProto::getArchetypeValueWitness(IGF, this, which);
    }

    void setWitnessTable(IRGenFunction &IGF, unsigned which,
                         llvm::Value *wtable) const {
      assert(which < NumProtocols);
      GenProto::setArchetypeValueWitness(IGF, this, which, wtable);
    }

    llvm::Value *getValueWitnessTable(IRGenFunction &IGF) const {
      // This can be called in any of the cases.
      return GenProto::getArchetypeValueWitness(IGF, this, 0U);
    }

    void setValueWitnessTable(IRGenFunction &IGF, llvm::Value *wtable) const {
      assert(NumProtocols == 0);
      GenProto::setArchetypeValueWitness(IGF, this, 0U, wtable);
    }

    /// Create an uninitialized archetype object.
    OwnedAddress allocate(IRGenFunction &IGF, Initialization &init,
                          InitializedObject object, OnHeap_t onHeap,
                          const llvm::Twine &name) const {
      if (onHeap) {
        // Lay out the type as a heap object.
        HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, this);
        assert(!layout.empty() && "non-empty type had empty layout?");
        auto &elt = layout.getElements()[0];

        // Allocate a new object.
        // TODO: lifetime intrinsics?
        llvm::Value *allocation = IGF.emitUnmanagedAlloc(layout,
                                                         name + ".alloc");

        // Cast and GEP down to the element.
        Address rawAddr = layout.emitCastOfAlloc(IGF, allocation);
        rawAddr = elt.project(IGF, rawAddr, name);

        // Push a cleanup to dealloc the allocation.
        // FIXME: don't emit the size twice!
        CleanupsDepth deallocCleanup
          = IGF.pushDeallocCleanup(allocation, layout.emitSize(IGF));

        OwnedAddress addr(rawAddr, allocation);
        init.markAllocated(IGF, object, addr, deallocCleanup);
        return addr;
      }

      // Make a fixed-size buffer.
      Address buffer = IGF.createAlloca(IGF.IGM.getFixedBufferTy(),
                                        getFixedBufferAlignment(IGF.IGM),
                                        name);

      // Allocate an object of the appropriate type within it.
      llvm::Value *wtable = getValueWitnessTable(IGF);
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
      emitAssignWithCopyCall(IGF, getValueWitnessTable(IGF),
                             dest.getAddress(), src.getAddress());
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      emitAssignWithTakeCall(IGF, getValueWitnessTable(IGF),
                             dest.getAddress(), src.getAddress());
    }

    void initializeWithCopy(IRGenFunction &IGF,
                            Address dest, Address src) const {
      emitInitializeWithCopyCall(IGF, getValueWitnessTable(IGF),
                                 dest.getAddress(), src.getAddress());
    }

    void initializeWithTake(IRGenFunction &IGF,
                            Address dest, Address src) const {
      emitInitializeWithTakeCall(IGF, getValueWitnessTable(IGF),
                                 dest.getAddress(), src.getAddress());
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      emitDestroyCall(IGF, getValueWitnessTable(IGF), addr.getAddress());
    }

    std::pair<llvm::Value*,llvm::Value*>
    getSizeAndAlignment(IRGenFunction &IGF) const {
      llvm::Value *wtable = getValueWitnessTable(IGF);
      auto size = loadValueWitness(IGF, wtable, ValueWitness::Size);
      auto align = loadValueWitness(IGF, wtable, ValueWitness::Alignment);
      return std::make_pair(size, align);
    }

    llvm::Value *getSize(IRGenFunction &IGF) const {
      llvm::Value *wtable = getValueWitnessTable(IGF);
      return loadValueWitness(IGF, wtable, ValueWitness::Size);
    }

    llvm::Value *getAlignment(IRGenFunction &IGF) const {
      llvm::Value *wtable = getValueWitnessTable(IGF);
      return loadValueWitness(IGF, wtable, ValueWitness::Alignment);
    }

    llvm::Value *getStride(IRGenFunction &IGF) const {
      llvm::Value *wtable = getValueWitnessTable(IGF);
      return loadValueWitness(IGF, wtable, ValueWitness::Stride);
    }

    llvm::Constant *getStaticSize(IRGenModule &IGM) const { return nullptr; }
    llvm::Constant *getStaticAlignment(IRGenModule &IGM) const { return nullptr; }
    llvm::Constant *getStaticStride(IRGenModule &IGM) const { return nullptr; }
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
    buffer = IGF.Builder.CreateBitCast(buffer, IGF.IGM.Int8PtrPtrTy);
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
      IGF.Builder.CreateBitCast(buffer, IGF.IGM.Int8PtrPtrTy);
    llvm::Value *addr = IGF.Builder.CreateLoad(slot, "storage");
    IGF.emitDeallocRawCall(addr, type.getSize(IGF));
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
static void buildValueWitnessFunction(IRGenModule &IGM,
                                      llvm::Function *fn,
                                      ValueWitness index,
                                      FixedPacking packing,
                                      const TypeInfo &type) {
  assert(isValueWitnessFunction(index));

  IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  auto argv = fn->arg_begin();
  switch (index) {
  case ValueWitness::AllocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    Address result = emitAllocateBuffer(IGF, buffer, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::AssignWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitAssignWithCopy(IGF, src, dest, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::AssignWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitAssignWithTake(IGF, src, dest, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
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
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithCopy: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    Address result =
      emitInitializeBufferWithCopy(IGF, dest, src, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTake: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    Address result =
      emitInitializeBufferWithTake(IGF, dest, src, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitInitializeWithCopy(IGF, dest, src, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::InitializeWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitInitializeWithTake(IGF, dest, src, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::ProjectBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    Address result = emitProjectBuffer(IGF, buffer, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::Size:
  case ValueWitness::Alignment:
  case ValueWitness::Stride:
    llvm_unreachable("these value witnesses aren't functions");
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
                                                     llvm::Type *objectPtrTy,
                                                     ExistentialLayout layout) {
  llvm::Type *argTys[] = { objectPtrTy, objectPtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);

  // __swift_assign_existentials_N is the well-known function for
  // assigning existential types with N witness tables.
  llvm::SmallString<40> fnName;
  llvm::raw_svector_ostream(fnName)
    << "__swift_assign_existentials_" << layout.getNumTables();
  llvm::Constant *fn = IGM.Module.getOrInsertFunction(fnName, fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
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
    Address destBuffer = layout.projectExistentialBuffer(IGF, dest);
    Address srcBuffer = layout.projectExistentialBuffer(IGF, src);

    // Load the first dest and source tables.
    Address destTableSlot = layout.projectWitnessTable(IGF, dest, 0);
    llvm::Value *destTable = IGF.Builder.CreateLoad(destTableSlot);
    llvm::Value *srcTable = layout.loadWitnessTable(IGF, src, 0);

    // Check whether the tables match.
    // We're relying on the assumption that checking one table is good
    // enough; if it becomes possible to check both, we could be in
    // trouble.
    llvm::BasicBlock *matchBB = IGF.createBasicBlock("match");
    llvm::BasicBlock *noMatchBB = IGF.createBasicBlock("no-match");
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
    // TODO: should we copy-initialize and then destroy?  That's
    // possible if we copy aside, which is a small expensive but
    // always safe.  Otherwise the destroy (which can invoke user code)
    // could see invalid memory at this address.  These are basically
    // the madnesses that boost::variant has to go through, with the
    // advantage of address-invariance.
    IGF.Builder.emitBlock(noMatchBB);

    // Store the first table.
    IGF.Builder.CreateStore(srcTable, destTableSlot);

    // Store the rest of the tables.
    for (unsigned i = 1, e = layout.getNumTables(); i != e; ++i) {
      Address destTableSlot = layout.projectWitnessTable(IGF, dest, i);
      llvm::Value *srcTable = layout.loadWitnessTable(IGF, src, i);
      IGF.Builder.CreateStore(srcTable, destTableSlot);
    }

    // Destroy the old value.
    emitDestroyBufferCall(IGF, destTable, destBuffer);

    // Copy-initialize with the new value.
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
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.WitnessTablePtrTy };
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
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.WitnessTablePtrTy };
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
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assignWithCopy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
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
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assignWithTake_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
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
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_initWithCopy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
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
  llvm::Type *argTys[] = { IGM.Int8PtrPtrTy, IGM.WitnessTablePtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_destroy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
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
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy, IGM.WitnessTablePtrTy };
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
    IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, type.StorageAlignment);
    Address src(it++, type.StorageAlignment);
    IGF.emitMemCpy(dest, src, type.StorageSize);
    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Find a witness to the fact that a type is a value type.
/// Always returns an i8*.
static llvm::Constant *getValueWitness(IRGenModule &IGM,
                                       ValueWitness index,
                                       FixedPacking packing,
                                       CanType concreteType,
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

  case ValueWitness::Size: {
    if (auto value = concreteTI.getStaticSize(IGM))
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);

    // Just fill in null here if the type can't be statically laid out.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  case ValueWitness::Alignment: {
    if (auto value = concreteTI.getStaticAlignment(IGM))
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);

    // Just fill in null here if the type can't be statically laid out.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  case ValueWitness::Stride: {
    if (auto value = concreteTI.getStaticStride(IGM))
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);

    // Just fill in null here if the type can't be statically laid out.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  }
  llvm_unreachable("bad value witness kind");

 standard:
  llvm::Function *fn =
    IGM.getAddrOfValueWitness(concreteType, index);
  if (fn->empty())
    buildValueWitnessFunction(IGM, fn, index, packing, concreteTI);
  return asOpaquePtr(IGM, fn);
}

namespace {
  /// A class which builds a single witness.
  class WitnessBuilder {
    IRGenModule &IGM;
    llvm::Constant *ImplPtr;
    CanType ImplTy;
    CanType SignatureTy;
    CanType SignatureResultTy;
    CanType ImplResultTy;
    SmallVector<AnyFunctionType *, 4> ImplTyAtUncurry;
    unsigned UncurryLevel;
    ArrayRef<Substitution> Substitutions;

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
                   CanType implTy, CanType sigTy, unsigned uncurryLevel,
                   ArrayRef<Substitution> subs)
      : IGM(IGM), ImplPtr(impl), UncurryLevel(uncurryLevel),
        Substitutions(subs) {

      implTy = implTy->getUnlabeledType(IGM.Context)->getCanonicalType();
      ImplTy = implTy;

      sigTy = sigTy->getUnlabeledType(IGM.Context)->getCanonicalType();
      SignatureTy = sigTy;

      // Find abstract parameters.
      HasAbstractedArg = false;
      for (unsigned i = 0; i != uncurryLevel + 1; ++i) {
        AnyFunctionType *sigFnTy = cast<AnyFunctionType>(sigTy);
        AnyFunctionType *implFnTy = cast<AnyFunctionType>(implTy);
        ImplTyAtUncurry.push_back(implFnTy);

        sigTy = CanType(sigFnTy->getResult());
        implTy = CanType(implFnTy->getResult());

        UncurryClauseBegin.push_back(Args.size());
        findAbstractParameters(CanType(implFnTy->getInput()),
                               CanType(sigFnTy->getInput()));
      }

      ImplResultTy = implTy;
      SignatureResultTy = sigTy;
      HasAbstractedResult = hasAbstractResult(sigTy);
    }

    /// Add entries to Args for all the abstraction points in this type.
    void collectNestedArgs(Type param) {
      Args.push_back(Arg(Arg::Nested));
      if (TupleType *tuple = param->getAs<TupleType>()) {
        for (auto &field : tuple->getFields())
          collectNestedArgs(field.getType());
      }
    }

    /// Find the abstract parameters.
    void findAbstractParameters(CanType impl, CanType sig) {
      // Walk recursively into tuples.
      if (auto sigTuple = dyn_cast<TupleType>(sig)) {
        // The tuple itself is a potential point of abstraction.
        Args.push_back(Arg(Arg::DecomposedTuple));

        auto sigFields = sigTuple->getFields();
        auto implFields = cast<TupleType>(impl)->getFields();
        assert(sigFields.size() == implFields.size());

        for (unsigned i = 0, e = sigFields.size(); i != e; ++i)
          findAbstractParameters(CanType(implFields[i].getType()),
                                 CanType(sigFields[i].getType()));
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

      // We don't have a representation for metatypes yet, so no conversion
      // is required.
      if (isa<MetaTypeType>(sig)) {
        assert(isa<MetaTypeType>(impl));
        Args.push_back(Arg(Arg::Direct, implTI));
        return;
      }

      // That's all the structural types, so the types really ought to
      // match perfectly now.
      assert(sig == impl);
      Args.push_back(Arg(Arg::Direct, implTI));
    }

    /// If the given type includes an archetype, we need an abstracted
    /// result because we have to return into a buffer.
    bool hasAbstractResult(CanType sig) {
      if (auto tuple = dyn_cast<TupleType>(sig))
        for (auto &field : tuple->getFields())
          if (hasAbstractResult(CanType(field.getType())))
            return true;
      return isa<ArchetypeType>(sig);
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
      IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
                        ExplosionKind::Minimal, UncurryLevel, fn,
                        Prologue::Bare);
      emitThunk(IGF);

      return asOpaquePtr(IGM, fn);
    }

    void emitThunk(IRGenFunction &IGF) {
      Explosion outerArgs = IGF.collectParameters();

      const TypeInfo &innerResultTI = IGM.getFragileTypeInfo(ImplResultTy);
      bool innerHasIndirectResult =
        innerResultTI.getSchema(ExplosionKind::Minimal).requiresIndirectResult();

      Address resultAddr;
      if (HasAbstractedResult) {
        llvm::Value *resultPtr = outerArgs.claimUnmanagedNext();
        llvm::Type *resultPtrTy = innerResultTI.StorageType->getPointerTo();
        resultPtr = IGF.Builder.CreateBitCast(resultPtr, resultPtrTy);
        resultAddr = Address(resultPtr, innerResultTI.StorageAlignment);
      } else if (innerHasIndirectResult) {
        resultAddr = Address(outerArgs.claimUnmanagedNext(),
                              innerResultTI.StorageAlignment);
      }

      // FIXME: Pass in the right types so that differsByAbstraction works
      // correctly.
      CallEmission emission(IGF,
          Callee::forFreestandingFunction(ImplTy, ImplResultTy,
                                          Substitutions, ImplPtr,
                                          ExplosionKind::Minimal,
                                          UncurryLevel));

      // Walk backwards through the parameter clauses, forward the arguments.
      std::vector<Explosion> innerArgs;
      for (unsigned i = UncurryLevel + 1; i != 0; ) {
        innerArgs.emplace_back(ExplosionKind::Minimal);
        Explosion& innerArg = innerArgs.back();
        unsigned clauseBegin = UncurryClauseBegin[--i];
        unsigned clauseEnd =
          (i == UncurryLevel ? Args.size() : UncurryClauseBegin[i+1]);

        forwardArgs(IGF, outerArgs, innerArg,
                    ArrayRef<Arg>(&Args[clauseBegin], clauseEnd - clauseBegin));
        if (auto polyFn = dyn_cast<PolymorphicFunctionType>(ImplTyAtUncurry[i]))
          emitPolymorphicArguments(IGF, polyFn, Substitutions, innerArg);
      }
      for (unsigned i = innerArgs.size(); i != 0; --i)
        emission.addArg(innerArgs[i-1]);
      
      if (HasAbstractedResult || innerHasIndirectResult) {
        // If the result needs to be in memory, emit it to memory.
        emission.emitToMemory(resultAddr, innerResultTI);
        IGF.Builder.CreateRetVoid();
      } else {
        // Otherwise, just forward the result in registers.
        Explosion resultExplosion(ExplosionKind::Minimal);
        emission.emitToExplosion(resultExplosion);
        IGF.emitScalarReturn(resultExplosion);
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
    CanType ConcreteType;
    const TypeInfo &ConcreteTI;
    const ProtocolConformance &Conformance;
    ArrayRef<Substitution> Substitutions;

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
          Substitutions = boundTy->getSubstitutions();
        } else {
          assert(!ty || !ty->isSpecialized());
        }
      }
    }

  public:
    WitnessTableBuilder(IRGenModule &IGM,
                        SmallVectorImpl<llvm::Constant*> &table,
                        FixedPacking packing,
                        CanType concreteType, const TypeInfo &concreteTI,
                        const ProtocolConformance &conformance)
      : WitnessVisitor(IGM), Table(table), Packing(packing),
        ConcreteType(concreteType), ConcreteTI(concreteTI),
        Conformance(conformance) {
      computeSubstitutionsForType();
    }

    /// A base protocol is witnessed by a pointer to the conformance
    /// of this type to that protocol.
    void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
      // Look for a protocol type info.
      const ProtocolInfo &basePI = IGM.getProtocolInfo(baseProto);
      const ProtocolConformance *astConf =
        Conformance.InheritedMapping.find(baseProto)->second;
      assert(astConf && "couldn't find base conformance!");
      const ConformanceInfo &conf =
        basePI.getConformance(IGM, ConcreteType, ConcreteTI,
                              baseProto, *astConf);

      llvm::Constant *baseWitness = conf.tryGetConstantTable();
      assert(baseWitness && "couldn't get a constant table!");
      Table.push_back(asOpaquePtr(IGM, baseWitness));
    }

    void addStaticMethod(FuncDecl *iface) {
      FuncDecl *impl = cast<FuncDecl>(Conformance.Mapping.find(iface)->second);
      Table.push_back(getStaticMethodWitness(impl,
                                      iface->getType()->getCanonicalType()));
    }

    void addInstanceMethod(FuncDecl *iface) {
      FuncDecl *impl = cast<FuncDecl>(Conformance.Mapping.find(iface)->second);
      Table.push_back(getInstanceMethodWitness(impl,
                                      iface->getType()->getCanonicalType()));
    }

    /// Returns a function which calls the given implementation under
    /// the given interface.
    llvm::Constant *getInstanceMethodWitness(FuncDecl *impl,
                                             CanType ifaceType) {
      llvm::Constant *implPtr =
        IGM.getAddrOfFunction(FunctionRef(impl, ExplosionKind::Minimal, 1),
                              /*data*/ false);
      return getWitness(implPtr, impl->getType()->getCanonicalType(),
                        ifaceType, 1);
    }

    /// Returns a function which calls the given implementation under
    /// the given interface.
    llvm::Constant *getStaticMethodWitness(FuncDecl *impl,
                                           CanType ifaceType) {
      if (impl->getDeclContext()->isModuleContext()) {
        llvm::Constant *implPtr =
          IGM.getAddrOfFunction(FunctionRef(impl, ExplosionKind::Minimal, 0),
                                /*data*/ false);
        // FIXME: This is an ugly hack: we're pretending that the function
        // has a different type from its actual type.  This works because the
        // LLVM representation happens to be the same.
        Type concreteMeta = MetaTypeType::get(ConcreteType, IGM.Context);
        Type implTy = 
          FunctionType::get(concreteMeta, impl->getType(), IGM.Context);
        return getWitness(implPtr, implTy->getCanonicalType(), ifaceType, 1);
      }
      llvm::Constant *implPtr =
        IGM.getAddrOfFunction(FunctionRef(impl, ExplosionKind::Minimal, 1),
                              /*data*/ false);
      return getWitness(implPtr, impl->getType()->getCanonicalType(),
                        ifaceType, 1);
    }

    llvm::Constant *getWitness(llvm::Constant *fn, CanType fnTy,
                               CanType ifaceTy, unsigned uncurryLevel) {
      return WitnessBuilder(IGM, fn, fnTy, ifaceTy, uncurryLevel,
                            Substitutions).get();
    }
  };
}

/// Collect the value witnesses for a particular type.
static void addValueWitnesses(IRGenModule &IGM, FixedPacking packing,
                              CanType concreteType, const TypeInfo &concreteTI,
                              SmallVectorImpl<llvm::Constant*> &table) {
  for (unsigned i = 0; i != NumValueWitnesses; ++i) {
    table.push_back(getValueWitness(IGM, ValueWitness(i),
                                    packing, concreteType,
                                    concreteTI));
  }
}

/// Construct a global variable to hold a witness table.
///
/// \param protocol - optional; null if this is the trivial
///   witness table
 /// \return a value of type IGM.WitnessTablePtrTy
static llvm::Constant *buildWitnessTable(IRGenModule &IGM,
                                         CanType concreteType,
                                         ProtocolDecl *protocol,
                                         ArrayRef<llvm::Constant*> witnesses) {
  assert(witnesses.size() >= NumValueWitnesses);

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
  return llvm::ConstantExpr::getInBoundsGetElementPtr(var, indices);
}

/// Get or create the trivial witness table for a type.
static llvm::Constant *
getTrivialWitnessTable(IRGenModule &IGM, CanType concreteType,
                       const TypeInfo &concreteTI, FixedPacking packing) {
  auto &cache = GenProto::getTrivialWitnessTablesCache(IGM);

  // Check whether we've already made an entry for this.
  TypeBase *key = concreteType.getPointer();
  auto it = cache.find(key);
  if (it != cache.end())
    return it->second;

  // Build the actual table.
  SmallVector<llvm::Constant*, NumValueWitnesses> witnesses;
  addValueWitnesses(IGM, packing, concreteType, concreteTI, witnesses);
  llvm::Constant *table =
    buildWitnessTable(IGM, concreteType, nullptr, witnesses);

  // Add to the cache and return.
  cache.insert(std::make_pair(key, table));
  return table;
}

/// Emit a value-witness table for the given type, which is assumed to
/// be non-dependent.
llvm::Constant *irgen::emitValueWitnessTable(IRGenModule &IGM,
                                             CanType concreteType) {
  auto &concreteTI = IGM.getFragileTypeInfo(concreteType);
  FixedPacking packing = computePacking(IGM, concreteTI);

  SmallVector<llvm::Constant*, NumValueWitnesses> witnesses;
  addValueWitnesses(IGM, packing, concreteType, concreteTI, witnesses);

  auto tableTy = llvm::ArrayType::get(IGM.Int8PtrTy, witnesses.size());
  auto table = llvm::ConstantArray::get(tableTy, witnesses);

  auto addr = IGM.getAddrOfValueWitnessTable(concreteType, table->getType());
  auto global = cast<llvm::GlobalVariable>(addr);
  global->setConstant(true);
  global->setInitializer(table);

  return llvm::ConstantExpr::getBitCast(global, IGM.WitnessTablePtrTy);
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
ProtocolInfo::getConformance(IRGenModule &IGM, CanType concreteType,
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
  addValueWitnesses(IGM, packing, concreteType, concreteTI, witnesses);

  // Next, build the protocol witnesses.
  WitnessTableBuilder(IGM, witnesses, packing, concreteType, concreteTI,
                      conformance).visit(protocol);

  // Build the actual global variable.
  llvm::Constant *table =
    buildWitnessTable(IGM, concreteType, protocol, witnesses);

  ConformanceInfo *info = new ConformanceInfo;
  info->Table = table;
  info->Packing = packing;

  auto res = Conformances.insert(std::make_pair(&conformance, info));
  return *res.first->second;
}

static const TypeInfo *createExistentialTypeInfo(IRGenModule &IGM,
                                                 llvm::StructType *type,
                                        ArrayRef<ProtocolDecl*> protocols) {
  assert(type->isOpaque() && "creating existential type in concrete struct");

  SmallVector<llvm::Type*, 5> fields;
  SmallVector<ProtocolEntry, 4> entries;

  if (protocols.empty()) {
    // We always need at least one witness table.
    fields.push_back(IGM.WitnessTablePtrTy);
  } else {
    for (auto protocol : protocols) {
      // Find the protocol layout.
      const ProtocolInfo &impl = IGM.getProtocolInfo(protocol);
      entries.push_back(ProtocolEntry(protocol, impl));

      // Each protocol gets a witness table.
      fields.push_back(IGM.WitnessTablePtrTy);
    }
  }

  ExistentialLayout layout(fields.size());

  // Add the value buffer to the fields.
  fields.push_back(IGM.getFixedBufferTy());
  type->setBody(fields);

  Alignment align = getFixedBufferAlignment(IGM);
  assert(align >= IGM.getPointerAlignment());

  Size size = layout.getBufferOffset(IGM);
  assert(size.roundUpToAlignment(align) == size);
  size += getFixedBufferSize(IGM);

  return ExistentialTypeInfo::create(type, size, align, entries);
}

const TypeInfo *TypeConverter::convertProtocolType(ProtocolType *T) {
  // Protocol types are nominal.
  llvm::StructType *type = IGM.createNominalType(T->getDecl());
  return createExistentialTypeInfo(IGM, type, T->getDecl());
}

const TypeInfo *
TypeConverter::convertProtocolCompositionType(ProtocolCompositionType *T) {
  // Protocol composition types are not nominal, but we name them anyway.
  llvm::StructType *type = IGM.createNominalType(T);

  // Find the canonical protocols.  There might not be any.
  SmallVector<ProtocolDecl*, 4> protocols;
  bool isExistential = T->isExistentialType(protocols);
  assert(isExistential); (void) isExistential;

  return createExistentialTypeInfo(IGM, type, protocols);
}

const TypeInfo *TypeConverter::convertArchetypeType(ArchetypeType *T) {
  // Compute layouts for the protocols we ascribe to.
  SmallVector<ProtocolEntry, 4> protocols;
  for (auto protocol : T->getConformsTo()) {
    const ProtocolInfo &impl = IGM.getProtocolInfo(protocol);
    protocols.push_back(ProtocolEntry(protocol, impl));
  }
  
  // For now, just always use the same type.
  llvm::Type *storageType = IGM.OpaquePtrTy->getElementType();
  return ArchetypeTypeInfo::create(storageType, protocols);
}

/// Inform IRGenFunction that the given archetype has the given value
/// witness value within this scope.
void IRGenFunction::bindArchetype(ArchetypeType *archetype,
                                  llvm::Value *metadata,
                                  ArrayRef<llvm::Value*> wtables) {
  auto &ti = getFragileTypeInfo(archetype).as<ArchetypeTypeInfo>();

  // Set the metadata pointer.
  metadata->setName(archetype->getFullName());
  ti.setMetadataRef(*this, metadata);

  // Set the protocol witness tables.

  assert(wtables.size() == ti.getProtocols().size());
  if (ti.getProtocols().empty()) {
    // TODO: do this lazily.
    auto wtable = emitValueWitnessTableRefForMetadata(*this, metadata);
    wtable->setName(Twine(archetype->getFullName()) + "." + "value");
    ti.setValueWitnessTable(*this, wtable);
  } else {
    for (unsigned i = 0, e = ti.getProtocols().size(); i != e; ++i) {
      auto &protoI = ti.getProtocols()[i];
      auto wtable = wtables[i];
      wtable->setName(Twine(archetype->getFullName()) + "." +
                        protoI.getProtocol()->getName().str());
      ti.setWitnessTable(*this, i, wtable);
    }
  }
}

/// Perform all the bindings necessary to emit the given declaration.
void irgen::emitPolymorphicParameters(IRGenFunction &IGF,
                                      PolymorphicFunctionType *type,
                                      Explosion &in) {
  auto &generics = type->getGenericParams();

  // For now, treat all archetypes independently.
  // FIXME: Later, we'll want to emit only the minimal set of archetypes,
  // because non-primary archetypes (which correspond to associated types)
  // will have their witness tables embedded in the witness table corresponding
  // to their parent.
  for (auto archetype : generics.getAllArchetypes()) {
    auto &archetypeTI =
      IGF.getFragileTypeInfo(archetype).as<ArchetypeTypeInfo>();

    auto metadata = in.claimUnmanagedNext();

    // Find the protocols the parameter implements.
    SmallVector<llvm::Value*, 4> wtables;
    in.claimUnmanaged(archetypeTI.getProtocols().size(), wtables);

    IGF.bindArchetype(archetype, metadata, wtables);
  }
}

/// Emit a reference to the metadata object for an archetype.
llvm::Value *irgen::emitArchetypeMetadataRef(IRGenFunction &IGF,
                                             ArchetypeType *type) {
  auto &archetypeTI = IGF.getFragileTypeInfo(type).as<ArchetypeTypeInfo>();
  return archetypeTI.getMetadataRef(IGF);
}

void irgen::getValueWitnessTableElements(CanType T,
                                      llvm::SetVector<ArchetypeType*> &types) {
  // We need a value witness table for T
  if (auto archetype = dyn_cast<ArchetypeType>(T)) {
    types.insert(archetype);
    return;
  }

  // In (T, U). we need witness tables for T and U
  if (auto tuple = dyn_cast<TupleType>(T)) {
    for (auto element : tuple->getFields())
      getValueWitnessTableElements(CanType(element.getType()), types);
    return;
  }

  // FIXME: For X<T>, we need a witness table for T if X is a struct or oneof.
}

void irgen::getValueWitnessTables(IRGenFunction &IGF,
                                  ArrayRef<ArchetypeType*> archetypes,
                                  Explosion &tables) {
  for (auto archetype : archetypes) {
    auto &archetypeTI =
      IGF.getFragileTypeInfo(archetype).as<ArchetypeTypeInfo>();
    tables.addUnmanaged(archetypeTI.getValueWitnessTable(IGF));
  }
}

void irgen::setValueWitnessTables(IRGenFunction &IGF,
                                  ArrayRef<ArchetypeType*> archetypes,
                                  Explosion &tables) {
  for (auto archetype : archetypes) {
    auto &archetypeTI =
      IGF.getFragileTypeInfo(archetype).as<ArchetypeTypeInfo>();
    llvm::Value *wtable = tables.claimUnmanagedNext();
    wtable->setName(archetype->getFullName());
    archetypeTI.setValueWitnessTable(IGF, wtable);
  }
}

/// Emit the witness table references required for the given type
/// substitution.
void irgen::emitWitnessTableRefs(IRGenFunction &IGF,
                                 const Substitution &sub,
                                 SmallVectorImpl<llvm::Value*> &out) {
  // We don't need to do anything if we have no protocols to conform to.
  auto archetypeProtos = sub.Archetype->getConformsTo();
  if (archetypeProtos.empty()) return;

  // Look at the replacement type.
  CanType replType = sub.Replacement->getCanonicalType();
  auto &replTI = IGF.getFragileTypeInfo(replType);

  // If it's an archetype, we'll need to grab from the local context.
  if (isa<ArchetypeType>(replType)) {
    auto &archTI = replTI.as<ArchetypeTypeInfo>();

    for (auto proto : archetypeProtos) {
      ProtocolPath path(IGF.IGM, archTI.getProtocols(), proto);
      auto wtable = archTI.getWitnessTable(IGF, path.getOriginIndex());
      wtable = path.apply(IGF, wtable);
      out.push_back(wtable);
    }
    return;
  }

  // Otherwise, we can construct the witnesses from the protocol
  // conformances.
  assert(archetypeProtos.size() == sub.Conformance.size());
  for (unsigned j = 0, je = archetypeProtos.size(); j != je; ++j) {
    auto proto = archetypeProtos[j];
    auto &protoI = IGF.IGM.getProtocolInfo(proto);
    auto &confI = protoI.getConformance(IGF.IGM, replType, replTI, proto,
                                        *sub.Conformance[j]);

    llvm::Value *wtable = confI.getTable(IGF);
    out.push_back(wtable);
  }
}

/// Pass all the arguments necessary for the given function.
void irgen::emitPolymorphicArguments(IRGenFunction &IGF,
                                     PolymorphicFunctionType *polyFn,
                                     ArrayRef<Substitution> subs,
                                     Explosion &out) {
  auto &generics = polyFn->getGenericParams();
  assert(generics.getAllArchetypes().size() == subs.size());
  
  // For now, treat all archetypes independently.
  // FIXME: Later, we'll want to emit only the minimal set of archetypes,
  // because non-primary archetypes (which correspond to associated types)
  // will have their witness tables embedded in the witness table corresponding
  // to their parent.
  for (const auto &sub : subs) {
    auto archetype = sub.Archetype;
    auto archetypeProtos = archetype->getConformsTo();
    CanType argType = sub.Replacement->getCanonicalType();
    auto &argTI = IGF.getFragileTypeInfo(argType);

    // If the type argument is an archetype, we can gather the
    // protocol witnesses from that.
    if (argType->is<ArchetypeType>()) {
      auto &archTI = argTI.as<ArchetypeTypeInfo>();

      // Add the metadata reference.
      out.addUnmanaged(archTI.getMetadataRef(IGF));

      // Add witness tables for each of the required protocols.
      for (auto proto : archetypeProtos) {
        ProtocolPath path(IGF.IGM, archTI.getProtocols(), proto);
        auto wtable = archTI.getWitnessTable(IGF, path.getOriginIndex());
        wtable = path.apply(IGF, wtable);
        out.addUnmanaged(wtable);
      }
      continue;
    }

    // If the type argument is an existential type, we can construct
    // protocol witnesses that apply generically to existentials.
    // Or, well, at least we can do this for the value witnesses.
    // Hopefully the type-checker won't let these through for protocols
    // that aren't theoretically sound?
    assert(archetypeProtos.empty() || !argType->isExistentialType());

    // Okay, we have a concrete type.

    // Add the metadata reference.
    out.addUnmanaged(emitTypeMetadataRef(IGF, argType));

    // Fast path: we don't need protocol witnesses.
    if (archetypeProtos.empty())
      continue;

    SmallVector<llvm::Value*, 4> wtables;
    emitWitnessTableRefs(IGF, sub, wtables);
    for (auto wtable : wtables)
      out.addUnmanaged(wtable);
  }
}

/// Given a generic signature, add the argument types required in order to call it.
void irgen::expandPolymorphicSignature(IRGenModule &IGM,
                                       PolymorphicFunctionType *polyFn,
                                       SmallVectorImpl<llvm::Type*> &out) {
  auto &generics = polyFn->getGenericParams();
  for (auto archetype : generics.getAllArchetypes()) {
    // Pass the type argument.
    out.push_back(IGM.TypeMetadataPtrTy);

    // For now, pass each signature requirement separately.
    unsigned n = unsigned(archetype->getConformsTo().size());
    while (n--) out.push_back(IGM.WitnessTablePtrTy);
  }
}

/// Emit an erasure expression as an initializer for the given memory.
void irgen::emitErasureAsInit(IRGenFunction &IGF, ErasureExpr *E,
                              Address dest, const TypeInfo &rawTI) {
  auto &destTI = rawTI.as<ExistentialTypeInfo>();
  ExistentialLayout destLayout = destTI.getLayout();
  ArrayRef<ProtocolEntry> destEntries = destTI.getProtocols();
  assert(destEntries.size() == E->getConformances().size());

  CanType srcType = E->getSubExpr()->getType()->getCanonicalType();
  if (srcType->isExistentialType()) {
    // Special case: we're converting from an existential type to another
    // existential type in some trivial manner (e.g., to an inherited protocol).
    auto &srcTI = IGF.getFragileTypeInfo(srcType).as<ExistentialTypeInfo>();

    Address src;

    // If the layouts are equivalent, we can evaluate in-place and
    // then modify the witness table.  Regardless of the other
    // decisions we make, we won't need to touch the buffer.
    auto srcLayout = srcTI.getLayout();
    bool evaluateInPlace = (srcLayout == destLayout);
    if (evaluateInPlace) {
      auto srcPtrTy = srcTI.getStorageType()->getPointerTo();
      src = IGF.Builder.CreateBitCast(dest, srcPtrTy);
      IGF.emitRValueAsInit(E->getSubExpr(), src, srcTI);

    // Otherwise we'll need to evaluate to a different place and then
    // 'take' the buffer.
    } else {
      // We can use createAlloca instead of TypeInfo::allocate because
      // existentials are always fixed in layout.
      src = IGF.createAlloca(srcTI.getStorageType(), srcTI.StorageAlignment,
                             "erasure.temp");
      IGF.emitRValueAsInit(E->getSubExpr(), src, srcTI);

      // Take the data out of the other buffer (by memcpy'ing it).
      // ErasureExpr never implies a transformation of the *value*,
      // just of the *witnesses*.
      Address destBuffer = destLayout.projectExistentialBuffer(IGF, dest);
      Address srcBuffer = srcLayout.projectExistentialBuffer(IGF, src);
      IGF.emitMemCpy(destBuffer, srcBuffer, getFixedBufferSize(IGF.IGM));
    }

    // Okay, the buffer on dest has been meaningfully filled in.
    // Fill in the witnesses.

    // If we're erasing *all* protocols, just grab any witness table
    // from the source.
    if (destEntries.empty()) {
      // In the special case of not needing any destination protocols,
      // that's enough; the presumably richer protocol on the source
      // also works as a set of value witnesses.
      if (evaluateInPlace) return;

      // Otherwise copy over the first witness table.
      Address destSlot = destLayout.projectWitnessTable(IGF, dest, 0);
      llvm::Value *table = srcLayout.loadWitnessTable(IGF, src, 0);
      IGF.Builder.CreateStore(table, destSlot);
      return;
    }

    // Okay, we're erasing to a non-trivial set of protocols.

    // First, find all the destination tables.  We can't write these
    // into dest immediately because later fetches of protocols might
    // give us trouble.
    SmallVector<llvm::Value*, 4> destTables;
    for (auto &entry : destTI.getProtocols()) {
      auto table = srcTI.findWitnessTable(IGF, src, entry.getProtocol());
      destTables.push_back(table);
    }

    // Now write those into the destination.
    for (unsigned i = 0, e = destTables.size(); i != e; ++i) {
      Address destSlot = destLayout.projectWitnessTable(IGF, dest, i);
      IGF.Builder.CreateStore(destTables[i], destSlot);
    }
    return;
  }

  // We're converting from a concrete type to an existential type.

  if (isa<ArchetypeType>(srcType)) {
    IGF.unimplemented(E->getLoc(), "erasure from an archetype");
    return;
  }

  // Find the concrete type.
  const TypeInfo &srcTI = IGF.getFragileTypeInfo(srcType);
  FixedPacking packing = computePacking(IGF.IGM, srcTI);

  // First, evaluate into the buffer.

  // If the type is provably empty, just emit the operand as ignored.
  if (srcTI.isEmpty(ResilienceScope::Local)) {
    assert(packing == FixedPacking::OffsetZero);
    IGF.emitIgnored(E->getSubExpr());
  } else {
    // Otherwise, allocate if necessary.
    Address buffer = destLayout.projectExistentialBuffer(IGF, dest);
    Address object = emitAllocateBuffer(IGF, buffer, packing, srcTI);

    // Push a cleanup to destroy that.
    CleanupsDepth deallocCleanup;
    bool needsDeallocCleanup = !isNeverAllocated(packing);
    if (needsDeallocCleanup) {
      IGF.pushFullExprCleanup<DeallocateConcreteBuffer>(buffer, packing, srcTI);
      deallocCleanup = IGF.getCleanupsDepth();
    }

    // Emit the object in-place.
    IGF.emitRValueAsInit(E->getSubExpr(), object, srcTI);

    // Deactivate the dealloc cleanup.
    if (needsDeallocCleanup) {
      IGF.setCleanupState(deallocCleanup, CleanupState::Dead);
    }
  }

  // Okay, now write the witness table pointers into the existential.

  // If this is the trivial composition type, grab the witness table.
  if (destEntries.empty()) {
    llvm::Constant *wtable =
      getTrivialWitnessTable(IGF.IGM, srcType, srcTI, packing);
    Address tableSlot = destLayout.projectWitnessTable(IGF, dest, 0);
    IGF.Builder.CreateStore(wtable, tableSlot);
    return;
  }

  for (unsigned i = 0, e = destEntries.size(); i != e; ++i) {
    ProtocolDecl *proto = destEntries[i].getProtocol();
    auto &protoI = destEntries[i].getInfo();

    auto astConformance = E->getConformances()[i];
    assert(astConformance);

    // Compute the conformance information.
    const ConformanceInfo &conformance =
      protoI.getConformance(IGF.IGM, srcType, srcTI, proto, *astConformance);
    assert(packing == conformance.getPacking());

    // Now produce the conformance table and store that into the
    // destination.
    llvm::Value *table = conformance.getTable(IGF);
    Address tableSlot = destLayout.projectWitnessTable(IGF, dest, i);
    IGF.Builder.CreateStore(table, tableSlot);
  }
}

/// Emit an expression which erases the concrete type of its value and
/// replaces it with a generic type.
void irgen::emitErasure(IRGenFunction &IGF, ErasureExpr *E, Explosion &out) {
  const ExistentialTypeInfo &protoTI =
    IGF.getFragileTypeInfo(E->getType()).as<ExistentialTypeInfo>();

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
  assert(E->getType()->is<AnyFunctionType>());
  IGF.unimplemented(E->getLoc(),
              "forming implicit closure over existential member reference");
  IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), out);
}

/// Emit an expression which accesses a member out of an existential type.
LValue irgen::emitExistentialMemberRefLValue(IRGenFunction &IGF,
                                             ExistentialMemberRefExpr *E) {

  IGF.unimplemented(E->getLoc(),
                    "using existential member reference as l-value");
  return IGF.emitFakeLValue(E->getType());
}

/// Emit an expression which accesses a subscript out of an existential type.
LValue irgen::emitExistentialSubscriptLValue(IRGenFunction &IGF,
                                             ExistentialSubscriptExpr *E) {
  IGF.unimplemented(E->getLoc(),
                    "using existential subscript as l-value");
  return IGF.emitFakeLValue(E->getType());
}

/// Emit a callee for a protocol method.
///
/// \param fn - the protocol member being called
/// \param witness - the actual function address being called
/// \param wtable - the witness table from which the witness was loaded
/// \param thisObject - the object (if applicable) to call the method on
static CallEmission prepareProtocolMethodCall(IRGenFunction &IGF, FuncDecl *fn,
                                              CanType substResultType,
                                              ArrayRef<Substitution> subs,
                                              llvm::Value *witness,
                                              llvm::Value *wtable,
                                              Address thisObject) {
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  unsigned uncurryLevel = 1;

  CanType origFnType = fn->getType()->getCanonicalType();
  llvm::FunctionType *fnTy =
    IGF.IGM.getFunctionType(origFnType, explosionLevel, uncurryLevel,
                            /*data*/ false);
  witness = IGF.Builder.CreateBitCast(witness, fnTy->getPointerTo());

  Callee callee =
    Callee::forKnownFunction(fn->isStatic() ? AbstractCC::Freestanding
                                            : AbstractCC::Method,
                             origFnType, substResultType, subs,
                             witness, ManagedValue(nullptr),
                             explosionLevel, uncurryLevel);

  CallEmission emission(IGF, callee);

  // If this isn't a static method, add the temporary address as a
  // callee argument.  This argument is bitcast to the type of the
  // 'this' argument (a [byref] to the archetype This), although the
  // underlying function actually takes the underlying object pointer.
  if (!fn->isStatic()) {
    thisObject = IGF.Builder.CreateBitCast(thisObject, IGF.IGM.OpaquePtrTy,
                                           "object-pointer");
    Explosion arg(ExplosionKind::Minimal);
    arg.addUnmanaged(thisObject.getAddress());
    emission.addArg(arg);

  // If this is a static method, add an empty argument as the metatype.
  // This is really questionable.
  } else {
    emission.addEmptyArg();
  }

  return emission;
}

/// Emit an existential member reference as a callee.
CallEmission
irgen::prepareExistentialMemberRefCall(IRGenFunction &IGF,
                                       ExistentialMemberRefExpr *E,
                                       CanType substResultType,
                                       ArrayRef<Substitution> subs,
                                       ExplosionKind maxExplosionLevel,
                                       unsigned maxUncurry) {
  // The protocol we're calling on.
  // TODO: support protocol compositions here.
  Type baseTy = E->getBase()->getType();
  baseTy = baseTy->castTo<LValueType>()->getObjectType();
  assert(baseTy->isExistentialType());
  auto &baseTI = IGF.getFragileTypeInfo(baseTy).as<ExistentialTypeInfo>();

  // The function we're going to call.
  FuncDecl *fn = cast<FuncDecl>(E->getDecl());
  ProtocolDecl *fnProto = cast<ProtocolDecl>(fn->getDeclContext());

  // The base of an existential member reference is always an l-value.
  LValue lvalue = IGF.emitLValue(E->getBase());
  Address existAddr = IGF.emitMaterializeWithWriteback(std::move(lvalue),
                                                       NotOnHeap);

  // Load the witness table.
  llvm::Value *wtable = baseTI.findWitnessTable(IGF, existAddr, fnProto);

  // The thing we're going to invoke a method on.
  Address thisObject;

  // For a non-static method, copy the object into a new buffer.
  // This is important for type-safety, because someone might assign
  // to the object while the call is in progress.
  //
  // TODO: we have a proposal to avoid this copy by "locking" the
  //   existential object from any assignment.  Do that instead.
  // TODO: Or just do a project when the source object is obviously
  //   not aliased.
  if (!fn->isStatic()) {
    Address copyBuffer = IGF.createAlloca(IGF.IGM.getFixedBufferTy(),
                                          getFixedBufferAlignment(IGF.IGM),
                                          "copy-for-call");
    auto layout = baseTI.getLayout();
    Address srcBuffer = layout.projectExistentialBuffer(IGF, existAddr);

    // The result of the copy is an i8* which points at the new object.
    llvm::Value *copy =
      emitInitializeBufferWithCopyOfBufferCall(IGF, wtable, copyBuffer,
                                               srcBuffer);

    // Enter a cleanup for the temporary.  
    IGF.pushFullExprCleanup<DestroyBuffer>(copyBuffer, wtable);

    thisObject = Address(copy, Alignment(1));
  }

  // Find the actual witness.
  auto &fnProtoInfo = IGF.IGM.getProtocolInfo(fnProto);
  auto index = fnProtoInfo.getWitnessEntry(fn).getFunctionIndex();
  llvm::Value *witness = loadOpaqueWitness(IGF, wtable, index);

  // Emit the callee.
  return prepareProtocolMethodCall(IGF, fn, substResultType, subs,
                                   witness, wtable, thisObject);
}

/// Emit an existential member reference as a callee.
CallEmission
irgen::prepareArchetypeMemberRefCall(IRGenFunction &IGF,
                                     ArchetypeMemberRefExpr *E,
                                     CanType substResultType,
                                     ArrayRef<Substitution> subs,
                                     ExplosionKind maxExplosionLevel,
                                     unsigned maxUncurry) {
  // The function we're going to call.
  FuncDecl *fn = cast<FuncDecl>(E->getDecl());

  // The base of an archetype member reference is always an l-value.
  // If we're accessing a static function, though, the l-value is
  // unnecessary.
  Address thisObject;
  if (fn->isStatic()) {
    IGF.emitIgnored(E->getBase());
  } else {
    LValue lvalue = IGF.emitLValue(E->getBase());
    thisObject = IGF.emitMaterializeWithWriteback(std::move(lvalue), NotOnHeap);
  }

  // Find the archetype we're calling on.
  CanType baseTy = E->getBase()->getType()->getCanonicalType();
  ArchetypeType *archetype;
  if (auto baseLV = dyn_cast<LValueType>(baseTy)) {
    archetype = cast<ArchetypeType>(CanType(baseLV->getObjectType()));
  } else {
    archetype = cast<ArchetypeType>(
                     CanType(cast<MetaTypeType>(baseTy)->getInstanceType()));
  }

  // The protocol we're calling on.
  ProtocolDecl *fnProto = cast<ProtocolDecl>(fn->getDeclContext());

  // Find the witness table.
  auto &archetypeTI = IGF.getFragileTypeInfo(archetype).as<ArchetypeTypeInfo>();
  ProtocolPath path(IGF.IGM, archetypeTI.getProtocols(), fnProto);
  llvm::Value *origin = archetypeTI.getWitnessTable(IGF, path.getOriginIndex());
  llvm::Value *wtable = path.apply(IGF, origin);

  // Find the actual witness.
  auto &fnProtoInfo = IGF.IGM.getProtocolInfo(fnProto);
  auto index = fnProtoInfo.getWitnessEntry(fn).getFunctionIndex();
  llvm::Value *witness = loadOpaqueWitness(IGF, wtable, index);

  // Emit the callee.
  return prepareProtocolMethodCall(IGF, fn, substResultType, subs,
                                   witness, wtable, thisObject);
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

/// Emit an expression which accesses a member out of an archetype.
void irgen::emitArchetypeMemberRef(IRGenFunction &IGF,
                                   ArchetypeMemberRefExpr *E,
                                   Explosion &out) {
  // The l-value case should have been weeded out.
  assert(!E->getType()->is<LValueType>());

  // The remaining case is to construct an implicit closure.
  // Just refuse to do this for now.
  assert(E->getType()->is<AnyFunctionType>());
  IGF.unimplemented(E->getLoc(),
              "forming implicit closure over existential member reference");
  IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), out);
}

/// Emit an expression which accesses a member out of an archetype.
LValue irgen::emitArchetypeMemberRefLValue(IRGenFunction &IGF,
                                           ArchetypeMemberRefExpr *E) {
  IGF.unimplemented(E->getLoc(), "l-value member reference into archetype");
  return IGF.emitFakeLValue(E->getType());
}

/// Emit an expression which accesses a subscript out of an archetype.
LValue irgen::emitArchetypeSubscriptLValue(IRGenFunction &IGF,
                                           ArchetypeSubscriptExpr *E) {
  IGF.unimplemented(E->getLoc(), "l-value subscript into archetype");
  return IGF.emitFakeLValue(E->getType());
}

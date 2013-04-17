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
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

#include "CallEmission.h"
#include "Cleanup.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "FunctionRef.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenPoly.h"
#include "GenType.h"
#include "IndirectTypeInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "NecessaryBindings.h"
#include "ProtocolInfo.h"
#include "TypeInfo.h"
#include "TypeVisitor.h"

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

static llvm::FunctionType *createWitnessFunctionType(IRGenModule &IGM,
                                                     ValueWitness index) {
  switch (index) {

  // void (*deallocateBuffer)(B *buffer, M *self);
  // void (*destroyBuffer)(B *buffer, M *self);
  case ValueWitness::DeallocateBuffer:
  case ValueWitness::DestroyBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // void (*destroy)(T *object, witness_t *self);
  case ValueWitness::Destroy: {
    llvm::Type *args[] = { IGM.OpaquePtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, M *self);
  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
  }

  // T *(*allocateBuffer)(B *buffer, M *self);
  // T *(*projectBuffer)(B *buffer, M *self);
  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
  }

  // T *(*initializeBufferWithCopy)(B *dest, T *src, M *self);
  // T *(*initializeBufferWithTake)(B *dest, T *src, M *self);
  case ValueWitness::InitializeBufferWithCopy:
  case ValueWitness::InitializeBufferWithTake: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.OpaquePtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
  }

  // T *(*assignWithCopy)(T *dest, T *src, M *self);
  // T *(*assignWithTake)(T *dest, T *src, M *self);
  // T *(*initializeWithCopy)(T *dest, T *src, M *self);
  // T *(*initializeWithTake)(T *dest, T *src, M *self);
  case ValueWitness::AssignWithCopy:
  case ValueWitness::AssignWithTake:
  case ValueWitness::InitializeWithCopy:
  case ValueWitness::InitializeWithTake: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *args[] = { ptrTy, ptrTy, IGM.TypeMetadataPtrTy };
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

    /*
    friend bool operator==(ExistentialLayout a, ExistentialLayout b) {
      return a.NumTables == b.NumTables;
    }*/

    /// Given the offset of the buffer within an existential type.
    Size getBufferOffset(IRGenModule &IGM) const {
      return IGM.getPointerSize() * (NumTables + 1);
    }

    /// Given the address of an existential object, drill down to the
    /// buffer.
    Address projectExistentialBuffer(IRGenFunction &IGF, Address addr) const {
      return IGF.Builder.CreateStructGEP(addr, getNumTables() + 1,
                                         getBufferOffset(IGF.IGM));
    }

    /// Given the address of an existential object, drill down to the
    /// witness-table field.
    Address projectWitnessTable(IRGenFunction &IGF, Address addr,
                                unsigned which) const {
      assert(which < getNumTables());
      return IGF.Builder.CreateStructGEP(addr, which + 1,
                                         IGF.IGM.getPointerSize() * (which + 1));
    }

    /// Given the address of an existential object, load its witness table.
    llvm::Value *loadWitnessTable(IRGenFunction &IGF, Address addr,
                                  unsigned which) const {
      return IGF.Builder.CreateLoad(projectWitnessTable(IGF, addr, which),
                                    "witness-table");
    }
    
    llvm::Value *loadValueWitnessTable(IRGenFunction &IGF, Address addr,
                                       llvm::Value *metadata) {
      if (getNumTables() > 0)
        return loadWitnessTable(IGF, addr, 0);
      else
        return emitValueWitnessTableRefForMetadata(IGF, metadata);
    }

    /// Given the address of an existential object, drill down to the
    /// metadata field.
    Address projectMetadataRef(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateStructGEP(addr, 0, Size(0));
    }

    /// Given the address of an existential object, load its metadata
    /// object.
    llvm::Value *loadMetadataRef(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateLoad(projectMetadataRef(IGF, addr),
                               addr.getAddress()->getName() + ".metadata");
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
  // Set as nounwind.
  auto attrs = llvm::AttributeSet::get(call->getContext(),
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);

  attrs = attrs.addAttribute(call->getContext(), 1, llvm::Attribute::NoAlias);

  // Only set 'sret' if this is also the formal result.
  if (isFormalResult) {
    attrs = attrs.addAttribute(call->getContext(), 1,
                               llvm::Attribute::StructRet);
  }

  call->setAttributes(attrs);
}

/// Given a call to a helper function, set attributes appropriately.
static void setHelperAttributes(llvm::CallInst *call) {
  // Set as nounwind.
  auto attrs = llvm::AttributeSet::get(call->getContext(),
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);

  call->setAttributes(attrs);
}

/// Emit a call to do an 'initializeBufferWithCopyOfBuffer' operation.
static llvm::Value *emitInitializeBufferWithCopyOfBufferCall(IRGenFunction &IGF,
                                                     llvm::Value *witnessTable,
                                                     llvm::Value *metadata,
                                                     Address destBuffer,
                                                     Address srcBuffer) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                             ValueWitness::InitializeBufferWithCopyOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destBuffer.getAddress(),
                            srcBuffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributesForAggResult(call, false);

  return call;
}

/// Emit a call to do an 'allocateBuffer' operation.
static llvm::Value *emitAllocateBufferCall(IRGenFunction &IGF,
                                           llvm::Value *witnessTable,
                                           llvm::Value *metadata,
                                           Address buffer) {
  llvm::Value *allocateFn = loadValueWitness(IGF, witnessTable,
                                             ValueWitness::AllocateBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall2(allocateFn, buffer.getAddress(), metadata);
  result->setCallingConv(IGF.IGM.RuntimeCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do a 'projectBuffer' operation.
static llvm::Value *emitProjectBufferCall(IRGenFunction &IGF,
                                          llvm::Value *witnessTable,
                                           llvm::Value *metadata,
                                          Address buffer) {
  llvm::Value *projectFn = loadValueWitness(IGF, witnessTable,
                                            ValueWitness::ProjectBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall2(projectFn, buffer.getAddress(), metadata);
  result->setCallingConv(IGF.IGM.RuntimeCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do an 'initializeWithCopy' operation.
static void emitInitializeWithCopyCall(IRGenFunction &IGF,
                                       llvm::Value *witnessTable,
                                       llvm::Value *metadata,
                                       llvm::Value *destObject,
                                       llvm::Value *srcObject) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                                         ValueWitness::InitializeWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeWithTake' operation.
static void emitInitializeWithTakeCall(IRGenFunction &IGF,
                                       llvm::Value *witnessTable,
                                       llvm::Value *metadata,
                                       llvm::Value *destObject,
                                       llvm::Value *srcObject) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                                         ValueWitness::InitializeWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithCopy' operation.
static void emitAssignWithCopyCall(IRGenFunction &IGF,
                                   llvm::Value *witnessTable,
                                   llvm::Value *metadata,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                                         ValueWitness::AssignWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithTake' operation.
static void emitAssignWithTakeCall(IRGenFunction &IGF,
                                   llvm::Value *witnessTable,
                                   llvm::Value *metadata,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                                         ValueWitness::AssignWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do a 'destroy' operation.
static void emitDestroyCall(IRGenFunction &IGF,
                            llvm::Value *witnessTable,
                            llvm::Value *metadata,
                            llvm::Value *object) {
  llvm::Value *fn = loadValueWitness(IGF, witnessTable, ValueWitness::Destroy);
  llvm::CallInst *call = IGF.Builder.CreateCall2(fn, object, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'destroyBuffer' operation.
static void emitDestroyBufferCall(IRGenFunction &IGF,
                                  llvm::Value *witnessTable,
                                   llvm::Value *metadata,
                                  Address buffer) {
  llvm::Value *fn = loadValueWitness(IGF, witnessTable,
                                     ValueWitness::DestroyBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'deallocateBuffer' operation.
static void emitDeallocateBufferCall(IRGenFunction &IGF,
                                     llvm::Value *witnessTable,
                                     llvm::Value *metadata,
                                     Address buffer) {
  llvm::Value *fn = loadValueWitness(IGF, witnessTable,
                                     ValueWitness::DeallocateBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Given the address of an existential object, destroy it.
static void emitDestroyExistential(IRGenFunction &IGF, Address addr,
                                   ExistentialLayout layout) {
  llvm::Value *metadata = layout.loadMetadataRef(IGF, addr);

  // We need a value witness table.  Use one from the existential if
  // possible because (1) it should be in cache and (2) the address
  // won't be dependent on the metadata load.
  llvm::Value *wtable = layout.loadValueWitnessTable(IGF, addr, metadata);

  Address object = layout.projectExistentialBuffer(IGF, addr);
  emitDestroyBufferCall(IGF, wtable, metadata, object);
}

static llvm::Constant *getAssignExistentialsFunction(IRGenModule &IGM,
                                                     llvm::Type *objectPtrTy,
                                                     ExistentialLayout layout);

namespace {
  struct DestroyBuffer : Cleanup {
    Address Buffer;
    llvm::Value *Table;
    llvm::Value *Metatype;
    DestroyBuffer(Address buffer, llvm::Value *table, llvm::Value *metatype)
      : Buffer(buffer), Table(table), Metatype(metatype) {}

    void emit(IRGenFunction &IGF) const {
      emitDestroyBufferCall(IGF, Table, Metatype, Buffer);
    }
  };

  struct DeallocateBuffer : Cleanup {
    Address Buffer;
    llvm::Value *Table;
    llvm::Value *Metatype;
    DeallocateBuffer(Address buffer, llvm::Value *table, llvm::Value *metatype)
      : Buffer(buffer), Table(table), Metatype(metatype) {}

    void emit(IRGenFunction &IGF) const {
      emitDeallocateBufferCall(IGF, Table, Metatype, Buffer);
    }
  };
  
  struct DeallocateBox : Cleanup {
    llvm::Value *Box;
    llvm::Value *Type;
    DeallocateBox(llvm::Value *box, llvm::Value *type)
      : Box(box), Type(type) {}

    void emit(IRGenFunction &IGF) const {
      IGF.emitDeallocBoxCall(Box, Type);
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
      case DeclKind::InfixOperator:
      case DeclKind::PrefixOperator:
      case DeclKind::PostfixOperator:
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
      return ExistentialLayout(NumProtocols);
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
      return IGF.createAlloca(getStorageType(), getFixedAlignment(), name);
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

      llvm::Value *metadata = layout.loadMetadataRef(IGF, src);
      IGF.Builder.CreateStore(metadata, layout.projectMetadataRef(IGF, dest));

      // Load the witness tables and copy them into the new object.
      // Remember one of them for the copy later;  it doesn't matter which.
      llvm::Value *wtable = nullptr;
      for (unsigned i = 0, e = layout.getNumTables(); i != e; ++i) {
        llvm::Value *table = layout.loadWitnessTable(IGF, src, i);
        Address destSlot = layout.projectWitnessTable(IGF, dest, i);
        IGF.Builder.CreateStore(table, destSlot);

        if (i == 0) wtable = table;
      }

      // We need a witness table.  If we don't have one from the
      // protocol witnesses, load it from the metadata.
      if (wtable == nullptr) {
        wtable = emitValueWitnessTableRefForMetadata(IGF, metadata);
      }

      // Project down to the buffers and ask the witnesses to do a
      // copy-initialize.
      Address srcBuffer = layout.projectExistentialBuffer(IGF, src);
      Address destBuffer = layout.projectExistentialBuffer(IGF, dest);
      emitInitializeBufferWithCopyOfBufferCall(IGF, wtable, metadata,
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

    ArchetypeType *TheArchetype;

    ProtocolEntry *getProtocolsBuffer() {
      return reinterpret_cast<ProtocolEntry*>(this + 1);
    }
    const ProtocolEntry *getProtocolsBuffer() const {
      return reinterpret_cast<const ProtocolEntry*>(this + 1);
    }

    ArchetypeTypeInfo(ArchetypeType *archetype, llvm::Type *type,
                      ArrayRef<ProtocolEntry> protocols)
      : IndirectTypeInfo(type, Size(1), Alignment(1), IsNotPOD),
        TheArchetype(archetype) {
      assert(protocols.size() == archetype->getConformsTo().size());
      for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
        new (&getProtocolsBuffer()[i]) ProtocolEntry(protocols[i]);
      }
    }

  public:
    static const ArchetypeTypeInfo *create(ArchetypeType *archetype,
                                           llvm::Type *type,
                                           ArrayRef<ProtocolEntry> protocols) {
      void *buffer =
        operator new(sizeof(ArchetypeTypeInfo) +
                     protocols.size() * sizeof(ProtocolEntry));
      return new (buffer) ArchetypeTypeInfo(archetype, type, protocols);
    }

    unsigned getNumProtocols() const {
      return TheArchetype->getConformsTo().size();
    }

    ArrayRef<ProtocolEntry> getProtocols() const {
      return llvm::makeArrayRef(getProtocolsBuffer(), getNumProtocols());
    }

    llvm::Value *getMetadataRef(IRGenFunction &IGF) const {
      return IGF.getLocalTypeData(CanType(TheArchetype),
                                  LocalTypeData::Metatype);
    }

    /// Return the witness table that's been set for this type.
    llvm::Value *getWitnessTable(IRGenFunction &IGF, unsigned which) const {
      assert(which < getNumProtocols());
      return IGF.getLocalTypeData(CanType(TheArchetype),
                                  LocalTypeData(which));
    }

    llvm::Value *getValueWitnessTable(IRGenFunction &IGF) const {
      // This can be called in any of the cases.
      return IGF.getLocalTypeData(CanType(TheArchetype),
                                  LocalTypeData(0));
    }

    /// Create an uninitialized archetype object.
    OwnedAddress allocate(IRGenFunction &IGF, Initialization &init,
                          InitializedObject object, OnHeap_t onHeap,
                          const llvm::Twine &name) const {
      if (onHeap) {
        // Allocate a new object using the allocBox runtime call.
        llvm::Value *metadata = getMetadataRef(IGF);
        llvm::Value *box, *address;
        IGF.emitAllocBoxCall(metadata, box, address);
        Address rawAddr(address, Alignment(1));
        
        // Push a cleanup to dealloc the allocation.
        IGF.pushCleanup<DeallocateBox>(box, metadata);
        CleanupsDepth dealloc = IGF.getCleanupsDepth();
        OwnedAddress addr(rawAddr, box);
        init.markAllocated(IGF, object, addr, dealloc);
        return addr;
      }

      // Make a fixed-size buffer.
      Address buffer = IGF.createAlloca(IGF.IGM.getFixedBufferTy(),
                                        getFixedBufferAlignment(IGF.IGM),
                                        name);

      // Allocate an object of the appropriate type within it.
      llvm::Value *wtable = getValueWitnessTable(IGF);
      llvm::Value *metadata = getMetadataRef(IGF);
      Address allocated(emitAllocateBufferCall(IGF, wtable, metadata, buffer),
                        Alignment(1));
      OwnedAddress ownedAddr(allocated, IGF.IGM.RefCountedNull);

      // Push a cleanup to dealloc it.
      IGF.pushCleanup<DeallocateBuffer>(buffer, wtable, metadata);
      CleanupsDepth dealloc = IGF.getCleanupsDepth();
      init.markAllocated(IGF, object, ownedAddr, dealloc);
      return ownedAddr;
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      emitAssignWithCopyCall(IGF, getValueWitnessTable(IGF),
                             getMetadataRef(IGF),
                             dest.getAddress(), src.getAddress());
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      emitAssignWithTakeCall(IGF, getValueWitnessTable(IGF),
                             getMetadataRef(IGF),
                             dest.getAddress(), src.getAddress());
    }

    void initializeWithCopy(IRGenFunction &IGF,
                            Address dest, Address src) const {
      emitInitializeWithCopyCall(IGF, getValueWitnessTable(IGF),
                                 getMetadataRef(IGF),
                                 dest.getAddress(), src.getAddress());
    }

    void initializeWithTake(IRGenFunction &IGF,
                            Address dest, Address src) const {
      emitInitializeWithTakeCall(IGF, getValueWitnessTable(IGF),
                                 getMetadataRef(IGF),
                                 dest.getAddress(), src.getAddress());
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      emitDestroyCall(IGF, getValueWitnessTable(IGF), getMetadataRef(IGF),
                      addr.getAddress());
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

static void setMetadataRef(IRGenFunction &IGF,
                           ArchetypeType *archetype,
                           llvm::Value *metadata) {
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);
  IGF.setUnscopedLocalTypeData(CanType(archetype),
                               LocalTypeData::Metatype,
                               metadata);
}

static void setWitnessTable(IRGenFunction &IGF,
                            ArchetypeType *archetype,
                            unsigned protocolIndex,
                            llvm::Value *wtable) {
  assert(wtable->getType() == IGF.IGM.WitnessTablePtrTy);
  assert(protocolIndex < archetype->getConformsTo().size());
  IGF.setUnscopedLocalTypeData(CanType(archetype),
                               LocalTypeData(protocolIndex),
                               wtable);
}

static void setValueWitnessTable(IRGenFunction &IGF,
                                 ArchetypeType *archetype,
                                 llvm::Value *wtable) {
  assert(wtable->getType() == IGF.IGM.WitnessTablePtrTy);
  assert(archetype->getConformsTo().size() == 0);
  IGF.setUnscopedLocalTypeData(CanType(archetype),
                               LocalTypeData(0),
                               wtable);
}

/// Detail about how an object conforms to a protocol.
class irgen::ConformanceInfo {
  friend class ProtocolInfo;

  /// The pointer to the table.  In practice, it's not really
  /// reasonable for this to always be a constant!  It probably
  /// needs to be managed by the runtime, and the information stored
  /// here would just indicate how to find the actual thing.
  llvm::Constant *Table;

public:
  llvm::Value *getTable(IRGenFunction &IGF) const {
    return Table;
  }

  /// Try to get this table as a constant pointer.  This might just
  /// not be supportable at all.
  llvm::Constant *tryGetConstantTable() const {
    return Table;
  }
};

static FixedPacking computePacking(IRGenModule &IGM,
                                   const TypeInfo &concreteTI) {
  Size bufferSize = getFixedBufferSize(IGM);
  Size requiredSize = concreteTI.getFixedSize();

  // Flat out, if we need more space than the buffer provides,
  // we always have to allocate.
  // FIXME: there might be some interesting cases where this
  // is suboptimal for oneofs.
  if (requiredSize > bufferSize)
    return FixedPacking::Allocate;

  Alignment bufferAlign = getFixedBufferAlignment(IGM);
  Alignment requiredAlign = concreteTI.getFixedAlignment();

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
    return type.getAddressForPointer(address);
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

    addr = IGF.Builder.CreateBitCast(addr,
                                     type.getStorageType()->getPointerTo());
    return type.getAddressForPointer(addr);
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
  return type.getAddressForPointer(result);
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

    // Load the metadata tables.
    Address destMetadataSlot = layout.projectMetadataRef(IGF, dest);
    llvm::Value *destMetadata = IGF.Builder.CreateLoad(destMetadataSlot);
    llvm::Value *srcMetadata = layout.loadMetadataRef(IGF, src);

    // Check whether the metadata match.
    llvm::BasicBlock *matchBB = IGF.createBasicBlock("match");
    llvm::BasicBlock *noMatchBB = IGF.createBasicBlock("no-match");
    llvm::Value *sameMetadata =
      IGF.Builder.CreateICmpEQ(destMetadata, srcMetadata, "sameMetadata");
    IGF.Builder.CreateCondBr(sameMetadata, matchBB, noMatchBB);

    { // (scope to avoid contaminating other branches with these values)

      // If so, do a direct assignment.
      IGF.Builder.emitBlock(matchBB);
      llvm::Value *wtable = 
        emitValueWitnessTableRefForMetadata(IGF, destMetadata);

      llvm::Value *destObject =
        emitProjectBufferCall(IGF, wtable, destMetadata, destBuffer);
      llvm::Value *srcObject =
        emitProjectBufferCall(IGF, wtable, destMetadata, srcBuffer);
      emitAssignWithCopyCall(IGF, wtable, destMetadata, destObject, srcObject);
      IGF.Builder.CreateBr(doneBB);
    }

    // Otherwise, destroy and copy-initialize.
    // TODO: should we copy-initialize and then destroy?  That's
    // possible if we copy aside, which is a small expense but
    // always safe.  Otherwise the destroy (which can invoke user code)
    // could see invalid memory at this address.  These are basically
    // the madnesses that boost::variant has to go through, with the
    // advantage of address-invariance.
    IGF.Builder.emitBlock(noMatchBB);

    // Store the metadata ref.
    IGF.Builder.CreateStore(srcMetadata, destMetadataSlot);

    llvm::Value *firstDestTable = nullptr;
    llvm::Value *firstSrcTable = nullptr;

    // Store the protocol witness tables.
    unsigned numTables = layout.getNumTables();
    for (unsigned i = 0, e = numTables; i != e; ++i) {
      Address destTableSlot = layout.projectWitnessTable(IGF, dest, i);
      llvm::Value *srcTable = layout.loadWitnessTable(IGF, src, i);

      // Remember the first pair of witness tables if present.
      if (i == 0) {
        firstDestTable = IGF.Builder.CreateLoad(destTableSlot);
        firstSrcTable = srcTable;
      }

      // Overwrite the old witness table.
      IGF.Builder.CreateStore(srcTable, destTableSlot);
    }

    // Destroy the old value.  Pull a value witness table from the
    // destination metadata if we don't have any protocol witness
    // tables to use.
    if (numTables == 0)
      firstDestTable = emitValueWitnessTableRefForMetadata(IGF, destMetadata);
    emitDestroyBufferCall(IGF, firstDestTable, destMetadata, destBuffer);

    // Copy-initialize with the new value.  Again, pull a value
    // witness table from the source metadata if we can't use a
    // protocol witness table.
    if (numTables == 0)
      firstSrcTable = emitValueWitnessTableRefForMetadata(IGF, srcMetadata);
    emitInitializeBufferWithCopyOfBufferCall(IGF, firstSrcTable, srcMetadata,
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
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.TypeMetadataPtrTy };
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
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.TypeMetadataPtrTy };
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
    nameStream << type.getFixedSize().getValue();
    nameStream << '_';
    nameStream << type.getFixedAlignment().getValue();
  }

  llvm::Constant *fn = IGM.Module.getOrInsertFunction(name, fnTy);
  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, type.getFixedAlignment());
    Address src(it++, type.getFixedAlignment());
    IGF.emitMemCpy(dest, src, type.getFixedSize());
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

/// Look through any single-element labelled tuple types.
static CanType stripLabel(CanType input) {
  if (auto tuple = dyn_cast<TupleType>(input))
    if (tuple->getFields().size() == 1)
      return stripLabel(CanType(tuple->getFields()[0].getType()));
  return input;
}

namespace {
  /// A class which builds a single witness.
  class WitnessBuilder {
    IRGenModule &IGM;
    llvm::Constant *ImplPtr;
    CanType ImplTy;
    CanType SignatureTy;

    ExplosionKind ExplosionLevel;
    unsigned UncurryLevel;

    ArrayRef<Substitution> Substitutions;

    /// The first argument involves a lvalue-to-rvalue conversion.
    bool HasAbstractedThis;

    /// The function involves any difference in abstraction.
    bool HasAbstraction;

  public:
    WitnessBuilder(IRGenModule &IGM, llvm::Constant *impl,
                   CanType implTy, CanType sigTy,
                   ArrayRef<Substitution> subs,
                   ExplosionKind explosionLevel, unsigned uncurryLevel)
      : IGM(IGM), ImplPtr(impl),
        ExplosionLevel(explosionLevel), UncurryLevel(uncurryLevel),
        Substitutions(subs) {

      implTy = implTy->getUnlabeledType(IGM.Context)->getCanonicalType();
      ImplTy = implTy;

      sigTy = sigTy->getUnlabeledType(IGM.Context)->getCanonicalType();
      SignatureTy = sigTy;

      AnyFunctionType *sigFnTy = cast<AnyFunctionType>(sigTy);
      AnyFunctionType *implFnTy = cast<AnyFunctionType>(implTy);

      // The first argument isn't necessarily a simple substitution.
      // If so, we don't need to compute difference by abstraction.
      if (isa<LValueType>(CanType(sigFnTy->getInput())) &&
          !isa<LValueType>(CanType(implFnTy->getInput()))) {
        HasAbstractedThis = true;
        HasAbstraction = true;
        return;
      }

      // Otherwise, do so.
      // FIXME: the HACK here makes protocols work on generic types
      // because we're emitting the witness table for the bound generic
      // type rather than the unbound.  It's definitely not the right
      // thing to do in general.
      HasAbstractedThis = false;
      HasAbstraction =
        isa<PolymorphicFunctionType>(implFnTy) || // HACK
        differsByAbstractionAsFunction(IGM, sigFnTy, implFnTy,
                                       explosionLevel, uncurryLevel);
    }

    llvm::Constant *get() {
      // If we don't need any abstractions, we're golden.
      if (!HasAbstraction)
        return asOpaquePtr(IGM, ImplPtr);

      // Okay, mangle a name.
      llvm::SmallString<128> name;
      mangleThunk(name);

      // If a function with that name exists, use it.  We don't care
      // about the type.
      llvm::Function *fn = IGM.Module.getFunction(name);
      if (fn) return asOpaquePtr(IGM, fn);

      // Create the function.
      llvm::AttributeSet attrs;
      auto fnTy = IGM.getFunctionType(AbstractCC::Freestanding,
                                      SignatureTy, ExplosionKind::Minimal,
                                      UncurryLevel, ExtraData::Metatype,
                                      attrs);
      fn = llvm::Function::Create(fnTy, llvm::Function::InternalLinkage,
                                  name.str(), &IGM.Module);
      fn->setAttributes(attrs);
      //fn->setVisibility(llvm::Function::HiddenVisibility);

      // Start building it.
      IRGenFunction IGF(IGM, CanType(), ArrayRef<Pattern*>(),
                        ExplosionKind::Minimal, UncurryLevel, fn,
                        Prologue::Bare);
      emitThunk(IGF);

      return asOpaquePtr(IGM, fn);
    }

  private:
    struct ArgSite {
      AnyFunctionType *SigFnType;
      AnyFunctionType *ImplFnType;

      ArgSite(AnyFunctionType *sigFnType, AnyFunctionType *implFnType)
        : SigFnType(sigFnType), ImplFnType(implFnType) {}

      CanType getSigInputType() const {
        return CanType(SigFnType->getInput());
      }

      CanType getSigResultType() const {
        return CanType(SigFnType->getResult());
      }

      CanType getImplInputType() const {
        return CanType(ImplFnType->getInput());
      }

      CanType getImplResultType() const {
        return CanType(ImplFnType->getResult());
      }
    };

    void collectArgSites(AnyFunctionType *sigType,
                         AnyFunctionType *implType,
                         unsigned uncurryLevel,
                         SmallVectorImpl<ArgSite> &out) {
      out.push_back(ArgSite(sigType, implType));
      if (uncurryLevel > 0) {
        collectArgSites(cast<AnyFunctionType>(CanType(sigType->getResult())),
                        cast<AnyFunctionType>(CanType(implType->getResult())),
                        uncurryLevel - 1, out);
      }
    }

    /// Bind the metatype for 'This' in this witness.
    void bindThisArchetype(IRGenFunction &IGF, llvm::Value *metadata) {
      // Set a name for the metadata value.
      metadata->setName("This");

      // Find the This archetype.
      auto thisTy = SignatureTy;
      thisTy = stripLabel(CanType(cast<AnyFunctionType>(thisTy)->getInput()));
      if (auto lvalueTy = dyn_cast<LValueType>(thisTy)) {
        thisTy = CanType(lvalueTy->getObjectType());
      } else {
        thisTy = CanType(cast<MetaTypeType>(thisTy)->getInstanceType());
      }
      auto archetype = cast<ArchetypeType>(thisTy);

      // Set the metadata pointer.
      setMetadataRef(IGF, archetype, metadata);
    }

    void emitThunk(IRGenFunction &IGF) {
      // Collect the types for the arg sites.
      SmallVector<ArgSite, 4> argSites;
      collectArgSites(cast<AnyFunctionType>(SignatureTy),
                      cast<AnyFunctionType>(ImplTy),
                      UncurryLevel,
                      argSites);

      // Collect the parameters.
      Explosion sigParams = IGF.collectParameters();

      // The data parameter is a metatype; bind it as the This archetype.
      llvm::Value *metatype = sigParams.takeLast().getUnmanagedValue();
      bindThisArchetype(IGF, metatype);

      // Peel off the result address if necessary.
      auto &sigResultTI =
        IGF.getFragileTypeInfo(argSites.back().getSigResultType());
      llvm::Value *sigResultAddr = nullptr;
      if (sigResultTI.getSchema(ExplosionLevel).requiresIndirectResult()) {
        sigResultAddr = sigParams.claimUnmanagedNext();
      }

      // Collect the parameter clauses and bind polymorphic parameters.
      std::vector<Explosion> sigParamClauses;
      sigParamClauses.reserve(UncurryLevel + 1);
      for (unsigned i = 0, e = UncurryLevel + 1; i != e; ++i)
        sigParamClauses.emplace_back(ExplosionLevel);

      for (unsigned i = 0, e = UncurryLevel + 1; i != e; ++i) {
        ArgSite &argSite = argSites[e - 1 - i];
        Explosion &sigClause = sigParamClauses[e - 1 - i];

        auto sigInputType = argSite.getSigInputType();
        unsigned numParams = IGM.getExplosionSize(sigInputType, ExplosionLevel);
        sigParams.transferInto(sigClause, numParams);

        // Bind polymorphic parameters.
        if (auto sigPoly = dyn_cast<PolymorphicFunctionType>(argSite.SigFnType))
          emitPolymorphicParameters(IGF, sigPoly, sigParams);
      }

      assert(sigParams.empty() && "didn't drain all the parameters");

      // Begin our call emission.  CallEmission's builtin polymorphism
      // support is based on around calling a more-abstract function,
      // and we're actually calling a less-abstract function, so
      // instead we're going to emit this as a call to a monomorphic
      // function and do all our own translation.
      // FIXME: virtual calls!
      CallEmission emission(IGF,
          Callee::forFreestandingFunction(AbstractCC::Freestanding,
                                          ImplTy,
                                          argSites.back().getImplResultType(),
                                          ArrayRef<Substitution>(),
                                          ImplPtr,
                                          ExplosionLevel,
                                          UncurryLevel));

      // Now actually pass the arguments.
      for (unsigned i = 0, e = UncurryLevel + 1; i != e; ++i) {
        ArgSite &argSite = argSites[i];
        auto implInputType = argSite.getImplInputType();
        auto sigInputType = argSite.getSigInputType();

        Explosion &sigClause = sigParamClauses[i];
        Explosion implArgs(ExplosionLevel);

        // The input type we're going to pass to the implementation,
        // expressed in terms of signature archetypes.
        CanType sigInputTypeForImpl;

        // We need some special treatment for 'this'.
        if (i == 0 && HasAbstractedThis) {
          assert(isa<ClassType>(implInputType));
          assert(isa<LValueType>(sigInputType));

          sigInputTypeForImpl =
            CanType(cast<LValueType>(sigInputType)->getObjectType());
          assert(isa<ArchetypeType>(sigInputTypeForImpl));

          auto &implTI = IGF.getFragileTypeInfo(implInputType);

          // It's an l-value, so the next value is the address.
          auto sigThis = implTI.getAddressForPointer(sigClause.claimUnmanagedNext());

          // Cast to T* and load.  In theory this might require
          // remapping, but in practice the constraints (which we
          // assert just above) don't permit that.
          auto implPtrTy = implTI.getStorageType()->getPointerTo();
          sigThis = IGF.Builder.CreateBitCast(sigThis, implPtrTy);
          implTI.load(IGF, sigThis, implArgs);

        // Otherwise, the impl type is the result of some substitution
        // on the sig type.
        } else {
          reemitAsSubstituted(IGF, sigInputType, implInputType, Substitutions,
                              sigClause, implArgs);
          sigInputTypeForImpl = sigInputType;
        }

        // Pass polymorphic arguments.
        if (auto implPoly =
              dyn_cast<PolymorphicFunctionType>(argSite.ImplFnType)) {
          emitPolymorphicArguments(IGF, implPoly, sigInputTypeForImpl,
                                   Substitutions, implArgs);
        }

        emission.addArg(implArgs);
      }

      // Emit the call.
      CanType sigResultType = argSites.back().getSigResultType();
      CanType implResultType = argSites.back().getImplResultType();
      auto &implResultTI = IGM.getFragileTypeInfo(implResultType);

      // If we have a result address, emit to memory.
      if (sigResultAddr) {
        llvm::Value *implResultAddr;
        if (differsByAbstractionInMemory(IGM, sigResultType, implResultType)) {
          IGF.unimplemented(SourceLoc(),
                            "remapping memory result in prototype witness");
          implResultAddr = llvm::UndefValue::get(
                              implResultTI.getStorageType()->getPointerTo());
        } else {
          implResultAddr =
            IGF.Builder.CreateBitCast(sigResultAddr,
                              implResultTI.getStorageType()->getPointerTo());
        }

        emission.emitToMemory(implResultTI.getAddressForPointer(implResultAddr),
                              implResultTI);

        // TODO: remap here.

        IGF.Builder.CreateRetVoid();
        return;
      }

      // Otherwise, emit to explosion.
      Explosion implResult(ExplosionLevel);
      emission.emitToExplosion(implResult);

      // Fast-path an exact match.
      if (sigResultType == implResultType)
        return IGF.emitScalarReturn(implResult);

      // Otherwise, re-emit.
      Explosion sigResult(ExplosionLevel);
      reemitAsUnsubstituted(IGF, sigResultType, implResultType,
                            Substitutions, implResult, sigResult);
      IGF.emitScalarReturn(sigResult);
    }

    /// Mangle the name of the thunk this requires.
    void mangleThunk(SmallVectorImpl<char> &buffer) {
      llvm::raw_svector_ostream str(buffer);

      StringRef fnName =
        cast<llvm::Function>(ImplPtr->stripPointerCasts())->getName();
      str << "_Tnk_";
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
                        CanType concreteType, const TypeInfo &concreteTI,
                        const ProtocolConformance &conformance)
      : WitnessVisitor(IGM), Table(table),
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
                              ExtraData::None);
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
                                ExtraData::None);
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
                              ExtraData::None);
      return getWitness(implPtr, impl->getType()->getCanonicalType(),
                        ifaceType, 1);
    }

    llvm::Constant *getWitness(llvm::Constant *fn, CanType fnTy,
                               CanType ifaceTy, unsigned uncurryLevel) {
      return WitnessBuilder(IGM, fn, fnTy, ifaceTy, Substitutions,
                            ExplosionKind::Minimal, uncurryLevel).get();
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
  WitnessTableBuilder(IGM, witnesses, concreteType, concreteTI,
                      conformance).visit(protocol);

  // Build the actual global variable.
  llvm::Constant *table =
    buildWitnessTable(IGM, concreteType, protocol, witnesses);

  ConformanceInfo *info = new ConformanceInfo;
  info->Table = table;

  auto res = Conformances.insert(std::make_pair(&conformance, info));
  return *res.first->second;
}

static const TypeInfo *createExistentialTypeInfo(IRGenModule &IGM,
                                                 llvm::StructType *type,
                                        ArrayRef<ProtocolDecl*> protocols) {
  assert(type->isOpaque() && "creating existential type in concrete struct");

  SmallVector<llvm::Type*, 5> fields;
  SmallVector<ProtocolEntry, 4> entries;

  // The first field is the metadata reference.
  fields.push_back(IGM.TypeMetadataPtrTy);

  for (auto protocol : protocols) {
    // Find the protocol layout.
    const ProtocolInfo &impl = IGM.getProtocolInfo(protocol);
    entries.push_back(ProtocolEntry(protocol, impl));

    // Each protocol gets a witness table.
    fields.push_back(IGM.WitnessTablePtrTy);
  }

  ExistentialLayout layout(entries.size());

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

const TypeInfo *TypeConverter::convertArchetypeType(ArchetypeType *archetype) {
  // Compute layouts for the protocols we ascribe to.
  SmallVector<ProtocolEntry, 4> protocols;
  for (auto protocol : archetype->getConformsTo()) {
    const ProtocolInfo &impl = IGM.getProtocolInfo(protocol);
    protocols.push_back(ProtocolEntry(protocol, impl));
  }
  
  // For now, just always use the same type.
  llvm::Type *storageType = IGM.OpaquePtrTy->getElementType();
  return ArchetypeTypeInfo::create(archetype, storageType, protocols);
}

/// Inform IRGenFunction that the given archetype has the given value
/// witness value within this scope.
void IRGenFunction::bindArchetype(ArchetypeType *archetype,
                                  llvm::Value *metadata,
                                  ArrayRef<llvm::Value*> wtables) {
  // Set the metadata pointer.
  metadata->setName(archetype->getFullName());
  setMetadataRef(*this, archetype, metadata);

  // Set the protocol witness tables.

  assert(wtables.size() == archetype->getConformsTo().size());
  if (wtables.empty()) {
    // TODO: do this lazily.
    auto wtable = emitValueWitnessTableRefForMetadata(*this, metadata);
    wtable->setName(Twine(archetype->getFullName()) + "." + "value");
    setValueWitnessTable(*this, archetype, wtable);
  } else {
    for (unsigned i = 0, e = wtables.size(); i != e; ++i) {
      auto proto = archetype->getConformsTo()[i];
      auto wtable = wtables[i];
      wtable->setName(Twine(archetype->getFullName()) + "." +
                        proto->getName().str());
      setWitnessTable(*this, archetype, i, wtable);
    }
  }
}

namespace {
  struct Fulfillment {
    Fulfillment() = default;
    Fulfillment(unsigned depth, unsigned index) : Depth(depth), Index(index) {}

    /// The distance up the metadata chain.
    /// 0 is the origin metadata, 1 is the parent of that, etc.
    unsigned Depth;

    /// The generic argument index.
    unsigned Index;
  };

  /// A class for computing how to pass arguments to a polymorphic
  /// function.  The subclasses of this are the places which need to
  /// be updated if the convention changes.
  class PolymorphicConvention {
  public:
    enum class SourceKind {
      /// There is no source of additional information.
      None,

      /// The polymorphic arguments are derived from a source class
      /// pointer.
      ClassPointer,
      
      /// The polymorphic arguments are derived from a class metadata
      /// pointer.
      ClassMetadata,

      /// The polymorphic arguments are passed from generic type
      /// metadata for the origin type.
      GenericLValueMetadata
    };

  protected:
    PolymorphicFunctionType *FnType;
    SourceKind TheSourceKind;
    SmallVector<NominalTypeDecl*, 4> TypesForDepths;

    typedef std::pair<ArchetypeType*, ProtocolDecl*> FulfillmentKey;
    llvm::DenseMap<FulfillmentKey, Fulfillment> Fulfillments;

  public:
    PolymorphicConvention(PolymorphicFunctionType *fnType)
        : FnType(fnType) {
      assert(fnType->isCanonical());

      // We don't need to pass anything extra as long as all of the
      // archetypes (and their requirements) are producible from the
      // class-pointer argument.

      // If the argument is a single class pointer, and all the
      // archetypes exactly match those of the class, we're good.
      CanType argTy = stripLabel(CanType(fnType->getInput()));

      SourceKind source = SourceKind::None;
      if (auto classTy = dyn_cast<ClassType>(argTy)) {
        source = SourceKind::ClassPointer;
        considerNominalType(classTy, 0);
      } else if (auto boundTy = dyn_cast<BoundGenericType>(argTy)) {
        if (isa<ClassDecl>(boundTy->getDecl())) {
          source = SourceKind::ClassPointer;
          considerBoundGenericType(boundTy, 0);
        }
      } else if (auto lvalueTy = dyn_cast<LValueType>(argTy)) {
        CanType objTy = CanType(lvalueTy->getObjectType());
        if (auto nomTy = dyn_cast<NominalType>(objTy)) {
          source = SourceKind::GenericLValueMetadata;
          considerNominalType(nomTy, 0);
        } else if (auto boundTy = dyn_cast<BoundGenericType>(objTy)) {
          source = SourceKind::GenericLValueMetadata;
          considerBoundGenericType(boundTy, 0);
        }
      } else if (auto metatypeTy = dyn_cast<MetaTypeType>(argTy)) {
        CanType objTy = CanType(metatypeTy->getInstanceType());
        if (auto nomTy = dyn_cast<ClassType>(objTy)) {
          source = SourceKind::ClassMetadata;
          considerNominalType(nomTy, 0);
        } else if (auto boundTy = dyn_cast<BoundGenericType>(objTy)) {
          if (isa<ClassDecl>(boundTy->getDecl())) {
            source = SourceKind::ClassMetadata;
            considerBoundGenericType(boundTy, 0);
          }
        }
      }

      // If we didn't fulfill anything, there's no source.
      if (Fulfillments.empty()) source = SourceKind::None;

      TheSourceKind = source;
    }

    SourceKind getSourceKind() const { return TheSourceKind; }

  private:
    void considerParentType(CanType parent, unsigned depth) {
      // We might not have a parent type.
      if (!parent) return;

      // If we do, it has to be nominal one way or another.
      depth++;
      if (auto nom = dyn_cast<NominalType>(parent))
        considerNominalType(nom, depth);
      else
        considerBoundGenericType(cast<BoundGenericType>(parent), depth);
    }

    void considerNominalType(NominalType *type, unsigned depth) {
      assert(TypesForDepths.size() == depth);
      TypesForDepths.push_back(type->getDecl());

      // Nominal types add no generic arguments themselves, but they
      // may have the arguments of their parents.
      considerParentType(CanType(type->getParent()), depth);
    }

    void considerBoundGenericType(BoundGenericType *type, unsigned depth) {
      assert(TypesForDepths.size() == depth);
      TypesForDepths.push_back(type->getDecl());

      for (unsigned i = 0, e = type->getGenericArgs().size(); i != e; ++i) {
        CanType arg = CanType(type->getGenericArgs()[i]);

        // Right now, we can only pull things out of the direct
        // arguments, not out of nested metadata.  For example, this
        // prevents us from realizing that we can rederive T and U in the
        // following:
        //   \forall T U . Vector<T->U> -> ()
        if (auto argArchetype = dyn_cast<ArchetypeType>(arg)) {
          // Find the archetype from the generic type.
          auto params = type->getDecl()->getGenericParams()->getAllArchetypes();
          assert(params.size() == e); // should be parallel
          considerArchetype(argArchetype, params[i], depth, i);
        }
      }

      // Match against the parent first.  The polymorphic type
      // will start with any arguments from the parent.
      considerParentType(CanType(type->getParent()), depth);
    }

    /// We found a reference to the arg archetype at the given depth
    /// and index.  Add any fulfillments this gives us.
    void considerArchetype(ArchetypeType *arg, ArchetypeType *param,
                           unsigned depth, unsigned index) {
      // First, record that we can find this archetype at this point.
      addFulfillment(arg, nullptr, depth, index);

      // Now consider each of the protocols that the parameter guarantees.
      for (auto protocol : param->getConformsTo()) {
        // If arg == param, the second check is always true.  This is
        // a fast path for some common cases where we're defining a
        // method within the type we're matching against.
        if (arg == param || requiresFulfillment(arg, protocol))
          addFulfillment(arg, protocol, depth, index);
      }
    }

    /// Does the given archetype require the given protocol to be fulfilled?
    static bool requiresFulfillment(ArchetypeType *arg, ProtocolDecl *proto) {
      // TODO: protocol inheritance should be considered here somehow.
      for (auto argProto : arg->getConformsTo()) {
        if (argProto == proto)
          return true;
      }
      return false;
    }

    /// Testify that there's a fulfillment at the given depth and level.
    void addFulfillment(ArchetypeType *arg, ProtocolDecl *proto,
                        unsigned depth, unsigned index) {
      // Only add a fulfillment if it's not enough information otherwise.
      auto key = FulfillmentKey(arg, proto);
      if (!Fulfillments.count(key))
        Fulfillments.insert(std::make_pair(key, Fulfillment(depth, index)));
    }
  };

  /// A class for binding type parameters of a generic function.
  class EmitPolymorphicParameters : public PolymorphicConvention {
    IRGenFunction &IGF;
    SmallVector<llvm::Value*, 4> MetadataForDepths;

  public:
    EmitPolymorphicParameters(IRGenFunction &IGF,
                              PolymorphicFunctionType *fnType)
      : PolymorphicConvention(fnType), IGF(IGF) {}

    void emit(Explosion &in);

  private:
    CanType getArgType() const {
      return stripLabel(CanType(cast<AnyFunctionType>(FnType)->getInput()));
    }

    /// Emit the source value for parameters.
    llvm::Value *emitSourceForParameters(Explosion &in) {
      switch (getSourceKind()) {
      case SourceKind::None:
        return nullptr;

      case SourceKind::ClassMetadata:
        return in.getLastClaimed();
          
      case SourceKind::ClassPointer:
        return emitHeapMetadataRefForHeapObject(IGF, in.getLastClaimed(),
                                                getArgType(),
                                                /*suppress cast*/ true);

      case SourceKind::GenericLValueMetadata: {
        llvm::Value *metatype = in.claimUnmanagedNext();
        metatype->setName("This");

        // Mark this as the cached metatype for the l-value's object type.
        CanType argTy = CanType(cast<LValueType>(getArgType())->getObjectType());
        IGF.setUnscopedLocalTypeData(argTy, LocalTypeData::Metatype, metatype);
        return metatype;
      }
      }
      llvm_unreachable("bad source kind!");
    }

    /// Produce the metadata value for the given depth, using the
    /// given cache.
    llvm::Value *getMetadataForDepth(unsigned depth) {
      assert(!MetadataForDepths.empty());
      while (depth >= MetadataForDepths.size()) {
        auto child = MetadataForDepths.back();
        auto childDecl = TypesForDepths[MetadataForDepths.size()];
        auto parent = emitParentMetadataRef(IGF, childDecl, child);
        MetadataForDepths.push_back(parent);
      }
      return MetadataForDepths[depth];
    }
  };
};

/// Emit a polymorphic parameters clause, binding all the metadata necessary.
void EmitPolymorphicParameters::emit(Explosion &in) {
  auto &generics = FnType->getGenericParams();

  // Compute the first source metadata.
  MetadataForDepths.push_back(emitSourceForParameters(in));

  for (auto archetype : generics.getAllArchetypes()) {
    // Derive the appropriate metadata reference.
    llvm::Value *metadata;

    // If the reference is fulfilled by the source, go for it.
    auto it = Fulfillments.find(FulfillmentKey(archetype, nullptr));
    if (it != Fulfillments.end()) {
      auto &fulfillment = it->second;
      auto ancestor = getMetadataForDepth(fulfillment.Depth);
      auto ancestorDecl = TypesForDepths[fulfillment.Depth];
      metadata = emitArgumentMetadataRef(IGF, ancestorDecl,
                                         fulfillment.Index, ancestor);

    // Otherwise, it's just next in line.
    } else {
      metadata = in.claimUnmanagedNext();
    }

    // Collect all the witness tables.
    SmallVector<llvm::Value *, 8> wtables;
    for (auto protocol : archetype->getConformsTo()) {
      llvm::Value *wtable;

      // If the protocol witness table is fulfilled by the source, go for it.
      auto it = Fulfillments.find(FulfillmentKey(archetype, protocol));
      if (it != Fulfillments.end()) {
        auto &fulfillment = it->second;
        auto ancestor = getMetadataForDepth(fulfillment.Depth);
        auto ancestorDecl = TypesForDepths[fulfillment.Depth];
        wtable = emitArgumentWitnessTableRef(IGF, ancestorDecl,
                                             fulfillment.Index, protocol,
                                             ancestor);

      // Otherwise, it's just next in line.
      } else {
        wtable = in.claimUnmanagedNext();
      }
      wtables.push_back(wtable);
    }

    IGF.bindArchetype(archetype, metadata, wtables);
  }  
}

/// Perform all the bindings necessary to emit the given declaration.
void irgen::emitPolymorphicParameters(IRGenFunction &IGF,
                                      PolymorphicFunctionType *type,
                                      Explosion &in) {
  EmitPolymorphicParameters(IGF, type).emit(in);
}


namespace {
  /// A CRTP class for finding the archetypes we need to bind in order
  /// to perform value operations on the given type.
  struct FindArchetypesToBind : irgen::TypeVisitor<FindArchetypesToBind> {
    llvm::SetVector<ArchetypeType*> &Types;
  public:
    FindArchetypesToBind(llvm::SetVector<ArchetypeType*> &types)
      : Types(types) {}

    // We're collecting archetypes.
    void visitArchetypeType(ArchetypeType *type) {
      Types.insert(type);
    }

    // We need to walk into tuples.
    void visitTupleType(TupleType *tuple) {
      for (auto &elt : tuple->getFields())
        visit(CanType(elt.getType()));
    }

    // We need to walk into constant-sized arrays.
    void visitArrayType(ArrayType *type) {
      visit(CanType(type->getBaseType()));
    }

    // We do not need to walk into any of these types, because their
    // value operations do not depend on the specifics of their
    // sub-structure (or they have none).
    void visitAnyFunctionType(AnyFunctionType *fn) {}
    void visitBuiltinType(BuiltinType *type) {}
    void visitMetaTypeType(MetaTypeType *type) {}
    void visitModuleType(ModuleType *type) {}
    void visitProtocolCompositionType(ProtocolCompositionType *type) {}

    // L-values are impossible.
    void visitLValueType(LValueType *type) {
      llvm_unreachable("cannot store l-value type directly");
    }

    // For now, assume we don't need to add anything for nominal and
    // generic-nominal types.  We might actually need to bind all the
    // argument archetypes from this type and its parent.
    void visitNominalType(NominalType *type) {}
    void visitBoundGenericType(BoundGenericType *type) {}
  };
}

/// Initialize this set of necessary bindings.
NecessaryBindings::NecessaryBindings(IRGenModule &IGM, CanType type) {
  FindArchetypesToBind(Types).visit(type);
}

Size NecessaryBindings::getBufferSize(IRGenModule &IGM) const {
  return IGM.getPointerSize() * Types.size();
}

void NecessaryBindings::restore(IRGenFunction &IGF, Address buffer) const {
  if (Types.empty()) return;

  // Cast the buffer to %type**.
  auto metatypePtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
  buffer = IGF.Builder.CreateBitCast(buffer, metatypePtrPtrTy);

  for (unsigned i = 0, e = Types.size(); i != e; ++i) {
    auto archetype = Types[i];

    // GEP to the appropriate slot.
    Address slot = buffer;
    if (i) slot = IGF.Builder.CreateConstArrayGEP(slot, i,
                                                  IGF.IGM.getPointerSize());

    // Load the archetype's metatype.
    llvm::Value *metatype = IGF.Builder.CreateLoad(slot);
    metatype->setName(archetype->getFullName());
    setMetadataRef(IGF, archetype, metatype);

    // Also bind the witness table from the archetype. TODO: lazily?
    auto wtable = emitValueWitnessTableRefForMetadata(IGF, metatype);
    wtable->setName(Twine(archetype->getFullName()) + "." + "value");
    setValueWitnessTable(IGF, archetype, wtable);
  }
}

void NecessaryBindings::save(IRGenFunction &IGF, Address buffer) const {
  if (Types.empty()) return;

  // Cast the buffer to %type**.
  auto metatypePtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
  buffer = IGF.Builder.CreateBitCast(buffer, metatypePtrPtrTy);

  for (unsigned i = 0, e = Types.size(); i != e; ++i) {
    auto archetype = Types[i];

    // GEP to the appropriate slot.
    Address slot = buffer;
    if (i) slot = IGF.Builder.CreateConstArrayGEP(slot, i,
                                                  IGF.IGM.getPointerSize());

    // Find the metatype for the appropriate archetype and store it in
    // the slot.
    llvm::Value *metatype =
      IGF.getLocalTypeData(CanType(archetype), LocalTypeData::Metatype);
    IGF.Builder.CreateStore(metatype, slot);
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

namespace {
  class EmitPolymorphicArguments : public PolymorphicConvention {
    IRGenFunction &IGF;
  public:
    EmitPolymorphicArguments(IRGenFunction &IGF,
                             PolymorphicFunctionType *polyFn)
      : PolymorphicConvention(polyFn), IGF(IGF) {}

    void emit(CanType substInputType, ArrayRef<Substitution> subs,
              Explosion &out);

  private:
    void emitSource(CanType substInputType, Explosion &out) {
      switch (getSourceKind()) {
      case SourceKind::None: return;
      case SourceKind::ClassPointer: return;
      case SourceKind::ClassMetadata: return;
      case SourceKind::GenericLValueMetadata: {
        CanType argTy = stripLabel(substInputType);
        CanType objTy = CanType(cast<LValueType>(argTy)->getObjectType());
        out.addUnmanaged(emitTypeMetadataRef(IGF, objTy));
        return;
      }
      }
      llvm_unreachable("bad source kind!");
    }
  };
}

/// Pass all the arguments necessary for the given function.
void irgen::emitPolymorphicArguments(IRGenFunction &IGF,
                                     PolymorphicFunctionType *polyFn,
                                     CanType substInputType,
                                     ArrayRef<Substitution> subs,
                                     Explosion &out) {
  EmitPolymorphicArguments(IGF, polyFn).emit(substInputType, subs, out);
}

void EmitPolymorphicArguments::emit(CanType substInputType,
                                    ArrayRef<Substitution> subs,
                                    Explosion &out) {
  auto &generics = FnType->getGenericParams();
  (void)generics;

  emitSource(substInputType, out);
  
  // For now, treat all archetypes independently.
  // FIXME: Later, we'll want to emit only the minimal set of archetypes,
  // because non-primary archetypes (which correspond to associated types)
  // will have their witness tables embedded in the witness table corresponding
  // to their parent.
  for (auto *archetype : generics.getAllArchetypes()) {
    // Find the substitution for the archetype.
    auto const *subp = std::find_if(subs.begin(), subs.end(),
                                    [&](Substitution const &sub) {
                                      return sub.Archetype == archetype;
                                    });
    assert(subp != subs.end() && "no substitution for generic param?");
    auto const &sub = *subp;
    
    CanType argType = sub.Replacement->getCanonicalType();

    // Add the metadata reference unelss it's fulfilled.
    if (!Fulfillments.count(FulfillmentKey(archetype, nullptr))) {
      out.addUnmanaged(emitTypeMetadataRef(IGF, argType));
    }

    // Nothing else to do if there aren't any protocols to witness.
    auto protocols = archetype->getConformsTo();
    if (protocols.empty())
      continue;

    auto &argTI = IGF.getFragileTypeInfo(argType);

    // Add witness tables for each of the required protocols.
    for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
      auto protocol = protocols[i];

      // Skip this if it's fulfilled by the source.
      if (Fulfillments.count(FulfillmentKey(archetype, protocol)))
        continue;

      // If the target is an archetype, go to the type info.
      if (isa<ArchetypeType>(argType)) {
        auto &archTI = argTI.as<ArchetypeTypeInfo>();

        ProtocolPath path(IGF.IGM, archTI.getProtocols(), protocol);
        auto wtable = archTI.getWitnessTable(IGF, path.getOriginIndex());
        wtable = path.apply(IGF, wtable);
        out.addUnmanaged(wtable);
        continue;
      }

      // Otherwise, go to the conformances.
      auto &protoI = IGF.IGM.getProtocolInfo(protocol);
      auto &confI = protoI.getConformance(IGF.IGM, argType, argTI, protocol,
                                          *sub.Conformance[i]);
      llvm::Value *wtable = confI.getTable(IGF);
      out.addUnmanaged(wtable);
    }
  }
}

namespace {
  /// A class for expanding a polymorphic signature.
  class ExpandPolymorphicSignature : public PolymorphicConvention {
    IRGenModule &IGM;
  public:
    ExpandPolymorphicSignature(IRGenModule &IGM, PolymorphicFunctionType *fn)
      : PolymorphicConvention(fn), IGM(IGM) {}

    void expand(SmallVectorImpl<llvm::Type*> &out) {
      addSource(out);

      auto &generics = FnType->getGenericParams();
      for (auto archetype : generics.getAllArchetypes()) {
        // Pass the type argument if not fulfilled.
        if (!Fulfillments.count(FulfillmentKey(archetype, nullptr)))
          out.push_back(IGM.TypeMetadataPtrTy);

        // Pass each signature requirement separately (unless fulfilled).
        for (auto protocol : archetype->getConformsTo())
          if (!Fulfillments.count(FulfillmentKey(archetype, protocol)))
            out.push_back(IGM.WitnessTablePtrTy);
      }
    }

  private:
    /// Add signature elements for the source metadata.
    void addSource(SmallVectorImpl<llvm::Type*> &out) {
      switch (getSourceKind()) {
      case SourceKind::None: return;
      case SourceKind::ClassPointer: return; // already accounted for
      case SourceKind::ClassMetadata: return; // already accounted for
      case SourceKind::GenericLValueMetadata:
        return out.push_back(IGM.TypeMetadataPtrTy);
      }
      llvm_unreachable("bad source kind");
    }
  };
}

/// Given a generic signature, add the argument types required in order to call it.
void irgen::expandPolymorphicSignature(IRGenModule &IGM,
                                       PolymorphicFunctionType *polyFn,
                                       SmallVectorImpl<llvm::Type*> &out) {
  ExpandPolymorphicSignature(IGM, polyFn).expand(out);
}

static void emitProtocolWitnessTables(IRGenFunction &IGF,
                                  Address dest,
                                  ExistentialTypeInfo const &destTI,
                                  CanType srcType,
                                  ArrayRef<ProtocolConformance*> conformances,
                                  llvm::Value* &metadata,
                                  FixedPacking &packing,
                                  llvm::Value* &wtable) {
  const TypeInfo &srcTI = IGF.getFragileTypeInfo(srcType);
  ExistentialLayout destLayout = destTI.getLayout();
  ArrayRef<ProtocolEntry> destEntries = destTI.getProtocols();

  // First, write out the metadata.
  metadata = emitTypeMetadataRef(IGF, srcType);
  IGF.Builder.CreateStore(metadata, destLayout.projectMetadataRef(IGF, dest));
  
  // Compute basic layout information about the type.  If we have a
  // concrete type, we need to know how it packs into a fixed-size
  // buffer.  If we don't, we need an value-witness table.
  if (isa<ArchetypeType>(srcType)) { // FIXME: tuples of archetypes?
    packing = (FixedPacking) -1;
    wtable = srcTI.as<ArchetypeTypeInfo>().getValueWitnessTable(IGF);
  } else {
    packing = computePacking(IGF.IGM, srcTI);
    wtable = nullptr;
  }
  
  // Next, write the protocol witness tables.
  for (unsigned i = 0, e = destTI.getProtocols().size(); i != e; ++i) {
    ProtocolDecl *proto = destEntries[i].getProtocol();
    auto &protoI = destEntries[i].getInfo();
    
    llvm::Value *ptable;
    
    // If the source type is an archetype, look at what's bound.
    if (isa<ArchetypeType>(srcType)) {
      auto &archTI = srcTI.as<ArchetypeTypeInfo>();
      ProtocolPath path(IGF.IGM, archTI.getProtocols(), proto);
      ptable = archTI.getWitnessTable(IGF, path.getOriginIndex());
      ptable = path.apply(IGF, ptable);
      
      // All other source types should be concrete enough that we have
      // conformance information for them.
    } else {
      auto astConformance = conformances[i];
      assert(astConformance);
      
      // Compute the conformance information.
      const ConformanceInfo &conformance =
      protoI.getConformance(IGF.IGM, srcType, srcTI, proto, *astConformance);
      ptable = conformance.getTable(IGF);
    }
    
    // Now store the protocol witness table into the destination.
    Address ptableSlot = destLayout.projectWitnessTable(IGF, dest, i);
    IGF.Builder.CreateStore(ptable, ptableSlot);
  }

}

/// Emit an existential container initialization by copying the value and
/// witness tables from an existential container of a more specific type.
void irgen::emitExistentialContainerUpcast(IRGenFunction &IGF,
                                 Address dest,
                                 CanType destType,
                                 Address src,
                                 CanType srcType,
                                 bool isTakeOfSrc,
                                 ArrayRef<ProtocolConformance*> conformances) {
  auto &destTI = IGF.getFragileTypeInfo(destType).as<ExistentialTypeInfo>();
  auto &srcTI = IGF.getFragileTypeInfo(srcType).as<ExistentialTypeInfo>();

  auto destLayout = destTI.getLayout();
  auto srcLayout = srcTI.getLayout();
  
  ArrayRef<ProtocolEntry> destEntries = destTI.getProtocols();
  assert(destEntries.size() == conformances.size());

  // Take the data out of the other buffer.
  // UpcastExistential never implies a transformation of the *value*,
  // just of the *witnesses*.
  Address destBuffer = destLayout.projectExistentialBuffer(IGF, dest);
  Address srcBuffer = srcLayout.projectExistentialBuffer(IGF, src);
  llvm::Value *srcMetadata = srcLayout.loadMetadataRef(IGF, src);
  if (isTakeOfSrc) {
    // If we can take the source, we can just memcpy the buffer.
    IGF.emitMemCpy(destBuffer, srcBuffer, getFixedBufferSize(IGF.IGM));
  } else {
    // Otherwise, we have to do a copy-initialization of the buffer.
    llvm::Value *srcWtable = srcLayout.loadValueWitnessTable(IGF, src,
                                                             srcMetadata);
    emitInitializeBufferWithCopyOfBufferCall(IGF,
                                             srcWtable, srcMetadata,
                                             destBuffer, srcBuffer);
  }
  
  // Copy the metadata as well.
  Address destMetadataRef = destLayout.projectMetadataRef(IGF, dest);
  IGF.Builder.CreateStore(srcMetadata, destMetadataRef);
  
  // Okay, the buffer on dest has been meaningfully filled in.
  // Fill in the witnesses.
  
  // If we're erasing *all* protocols, we're done.
  if (destEntries.empty())
    return;
  
  // Okay, so we're erasing to a non-trivial set of protocols.
  
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
}

/// Emit an existential container initialization operation for a concrete type.
/// Returns the address of the uninitialized buffer for the concrete value.
Address irgen::emitExistentialContainerInit(IRGenFunction &IGF,
                                  Address dest,
                                  CanType destType,
                                  CanType srcType,
                                  ArrayRef<ProtocolConformance*> conformances) {
  auto &destTI = IGF.getFragileTypeInfo(destType).as<ExistentialTypeInfo>();
  const TypeInfo &srcTI = IGF.getFragileTypeInfo(srcType);
  ExistentialLayout destLayout = destTI.getLayout();
  assert(destTI.getProtocols().size() == conformances.size());
  
  assert(!srcType->isExistentialType() &&
         "SIL existential-to-existential erasure not yet supported");
  
  // First, write out the metadata and witness tables.
  
  llvm::Value *metadata = nullptr;
  FixedPacking packing = (FixedPacking) -1;
  llvm::Value *wtable = nullptr;
  emitProtocolWitnessTables(IGF,
                            dest,
                            destTI,
                            srcType,
                            conformances,
                            metadata,
                            packing,
                            wtable);
  
  // Finally, evaluate into the buffer.
  
  // Project down to the destination fixed-size buffer.
  Address buffer = destLayout.projectExistentialBuffer(IGF, dest);

  // If the type is provably empty, we're done.
  if (srcTI.isEmpty(ResilienceScope::Local)) {
    assert(packing == FixedPacking::OffsetZero);
    return buffer;
  }
  
  // Otherwise, allocate if necessary.
  
  if (wtable) {
    // If we're using a witness-table to do this, we need to emit a
    // value-witness call to allocate the fixed-size buffer.
    return Address(emitAllocateBufferCall(IGF, wtable, metadata, buffer),
                   Alignment(1));
  } else {
    // Otherwise, allocate using what we know statically about the type.
    return emitAllocateBuffer(IGF, buffer, packing, srcTI);
  }
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
/// \param metadata - the metatype data for the concrete type of thisObject
///   (or the type itself for a static method)
/// \param thisObject - the object (if applicable) to call the method on
static CallEmission prepareProtocolMethodCall(IRGenFunction &IGF, FuncDecl *fn,
                                              CanType substResultType,
                                              ArrayRef<Substitution> subs,
                                              llvm::Value *witness,
                                              llvm::Value *metadata,
                                              Address thisObject) {
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  unsigned uncurryLevel = 1;

  CanType origFnType = fn->getType()->getCanonicalType();
  AbstractCC convention = fn->isStatic()
    ? AbstractCC::Freestanding
    : AbstractCC::Method;
  llvm::AttributeSet attrs;
  llvm::FunctionType *fnTy =
    IGF.IGM.getFunctionType(convention,
                            origFnType, explosionLevel, uncurryLevel,
                            ExtraData::Metatype, attrs);
  witness = IGF.Builder.CreateBitCast(witness, fnTy->getPointerTo());

  Callee callee =
    Callee::forKnownFunction(convention,
                             origFnType, substResultType, subs,
                             witness, ManagedValue(metadata),
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
    // FIXME: also the metatype
    emission.addArg(arg);

  // If this is a static method, add the metatype argument.
  } else {
    Explosion arg(ExplosionKind::Minimal);
    arg.addUnmanaged(metadata);
    emission.addArg(arg);
  }

  return emission;
}

static void getWitnessMethodValue(IRGenFunction &IGF,
                                  FuncDecl *fn,
                                  ProtocolDecl *fnProto,
                                  llvm::Value *wtable,
                                  llvm::Value *metadata,
                                  Explosion &out) {
  // Find the actual witness.
  auto &fnProtoInfo = IGF.IGM.getProtocolInfo(fnProto);
  auto index = fnProtoInfo.getWitnessEntry(fn).getFunctionIndex();
  llvm::Value *witness = loadOpaqueWitness(IGF, wtable, index);

  // Cast the witness pointer to i8*.
  witness = IGF.Builder.CreateBitCast(witness, IGF.IGM.Int8PtrTy);
  
  // Build the value.
  out.addUnmanaged(witness);
  out.addUnmanaged(metadata);
}

void
irgen::getArchetypeMethodValue(IRGenFunction &IGF,
                               CanType baseTy,
                               SILConstant member,
                               CanType substResultType,
                               ArrayRef<Substitution> subs,
                               Explosion &out) {
  // The function we're going to call.
  // FIXME: Support getters and setters (and curried entry points?)
  assert(member.kind == SILConstant::Kind::Func
         && "getters and setters not yet supported");
  ValueDecl *vd = member.getDecl();
  FuncDecl *fn = cast<FuncDecl>(vd);
  
  // Find the archetype we're calling on.
  // FIXME: static methods
  ArchetypeType *archetype = cast<ArchetypeType>(baseTy);
  
  // The protocol we're calling on.
  ProtocolDecl *fnProto = cast<ProtocolDecl>(fn->getDeclContext());
  
  // Find the witness table.
  auto &archetypeTI = IGF.getFragileTypeInfo(archetype).as<ArchetypeTypeInfo>();
  ProtocolPath path(IGF.IGM, archetypeTI.getProtocols(), fnProto);
  llvm::Value *origin = archetypeTI.getWitnessTable(IGF, path.getOriginIndex());
  llvm::Value *wtable = path.apply(IGF, origin);
  
  // Acquire the archetype metadata.
  llvm::Value *metadata = archetypeTI.getMetadataRef(IGF);
  
  // Build the value.
  getWitnessMethodValue(IGF, fn, fnProto, wtable, metadata, out);
}

/// Extract the method pointer and metadata from a protocol witness table
/// as a function value.
void
irgen::getProtocolMethodValue(IRGenFunction &IGF,
                              Address existAddr,
                              CanType baseTy,
                              SILConstant member,
                              CanType substResultType,
                              ArrayRef<Substitution> subs,
                              Explosion &out) {
  // The protocol we're calling on.
  // TODO: support protocol compositions here.
  assert(baseTy->isExistentialType());
  auto &baseTI = IGF.getFragileTypeInfo(baseTy).as<ExistentialTypeInfo>();
  
  // The function we're going to call.
  // FIXME: Support getters and setters (and curried entry points?)
  assert(member.kind == SILConstant::Kind::Func
         && "getters and setters not yet supported");
  ValueDecl *vd = member.getDecl();
  FuncDecl *fn = cast<FuncDecl>(vd);
  ProtocolDecl *fnProto = cast<ProtocolDecl>(fn->getDeclContext());

  // Load the witness table.
  llvm::Value *wtable = baseTI.findWitnessTable(IGF, existAddr, fnProto);
  
  // Load the metadata.
  auto existLayout = baseTI.getLayout();  
  llvm::Value *metadata = existLayout.loadMetadataRef(IGF, existAddr);

  // Build the value.
  getWitnessMethodValue(IGF, fn, fnProto, wtable, metadata, out);
}

/// Emit an existential member reference as a callee.
CallEmission
irgen::prepareExistentialMemberRefCall(IRGenFunction &IGF,
                                       ExistentialMemberRefExpr *E,
                                       CanType substResultType,
                                       ArrayRef<Substitution> subs,
                                       ExplosionKind maxExplosionLevel,
                                       unsigned maxUncurry) {
  abort();
  
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
  LValue lvalue;
  Address existAddr;

  // Load the witness table.
  llvm::Value *wtable = baseTI.findWitnessTable(IGF, existAddr, fnProto);

  // The thing we're going to invoke a method on.
  Address thisObject;
  auto existLayout = baseTI.getLayout();

  // Load the metadata.
  llvm::Value *metadata = existLayout.loadMetadataRef(IGF, existAddr);

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
    Address srcBuffer = existLayout.projectExistentialBuffer(IGF, existAddr);

    // The result of the copy is an i8* which points at the new object.
    llvm::Value *copy =
      emitInitializeBufferWithCopyOfBufferCall(IGF, wtable, metadata,
                                               copyBuffer, srcBuffer);

    // Enter a cleanup for the temporary.  
    IGF.pushFullExprCleanup<DestroyBuffer>(copyBuffer, wtable, metadata);

    thisObject = Address(copy, Alignment(1));
  }

  // Find the actual witness.
  auto &fnProtoInfo = IGF.IGM.getProtocolInfo(fnProto);
  auto index = fnProtoInfo.getWitnessEntry(fn).getFunctionIndex();
  llvm::Value *witness = loadOpaqueWitness(IGF, wtable, index);

  // Emit the callee.
  return prepareProtocolMethodCall(IGF, fn, substResultType, subs,
                                   witness, metadata, thisObject);
}

/// Emit an existential member reference as a callee.
CallEmission
irgen::prepareArchetypeMemberRefCall(IRGenFunction &IGF,
                                     ArchetypeMemberRefExpr *E,
                                     CanType substResultType,
                                     ArrayRef<Substitution> subs,
                                     ExplosionKind maxExplosionLevel,
                                     unsigned maxUncurry) {
  abort();
  
  // The function we're going to call.
  FuncDecl *fn = cast<FuncDecl>(E->getDecl());

  // The base of an archetype member reference is always an l-value.
  // If we're accessing a static function, though, the l-value is
  // unnecessary.
  Address thisObject;

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

  // Acquire the archetype metadata.
  llvm::Value *metadata = archetypeTI.getMetadataRef(IGF);

  // Emit the callee.
  return prepareProtocolMethodCall(IGF, fn, substResultType, subs,
                                   witness, metadata, thisObject);
}


/// Determine the natural limits on how we can call the given protocol
/// member function.
AbstractCallee irgen::getAbstractProtocolCallee(IRGenFunction &IGF,
                                                FuncDecl *fn) {
  // TODO: consider adding non-minimal or curried entrypoints.
  if (fn->isStatic())
    return AbstractCallee(AbstractCC::Freestanding, ExplosionKind::Minimal,
                          0, 0, ExtraData::Metatype);
  return AbstractCallee(AbstractCC::Method, ExplosionKind::Minimal,
                        1, 1, ExtraData::Metatype);
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

/// Emit a projection from an existential container to its concrete value
/// buffer.
Address irgen::emitExistentialProjection(IRGenFunction &IGF,
                                         Address base,
                                         CanType baseTy) {
  assert(baseTy->isExistentialType());
  auto &baseTI = IGF.getFragileTypeInfo(baseTy).as<ExistentialTypeInfo>();
  auto layout = baseTI.getLayout();

  llvm::Value *metadata = layout.loadMetadataRef(IGF, base);
  llvm::Value *wtable = layout.loadValueWitnessTable(IGF, base, metadata);
  Address buffer = layout.projectExistentialBuffer(IGF, base);
  llvm::Value *object = emitProjectBufferCall(IGF, wtable, metadata, buffer);
  return Address(object, Alignment(1));
}

//===--- IRBuilder.h - Swift IR Builder -------------------------*- C++ -*-===//
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
// This file defines Swift's specialization of llvm::IRBuilder.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRBUILDER_H
#define SWIFT_IRGEN_IRBUILDER_H

#include "llvm/ADT/PointerUnion.h"
#include "llvm/IR/IRBuilder.h"
#include "swift/Basic/LLVM.h"
#include "Address.h"
#include "IRGen.h"

namespace swift {
namespace irgen {

typedef llvm::IRBuilder<> IRBuilderBase;

class IRBuilder : public IRBuilderBase {
  // Without this, it keeps resolving to llvm::IRBuilderBase because
  // of the injected class name.
  typedef irgen::IRBuilderBase IRBuilderBase;

  /// The block containing the insertion point when the insertion
  /// point was last cleared.  Used only for preserving block
  /// ordering.
  llvm::BasicBlock *ClearedIP;

public:
  IRBuilder(llvm::LLVMContext &Context)
    : IRBuilderBase(Context), ClearedIP(nullptr) {}

  /// Determines if the current location is apparently reachable.  The
  /// invariant we maintain is that the insertion point of the builder
  /// always points within a block unless the current location is
  /// logically unreachable.  All the low-level routines which emit
  /// branches leave the insertion point in the original block, just
  /// after the branch.  High-level routines may then indicate
  /// unreachability by clearing the insertion point.
  bool hasValidIP() const { return GetInsertBlock() != nullptr; }

  /// Determines whether we're currently inserting after a terminator.
  /// This is really just there for asserts.
  bool hasPostTerminatorIP() const {
    return GetInsertBlock() != nullptr &&
           !GetInsertBlock()->empty() &&
           isa<llvm::TerminatorInst>(GetInsertBlock()->back());
  }

  void ClearInsertionPoint() {
    assert(hasValidIP() && "clearing invalid insertion point!");
    assert(ClearedIP == nullptr);

    /// Whenever we clear the insertion point, remember where we were.
    ClearedIP = GetInsertBlock();
    IRBuilderBase::ClearInsertionPoint();
  }

  void SetInsertPoint(llvm::BasicBlock *BB) {
    ClearedIP = nullptr;
    IRBuilderBase::SetInsertPoint(BB);
  }
  
  void SetInsertPoint(llvm::BasicBlock *BB, llvm::BasicBlock::iterator before) {
    ClearedIP = nullptr;
    IRBuilderBase::SetInsertPoint(BB, before);
  }

  /// A stable insertion point in the function.  "Stable" means that
  /// it will point to the same location in the function, even if
  /// instructions are subsequently added to the the current basic
  /// block.
  class StableIP {
    /// Either an instruction that we're inserting after or the basic
    /// block that we're inserting at the beginning of.
    typedef llvm::PointerUnion<llvm::Instruction*, llvm::BasicBlock*> UnionTy;
    UnionTy After;
  public:
    StableIP() = default;
    explicit StableIP(const IRBuilder &Builder) {
      if (!Builder.hasValidIP()) {
        After = UnionTy();
        assert(!isValid());
        return;
      }

      llvm::BasicBlock *curBlock = Builder.GetInsertBlock();
      assert(Builder.GetInsertPoint() == curBlock->end());
      if (curBlock->empty())
        After = curBlock;
      else
        After = &curBlock->back();
    }

    /// Does this stable IP point to a valid location?
    bool isValid() const {
      return !After.isNull();
    }

    /// Insert an unparented instruction at this insertion point.
    /// Note that inserting multiple instructions at an IP will cause
    /// them to end up in reverse order.
    void insert(llvm::Instruction *I) {
      assert(isValid() && "inserting at invalid location!");
      assert(I->getParent() == nullptr);
      if (llvm::BasicBlock *block = After.dyn_cast<llvm::BasicBlock*>()) {
        block->getInstList().push_front(I);
      } else {
        llvm::Instruction *afterInsn = After.get<llvm::Instruction*>();
        afterInsn->getParent()->getInstList().insertAfter(afterInsn, I);
      }
    }

    // Support for being placed in pointer unions.
    void *getOpaqueValue() const { return After.getOpaqueValue(); }
    static StableIP getFromOpaqueValue(void *p) {
      StableIP result;
      result.After = UnionTy::getFromOpaqueValue(p);
      return result;
    }
    enum { NumLowBitsAvailable
             = llvm::PointerLikeTypeTraits<UnionTy>::NumLowBitsAvailable };
  };

  /// Capture a stable reference to the current IP.
  StableIP getStableIP() const {
    return StableIP(*this);
  }

  llvm::LoadInst *CreateLoad(llvm::Value *addr, Alignment align,
                             const llvm::Twine &name = "") {
    llvm::LoadInst *load = IRBuilderBase::CreateLoad(addr, name);
    load->setAlignment(align.getValue());
    return load;
  }
  llvm::LoadInst *CreateLoad(Address addr, const llvm::Twine &name = "") {
    return CreateLoad(addr.getAddress(), addr.getAlignment(), name);
  }

  llvm::StoreInst *CreateStore(llvm::Value *value, llvm::Value *addr,
                               Alignment align) {
    llvm::StoreInst *store = IRBuilderBase::CreateStore(value, addr);
    store->setAlignment(align.getValue());
    return store;
  }
  llvm::StoreInst *CreateStore(llvm::Value *value, Address addr) {
    return CreateStore(value, addr.getAddress(), addr.getAlignment());
  }

  // These are deleted because we want to force the caller to specify
  // an alignment.
  llvm::LoadInst *CreateLoad(llvm::Value *addr,
                             const llvm::Twine &name = "") = delete;
  llvm::StoreInst *CreateStore(llvm::Value *value, llvm::Value *addr) = delete;
  
  using IRBuilderBase::CreateStructGEP;
  Address CreateStructGEP(Address address, unsigned index, Size size,
                          const llvm::Twine &name = "") {
    llvm::Value *addr = CreateStructGEP(address.getAddress(), index, name);
    return Address(addr, address.getAlignment().alignmentAtOffset(size));
  }

  /// Given a pointer to an array element, GEP to the array element
  /// N elements past it.  The type is not changed.
  Address CreateConstArrayGEP(Address base, unsigned index, Size eltSize,
                              const llvm::Twine &name = "") {
    auto addr = CreateConstInBoundsGEP1_32(base.getAddress(), index, name);
    return Address(addr,
                   base.getAlignment().alignmentAtOffset(eltSize * index));
  }

  /// Given an i8*, GEP to N bytes past it.
  Address CreateConstByteArrayGEP(Address base, Size offset,
                                  const llvm::Twine &name = "") {
    auto addr = CreateConstInBoundsGEP1_32(base.getAddress(),
                                           offset.getValue(),
                                           name);
    return Address(addr, base.getAlignment().alignmentAtOffset(offset));
  }

  using IRBuilderBase::CreateBitCast;
  Address CreateBitCast(Address address, llvm::Type *type,
                        const llvm::Twine &name = "") {
    llvm::Value *addr = CreateBitCast(address.getAddress(), type, name);
    return Address(addr, address.getAlignment());
  }

  /// Insert the given basic block after the IP block and move the
  /// insertion point to it.  Only valid if the IP is valid.
  void emitBlock(llvm::BasicBlock *BB);

  using IRBuilderBase::CreateMemCpy;
  llvm::CallInst *CreateMemCpy(Address dest, Address src, Size size) {
    return CreateMemCpy(dest.getAddress(), src.getAddress(),
                        size.getValue(),
                        std::min(dest.getAlignment(),
                                 src.getAlignment()).getValue());
  }
};

} // end namespace irgen
} // end namespace swift

namespace llvm {
  template <> class PointerLikeTypeTraits<swift::irgen::IRBuilder::StableIP> {
    typedef swift::irgen::IRBuilder::StableIP type;
  public:
    static void *getAsVoidPointer(type IP) {
      return IP.getOpaqueValue();
    }
    static type getFromVoidPointer(void *p) {
      return type::getFromOpaqueValue(p);
    }

    // The number of bits available are the min of the two pointer types.
    enum {
      NumLowBitsAvailable = type::NumLowBitsAvailable
    };
  };
}

#endif

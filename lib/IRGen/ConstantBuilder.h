//===--- ConstantBuilder.h - IR generation for constant structs -*- C++ -*-===//
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
//  This file implements IR generation of constant packed LLVM structs.
//===----------------------------------------------------------------------===//

#include "swift/AST/Mangle.h"
#include "swift/ABI/MetadataValues.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"

#include "Address.h"
#include "IRGenModule.h"
#include "IRGenFunction.h"

namespace swift {
namespace irgen {

class ConstantBuilderBase {
protected:
  IRGenModule &IGM;
  ConstantBuilderBase(IRGenModule &IGM) : IGM(IGM) {}
};

template <class Base = ConstantBuilderBase>
class ConstantBuilder : public Base {
protected:
  template <class... T>
  ConstantBuilder(T &&...args) : Base(std::forward<T>(args)...) {}

  IRGenModule &IGM = Base::IGM;

private:
  llvm::GlobalVariable *relativeAddressBase = nullptr;
  llvm::SmallVector<llvm::Constant*, 16> Fields;
  Size NextOffset = Size(0);

protected:
  Size getNextOffset() const { return NextOffset; }

  void addStruct(llvm::Constant *st) {
    assert(st->getType()->isStructTy());
    Fields.push_back(st);
    NextOffset += Size(IGM.DataLayout.getTypeStoreSize(st->getType()));
  }

  /// Add a constant word-sized value.
  void addConstantWord(int64_t value) {
    addWord(llvm::ConstantInt::get(IGM.SizeTy, value));
  }

  /// Add a word-sized value.
  void addWord(llvm::Constant *value) {
    assert(value->getType() == IGM.IntPtrTy ||
           value->getType()->isPointerTy());
    Fields.push_back(value);
    NextOffset += IGM.getPointerSize();
  }

  void setRelativeAddressBase(llvm::GlobalVariable *base) {
    relativeAddressBase = base;
  }

  llvm::Constant *getRelativeAddressFromNextField(ConstantReference referent,
                                            llvm::IntegerType *addressTy) {
    assert(relativeAddressBase && "no relative address base set");
    
    // Determine the address of the next field in the initializer.
    llvm::Constant *fieldAddr =
      llvm::ConstantExpr::getPtrToInt(relativeAddressBase, IGM.IntPtrTy);
    fieldAddr = llvm::ConstantExpr::getAdd(fieldAddr,
                          llvm::ConstantInt::get(IGM.SizeTy,
                                                 getNextOffset().getValue()));
    llvm::Constant *referentValue =
      llvm::ConstantExpr::getPtrToInt(referent.getValue(), IGM.IntPtrTy);

    llvm::Constant *relative
      = llvm::ConstantExpr::getSub(referentValue, fieldAddr);

    if (relative->getType() != addressTy)
      relative = llvm::ConstantExpr::getTrunc(relative, addressTy);

    if (referent.isIndirect()) {
      relative = llvm::ConstantExpr::getAdd(relative,
                                       llvm::ConstantInt::get(addressTy, 1));
    }
    
    return relative;
  }

  /// Add a 32-bit relative address from the current location in the local
  /// being built to another global variable.
  void addRelativeAddress(llvm::Constant *referent) {
    addRelativeAddress({referent, ConstantReference::Direct});
  }
  void addRelativeAddress(ConstantReference referent) {
    addInt32(getRelativeAddressFromNextField(referent, IGM.RelativeAddressTy));
  }

  /// Add a pointer-sized relative address from the current location in the
  /// local being built to another global variable.
  void addFarRelativeAddress(llvm::Constant *referent) {
    addFarRelativeAddress({referent, ConstantReference::Direct});
  }
  void addFarRelativeAddress(ConstantReference referent) {
    addWord(getRelativeAddressFromNextField(referent,
                                            IGM.FarRelativeAddressTy));
  }

  /// Add a 32-bit relative address from the current location in the local
  /// being built to another global variable, or null if a null referent
  /// is passed.
  void addRelativeAddressOrNull(llvm::Constant *referent) {
    addRelativeAddressOrNull({referent, ConstantReference::Direct});
  }
  void addRelativeAddressOrNull(ConstantReference referent) {
    if (referent)
      addRelativeAddress(referent);
    else
      addConstantInt32(0);
  }

  /// Add a pointer-sized relative address from the current location in the
  /// local being built to another global variable, or null if a null referent
  /// is passed.
  void addFarRelativeAddressOrNull(llvm::Constant *referent) {
    addFarRelativeAddressOrNull({referent, ConstantReference::Direct});
  }
  void addFarRelativeAddressOrNull(ConstantReference referent) {
    if (referent)
      addFarRelativeAddress(referent);
    else
      addConstantWord(0);
  }

  /// Add a 32-bit relative address from the current location in the local
  /// being built to another global variable. Pack a constant integer into
  /// the alignment bits of the pointer.
  void addRelativeAddressWithTag(llvm::Constant *referent,
                                 unsigned tag) {
    assert(tag < 4 && "tag too big to pack in relative address");
    llvm::Constant *relativeAddr =
      getRelativeAddressFromNextField({referent, ConstantReference::Direct},
                                      IGM.RelativeAddressTy);
    relativeAddr = llvm::ConstantExpr::getAdd(relativeAddr,
                          llvm::ConstantInt::get(IGM.RelativeAddressTy, tag));
    addInt32(relativeAddr);
  }

  /// Add a pointer-size relative address from the current location in the
  /// local being built to another global variable. Pack a constant integer
  /// into the alignment bits of the pointer.
  void addFarRelativeAddressWithTag(llvm::Constant *referent,
                                    unsigned tag) {
    // FIXME: could be 8 when targeting 64-bit platforms
    assert(tag < 4 && "tag too big to pack in relative address");
    llvm::Constant *relativeAddr =
      getRelativeAddressFromNextField(referent, IGM.FarRelativeAddressTy);
    relativeAddr = llvm::ConstantExpr::getAdd(relativeAddr,
                       llvm::ConstantInt::get(IGM.FarRelativeAddressTy, tag));
    addWord(relativeAddr);
  }

  /// Add a uint32_t value that represents the given offset
  /// scaled to a number of words.
  void addConstantInt32InWords(Size value) {
    addConstantInt32(IGM.getOffsetInWords(value));
  }

  /// Add a constant 32-bit value.
  void addConstantInt32(int32_t value) {
    addInt32(llvm::ConstantInt::get(IGM.Int32Ty, value));
  }

  /// Add a 32-bit value.
  void addInt32(llvm::Constant *value) {
    assert(value->getType() == IGM.Int32Ty);
    Fields.push_back(value);
    NextOffset += Size(4);
  }

  /// Add a constant 16-bit value.
  void addConstantInt16(int16_t value) {
    addInt16(llvm::ConstantInt::get(IGM.Int16Ty, value));
  }

  /// Add a 16-bit value.
  void addInt16(llvm::Constant *value) {
    assert(value->getType() == IGM.Int16Ty);
    Fields.push_back(value);
    NextOffset += Size(2);
  }

  /// Add a constant 8-bit value.
  void addConstantInt8(int8_t value) {
    addInt8(llvm::ConstantInt::get(IGM.Int8Ty, value));
  }

  /// Add an 8-bit value.
  void addInt8(llvm::Constant *value) {
    assert(value->getType() == IGM.Int8Ty);
    Fields.push_back(value);
    NextOffset += Size(1);
  }

  /// Add a constant of the given size.
  void addStruct(llvm::Constant *value, Size size) {
    assert(size.getValue()
             == IGM.DataLayout.getTypeStoreSize(value->getType()));
    Fields.push_back(value);
    NextOffset += size;
  }
  
  class ReservationToken {
    size_t Index;
    ReservationToken(size_t index) : Index(index) {}
    friend ConstantBuilder<Base>;
  };
  ReservationToken reserveFields(unsigned numFields, Size size) {
    unsigned index = Fields.size();
    Fields.append(numFields, nullptr);
    NextOffset += size;
    return ReservationToken(index);
  }
  MutableArrayRef<llvm::Constant*> claimReservation(ReservationToken token,
                                                    unsigned numFields) {
    return MutableArrayRef<llvm::Constant*>(&Fields[0] + token.Index,
                                            numFields);
  }

public:
  llvm::Constant *getInit() const {
    if (Fields.empty())
      return nullptr;
    return llvm::ConstantStruct::getAnon(Fields, /*packed*/ true);
  }

  /// An optimization of getInit for when we have a known type we
  /// can use when there aren't any extra fields.
  llvm::Constant *getInitWithSuggestedType(unsigned numFields,
                                           llvm::StructType *type) {
    if (Fields.size() == numFields) {
      return llvm::ConstantStruct::get(type, Fields);
    } else {
      return getInit();
    }
  }
};

} // end namespace irgen
} // end namespace swift

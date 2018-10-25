//===--- GenConstant.cpp - Swift IR Generation For Constants --------------===//
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
//  This file implements IR generation for constant values.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Constants.h"

#include "GenConstant.h"
#include "GenStruct.h"
#include "GenTuple.h"
#include "TypeInfo.h"
#include "StructLayout.h"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILModule.h"

using namespace swift;
using namespace irgen;

llvm::Constant *irgen::emitConstantInt(IRGenModule &IGM,
                                       IntegerLiteralInst *ILI) {
  APInt value = ILI->getValue();
  BuiltinIntegerWidth width
    = ILI->getType().castTo<BuiltinIntegerType>()->getWidth();

  // The value may need truncation if its type had an abstract size.
  if (!width.isFixedWidth()) {
    assert(width.isPointerWidth() && "impossible width value");

    unsigned pointerWidth = IGM.getPointerSize().getValueInBits();
    assert(pointerWidth <= value.getBitWidth()
           && "lost precision at AST/SIL level?!");
    if (pointerWidth < value.getBitWidth())
      value = value.trunc(pointerWidth);
  }

  return llvm::ConstantInt::get(IGM.LLVMContext, value);
}

llvm::Constant *irgen::emitConstantFP(IRGenModule &IGM, FloatLiteralInst *FLI) {
  return llvm::ConstantFP::get(IGM.LLVMContext, FLI->getValue());
}

llvm::Constant *irgen::emitAddrOfConstantString(IRGenModule &IGM,
                                                StringLiteralInst *SLI) {
  switch (SLI->getEncoding()) {
  case StringLiteralInst::Encoding::Bytes:
  case StringLiteralInst::Encoding::UTF8:
    return IGM.getAddrOfGlobalString(SLI->getValue());

  case StringLiteralInst::Encoding::UTF16: {
    // This is always a GEP of a GlobalVariable with a nul terminator.
    auto addr = IGM.getAddrOfGlobalUTF16String(SLI->getValue());

    // Cast to Builtin.RawPointer.
    return llvm::ConstantExpr::getBitCast(addr, IGM.Int8PtrTy);
  }

  case StringLiteralInst::Encoding::ObjCSelector:
    llvm_unreachable("cannot get the address of an Objective-C selector");
  }
  llvm_unreachable("bad string encoding");
}

static llvm::Constant *emitConstantValue(IRGenModule &IGM, SILValue operand) {
  if (auto *SI = dyn_cast<StructInst>(operand)) {
    return emitConstantStruct(IGM, SI);
  } else if (auto *TI = dyn_cast<TupleInst>(operand)) {
    return emitConstantTuple(IGM, TI);
  } else if (auto *ILI = dyn_cast<IntegerLiteralInst>(operand)) {
    return emitConstantInt(IGM, ILI);
  } else if (auto *FLI = dyn_cast<FloatLiteralInst>(operand)) {
    return emitConstantFP(IGM, FLI);
  } else if (auto *SLI = dyn_cast<StringLiteralInst>(operand)) {
    return emitAddrOfConstantString(IGM, SLI);
  } else if (auto *BI = dyn_cast<BuiltinInst>(operand)) {
    switch (IGM.getSILModule().getBuiltinInfo(BI->getName()).ID) {
      case BuiltinValueKind::PtrToInt: {
        llvm::Constant *ptr = emitConstantValue(IGM, BI->getArguments()[0]);
        return llvm::ConstantExpr::getPtrToInt(ptr, IGM.IntPtrTy);
      }
      case BuiltinValueKind::ZExtOrBitCast: {
        llvm::Constant *value = emitConstantValue(IGM, BI->getArguments()[0]);
        return llvm::ConstantExpr::getZExtOrBitCast(value, IGM.getStorageType(BI->getType()));
      }
      case BuiltinValueKind::StringObjectOr: {
        llvm::Constant *lhs = emitConstantValue(IGM, BI->getArguments()[0]);
        llvm::Constant *rhs = emitConstantValue(IGM, BI->getArguments()[1]);
        // It is a requirement that the or'd bits in the left argument are
        // initialized with 0. Therefore the or-operation is equivalent to an
        // addition. We need an addition to generate a valid relocation.
        return llvm::ConstantExpr::getAdd(lhs, rhs);
      }
      default:
        llvm_unreachable("unsupported builtin for constant expression");
    }
  } else if (auto *VTBI = dyn_cast<ValueToBridgeObjectInst>(operand)) {
    auto *val = emitConstantValue(IGM, VTBI->getOperand());
    auto *sTy = IGM.getTypeInfo(VTBI->getType()).getStorageType();
    return llvm::ConstantExpr::getIntToPtr(val, sTy);
  } else {
    llvm_unreachable("Unsupported SILInstruction in static initializer!");
  }
}

namespace {

/// Fill in the missing values for padding.
void insertPadding(SmallVectorImpl<llvm::Constant *> &Elements,
                   llvm::StructType *sTy) {
  // fill in any gaps, which are the explicit padding that swiftc inserts.
  for (unsigned i = 0, e = Elements.size(); i != e; i++) {
    auto &elt = Elements[i];
    if (elt == nullptr) {
      auto *eltTy = sTy->getElementType(i);
      assert(eltTy->isArrayTy() &&
             eltTy->getArrayElementType()->isIntegerTy(8) &&
             "Unexpected non-byte-array type for constant struct padding");
      elt = llvm::UndefValue::get(eltTy);
    }
  }
}

template <typename InstTy, typename NextIndexFunc>
llvm::Constant *emitConstantStructOrTuple(IRGenModule &IGM, InstTy inst,
                                          NextIndexFunc nextIndex) {
  auto type = inst->getType();
  auto *sTy = cast<llvm::StructType>(IGM.getTypeInfo(type).getStorageType());

  SmallVector<llvm::Constant *, 32> elts(sTy->getNumElements(), nullptr);

  // run over the Swift initializers, putting them into the struct as
  // appropriate.
  for (unsigned i = 0, e = inst->getElements().size(); i != e; i++) {
    auto operand = inst->getOperand(i);
    Optional<unsigned> index = nextIndex(IGM, type, i);
    if (index.hasValue()) {
      assert(elts[index.getValue()] == nullptr &&
             "Unexpected constant struct field overlap");

      elts[index.getValue()] = emitConstantValue(IGM, operand);
    }
  }
  insertPadding(elts, sTy);
  return llvm::ConstantStruct::get(sTy, elts);
}
} // end anonymous namespace

llvm::Constant *irgen::emitConstantStruct(IRGenModule &IGM, StructInst *SI) {
  // The only way to get a struct's stored properties (which we need to map to
  // their physical/LLVM index) is to iterate over the properties
  // progressively. Fortunately the iteration order matches the order of
  // operands in a StructInst.
  auto StoredProperties = SI->getStructDecl()->getStoredProperties();
  auto Iter = StoredProperties.begin();

  return emitConstantStructOrTuple(
      IGM, SI, [&Iter](IRGenModule &IGM, SILType Type, unsigned _i) mutable {
        (void)_i;
        auto *FD = *Iter++;
        return irgen::getPhysicalStructFieldIndex(IGM, Type, FD);
      });
}

llvm::Constant *irgen::emitConstantTuple(IRGenModule &IGM, TupleInst *TI) {
  return emitConstantStructOrTuple(IGM, TI,
                                   irgen::getPhysicalTupleElementStructIndex);
}

llvm::Constant *irgen::emitConstantObject(IRGenModule &IGM, ObjectInst *OI,
                                         StructLayout *ClassLayout) {
  auto *sTy = cast<llvm::StructType>(ClassLayout->getType());
  SmallVector<llvm::Constant *, 32> elts(sTy->getNumElements(), nullptr);

  unsigned NumElems = OI->getAllElements().size();
  assert(NumElems == ClassLayout->getElements().size());

  // Construct the object init value including tail allocated elements.
  for (unsigned i = 0; i != NumElems; i++) {
    SILValue Val = OI->getAllElements()[i];
    const ElementLayout &EL = ClassLayout->getElements()[i];
    if (!EL.isEmpty()) {
      unsigned EltIdx = EL.getStructIndex();
      assert(EltIdx != 0 && "the first element is the object header");
      elts[EltIdx] = emitConstantValue(IGM, Val);
    }
  }
  // Construct the object header.
  llvm::Type *ObjectHeaderTy = sTy->getElementType(0);
  assert(ObjectHeaderTy->isStructTy());
  elts[0] = llvm::Constant::getNullValue(ObjectHeaderTy);
  insertPadding(elts, sTy);
  return llvm::ConstantStruct::get(sTy, elts);
}

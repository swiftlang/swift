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
#include "swift/Basic/Range.h"

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
  if (auto *SI = dyn_cast<StructInst>(operand))
    return emitConstantStruct(IGM, SI);
  else if (auto *TI = dyn_cast<TupleInst>(operand))
    return emitConstantTuple(IGM, TI);
  else if (auto *ILI = dyn_cast<IntegerLiteralInst>(operand))
    return emitConstantInt(IGM, ILI);
  else if (auto *FLI = dyn_cast<FloatLiteralInst>(operand))
    return emitConstantFP(IGM, FLI);
  else if (auto *SLI = dyn_cast<StringLiteralInst>(operand))
    return emitAddrOfConstantString(IGM, SLI);
  else
    llvm_unreachable("Unsupported SILInstruction in static initializer!");
}

namespace {
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
    unsigned index = nextIndex(IGM, type, i);

    assert(elts[index] == nullptr &&
           "Unexpected constant struct field overlap");

    elts[index] = emitConstantValue(IGM, operand);
  }

  // fill in any gaps, which are the explicit padding that swiftc inserts.
  for (unsigned i = 0, e = elts.size(); i != e; i++) {
    auto &elt = elts[i];
    if (elt == nullptr) {
      auto *eltTy = sTy->getElementType(i);
      assert(eltTy->isArrayTy() &&
             eltTy->getArrayElementType()->isIntegerTy(8) &&
             "Unexpected non-byte-array type for constant struct padding");
      elt = llvm::UndefValue::get(eltTy);
    }
  }

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
  return emitConstantStructOrTuple(IGM, TI, irgen::getTupleElementStructIndex);
}

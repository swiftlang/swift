//===--- StructLayout.cpp - Layout of structures -------------------------===//
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
//  This file implements algorithms for laying out structures.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetData.h"
#include "llvm/DerivedTypes.h"

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "GenType.h"
#include "StructLayout.h"

using namespace swift;
using namespace irgen;

/// Given a layout strategy, find the resilience scope at which we
/// must operate.
static ResilienceScope getResilienceScopeForStrategy(LayoutStrategy strategy) {
  switch (strategy) {
  case LayoutStrategy::Optimal: return ResilienceScope::Local;
  }
  llvm_unreachable("bad layout strategy!");
}

/// Does this layout kind require a heap header?
static bool requiresHeapHeader(LayoutKind kind) {
  switch (kind) {
  case LayoutKind::NonHeapObject: return false;
  case LayoutKind::HeapObject: return true;
  }
  llvm_unreachable("bad layout kind!");
}

void swift::irgen::addHeapHeaderToLayout(IRGenModule &IGM,
                                         Size &size, Alignment &align,
                                         SmallVectorImpl<llvm::Type*> &fields) {
  assert(size.isZero() && align.isOne() && fields.empty());
  size += IGM.getPointerSize() * 2;
  align = IGM.getPointerAlignment();
  fields.push_back(IGM.RefCountedStructTy);
}

/// Perform structure layout on the given types.
StructLayout::StructLayout(IRGenModule &IGM, LayoutKind layoutKind,
                           LayoutStrategy strategy,
                           llvm::ArrayRef<const TypeInfo *> types) {
  // For now, we actually only have one algorithm, and it's not
  // exactly optimal.

  Size storageSize(0);
  Alignment storageAlign(1);
  llvm::SmallVector<llvm::Type*, 8> storageTypes;

  // Add the heap header if necessary.
  if (requiresHeapHeader(layoutKind)) {
    storageSize += IGM.getPointerSize() * 2;
    storageAlign = IGM.getPointerAlignment();
    storageTypes.push_back(IGM.RefCountedStructTy);
  }
  
  ResilienceScope resilience = getResilienceScopeForStrategy(strategy);

  bool isEmpty = true;
  for (const TypeInfo *type : types) {
    // Skip types known to be empty.
    if (type->isEmpty(resilience)) {
      ElementLayout element = { Size(0), (unsigned) -1, type };
      Elements.push_back(element);
      continue;
    }

    // The struct is no longer empty.
    isEmpty = false;

    // The struct alignment is the max of the alignment of the fields.
    storageAlign = std::max(storageAlign, type->StorageAlignment);

    // If the current tuple size isn't a multiple of the field's
    // required alignment, we need padding.
    if (Size offsetFromAlignment = storageSize % type->StorageAlignment) {
      unsigned paddingRequired
        = type->StorageAlignment.getValue() - offsetFromAlignment.getValue();

      // We don't actually need to uglify the IR unless the natural
      // alignment of the IR type for the field isn't good enough.
      Alignment fieldIRAlignment(
          IGM.TargetData.getABITypeAlignment(type->StorageType));
      assert(fieldIRAlignment <= type->StorageAlignment);
      if (fieldIRAlignment != type->StorageAlignment) {
        storageTypes.push_back(llvm::ArrayType::get(IGM.Int8Ty,
                                                    paddingRequired));
      }

      // Regardless, the storage size goes up.
      storageSize += Size(paddingRequired);
    }

    ElementLayout element =
      { storageSize, (unsigned) storageTypes.size(), type };
    Elements.push_back(element);

    storageTypes.push_back(type->getStorageType());
    storageSize += type->StorageSize;
  }

  // If we concluded that the overall type is empty, bail out.
  if (isEmpty) {
    Align = Alignment(1);
    TotalSize = Size(0);
    Ty = IGM.Int8Ty;
    return;
  }

  Align = storageAlign;
  TotalSize = storageSize;
  Ty = llvm::StructType::get(IGM.getLLVMContext(), storageTypes);
}

llvm::Value *StructLayout::emitSize(IRGenFunction &IGF) const {
  assert(hasStaticLayout());
  return llvm::ConstantInt::get(IGF.IGM.SizeTy, getSize().getValue());
}

llvm::Value *StructLayout::emitAlign(IRGenFunction &IGF) const {
  assert(hasStaticLayout());
  return llvm::ConstantInt::get(IGF.IGM.SizeTy, getAlignment().getValue());
}

Address ElementLayout::project(IRGenFunction &IGF, Address baseAddr,
                               const llvm::Twine &suffix) const {
  return IGF.Builder.CreateStructGEP(baseAddr, StructIndex, ByteOffset,
                                     baseAddr.getAddress()->getName() + suffix);
}

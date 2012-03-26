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

#include "llvm/Target/TargetData.h"
#include "llvm/DerivedTypes.h"
#include "llvm/DerivedTypes.h"

#include "IRGenModule.h"
#include "GenType.h"
#include "StructLayout.h"

using namespace swift;
using namespace irgen;

/// Perform structure layout on the given types.
StructLayout::StructLayout(IRGenModule &IGM, LayoutKind layoutKind,
                           LayoutStrategy strategy,
                           llvm::ArrayRef<const TypeInfo *> types) {
  // For now, we actually only have one algorithm, and it's not
  // exactly optimal.

  Size storageSize(0);
  Alignment storageAlign(1);
  llvm::SmallVector<llvm::Type*, 8> storageTypes;

  // FIXME: heap header.

  for (const TypeInfo *type : types) {
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

    ElementLayout element = { storageSize, (unsigned) storageTypes.size() };
    Elements.push_back(element);

    storageTypes.push_back(type->getStorageType());
    storageSize += type->StorageSize;
  }

  Align = storageAlign;
  TotalSize = storageSize;

  if (storageTypes.empty()) {
    Ty = IGM.Int8Ty;
  } else {
    Ty = llvm::StructType::get(IGM.getLLVMContext(), storageTypes);
  }
}

//===--- ClassLayout.cpp - Layout of class instances ---------------------===//
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
//  This file implements algorithms for laying out class instances.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "ClassLayout.h"
#include "TypeInfo.h"

using namespace swift;
using namespace irgen;

ClassLayout::ClassLayout(const StructLayoutBuilder &builder,
                         bool isFixedSize,
                         bool metadataRequiresInitialization,
                         bool metadataRequiresRelocation,
                         llvm::Type *classTy,
                         ArrayRef<VarDecl *> allStoredProps,
                         ArrayRef<FieldAccess> allFieldAccesses,
                         ArrayRef<ElementLayout> allElements)
  : MinimumAlign(builder.getAlignment()),
    MinimumSize(builder.getSize()),
    IsFixedLayout(builder.isFixedLayout()),
    IsFixedSize(isFixedSize),
    MetadataRequiresInitialization(metadataRequiresInitialization),
    MetadataRequiresRelocation(metadataRequiresRelocation),
    Ty(classTy),
    AllStoredProperties(allStoredProps),
    AllFieldAccesses(allFieldAccesses),
    AllElements(allElements) { }

Size ClassLayout::getInstanceStart() const {
  if (AllElements.empty())
    return getSize();

  auto element = AllElements[0];
  if (element.getKind() == ElementLayout::Kind::Fixed ||
      element.getKind() == ElementLayout::Kind::Empty) {
    // FIXME: assumes layout is always sequential!
    return element.getByteOffset();
  }

  return Size(0);
}

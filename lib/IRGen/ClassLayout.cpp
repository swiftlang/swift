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
                         ClassMetadataOptions options,
                         llvm::Type *classTy,
                         ArrayRef<VarDecl *> allStoredProps,
                         ArrayRef<FieldAccess> allFieldAccesses,
                         ArrayRef<ElementLayout> allElements)
  : MinimumAlign(builder.getAlignment()),
    MinimumSize(builder.getSize()),
    IsFixedLayout(builder.isFixedLayout()),
    Options(options),
    Ty(classTy),
    AllStoredProperties(allStoredProps),
    AllFieldAccesses(allFieldAccesses),
    AllElements(allElements) { }

Size ClassLayout::getInstanceStart() const {
  ArrayRef<ElementLayout> elements = AllElements;
  while (!elements.empty()) {
    auto element = elements.front();
    elements = elements.drop_front();

    // Ignore empty elements.
    if (element.isEmpty()) {
      continue;
    } else if (element.hasByteOffset()) {
      // FIXME: assumes layout is always sequential!
      return element.getByteOffset();
    } else {
      return Size(0);
    }
  }

  // If there are no non-empty elements, just return the computed size.
  return getSize();
}

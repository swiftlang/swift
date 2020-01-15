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
                         ArrayRef<ElementLayout> allElements,
                         Size headerSize)
  : MinimumAlign(builder.getAlignment()),
    MinimumSize(builder.getSize()),
    IsFixedLayout(builder.isFixedLayout()),
    Options(options),
    Ty(classTy),
    HeaderSize(headerSize),
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
      // We used to crash for classes that have an empty and a resilient field
      // during intialization.
      //   class CrashInInit {
      //     var empty = EmptyStruct()
      //     var resilient = ResilientThing()
      //   }
      // What happened was that for such a class we we would compute a
      // instanceStart of 0. The shared cache builder would then slide the value
      // of the constant ivar offset for the empty field from 0 to 16. However
      // the field offset for empty fields is assume to be zero and the runtime
      // does not compute a different value for the empty field and so the field
      // offset for the empty field stays 0. The runtime then trys to reconcile
      // the field offset and the ivar offset trying to write to the ivar
      // offset. However, the ivar offset is marked as constant and so we
      // crashed.
      // This can be avoided by correctly computing the instanceStart for such a
      // class to be 16 such that the shared cache builder does not update the
      // value of the empty field.
      if (!Options.contains(ClassMetadataFlags::ClassHasObjCAncestry))
        return HeaderSize;
      return Size(0);
    }
  }

  // If there are no non-empty elements, just return the computed size.
  return getSize();
}

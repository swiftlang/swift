//===--- SILTypeResolutionContext.h -----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_SILTYPERESOLUTIONCONTEXT_H
#define SWIFT_SEMA_SILTYPERESOLUTIONCONTEXT_H

#include "llvm/ADT/DenseMap.h"
#include "swift/Basic/UUID.h"

namespace swift {
class GenericParamList;
class GenericEnvironment;

class SILTypeResolutionContext {
public:
  struct OpenedPackElement {
    SourceLoc DefinitionPoint;
    GenericParamList *Params;
    GenericEnvironment *Environment;
  };
  using OpenedPackElementsMap = llvm::DenseMap<UUID, OpenedPackElement>;

  /// Are we requesting a SIL type?
  bool IsSILType;

  /// Look up types in the given parameter list.
  GenericParamList *GenericParams;

  /// Look up @pack_element environments in this map.
  OpenedPackElementsMap *OpenedPackElements;

  SILTypeResolutionContext(bool isSILType,
                           GenericParamList *genericParams,
                           OpenedPackElementsMap *openedPackElements)
    : IsSILType(isSILType),
      GenericParams(genericParams),
      OpenedPackElements(openedPackElements) {}
};

}

#endif

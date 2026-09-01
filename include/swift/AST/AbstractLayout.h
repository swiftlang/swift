//===--- AbstractLayout.h - Abstract type layout information ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines data structures for abstract type layout information,
// used to encode the layout of hidden C types in .swiftmodule files.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ABSTRACTLAYOUT_H
#define SWIFT_AST_ABSTRACTLAYOUT_H

#include <cstdint>
#include <memory>
#include <optional>
#include <string>

#include "swift/AST/ReferenceCounting.h"

namespace swift {

class NominalTypeDecl;
class SerializableHiddenTypeInfoRepresentation;

struct AbstractSILTypeProperties {
  bool isTrivial = true;
  bool isFixedABI = true;
  bool isAddressOnly = false;
  bool isResilient = false;
  bool isTypeExpansionSensitive = false;
  bool hasRawPointer = false;
  bool isLexical = false;
  bool hasPack = false;
  bool isAddressableForDependencies = false;
  bool hasRawLayout = false;
  bool mayHaveCustomDeinit = false;
  bool isVeryLargeType = false;
  bool definitelyIsAddressableForDependencies = false;
  bool definitelyHasRawLayout = false;
  bool isEscapable = true;
};

struct AbstractTypeLayout {
  std::string mangledName;
  uint64_t size;
  uint64_t alignment;
  uint64_t stride;
  bool bitwiseCopyable;
  bool isOpaque;
  AbstractSILTypeProperties typeProperties;
  std::optional<ReferenceCounting> referenceCountingSystem;
  std::shared_ptr<SerializableHiddenTypeInfoRepresentation>
      typeInfoRepresentation;
};

std::optional<AbstractTypeLayout>
computeClangAbstractLayout(const NominalTypeDecl *decl);

} // namespace swift

#endif // SWIFT_AST_ABSTRACTLAYOUT_H

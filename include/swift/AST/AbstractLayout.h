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
#include <optional>
#include <string>

#include "swift/AST/ReferenceCounting.h"

namespace swift {

class NominalTypeDecl;
class SerializableHiddenTypeInfoRepresentation;

struct AbstractTypeLayout {
  std::string mangledName;
  uint64_t size = 0;
  uint64_t alignment = 0;
  uint64_t stride = 0;
  bool bitwiseCopyable = false;
  bool isOpaque = false;
  std::optional<ReferenceCounting> referenceCountingSystem;
  SerializableHiddenTypeInfoRepresentation *typeInfoRepresentation = nullptr;
};

std::optional<AbstractTypeLayout>
computeClangAbstractLayout(const NominalTypeDecl *decl);

} // namespace swift

#endif // SWIFT_AST_ABSTRACTLAYOUT_H

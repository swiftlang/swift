//===--- IRABIDetailsProvider.h - Get ABI details for decls -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRABIDETAILSPROVIDER_H
#define SWIFT_IRGEN_IRABIDETAILSPROVIDER_H

#include "llvm/ADT/Optional.h"
#include <cstdint>
#include <memory>
#include <utility>

namespace swift {

class IRGenOptions;
class ModuleDecl;
class NominalTypeDecl;

class IRABIDetailsProviderImpl;

/// Provides access to the IRGen-based queries that can be performed on
/// declarations to get their various ABI details.
class IRABIDetailsProvider {
public:
  IRABIDetailsProvider(ModuleDecl &mod, const IRGenOptions &opts);
  ~IRABIDetailsProvider();

  using SizeType = uint64_t;

  struct SizeAndAlignment {
    SizeType size;
    SizeType alignment;
  };

  /// Returns the size and alignment for the given type, or \c None if the type
  /// is not a fixed layout type.
  llvm::Optional<SizeAndAlignment>
  getTypeSizeAlignment(const NominalTypeDecl *TD);

private:
  std::unique_ptr<IRABIDetailsProviderImpl> impl;
};

} // namespace swift

#endif

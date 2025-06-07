//===--- TypeInfoProvider.h - Abstract access to type info ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file declares an abstract interface for reading type layout info.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_TYPEINFOPROVIDER_H
#define SWIFT_REMOTE_TYPEINFOPROVIDER_H

#include <stdint.h>
namespace swift {
namespace reflection {
class TypeInfo;
}
namespace remote {

/// An abstract interface for providing external type layout information.
struct TypeInfoProvider {
  using IdType = void *;

  virtual ~TypeInfoProvider() = default;

  /// Attempt to read type information about (Clang)imported types that are not
  /// represented in the metadata. LLDB can read this information from debug
  /// info, for example.
  virtual const reflection::TypeInfo *
  getTypeInfo(llvm::StringRef mangledName) = 0;

  /// A key that can be used to identify the type info provider (for example, 
  /// for caching purposes).
  virtual IdType getId() {
    // Default implementation is the instance's ID.
    return (void *) this;
  }
};

} // namespace remote
} // namespace swift
#endif

//===--------------- DescriptorFinder.h -------------------------*- C++ -*-===//
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

#ifndef SWIFT_REFLECTION_DESCRIPTOR_FINDER_H
#define SWIFT_REFLECTION_DESCRIPTOR_FINDER_H

#include "llvm/ADT/StringRef.h"

namespace swift {
namespace reflection {

class TypeRef;

/// An abstract interface for a builtin type descriptor.
struct BuiltinTypeDescriptorBase {
  const uint32_t Size;
  const uint32_t Alignment;
  const uint32_t Stride;
  const uint32_t NumExtraInhabitants;
  const bool IsBitwiseTakable;

  BuiltinTypeDescriptorBase(uint32_t Size, uint32_t Alignment, uint32_t Stride,
                            uint32_t NumExtraInhabitants, bool IsBitwiseTakable)
      : Size(Size), Alignment(Alignment), Stride(Stride),
        NumExtraInhabitants(NumExtraInhabitants),
        IsBitwiseTakable(IsBitwiseTakable) {}

  virtual ~BuiltinTypeDescriptorBase(){};

  virtual llvm::StringRef getMangledTypeName() = 0;
};

/// Interface for finding type descriptors. Implementors may provide descriptors
/// that live inside or outside reflection metadata.
struct DescriptorFinder {
  virtual ~DescriptorFinder(){};

  virtual std::unique_ptr<BuiltinTypeDescriptorBase>
  getBuiltinTypeDescriptor(const TypeRef *TR) = 0;
};

} // namespace reflection
} // namespace swift
#endif

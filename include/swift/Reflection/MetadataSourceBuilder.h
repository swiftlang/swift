//===--- MetadataSourceBuilder.h - Metadata Source Builder ------*- C++ -*-===//
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
// Implements utilities for constructing MetadataSources.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_METADATASOURCEBUILDER_H
#define SWIFT_REFLECTION_METADATASOURCEBUILDER_H

#include "swift/Reflection/MetadataSource.h"

namespace swift {
namespace reflection {

class MetadataSourceBuilder {
  std::vector<std::unique_ptr<const MetadataSource>> MetadataSourcePool;
public:
  using Source = const MetadataSource *;

  MetadataSourceBuilder() {}

  MetadataSourceBuilder(const MetadataSourceBuilder &Other) = delete;
  MetadataSourceBuilder &operator=(const MetadataSourceBuilder &Other) = delete;

  template <typename MetadataSourceTy, typename... Args>
  MetadataSourceTy *make_source(Args... args) {
    auto MS = new MetadataSourceTy(::std::forward<Args>(args)...);
    MetadataSourcePool.push_back(std::unique_ptr<const MetadataSource>(MS));
    return MS;
  }

  const GenericArgumentMetadataSource *
  createGenericArgument(unsigned Index, const MetadataSource *Source) {
    return GenericArgumentMetadataSource::create(*this, Index, Source);
  }

  const MetadataCaptureMetadataSource *
  createMetadataCapture(unsigned Index) {
    return MetadataCaptureMetadataSource::create(*this, Index);
  }

  const ReferenceCaptureMetadataSource *
  createReferenceCapture(unsigned Index) {
    return ReferenceCaptureMetadataSource::create(*this, Index);
  }

  const ClosureBindingMetadataSource *
  createClosureBinding(unsigned Index) {
    return ClosureBindingMetadataSource::create(*this, Index);
  }

  const ParentMetadataSource *
  createParent(const MetadataSource *Child) {
    return ParentMetadataSource::create(*this, Child);
  }

  const SelfMetadataSource *
  createSelf() {
    return SelfMetadataSource::create(*this);
  }

  const SelfWitnessTableMetadataSource *
  createSelfWitnessTable() {
    return SelfWitnessTableMetadataSource::create(*this);
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_METADATASOURCEBUILDER_H

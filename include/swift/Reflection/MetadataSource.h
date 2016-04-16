//===--- MetadataSource.h - Swift Metadata Sources for Reflection ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements a description of a "metadata source": at runtime, emission of
// metadata pointers that you can directly follow may be omitted as an
// optimization, because the compiler knows you can get to metadata by some
// other means. For example, all heap objects have a pointer to some metadata
// describing it, so pointers to class instances can eventually lead to their
// metadata. These nodes describe those kinds of paths to metadata at runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_METADATASOURCE_H
#define SWIFT_REFLECTION_METADATASOURCE_H

#include "llvm/Support/Casting.h"

using llvm::cast;

#include <iostream>

namespace swift {
namespace reflection {

enum class MetadataSourceKind {
#define METADATA_SOURCE(Id, Parent) Id,
#include "swift/Reflection/MetadataSources.def"
#undef METADATA_SOURCE
};

class MetadataSource {
  MetadataSourceKind Kind;
public:
  MetadataSource(MetadataSourceKind Kind) : Kind(Kind) {}

  MetadataSourceKind getKind() const {
    return Kind;
  }

  void dump() const;
  void dump(std::ostream &OS, unsigned Indent = 0) const;

  virtual ~MetadataSource() = default;
};

/// Represents a metadata pointer stashed in the "necessary bindings"
/// structure at the head of a heap closure. These can be followed
/// directly to some instantiated metadata.
class ClosureBindingMetadataSource final : public MetadataSource {
  unsigned Index;

public:
  ClosureBindingMetadataSource(unsigned Index) :
    MetadataSource(MetadataSourceKind::ClosureBinding) {}

  unsigned getIndex() const {
    return Index;
  }

  static bool classof(const MetadataSource *MS) {
    return MS->getKind() == MetadataSourceKind::ClosureBinding;
  }
};

/// Represents a capture of a reference to heap object. These can
/// be followed to the heap instance's data, then its metadata pointer.
class ReferenceCaptureMetadataSource final : public MetadataSource {
  unsigned Index;
public:
  ReferenceCaptureMetadataSource(unsigned Index):
    MetadataSource(MetadataSourceKind::ReferenceCapture) {}

  unsigned getIndex() const {
    return Index;
  }

  static bool classof(const MetadataSource *MS) {
    return MS->getKind() == MetadataSourceKind::ReferenceCapture;
  }
};

/// Represents the nth generic argument in some other source of instantiated
/// metadata.
///
/// If you have a pointer to a class MyClass<T, U>, and you need the metadata
/// for its `T`, you can follow the pointer to the instance data, then its
/// metadata pointer at the start of the instance, and fetch its first
/// generic argument.
class GenericArgumentMetadataSource final : public MetadataSource {
  unsigned Index;
  const MetadataSource *Source;

public:
  GenericArgumentMetadataSource(unsigned Index,
                                const MetadataSource *Source)
    : MetadataSource(MetadataSourceKind::GenericArgument),
      Index(Index),
      Source(Source) {}

  unsigned getIndex() const {
    return Index;
  }

  const MetadataSource *getSource() const {
    return Source;
  }

  static bool classof(const MetadataSource *MS) {
    return MS->getKind() == MetadataSourceKind::GenericArgument;
  }
};

template <typename ImplClass, typename RetTy = void, typename... Args>
class MetadataSourceVisitor {
public:

  RetTy visit(const MetadataSource *MS, Args... args) {
    switch (MS->getKind()) {
#define METADATA_SOURCE(Id, Parent) \
    case MetadataSourceKind::Id: \
      return static_cast<ImplClass*>(this) \
        ->visit##Id##MetadataSource(cast<Id##MetadataSource>(MS), \
                           ::std::forward<Args>(args)...);
#include "swift/Reflection/MetadataSources.def"
    }
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_METADATASOURCE_H

//===--- MetadataSource.h - Swift Metadata Sources for Reflection ---------===//
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

#include "llvm/ADT/Optional.h"
#include "llvm/Support/Casting.h"

using llvm::cast;

#include <climits>
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

  static bool decodeNatural(std::string::const_iterator &it,
                            const std::string::const_iterator &end,
                            unsigned &result) {
    auto begin = it;
    for (; it < end && *it >= '0' && *it <= '9'; ++it)
      ;

    if (std::distance(begin, it) == 0)
      return false;

    long int decoded = std::strtol(&*begin, nullptr, 10);
    if ((decoded == LONG_MAX || decoded == LONG_MIN) && errno == ERANGE)
      return false;

    result = static_cast<unsigned>(decoded);
    return true;
  }

  template <typename Allocator>
  static const MetadataSource *
  decodeClosureBinding(Allocator &A,
                       std::string::const_iterator &it,
                       const std::string::const_iterator &end) {
    if (it == end)
      return nullptr;

    if (*it == 'B')
      ++it;
    else
      return nullptr;

    unsigned Index;
    if (!decodeNatural(it, end, Index))
      return nullptr;
    return A.createClosureBinding(Index);
  }

  template <typename Allocator>
  static const MetadataSource *
  decodeReferenceCapture(Allocator &A,
                         std::string::const_iterator &it,
                         const std::string::const_iterator &end) {
    if (it == end)
      return nullptr;

    if (*it == 'R')
      ++it;
    else
      return nullptr;

    unsigned Index;
    if (!decodeNatural(it, end, Index))
      return nullptr;
    return A.createReferenceCapture(Index);
  }

  template <typename Allocator>
  static const MetadataSource *
  decodeMetadataCapture(Allocator &A,
                        std::string::const_iterator &it,
                        const std::string::const_iterator &end) {
    if (it == end)
      return nullptr;

    if (*it == 'M')
      ++it;
    else
      return nullptr;

    unsigned Index;
    if (!decodeNatural(it, end, Index))
      return nullptr;
    return A.createMetadataCapture(Index);
  }

  template <typename Allocator>
  static const MetadataSource *
  decodeGenericArgument(Allocator &A,
                        std::string::const_iterator &it,
                        const std::string::const_iterator &end) {
    if (it == end)
      return nullptr;

    if (*it == 'G')
      ++it;
    else
      return nullptr;

    unsigned Index;
    if (!decodeNatural(it, end, Index))
      return nullptr;

    auto Source = decode(A, it, end);
    if (!Source)
      return nullptr;

    if (it == end || *it != '_')
      return nullptr;

    ++it;

    return A.createGenericArgument(Index, Source);
  }

  template <typename Allocator>
  static const MetadataSource*
  decodeParent(Allocator &A,
               std::string::const_iterator &it,
               const std::string::const_iterator &end) {
    if (it == end || *it != 'P')
      return nullptr;

    ++it;
    auto Child = decode(A, it, end);
    if (!Child)
      return nullptr;

    if (it == end || *it != '_')
      return nullptr;

    return A.createParent(Child);
  }

  template <typename Allocator>
  static const MetadataSource *decode(Allocator &A,
                                      std::string::const_iterator &it,
                                      const std::string::const_iterator &end) {
    if (it == end) return nullptr;

    switch (*it) {
      case 'B':
        return decodeClosureBinding(A, it, end);
      case 'R':
        return decodeReferenceCapture(A, it, end);
      case 'M':
        return decodeMetadataCapture(A, it, end);
      case 'G':
        return decodeGenericArgument(A, it, end);
      case 'P':
        return decodeParent(A, it, end);
      case 'S':
        ++it;
        return A.createSelf();
      default:
        return nullptr;
    }
  }

public:
  MetadataSource(MetadataSourceKind Kind) : Kind(Kind) {}

  MetadataSourceKind getKind() const {
    return Kind;
  }

  void dump() const;
  void dump(std::ostream &OS, unsigned Indent = 0) const;
  template <typename Allocator>
  static const MetadataSource *decode(Allocator &A, const std::string &str) {
    auto begin = str.begin();
    return MetadataSource::decode<Allocator>(A, begin, str.end());
  }

  virtual ~MetadataSource() = default;
};

/// Represents a metadata pointer stashed in the "necessary bindings"
/// structure at the head of a heap closure. These can be followed
/// directly to some instantiated metadata.
class ClosureBindingMetadataSource final : public MetadataSource {
  unsigned Index;

public:

  ClosureBindingMetadataSource(unsigned Index)
    : MetadataSource(MetadataSourceKind::ClosureBinding), Index(Index) {}

  template <typename Allocator>
  static const ClosureBindingMetadataSource *
  create(Allocator &A, unsigned Index) {
    return A.template make_source<ClosureBindingMetadataSource>(Index);
  }

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
  ReferenceCaptureMetadataSource(unsigned Index)
    : MetadataSource(MetadataSourceKind::ReferenceCapture), Index(Index) {}

  template <typename Allocator>
  static const ReferenceCaptureMetadataSource *
  create(Allocator &A, unsigned Index) {
    return A.template make_source<ReferenceCaptureMetadataSource>(Index);
  }

  unsigned getIndex() const {
    return Index;
  }

  static bool classof(const MetadataSource *MS) {
    return MS->getKind() == MetadataSourceKind::ReferenceCapture;
  }
};

/// Represents a capture of a metadata pointer as an argument to a polymorphic function. These are direct sources of metadata.
class MetadataCaptureMetadataSource final : public MetadataSource {
  unsigned Index;

public:
  MetadataCaptureMetadataSource(unsigned Index)
  : MetadataSource(MetadataSourceKind::MetadataCapture), Index(Index) {}

  template <typename Allocator>
  static const MetadataCaptureMetadataSource *
  create(Allocator &A, unsigned Index) {
    return A.template make_source<MetadataCaptureMetadataSource>(Index);
  }

  unsigned getIndex() const {
    return Index;
  }

  static bool classof(const MetadataSource *MS) {
    return MS->getKind() == MetadataSourceKind::MetadataCapture;
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

  template <typename Allocator>
  static const GenericArgumentMetadataSource *
  create(Allocator &A, unsigned Index, const MetadataSource *Source) {
    return A.template make_source<GenericArgumentMetadataSource>(Index, Source);
  }

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

/// Metadata gotten through the parent of a nominal type's metadata.
class ParentMetadataSource final : public MetadataSource {
  const MetadataSource *Child;
public:
  ParentMetadataSource(const MetadataSource *Child)
    : MetadataSource(MetadataSourceKind::Parent),
      Child(Child) {}

  template <typename Allocator>
  static const ParentMetadataSource*
  create(Allocator &A, const MetadataSource *Child) {
    return A.template make_source<ParentMetadataSource>(Child);
  }

  const MetadataSource *getChild() const {
    return Child;
  }

  static bool classof(const MetadataSource *MS) {
    return MS->getKind() == MetadataSourceKind::Parent;
  }
};

/// A source of metadata from the Self metadata parameter passed via
/// a witness_method convention function.
class SelfMetadataSource final : public MetadataSource {
public:
  SelfMetadataSource() : MetadataSource(MetadataSourceKind::Self) {}

  template <typename Allocator>
  static const SelfMetadataSource*
  create(Allocator &A) {
    return A.template make_source<SelfMetadataSource>();
  }

  static bool classof(const MetadataSource *MS) {
    return MS->getKind() == MetadataSourceKind::Self;
  }
};

/// A source of metadata from the Self witness table parameter passed via
/// a witness_method convention function.
class SelfWitnessTableMetadataSource final : public MetadataSource {
public:
  SelfWitnessTableMetadataSource()
    : MetadataSource(MetadataSourceKind::SelfWitnessTable) {}

  template <typename Allocator>
  static const SelfWitnessTableMetadataSource*
  create(Allocator &A) {
    return A.template make_source<SelfWitnessTableMetadataSource>();
  }

  static bool classof(const MetadataSource *MS) {
    return MS->getKind() == MetadataSourceKind::SelfWitnessTable;
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

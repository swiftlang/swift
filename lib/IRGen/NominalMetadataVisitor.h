//===--- NominalMetadataVisitor.h - CRTP for metadata layout ----*- C++ -*-===//
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
// A CRTP helper class for visiting all of the fields in a nominal type
// metadata object.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_NOMINALMETADATAVISITOR_H
#define SWIFT_IRGEN_NOMINALMETADATAVISITOR_H

#include "GenericRequirement.h"
#include "GenProto.h"
#include "GenMeta.h"
#include "IRGenModule.h"
#include "MetadataVisitor.h"

namespace swift {
namespace irgen {

/// A CRTP class for laying out type metadata for nominal types. Note that this
/// does *not* handle the metadata template stuff.
template <class Impl> class NominalMetadataVisitor
       : public MetadataVisitor<Impl> {
  using super = MetadataVisitor<Impl>;

protected:
  using super::asImpl;

  NominalMetadataVisitor(IRGenModule &IGM) : super(IGM) {}

public:
  /// Add fields related to the generics of this class declaration.
  /// TODO: don't add new fields that are implied by the superclass
  /// fields.  e.g., if B<T> extends A<T>, the witness for T in A's
  /// section should be enough.
  template <class... T>
  void addGenericFields(NominalTypeDecl *typeDecl, T &&...args) {
    // The archetype order here needs to be consistent with
    // NominalTypeDescriptorBase::addGenericParams.
    
    // Note that we intentionally don't std::forward 'args'.
    asImpl().noteStartOfGenericRequirements(args...);

    GenericTypeRequirements requirements(super::IGM, typeDecl);
    for (auto reqt : requirements.getRequirements()) {
      if (reqt.Protocol) {
        asImpl().addGenericWitnessTable(reqt, args...);
      } else {
        asImpl().addGenericArgument(reqt, args...);
      }
    }

    asImpl().noteEndOfGenericRequirements(args...);
  }

  template <class... T>
  void noteStartOfGenericRequirements(T &&...args) {}

  template <class... T>
  void noteEndOfGenericRequirements(T &&...args) {}
};

} // end namespace irgen
} // end namespace swift

#endif

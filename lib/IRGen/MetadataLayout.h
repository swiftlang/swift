//===--- MetadataLayout.h - CRTP for metadata layout ------------*- C++ -*-===//
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
// A CRTP helper class for laying out type metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_METADATALAYOUT_H
#define SWIFT_IRGEN_METADATALAYOUT_H

#include "llvm/ADT/SmallVector.h"
#include "swift/AST/Decl.h"
#include "swift/SIL/TypeLowering.h"
#include "GenericRequirement.h"
#include "GenProto.h"
#include "IRGenModule.h"

namespace swift {
namespace irgen {

/// A CRTP class for laying out type metadata.  Note that this does
/// *not* handle the metadata template stuff.
template <class Impl> class MetadataLayout {
protected:
  Impl &asImpl() { return *static_cast<Impl*>(this); }

protected:
  IRGenModule &IGM;

  MetadataLayout(IRGenModule &IGM) : IGM(IGM) {}

public:
  void layout() {
    // Common fields.
    asImpl().addValueWitnessTable();
    asImpl().noteAddressPoint();
    asImpl().addMetadataFlags();
  }

  /// This is the address point.
  void noteAddressPoint() {}

  /// Add fields related to the generics of this class declaration.
  /// TODO: don't add new fields that are implied by the superclass
  /// fields.  e.g., if B<T> extends A<T>, the witness for T in A's
  /// section should be enough.
  template <class... T>
  void addGenericFields(NominalTypeDecl *typeDecl, Type type,
                        T &&...args) {
    // The archetype order here needs to be consistent with
    // NominalTypeDescriptorBase::addGenericParams.
    
    // Note that we intentionally don't std::forward 'args'.
    asImpl().noteStartOfGenericRequirements(args...);

    GenericTypeRequirements requirements(IGM, typeDecl);
    if (requirements.empty()) return;

    auto subs = type->castTo<BoundGenericType>()
                    ->getContextSubstitutionMap(IGM.getSwiftModule(),
                                                typeDecl);
    requirements.enumerateFulfillments(IGM, subs,
                    [&](unsigned reqtIndex, CanType argType,
                        Optional<ProtocolConformanceRef> conf) {
      if (conf) {
        asImpl().addGenericWitnessTable(argType, *conf, args...);
      } else {
        asImpl().addGenericArgument(argType, args...);
      }
    });

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

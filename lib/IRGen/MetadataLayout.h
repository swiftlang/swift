//===--- MetadataLayout.h - CRTP for metadata layout ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "GenProto.h"

namespace swift {
namespace irgen {

class IRGenModule;

/// A CRTP class for laying out class metadata.  Note that this does
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
    asImpl().addMetadataFlags();
    asImpl().addValueWitnessTable();
  }

  /// Add fields related to the generics of this class declaration.
  /// TODO: don't add new fields that are implied by base class
  /// fields.  e.g., if B<T> extends A<T>, the witness for T in A's
  /// section should be enough.
  template <class... T>
  void addGenericFields(const GenericParamList &generics,
                        T &&...args) {
    SmallVector<llvm::Type*, 4> signature;
    expandPolymorphicSignature(IGM, generics, signature);
    for (auto type : signature) {
      asImpl().addGenericWitness(type, args...); // intentionally not forwarding
    }
  }
};

} // end namespace irgen
} // end namespace swift

#endif

//===--- MetadataVisitor.h - CRTP for metadata layout -----------*- C++ -*-===//
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
// A CRTP helper class for preparing the common metadata of all metadata
// objects.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_METADATAVISITOR_H
#define SWIFT_IRGEN_METADATAVISITOR_H

namespace swift {
namespace irgen {

/// A CRTP class for laying out a type metadata's common fields. Note that this
/// does *not* handle the metadata template stuff.
template <class Impl> class MetadataVisitor {
protected:
  Impl &asImpl() { return *static_cast<Impl*>(this); }

  IRGenModule &IGM;

  MetadataVisitor(IRGenModule &IGM) : IGM(IGM) {}

public:
  void layout() {
    // Common fields.
    asImpl().addValueWitnessTable();
    asImpl().noteAddressPoint();
    asImpl().addMetadataFlags();
  }

  /// This is the address point.
  void noteAddressPoint() {}
};

} // end namespace irgen
} // end namespace swift

#endif

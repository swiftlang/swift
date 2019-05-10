//===--- TupleMetadataVisitor.h - CRTP for tuple metadata ------*- C++ -*-===//
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
// A CRTP class useful for laying out tuple metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_TUPLEMETADATALAYOUT_H
#define SWIFT_IRGEN_TUPLEMETADATALAYOUT_H

//#include "NominalMetadataVisitor.h"

namespace swift {
namespace irgen {

/// A CRTP class for laying out tuple metadata.
///
/// This produces an object corresponding to a TupleTypeMetadata type.
/// It does not itself doing anything special for metadata templates.
template <class Impl> struct TupleMetadataVisitor
       : public MetadataVisitor<Impl> {
  using super = MetadataVisitor<Impl>;

protected:
  using super::asImpl;

  TupleType *const Target;

  TupleMetadataVisitor(IRGenModule &IGM, TupleType *const target)
    : super(IGM), Target(target) {}

public:
  void layout() {
    super::layout();

    asImpl().addNumElementsInfo();
    asImpl().addLabelsInfo();

    for (unsigned i = 0, n = Target->getNumElements(); i < n; ++i) {
      asImpl().addElement(i, Target->getElement(i));
    }
  }
};

/// An "implementation" of TupleMetadataVisitor that just scans through
/// the metadata layout, maintaining the offset of all tuple elements.
template <class Impl> class TupleMetadataScanner
       : public TupleMetadataVisitor<Impl> {
  using super = TupleMetadataVisitor<Impl>;

protected:
  Size NextOffset = Size(0);

  TupleMetadataScanner(IRGenModule &IGM, TupleType *const target)
    : super(IGM, target) {}

public:
  void addValueWitnessTable() { addPointer(); }
  void addMetadataFlags() { addPointer(); }
  void addNumElementsInfo() { addPointer(); }
  void addLabelsInfo() { addPointer(); }
  void addElement(unsigned idx,
                  const TupleTypeElt &e) { addPointer(); addPointer(); }

private:
  void addPointer() { NextOffset += super::IGM.getPointerSize(); }
};

} // end namespace irgen
} // end namespace swift

#endif

//===--- FunctionMetadataVisitor.h - CRTP for function metadata -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A CRTP class useful for laying out function metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_FUNCTIONMETADATALAYOUT_H
#define SWIFT_IRGEN_FUNCTIONMETADATALAYOUT_H


namespace swift {
namespace irgen {

/// A CRTP class for laying out function metadata.
///
/// This produces an object corresponding to a FunctionTypeMetadata type.
/// It does not itself doing anything special for metadata templates.
template <class Impl> struct FunctionMetadataVisitor
       : public MetadataVisitor<Impl> {
  using super = MetadataVisitor<Impl>;

protected:
  using super::asImpl;

  FunctionType *const Target;

  FunctionMetadataVisitor(IRGenModule &IGM, FunctionType *const target)
    : super(IGM), Target(target) {}

public:

  void embeddedLayout() {
    // The embedded layout consists of:
    // -1 : vwt
    //  0 : metadata flags
    super::layout();
  }
};

} // end namespace irgen
} // end namespace swift

#endif

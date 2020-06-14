//===--- SILReferenceResolver.h - Class for resolving reference -*- C++ -*-===//
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

#ifndef SWIFT_SIL_REFERENCE_RESOLVER_H
#define SWIFT_SIL_REFERENCE_RESOLVER_H

#include "swift/AST/Decl.h"
#include "swift/SIL/SILFunction.h"

namespace swift {

/// An abstract class used to resolve external references.
///
/// The implementors should resolve SILFunction, SILVTable and SILWitnessTable
/// for external modules.
class SILReferenceResolver {
public:
  virtual ~SILReferenceResolver() = default;

  /// Attempt to load the SILFunction. Returns true if loading
  /// succeeded, false otherwise.
  virtual bool loadFunction(SILFunction *F) = 0;

  /// Look up the VTable mapped to the given ClassDecl. Returns null on failure.
  virtual SILVTable *lookUpVTable(const ClassDecl *C, bool deserializeLazily = true) = 0;

  /// Look up the SILWitnessTable representing the lowering of a protocol
  /// conformance, and collect the substitutions to apply to the referenced
  /// witnesses, if any.
  virtual SILWitnessTable *lookUpWitnessTable(const ProtocolConformance *C, bool deserializeLazily=true) = 0;
};

}
#endif

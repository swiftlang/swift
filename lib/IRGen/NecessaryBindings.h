//===--- NecessaryBindings.h - Optimizing archetype bindings ----*- C++ -*-===//
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
// This file defines a utility class for saving and restoring the
// archetype metadata necessary in order to carry out value operations
// on a type.
//
// This is a supplemental API of GenProto.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_NECESSARYBINDINGS_H
#define SWIFT_IRGEN_NECESSARYBINDINGS_H

#include "llvm/ADT/SetVector.h"

namespace swift {
  class ArchetypeType;

namespace irgen {
  class Address;
  class IRGenFunction;
  class IRGenModule;
  class Size;

/// NecessaryBindings - The set of metadata that must be saved in
/// order to perform value operations on the target type.
class NecessaryBindings {
  llvm::SetVector<ArchetypeType*> Types;

public:
  NecessaryBindings(IRGenModule &IGM, CanType type);

  /// Is the work to do trivial?
  bool empty() const { return Types.empty(); }

  /// Returns the required size of the bindings.
  /// Pointer alignment is sufficient.
  Size getBufferSize(IRGenModule &IGM) const;

  /// Save the necessary bindings to the given buffer.
  void save(IRGenFunction &IGF, Address buffer) const;

  /// Restore the necessary bindings from the given buffer.
  void restore(IRGenFunction &IGF, Address buffer) const;
};

} // end namespace irgen
} // end namespace swift

#endif

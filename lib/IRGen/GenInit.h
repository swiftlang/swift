//===--- GenInit.h - Swift IR generation for initialization -----*- C++ -*-===//
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
//  This file provides the private interface to the initialization code.
//
//  Most of the complexity here is in the dance of cleanups.  A variable
//  may have up to two cleanups associated with it:
//
//    - A deallocation cleanup, which deletes a heap-allocated variable
//      without invoking its destructor.  This is required when the
//      initializer can throw without the variable being properly
//      initialized.  This is a full-expression cleanup pushed
//      immediately within the heap-allocation but contained within the
//      initializer.  It is deactivated at the instant of initialization for
//      the initializer.
//    - A release/destroy cleanup.  For a heap-allocated variable, this
//      releases the owner, possibly invoking the destructor;  for a
//      stack-allocated variable, this simply destroys the object.
//      It has the lifetime of the actual variable.  It is pushed,
//      inactive, outside the initializer full-expression.

//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENINIT_H
#define SWIFT_IRGEN_GENINIT_H

#include "IRGenFunction.h"
#include "swift/SIL/SILValue.h"

namespace swift {
  class ValueDecl;

namespace irgen {

/// An Initialization object manages the cleanups and lifetimes of a
/// variable initialization.
///
/// To use this, you need to:
///   - create an abstract Object for the object you'd like to initialize,
///     using one of the getObject* methods;
///
/// This class also provides several convenience methods for
/// performing one or more stages of this process.
class Initialization {
public:

  /// Create a local variable.
  ///
  /// Precondition: the abstract object has been registered
  ///   with this Initialization, but is not marked as allocated
  ///   or initialized.
  /// Postcondition: the abstract object will have been marked as
  ///   allocated, but not marked as initialized.
  OwnedAddress emitLocalAllocation(IRGenFunction &IGF,
                                   OnHeap_t onHeap, const TypeInfo &type,
                                   const Twine &name);

};

} // end namespace irgen
} // end namespace swift

#endif

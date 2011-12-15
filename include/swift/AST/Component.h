//===--- Component.h - Swift Language Component ASTs ------------*- C++ -*-===//
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
// This file defines the Component class.  Components are the units of
// code distribution;  ABI resilience applies across component boundaries.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_COMPONENT_H
#define SWIFT_COMPONENT_H

namespace swift {
  class Module;
  
/// Component - A unit of distribution.
class Component {
public:
  /// isResilient - Must arbitrary declarations from the given module
  /// be made resilient when evaluated as part of this component?
  bool isResilient(Module *M) { return true; }
};
  
} // end namespace swift

#endif

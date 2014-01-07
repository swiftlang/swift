//===--- SILLinkage.h - Defines the SILLinkage type -------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILLINKAGE_H
#define SWIFT_SIL_SILLINKAGE_H

namespace swift {

/// Linkage for a SIL object.  This concept combines the notions
/// of symbol linkage and visibility.
enum class SILLinkage : unsigned char {
  External,
  Thunk,
  Internal,
  Deserialized // Deserialized from a module.
};

}

#endif

//===--- Debug.h - Requirement machine debugging flags ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RQM_DEBUG_H
#define SWIFT_RQM_DEBUG_H

#include "swift/Basic/OptionSet.h"

namespace swift {

namespace rewriting {

enum class DebugFlags : unsigned {
  /// Print debug output when simplifying terms.
  Simplify = (1<<0),

  /// Print debug output when adding rules.
  Add = (1<<1),

  /// Print debug output when merging associated types.
  Merge = (1<<2),

  /// Print debug output from the Knuth-Bendix algorithm.
  Completion = (1<<3),

  /// Print debug output when unifying concrete types in the property map.
  ConcreteUnification = (1<<4),

  /// Print debug output when concretizing nested types in the property map.
  ConcretizeNestedTypes = (1<<5),

  /// Print debug output from the homotopy reduction algorithm.
  HomotopyReduction = (1<<6),

  /// Print debug output from the generating conformances algorithm.
  GeneratingConformances = (1<<7),
};

using DebugOptions = OptionSet<DebugFlags>;

}

}

#endif

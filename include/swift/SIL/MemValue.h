//===------------------------ MemValue.h ----------------------------- -===//
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
// This file defines the class MemValue. A MemValue is an abstraction of
// an object field value in program. It consists of a base that is the tracked
// SILValue, and a projection path to the represented field.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MEM_VALUE_H
#define SWIFT_MEM_VALUE_H

#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SIL/Projection.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

namespace swift {

//===----------------------------------------------------------------------===//
//                              Memory Value 
//===----------------------------------------------------------------------===//
/// Forward declaration.
class MemValue;
using MemValueList = llvm::SmallVector<MemValue, 8>;

class MemValue {
private:
  /// The base of the memory value. 
  SILValue Base;
  /// The path to reach the accessed field of the object.
  Optional<ProjectionPath> Path;

public:
  /// Constructors.
  MemValue() : Base() {}
  MemValue(SILValue B) : Base(B) {}
  MemValue(SILValue B, ProjectionPath &P)
      : Base(B), Path(std::move(P)) {}

  SILValue getBase() const { return Base; }
  Optional<ProjectionPath> &getPath() { return Path; }

  /// Returns whether the memory location has been initialized properly.
  bool isValid() const {
    return Base && Path.hasValue();
  }
};

} // end swift namespace


#endif  // SWIFT_MEM_VALUE_H

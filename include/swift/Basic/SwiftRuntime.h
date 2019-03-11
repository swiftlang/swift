//===--- SwiftRuntime.h - Swift Runtime Handling ----------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_SWIFTRUNTIME_H
#define SWIFT_BASIC_SWIFTRUNTIME_H

#include "llvm/ADT/Triple.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/VersionTuple.h"

namespace swift {

class SwiftRuntime {
  VersionTuple Version;

public:
  SwiftRuntime(const VersionTuple &V) : Version(V) {}

  const VersionTuple &getVersion() const { return Version; }


  bool hasDefaultForeignClassValueWitnesses() const {
    return Version >= VersionTuple(5, 1);
  }


  bool tryParse(StringRef S);

  std::string getAsString() const;

  friend bool operator==(const SwiftRuntime &LHS, const SwiftRuntime &RHS) {
    return LHS.getVersion() == RHS.getVersion();
  }

  friend bool operator!=(const SwiftRuntime &LHS, const SwiftRuntime &RHS) {
    return !(LHS == RHS);
  }
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &OS, const SwiftRuntime &V);

}

#endif


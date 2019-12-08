//===--- Located.h - Source Location and Associated Value ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file forward declares and imports various common LLVM datatypes that
// swift wants to use unqualified.
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_BASIC_LOCATED_H
#define SWIFT_BASIC_LOCATED_H
#include "swift/Basic/SourceLoc.h"

namespace swift {

template<typename T>
struct Located {

  T item;

  SourceLoc loc;

  template<typename U>
  friend bool operator ==(const Located<U> lhs, const Located<U> rhs) {
    return lhs.item == rhs.item && lhs.loc == rhs.loc;
  }
};
}

#endif // SWIFT_BASIC_LOCATED_H

//===--- SubstitutionList.h - Compact SubstitutionMap -----------*- C++ -*-===//
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
//
// This file defines the SubstitutionList class, which is a memory-efficient
// representation of a SubstitutionMap, intended to be stored in AST nodes and
// SIL instructions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SUBSTITUTION_LIST_H
#define SWIFT_AST_SUBSTITUTION_LIST_H

#include "swift/AST/Substitution.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {

// FIXME: Soon this will change.
using SubstitutionList = ArrayRef<Substitution>;

void dump(SubstitutionList subs);

} // end namespace swift

#endif

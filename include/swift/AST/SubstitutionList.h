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
#include "llvm/ADT/FoldingSet.h"

namespace llvm {
class FoldingSetNodeID;
} // end namespace llvm

namespace swift {

// FIXME: Soon this will change.
using SubstitutionList = ArrayRef<Substitution>;

void dump(SubstitutionList subs);

/// Create a canonicalized substitution list from subs.
/// subs is the substitution list to be canonicalized.
/// canSubs is an out-parameter, which is used to store the results in case
/// the list of substitutions was not canonical.
/// The function returns a list of canonicalized substitutions.
/// If the substitution list subs was canonical already, it will be returned and
/// canSubs out-parameter will be empty.
/// If something had to be canonicalized, then the canSubs out-parameter will be
/// populated and the returned SubstitutionList would refer to canSubs storage.
SubstitutionList
getCanonicalSubstitutionList(SubstitutionList subs,
                             SmallVectorImpl<Substitution> &canSubs);

/// Profile the substitution list for use in a folding set.
void profileSubstitutionList(llvm::FoldingSetNodeID &id, SubstitutionList subs);

} // end namespace swift

#endif

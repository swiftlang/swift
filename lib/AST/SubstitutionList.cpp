//===--- SubstitutionList.cpp - Compact SubstitutionMap -------------------===//
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

#include "swift/AST/SubstitutionList.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "llvm/ADT/FoldingSet.h"

using namespace swift;

void swift::profileSubstitutionList(llvm::FoldingSetNodeID &id,
                                    SubstitutionList subs) {
  id.AddInteger(subs.size());
  for (auto &sub : subs) {
    id.AddPointer(sub.getReplacement().getPointer());
    for (auto conformance : sub.getConformances())
      id.AddPointer(conformance.getOpaqueValue());
  }
}

//===--- SILWitnessTable.h - Defines the SILWitnessTable class ------------===//
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
// This file defines the SILWitnessTable class, which is used to map a protocol
// conformance for a type to its implementing SILFunctions. This information is
// (FIXME will be) used by IRGen to create witness tables for protocol dispatch.
// It can also be used by generic specialization and existential
// devirtualization passes to promote archetype_method and protocol_method
// instructions to static function_refs.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILWitnessTable *SILWitnessTable::create(SILModule &M,
                                     NormalProtocolConformance *Conformance,
                                     ArrayRef<SILWitnessTable::Entry> entries) {
  void *buf = M.allocate(sizeof(SILWitnessTable)
                           + sizeof(Entry) * (entries.size()-1),
                         alignof(SILWitnessTable));
  SILWitnessTable *wt = ::new (buf) SILWitnessTable(Conformance, entries);
  M.witnessTables.push_back(wt);
  return wt;
}

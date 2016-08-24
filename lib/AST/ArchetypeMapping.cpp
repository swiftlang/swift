//===--- ArchetypeMapping.h - Archetype Mapping AST -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the ArchetypeMapping class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ArchetypeMapping.h"
#include "swift/AST/ASTContext.h"

using namespace swift;

ArchetypeMapping::ArchetypeMapping(TypeSubstitutionMap interfaceToArchetypeMap)
  : InterfaceToArchetypeMap(interfaceToArchetypeMap) {

  // Compute reverse mapping.
  for (auto entry : interfaceToArchetypeMap)
    if (auto *archetypeTy = entry.second->getAs<ArchetypeType>())
      ArchetypeToInterfaceMap[archetypeTy] = entry.first;
}

void *ArchetypeMapping::operator new(size_t bytes, const ASTContext &ctx) {
  return ctx.Allocate(bytes, alignof(ArchetypeMapping), AllocationArena::Permanent);
}


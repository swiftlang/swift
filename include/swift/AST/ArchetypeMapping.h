//===--- ArchetypeMapping.h - Archetype Mapping AST -------------*- C++ -*-===//
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
// This file defines the ArchetypeMapping class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ARCHETYPE_MAPPING_H
#define SWIFT_AST_ARCHETYPE_MAPPING_H

#include "swift/AST/ProtocolConformance.h"

namespace swift {

class ASTContext;

/// Describes the mapping between archetypes and interface types for the
/// generic parameters of a DeclContext.
class ArchetypeMapping final {
  TypeSubstitutionMap ArchetypeToInterfaceMap;
  TypeSubstitutionMap InterfaceToArchetypeMap;

public:
  explicit ArchetypeMapping(TypeSubstitutionMap interfaceToArchetypeMap);

  static ArchetypeMapping *get(ASTContext &ctx,
                               TypeSubstitutionMap interfaceToArchetypeMap);

  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  // Only allow allocation of ArchetypeMappings using the allocator
  // in ASTContext.
  void *operator new(size_t bytes, const ASTContext &ctx);
};
  
} // end namespace swift

#endif // SWIFT_AST_ARCHETYPE_MAPPING_H


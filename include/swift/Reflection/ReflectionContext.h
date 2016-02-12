//===--- ReflectionContext.h - Swift Type Reflection Context ----*- C++ -*-===//
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
// Implements the context for allocations and management of structures related
// to reflection, such as TypeRefs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_REFLECTIONCONTEXT_H
#define SWIFT_REFLECTION_REFLECTIONCONTEXT_H

#include "swift/Reflection/Reader.h"
#include "swift/Reflection/TypeRef.h"

#include <iostream>

class NodePointer;

namespace swift {
namespace reflection {

class ReflectionContext {
  MemoryReader &Reader;

  void dumpTypeRef(const std::string &MangledName,
                   std::ostream &OS, bool printTypeName = false) const {
    auto TypeName = Demangle::demangleTypeAsString(MangledName);
    auto DemangleTree = Demangle::demangleTypeAsNode(MangledName);
    auto TR = decodeDemangleNode(DemangleTree);
    OS << TypeName << '\n';
    TR->dump(OS);
    std::cout << std::endl;
  }

public:
  ReflectionContext(MemoryReader &Reader) : Reader(Reader) {}

  void dumpFieldSection(std::ostream &OS) const {
    for (const auto &sections : Reader.getInfo()) {
      for (const auto &descriptor : sections.Fields) {
        dumpTypeRef(descriptor.getMangledTypeName(), OS);
        for (auto &field : descriptor) {
          OS << field.getFieldName() << ": ";
          dumpTypeRef(field.getMangledTypeName(), OS);
        }
      }
    }
  }

  void dumpAssociatedTypeSection(std::ostream &OS) const {
    for (const auto &sections : Reader.getInfo()) {
      for (const auto &descriptor : sections.AssociatedTypes) {
        auto conformingTypeName = Demangle::demangleTypeAsString(
          descriptor.getMangledConformingTypeName());
        auto protocolName = Demangle::demangleTypeAsString(
          descriptor.getMangledProtocolTypeName());

        OS << conformingTypeName << " : " << protocolName;
        OS << std::endl;

        for (const auto &associatedType : descriptor) {
          OS << "typealias " << associatedType.getName() << " = ";
          dumpTypeRef(associatedType.getMangledSubstitutedTypeName(), OS);
        }
      }
    }
  }

  void dumpAllSections(std::ostream &OS) const {
    OS << "FIELDS:\n";
    for (size_t i = 0; i < 7; ++i) OS << '=';
    OS << std::endl;
    dumpFieldSection(OS);
    OS << "\nASSOCIATED TYPES:\n";
    for (size_t i = 0; i < 17; ++i) OS << '=';
    OS << std::endl;
    dumpAssociatedTypeSection(OS);
    OS << std::endl;
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H

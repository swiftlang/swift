//===--- IndexSymbol.h - Index symbol data types ----------------*- C++ -*-===//
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

#ifndef SWIFT_INDEX_INDEXSYMBOL_H
#define SWIFT_INDEX_INDEXSYMBOL_H

#include "swift/Basic/LLVM.h"
#include "clang/Index/IndexSymbol.h"

namespace swift {
class Decl;
class ValueDecl;
class ModuleEntity;
enum class AccessorKind;

namespace index {

using clang::index::SymbolKind;
using clang::index::SymbolLanguage;
using clang::index::SymbolSubKind;
using clang::index::SymbolProperty;
using clang::index::SymbolPropertySet;
using clang::index::SymbolRole;
using clang::index::SymbolRoleSet;
using clang::index::SymbolRelation;
using clang::index::SymbolInfo;

inline SymbolPropertySet operator&(SymbolPropertySet SKSet, SymbolProperty SK) {
  return SKSet & (SymbolPropertySet)SK;
}
inline SymbolPropertySet operator|(SymbolPropertySet SKSet, SymbolProperty SK) {
  return SKSet | (SymbolPropertySet)SK;
}
inline SymbolPropertySet &operator|=(SymbolPropertySet &SKSet, SymbolProperty SK) {
  return SKSet = SKSet | SK;
}

struct IndexRelation {
  const Decl *decl;
  SymbolInfo symInfo;
  SymbolRoleSet roles = SymbolRoleSet(0);

  // The following strings are guaranteed to live at least as long as the
  // current indexing action.
  StringRef name;
  StringRef USR; // USR may be safely compared by pointer.
  StringRef group;

  IndexRelation(SymbolRoleSet Roles, const Decl *Sym, SymbolInfo SymInfo, StringRef Name, StringRef USR)
  : decl(Sym), symInfo(SymInfo), roles(Roles), name(Name), USR(USR) {}

  IndexRelation() = default;
};

struct IndexSymbol : IndexRelation {
  SmallVector<IndexRelation, 3> Relations;
  unsigned line = 0;
  unsigned column = 0;
  ValueDecl *originalDecl = nullptr;

  IndexSymbol() = default;

  StringRef getReceiverUSR() const {
    for (auto Relation: Relations) {
      if (Relation.roles & (SymbolRoleSet) SymbolRole::RelationReceivedBy)
        return Relation.USR;
    }
    return StringRef();
  }
};

SymbolInfo getSymbolInfoForModule(ModuleEntity Mod);
SymbolInfo getSymbolInfoForDecl(const Decl *D);
SymbolSubKind getSubKindForAccessor(AccessorKind AK);
bool isLocalSymbol(const Decl *D);

using clang::index::printSymbolProperties;

} // end namespace index
} // end namespace swift

#endif // SWIFT_INDEX_INDEXSYMBOL_H

//===--- IndexSymbol.h - Index symbol data types ----------------*- C++ -*-===//
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

#ifndef SWIFT_INDEX_INDEXSYMBOL_H
#define SWIFT_INDEX_INDEXSYMBOL_H

#include "swift/Basic/LLVM.h"
#include "clang/Index/IndexSymbol.h"
#include "llvm/ADT/SmallString.h"

namespace swift {
class Decl;

namespace index {

enum class SymbolKind {
  Unknown,

  Module,
  ClangModule, // FIXME: collapse into Module and use a separate Language field.

  Enum,
  Struct,
  Class,
  Protocol,
  Extension,

  TypeAlias,
  AssociatedType,
  GenericTypeParam,

  Function,
  Variable,
  PrefixOperator,
  PostfixOperator,
  InfixOperator,
  Accessor,
  Subscript,
  EnumElement,

  InstanceMethod,
  ClassMethod,
  StaticMethod,
  InstanceProperty,
  ClassProperty,
  StaticProperty,

  Constructor,
  Destructor,
};

enum class SymbolSubKind {
  None,

  AccessorGetter,
  AccessorSetter,
  AccessorWillSet,
  AccessorDidSet,
  AccessorAddressor,
  AccessorMutableAddressor,

  ExtensionOfStruct,
  ExtensionOfClass,
  ExtensionOfEnum,
  ExtensionOfProtocol,
};

using SymbolRole = clang::index::SymbolRole;
using SymbolRoleSet = clang::index::SymbolRoleSet;

struct IndexSymbol {
  enum TypeKind { Base, FuncDecl };
  TypeKind entityType = Base;

  SymbolKind kind;
  SymbolSubKind subKind = SymbolSubKind::None;
  SymbolRoleSet roles = SymbolRoleSet(0);
  // The following strings are guaranteed to live at least as long as the
  // current indexing action.
  StringRef name;
  StringRef USR; // USR may be safely compared by pointer.
  StringRef group;
  StringRef receiverUSR;
  unsigned line = 0;
  unsigned column = 0;

  IndexSymbol() = default;

protected:
  IndexSymbol(TypeKind TK) : entityType(TK) {}
};

struct FuncDeclIndexSymbol : public IndexSymbol {
  bool IsTestCandidate = false;

  FuncDeclIndexSymbol() : IndexSymbol(FuncDecl) {}
};


SymbolKind getSymbolKindForDecl(const Decl *D);

} // end namespace index
} // end namespace swift

#endif // SWIFT_INDEX_INDEXSYMBOL_H

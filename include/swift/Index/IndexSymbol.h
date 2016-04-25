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
#include "llvm/ADT/SmallString.h"

namespace swift {
class Decl;

namespace index {

enum class SymbolKind {
  Unknown,

  Module,
  ClangModule, // FIXME: collapse into Module and use a separate Language field.
  SourceFile,

  Enum,
  Struct,
  Class,
  Protocol,
  Extension,

  TypeAlias,
  AssociatedType,
  GenericTypeParam,

  Function,
  PrefixOperator,
  PostfixOperator,
  InfixOperator,

  LocalVariable,
  GlobalVariable,
  ParamVariable,

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
  AccessorMaterializeForSet,
  AccessorAddressor,
  AccessorMutableAddressor,

  ExtensionOfStruct,
  ExtensionOfClass,
  ExtensionOfEnum,
  ExtensionOfProtocol,
};

struct IndexSymbol {
  enum TypeKind { Base, FuncDecl, CallReference };
  TypeKind entityType = Base;

  SymbolKind kind;
  SymbolSubKind subKind = SymbolSubKind::None;
  bool isRef;
  // The following strings are guaranteed to live at least as long as the
  // current indexing action.
  StringRef name;
  StringRef USR; // USR may be safely compared by pointer.
  StringRef group;
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

struct CallRefIndexSymbol : public IndexSymbol {
  StringRef ReceiverUSR;
  bool IsDynamic = false;

  CallRefIndexSymbol() : IndexSymbol(CallReference) {}
};

SymbolKind getSymbolKindForDecl(const Decl *D);

} // end namespace index
} // end namespace swift

#endif // SWIFT_INDEX_INDEXSYMBOL_H

//===--- SourceType.h - Swift Language AST for Source Types -----*- C++ -*-===//
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
// This file defines the SourceType class and subclasses, which are used to
// represent the parse tree of types as written in the code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SOURCETYPE_H
#define SWIFT_SOURCETYPE_H

#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Identifier.h"

namespace swift {

/// SourceTypeKind - The various kinds of SourceTypes.
enum class SourceTypeKind {
  Identifier,
  Function,
  Tuple,
  Paren
};
  
/// SourceType - This is the base class for the classes that represent types as
/// written by the user, with source location information.
class SourceType {
  SourceTypeKind Kind;
protected:
  SourceType(SourceTypeKind Kind) : Kind(Kind) {}
public:
  
  SourceTypeKind getKind() const { return Kind; }

  //SourceRange getSourceRange() const;
  
  
  void dump() const;
  void print(raw_ostream &OS) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const SourceType *) { return true; }
};
  
/// SourceIdentifierType - Represents a (potentially dotted) type written as an
/// identifier, e.g. "foo" or "foo.bar", which names another type.
class SourceIdentifierType : public SourceType {
public:
  class Component {
  public:
    SourceLoc Loc;
    Identifier Id;
    
    Component(SourceLoc Loc, Identifier Id) : Loc(Loc), Id(Id) {}
  };
private:
  /// The components that make this up.
  ArrayRef<Component> Components;

  

  
  SourceIdentifierType() : SourceType(SourceTypeKind::Identifier) {}
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const SourceType *) {
    return getKind() == SourceTypeKind::Identifier;
  }
  static bool classof(const SourceIdentifierType *) { return true; }
};
  
SourceType
  SourceIdentifierType (potentially dotted)
  SourceFunctionType
  SourceTupleType
  SourceParenType
  
  
} // end namespace swift

#endif

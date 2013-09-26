//===- SemanticSourceEntity.h - Routines for semantic source info ---------===//
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

#ifndef SWIFT_IDE_SEMANTIC_SOURCE_ENTITY_H
#define SWIFT_IDE_SEMANTIC_SOURCE_ENTITY_H

#include "swift/Basic/SourceLoc.h"
#include <functional>

namespace swift {
  class Decl;
  class ValueDecl;
  class TypeDecl;

namespace ide {

struct SemanticSourceEntity {
  CharSourceRange Range;
  ValueDecl *Dcl;
  /// This is set when the entity is a reference to a \c ConstructorDecl,
  /// to point to the type declaration that the source refers to.
  TypeDecl *CtorTyRef;
  bool IsRef;
};

/// A function receiving SemanticSourceEntity objects.
/// Returns true to continue receiving the next object, false to stop.
typedef std::function<bool(SemanticSourceEntity AnnoTok)>
    SemanticEntityReceiverFn;

/// Walks the provided declarations and passes SemanticSourceEntities to the
/// \c Receiver.
///
/// The entities are given in source-order, as long as the \c Decls array is
/// already in source-order.
///
/// \param Decls the array of declarations to traverse.
///
/// \param Receiver the function receiving the entities.
///
/// \returns true if all the entities were passed to the receiver, false if
/// the receiver stopped by returning false.
bool findSemanticSourceEntities(ArrayRef<Decl*> Decls,
                                SemanticEntityReceiverFn Receiver);

} // namespace ide
} // namespace swift

#endif

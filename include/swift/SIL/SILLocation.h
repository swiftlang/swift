//===--- SILLocation.h - Location information for SIL nodes -----*- C++ -*-===//
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

#ifndef SWIFT_SIL_LOCATION_H
#define SWIFT_SIL_LOCATION_H

#include "llvm/ADT/PointerUnion.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"

#include <cstddef>
#include <type_traits>

namespace swift {

class SourceLoc;

/// SILLocation - This is a pointer to the AST node that a SIL instruction was
/// derived from. This may be null if AST information is unavailable or
/// stripped.
///
/// FIXME: This should eventually include inlining history, generics
/// instantiation info, etc (when we get to it).
///
class SILLocation {
private:
  template <class T, class Enable = void>
  struct base_type;

  template <class T>
  struct base_type<T,
      typename std::enable_if<std::is_base_of<Decl, T>::value>::type> {
    using type = Decl;
  };

  template <class T>
  struct base_type<T,
      typename std::enable_if<std::is_base_of<Expr, T>::value>::type> {
    using type = Expr;
  };

  template <class T>
  struct base_type<T,
      typename std::enable_if<std::is_base_of<Stmt, T>::value>::type> {
    using type = Stmt;
  };

  template <class T>
  struct base_type<T,
      typename std::enable_if<std::is_base_of<Pattern, T>::value>::type> {
    using type = Pattern;
  };

  llvm::PointerUnion4<Stmt*, Expr*, Decl*, Pattern*> ASTNode;
public:
  SILLocation() {}
  SILLocation(Stmt* S) : ASTNode(S) {}
  SILLocation(Expr* E) : ASTNode(E) {}
  SILLocation(Decl* D) : ASTNode(D) {}
  SILLocation(Pattern* P) : ASTNode(P) {}

  bool isNull() const {
    return ASTNode.isNull();
  }
  LLVM_EXPLICIT operator bool() const { return !ASTNode.isNull(); }

  /// \brief If the current value is of the specified AST unit type T,
  /// return it, otherwise return null.
  template <typename T>
  T *getAs() const {
    using base = typename base_type<T>::type*;
    return dyn_cast_or_null<T>(ASTNode.dyn_cast<base>());
  }

  /// \brief Returns true if the Location currently points to the AST node
  /// matching type T.
  template <typename T>
  bool is() const {
    if (ASTNode.is<typename base_type<T>::type*>()) {
      return isa<T>(ASTNode.get<typename base_type<T>::type*>());
    }
    return false;
  }

  /// \brief Returns the value of the specified AST node type. If the specified
  /// type is incorrect, asserts.
  template <typename T>
  T *castTo() const {
    return cast<T>(ASTNode.get<typename base_type<T>::type*>());
  }

  SourceLoc getSourceLoc() const;
  SourceLoc getStartSourceLoc() const;
  SourceLoc getEndSourceLoc() const;
};

} // end swift namespace


#endif

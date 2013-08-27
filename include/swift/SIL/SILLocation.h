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

  typedef llvm::PointerUnion4<Stmt*, Expr*, Decl*, Pattern*> ASTNodeTy;

public:
  enum LocationKind {
    // FIXME: NoneKind is to be removed.
    NoneKind,
    RegularKind,
    ReturnKind,
    ImplicitReturnKind,
    InlinedKind,
    CleanupKind,
    ArtificialUnreachableKind,
    SILFileKind
  };

protected:
  /// \brief Primary AST location.
  ASTNodeTy ASTNode;
  /// \brief Secondary AST location.
  ASTNodeTy ASTNodeSecondary;
  // If coming from a .sil file, this is the location in the .sil file.
  // FIXME: We should be able to reuse the ASTNodes memory to store this. We
  // could just store Value.getPointer() in the pointer union.
  SourceLoc SILFileSourceLoc;

  /// \brief The kind of this SIL location.
  LocationKind Kind;

  template <typename T>
  T *getNodeAs(ASTNodeTy Node) const {
    using base = typename base_type<T>::type*;
    return dyn_cast_or_null<T>(Node.dyn_cast<base>());
  }

  template <typename T>
  bool isNode(ASTNodeTy Node) const {
    if (ASTNode.is<typename base_type<T>::type*>())
      return isa<T>(Node.get<typename base_type<T>::type*>());
    return false;
  }

  template <typename T>
  T *castNodeTo(ASTNodeTy Node) const {
    return cast<T>(Node.get<typename base_type<T>::type*>());
  }

public:
  // FIXME: These should become protected constructors. We should not allow
  // creation of a generic SILLocation.
  SILLocation() : Kind(NoneKind) {}
  SILLocation(LocationKind K) : Kind(K) {}
  SILLocation(Stmt *S, LocationKind K) : ASTNode(S), Kind(K) {}
  SILLocation(Expr *E, LocationKind K) : ASTNode(E), Kind(K) {}
  SILLocation(Decl *D, LocationKind K) : ASTNode(D), Kind(K) {}
  SILLocation(Pattern *P, LocationKind K) : ASTNode(P), Kind(K) {}

  /// When an ASTNode gets implicitely converted into a SILLocation we
  /// construct a RegularLocation. Since RegularLocations represent the majority
  /// of locations, this greatly simplifies the user code.
  SILLocation(Stmt *S) : ASTNode(S), Kind(RegularKind) {}
  SILLocation(Expr *E) : ASTNode(E), Kind(RegularKind) {}
  SILLocation(Decl *D) : ASTNode(D), Kind(RegularKind) {}
  SILLocation(Pattern *P) : ASTNode(P), Kind(RegularKind) {}

  bool isNull() const {
    return ASTNode.isNull() && SILFileSourceLoc.isInvalid();
  }
  LLVM_EXPLICIT operator bool() const { return !isNull(); }

  bool hasASTLocation() const { return !ASTNode.isNull(); }

  unsigned getKind() const { return Kind; }

  template <typename T>
  bool is() const {
    return T::isKind(*this);
  }

  template <typename T>
  T castAs() const {
    assert(T::isKind(*this));
    T t;
    SILLocation& tr = t;
    tr = *this;
    return t;
  }

  template <typename T>
  Optional<T> getAs() const {
    if (!T::isKind(*this))
      return Optional<T>();
    T t;
    SILLocation& tr = t;
    tr = *this;
    return t;
  }

  /// \brief If the current value is of the specified AST unit type T,
  /// return it, otherwise return null.
  template <typename T>
  T *getAsASTNode() const { return getNodeAs<T>(ASTNode); }

  /// \brief Returns true if the Location currently points to the AST node
  /// matching type T.
  template <typename T>
  bool isASTNode() const { return isNode<T>(ASTNode); }

  /// \brief Returns the primary value as the specified AST node type. If the
  /// specified type is incorrect, asserts.
  template <typename T>
  T *castToASTNode() const { return castNodeTo<T>(ASTNode); }

  SourceLoc getSourceLoc() const;
  SourceLoc getStartSourceLoc() const;
  SourceLoc getEndSourceLoc() const;

  /// Pretty-print the value.
  void dump(const SourceManager &SM) const;
  void print(raw_ostream &OS, const SourceManager &SM) const;
};

/// Allowed on any instruction.
class RegularLocation : public SILLocation {
public:
  RegularLocation() : SILLocation() {}
  RegularLocation(Stmt *S) : SILLocation(S, RegularKind) {}
  RegularLocation(Expr *E) : SILLocation(E, RegularKind) {}
  RegularLocation(Decl *D) : SILLocation(D, RegularKind) {}
  RegularLocation(Pattern *P) : SILLocation(P, RegularKind) {}

  /// \brief If the current value is of the specified AST unit type T,
  /// return it, otherwise return null.
  template <typename T>
  T *getAs() const { return getNodeAs<T>(ASTNode); }

  /// \brief Returns true if the Location currently points to the AST node
  /// matching type T.
  template <typename T>
  bool is() const { return isNode<T>(ASTNode); }

  /// \brief Returns the primary value as the specified AST node type. If the
  /// specified type is incorrect, asserts.
  template <typename T>
  T *castTo() const { return castNodeTo<T>(ASTNode); }

private:
  friend class SILLocation;
  static bool isKind(const SILLocation& L) {
    return L.getKind() == RegularKind;
  }
};

/// \brief Used to represent a return instruction in user code.
///
/// Allowed on an BranchInst, ReturnInst, AutoreleaseReturnInst.
class ReturnLocation : public SILLocation {
public:
  ReturnLocation(ReturnStmt *RS) : SILLocation(RS, ReturnKind) {}

  ReturnStmt *get() {
    return castToASTNode<ReturnStmt>();
  }

private:
  friend class SILLocation;
  static bool isKind(const SILLocation& L) {
    return L.getKind() == ReturnKind;
  }
  ReturnLocation() {}
};

/// \brief Used on the instruction that was generated to represent an implicit
/// return from a function.
///
/// This location wraps the CapturingExpr node.
///
/// Allowed on an BranchInst, ReturnInst, AutoreleaseReturnInst.
class ImplicitReturnLocation : public SILLocation {
public:
  ImplicitReturnLocation(CapturingExpr *RS)
    : SILLocation(RS, ImplicitReturnKind) {}

  CapturingExpr *get() {
    return castToASTNode<CapturingExpr>();
  }

private:
  friend class SILLocation;
  static bool isKind(const SILLocation& L) {
    return L.getKind() == ImplicitReturnKind;
  }
  ImplicitReturnLocation() {}
};

/// \brief Marks instructions that correspond to inlined function body and
/// setup code.
///
/// This location wraps the call site ASTNode.
///
/// Allowed on any instruction except for ReturnInst, AutoreleaseReturnInst.
class InlinedLocation : public SILLocation {
public:
  InlinedLocation(ApplyExpr *RS) : SILLocation(RS, InlinedKind) {}

  ApplyExpr *get() {
    return castToASTNode<ApplyExpr>();
  }

private:
  friend class SILLocation;
  static bool isKind(const SILLocation& L) {
    return L.getKind() == InlinedKind;
  }
  InlinedLocation() {}
};

/// \brief Used on the instruction performing auto-generated cleanup such as
/// deallocs, destructor calls.
///
/// This location wraps the statement representing the enclosing scope, for
/// example, FuncExpr, ParenExpr. The scope's end location points to
/// the SourceLoc that shows when the operation is performed at runtime. The
/// location also stores the AST node that triggered the cleanup.
///
/// Allowed on any instruction except for ReturnInst, AutoreleaseReturnInst.
/// CleanupInlinedDestructorLocation is also represented by this.
class CleanupLocation : public SILLocation {
public:
  CleanupLocation(CapturingExpr *RS) : SILLocation(RS, CleanupKind) {}

  CapturingExpr *get() {
    return castToASTNode<CapturingExpr>();
  }

  /// \brief Get the AST node that triggered the cleanup as a node of the
  /// given AST type.
  template <typename T>
  T *getOriginAs() const { return getNodeAs<T>(ASTNodeSecondary); }

  /// \brief Check if the node that triggered the cleanup is of the given
  /// type.
  template <typename T>
  bool isOrigin() const { return isNode<T>(ASTNodeSecondary); }

  /// \brief Returns the node that triggered the cleanup as the specified
  /// AST node type. If the specified type is incorrect, asserts.
  template <typename T>
  T *castOriginTo() const { return castNodeTo<T>(ASTNodeSecondary); }

private:
  friend class SILLocation;
  static bool isKind(const SILLocation& L) {
    return L.getKind() == CleanupKind;
  }
  CleanupLocation() {}
};

/// \brief Used to represent unreachable location that was auto-generated and
/// has no correspondance to user code. It should not be used in diagnostics
/// or for debugging.
///
/// Differentiates an unreachable instruction, which is generated by
/// DCE, from an unreachable instruction in user code (output of SILGen).
/// Allowed on an unreachable location.
class ArtificialUnreachableLocation : public SILLocation {
public:
  ArtificialUnreachableLocation() : SILLocation(ArtificialUnreachableKind) {}

private:
  friend class SILLocation;
  static bool isKind(const SILLocation& L) {
    return (L.getKind() == ArtificialUnreachableKind);
  }
};

/// \brief Used to represent locations coming from the parsed SIL file.
///
/// Allowed on any SILInstruction.
class SILFileLocation : public SILLocation {
public:
  SILFileLocation(SourceLoc L) : SILLocation(SILFileKind) {
    SILFileSourceLoc = L;
  }

private:
  friend class SILLocation;
  static bool isKind(const SILLocation& L) {
    return L.getKind() == SILFileKind;
  }
};

} // end swift namespace


#endif

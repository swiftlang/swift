//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SYNTHESIZEDDECLBUILDER_H
#define SWIFT_AST_SYNTHESIZEDDECLBUILDER_H

#include "swift/AST/AttrKind.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallVector.h"
#include <optional>
#include <utility>

namespace swift {

class ASTContext;
class BraceStmt;
class DeclAttribute;
class Expr;

AccessLevel
formalAccessForSynthesizedMember(const ValueDecl *source, bool parent = false);

class VarDeclBuilder {
public:
  using BodySynthesizer =
      std::pair<BraceStmt *, bool> (*)(AbstractFunctionDecl *, void *);

  VarDeclBuilder(DeclContext *DC, Identifier name);

  // MARK: Shape

  VarDeclBuilder &introducer(VarDecl::Introducer);
  VarDeclBuilder &static_(bool value = true);

  /// The name location — feeds diagnostics and source ranges. Defaults to an
  /// invalid `SourceLoc()`; set it (e.g. to the originating decl's loc) when the
  /// synthesized decl should point back at real source.
  VarDeclBuilder &at(SourceLoc nameLoc);

  // MARK: type / implicitness

  VarDeclBuilder &type(Type type);
  VarDeclBuilder &implicit(bool value = true);
  VarDeclBuilder &synthesized(bool value = true);

  // MARK: access

  VarDeclBuilder &readAccess(AccessLevel);
  VarDeclBuilder &writeAccess(AccessLevel);

  VarDeclBuilder &usableFromInline(bool value = true);

  // MARK: storage

  VarDeclBuilder &getter(BodySynthesizer body, void *context = nullptr);
  VarDeclBuilder &setter(BodySynthesizer body, void *context = nullptr);

  // MARK: misc

  VarDeclBuilder &alwaysEmitIntoClient(bool value = true);
  VarDeclBuilder &transparent(bool value = true);
  VarDeclBuilder &attribute(DeclAttribute *);

  operator VarDecl *();

private:
  ASTContext &C;
  DeclContext *DC;
  Identifier Name;
  SourceLoc NameLoc;
  Type InterfaceTy;

  VarDecl::Introducer Intro = VarDecl::Introducer::Var;
  bool IsStatic = false;
  bool IsImplicit = true;
  bool IsSynthesized = true;

  std::optional<AccessLevel> ReadAccess;
  std::optional<AccessLevel> WriteAccess;
  bool UsableFromInline = false;

  BodySynthesizer Getter = nullptr;
  void *GetterContext = nullptr;
  BodySynthesizer Setter = nullptr;
  void *SetterContext = nullptr;

  bool AlwaysEmitIntoClient = false;
  bool Transparent = false;

  SmallVector<DeclAttribute *, 2> Attributes;

  VarDecl *Result = nullptr;
};

class PatternBindingDeclBuilder {
public:
  explicit PatternBindingDeclBuilder(VarDecl *var);

  PatternBindingDeclBuilder &init(Expr *initExpr);
  PatternBindingDeclBuilder &implicit(bool value = true);

  operator PatternBindingDecl *();

  VarDecl *variable() const { return Var; }

private:
  VarDecl *Var;
  Expr *InitExpr = nullptr;
  bool IsImplicit = true;
  PatternBindingDecl *Result = nullptr;
};

} // namespace swift

#endif // SWIFT_AST_SYNTHESIZEDDECLBUILDER_H

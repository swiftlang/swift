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

#include "swift/AST/SynthesizedDeclBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/StorageImpl.h"
#include "swift/AST/Types.h"

using namespace swift;

namespace swift {
AccessLevel
formalAccessForSynthesizedMember(const ValueDecl *source, bool parent) {
  AccessLevel access = source->getFormalAccess();

  // To make something have the same access as a 'private' parent, it has to
  // be 'fileprivate' or greater.
  if (parent && access == AccessLevel::Private)
    access = AccessLevel::FilePrivate;

  // Synthesized member variables aren't overridable, so 'open' collapses to
  // 'public'.
  if (access == AccessLevel::Open)
    access = AccessLevel::Public;

  return access;
}
}

// MAKR: VarDeclBuilder

VarDeclBuilder::VarDeclBuilder(DeclContext *DC, Identifier name)
    : C(DC->getASTContext()), DC(DC), Name(name) {}

VarDeclBuilder &VarDeclBuilder::introducer(VarDecl::Introducer intro) {
  Intro = intro;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::static_(bool value) {
  IsStatic = value;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::at(SourceLoc nameLoc) {
  NameLoc = nameLoc;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::type(Type interfaceType) {
  InterfaceTy = interfaceType;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::implicit(bool value) {
  IsImplicit = value;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::synthesized(bool value) {
  IsSynthesized = value;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::readAccess(AccessLevel access) {
  ReadAccess = access;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::writeAccess(AccessLevel access) {
  WriteAccess = access;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::usableFromInline(bool value) {
  UsableFromInline = value;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::getter(BodySynthesizer body, void *context) {
  Getter = body;
  GetterContext = context;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::setter(BodySynthesizer body, void *context) {
  Setter = body;
  SetterContext = context;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::alwaysEmitIntoClient(bool value) {
  AlwaysEmitIntoClient = value;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::transparent(bool value) {
  Transparent = value;
  return *this;
}

VarDeclBuilder &VarDeclBuilder::attribute(DeclAttribute *attr) {
  Attributes.push_back(attr);
  return *this;
}

VarDeclBuilder::operator VarDecl *() {
  if (Result)
    return Result;

  auto *VD = new (C) VarDecl(IsStatic, Intro, NameLoc, Name, DC);
  if (IsImplicit)
    VD->setImplicit();
  if (IsSynthesized)
    VD->setSynthesized();
  if (InterfaceTy)
    VD->setInterfaceType(InterfaceTy);

  // Access: if a read access was requested, stamp it now.  Otherwise leave it
  // unset so that AccessLevelRequest can derive it (or a caller can apply
  // copyFormalAccessFrom afterwards).
  if (ReadAccess)
    VD->setAccess(*ReadAccess);

  // Inherit @usableFromInline, mirroring the guard in copyFormalAccessFrom.
  if (UsableFromInline &&
      !VD->getAttrs().hasAttribute<UsableFromInlineAttr>() &&
      DeclAttribute::canAttributeAppearOnDecl(DeclAttrKind::UsableFromInline,
                                              VD)) {
    VD->getAttrs().add(new (C) UsableFromInlineAttr(/*implicit=*/true));
  }

  for (DeclAttribute *attr : Attributes)
    VD->getAttrs().add(attr);

  if (Getter) {
    AccessorDecl *getter =
        AccessorDecl::create(C, /*FuncLoc=*/SourceLoc(),
                             /*AccessorKeywordLoc=*/SourceLoc(),
                             AccessorKind::Get, VD, /*Async=*/false,
                             /*AsyncLoc=*/SourceLoc(), /*Throws=*/false,
                             /*ThrowsLoc=*/SourceLoc(),
                             /*ThrownType=*/TypeLoc(),
                             ParameterList::createEmpty(C),
                             InterfaceTy, DC);
    getter->setImplicit();
    getter->setIsTransparent(Transparent);
    getter->copyFormalAccessFrom(VD);
    if (AlwaysEmitIntoClient) {
      auto AEIC = new (C) AlwaysEmitIntoClientAttr(/*implicit=*/true);
      getter->getAttrs().add(AEIC);
    }
    getter->setBodySynthesizer(Getter, GetterContext);

    if (Setter) {
      auto *param =
          new (C) ParamDecl(SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                            C.getIdentifier("newValue"), DC);
      param->setSpecifier(ParamSpecifier::Default);
      param->setImplicit();
      param->setInterfaceType(InterfaceTy);
      auto *params = ParameterList::create(C, {param});

      AccessorDecl *setter =
          AccessorDecl::create(C, /*FuncLoc=*/SourceLoc(),
                               /*AccessorKeywordLoc=*/SourceLoc(),
                               AccessorKind::Set, VD, /*Async=*/false,
                               /*AsyncLoc=*/SourceLoc(), /*Throws=*/false,
                               /*ThrowsLoc=*/SourceLoc(),
                               /*ThrownType=*/TypeLoc(), params,
                               /*ReturnType=*/Type(), DC);
      setter->setImplicit();

      AccessLevel setterAccess =
          WriteAccess.value_or(ReadAccess.value_or(VD->getFormalAccess()));
      setter->setAccess(setterAccess);
      VD->overwriteSetterAccess(setterAccess);
      setter->setBodySynthesizer(Setter, SetterContext);

      VD->setImplInfo(StorageImplInfo::getMutableComputed());
      VD->setAccessors(SourceLoc(), {getter, setter}, SourceLoc());
    } else {
      VD->setImplInfo(StorageImplInfo::getImmutableComputed());
      VD->setAccessors(SourceLoc(), {getter}, SourceLoc());
    }
  }

  assert((!WriteAccess || Setter) && "writeAccess set without a setter");

  Result = VD;
  return VD;
}

// MARK: PatternBindingDeclBuilder

PatternBindingDeclBuilder::PatternBindingDeclBuilder(VarDecl *var)
    : Var(var) {}

PatternBindingDeclBuilder &PatternBindingDeclBuilder::init(Expr *initExpr) {
  InitExpr = initExpr;
  return *this;
}

PatternBindingDeclBuilder &PatternBindingDeclBuilder::implicit(bool value) {
  IsImplicit = value;
  return *this;
}

PatternBindingDeclBuilder::operator PatternBindingDecl *() {
  if (Result)
    return Result;

  auto &C = Var->getASTContext();
  Type Ty = Var->getTypeInContext();

  Pattern *pattern =
      TypedPattern::createImplicit(C, NamedPattern::createImplicit(C, Var, Ty),
                                   Ty);

  Result = PatternBindingDecl::createImplicit(C, StaticSpellingKind::None,
                                              pattern, InitExpr,
                                              Var->getDeclContext());
  return Result;
}

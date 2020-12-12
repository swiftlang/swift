//===--- ASTGenType.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/ASTGen.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"

using namespace swift;
using namespace swift::syntax;

TypeRepr *ASTGen::generate(const syntax::TypeSyntax &Type,
                           const SourceLoc Loc) {
  auto typeLoc = advanceLocBegin(Loc, Type);

  // Check if we have recorded a type that hasn't been migrated to
  // libSyntax-parsing yet at this location
  if (hasType(typeLoc)) {
    return takeType(typeLoc);
  }

  // Otherwise, generate the AST node for the type.
  if (auto array = Type.getAs<ArrayTypeSyntax>()) {
    return generate(*array, Loc);
  } else if (auto completionTy = Type.getAs<CodeCompletionTypeSyntax>()) {
    return generate(*completionTy, Loc);
  } else if (auto dictionary = Type.getAs<DictionaryTypeSyntax>()) {
    return generate(*dictionary, Loc);
  } else if (auto memberIdentifier = Type.getAs<MemberTypeIdentifierSyntax>()) {
    return generate(*memberIdentifier, Loc);
  } else if (auto simpleIdentifier = Type.getAs<SimpleTypeIdentifierSyntax>()) {
    return generate(*simpleIdentifier, Loc);
  } else if (auto unknown = Type.getAs<UnknownTypeSyntax>()) {
    return generate(*unknown, Loc);
  }

  llvm_unreachable("ASTGen hasn't been tought how to generate this type");
}

TypeRepr *ASTGen::generate(const syntax::ArrayTypeSyntax &Type,
                           const SourceLoc Loc) {
  SourceLoc LBracketLoc = advanceLocBegin(Loc, Type);
  SourceLoc RBracketLoc = advanceLocEnd(Loc, Type);

  TypeRepr *ElementType = generate(Type.getElementType(), Loc);
  if (!ElementType) {
    return nullptr;
  }
  return new (Context) ArrayTypeRepr(ElementType, {LBracketLoc, RBracketLoc});
}

TypeRepr *ASTGen::generate(const syntax::CodeCompletionTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto base = Type.getBase();
  if (!base) {
    if (P.CodeCompletion) {
      P.CodeCompletion->completeTypeSimpleBeginning();
    }
    return nullptr;
  }

  if (auto *parsedTyR = generate(*base, Loc)) {
    if (P.CodeCompletion) {
      P.CodeCompletion->setParsedTypeLoc(parsedTyR);
      if (Type.getPeriod()) {
        P.CodeCompletion->completeTypeIdentifierWithDot();
      } else {
        P.CodeCompletion->completeTypeIdentifierWithoutDot();
      }
    }
    return parsedTyR;
  }

  return nullptr;
}

TypeRepr *ASTGen::generate(const syntax::DictionaryTypeSyntax &Type,
                           const SourceLoc Loc) {
  SourceLoc LBracketLoc = advanceLocBegin(Loc, Type);
  SourceLoc ColonLoc = advanceLocBegin(Loc, Type.getColon());
  SourceLoc RBracketLoc = advanceLocEnd(Loc, Type);

  TypeRepr *KeyType = generate(Type.getKeyType(), Loc);
  TypeRepr *ValueType = generate(Type.getValueType(), Loc);
  if (!KeyType || !ValueType) {
    return nullptr;
  }

  SourceRange Range{LBracketLoc, RBracketLoc};
  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const syntax::MemberTypeIdentifierSyntax &Type,
                           const SourceLoc Loc) {
  SmallVector<ComponentIdentTypeRepr *, 4> components;
  gatherTypeIdentifierComponents(Type, Loc, components);
  return IdentTypeRepr::create(Context, components);
}

TypeRepr *ASTGen::generate(const SimpleTypeIdentifierSyntax &Type,
                           const SourceLoc Loc) {
  if (Type.getName().getTokenKind() == tok::kw_Any) {
    auto anyLoc = advanceLocBegin(Loc, Type.getName());
    return CompositionTypeRepr::createEmptyComposition(Context, anyLoc);
  }

  auto typeRepr = generateTypeIdentifier(Type, Loc);
  return IdentTypeRepr::create(Context, {typeRepr});
}

TypeRepr *ASTGen::generate(const syntax::UnknownTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto ChildrenCount = Type.getNumChildren();
  // generate child 'TypeSyntax' anyway to trigger the side effects e.g.
  // code-completion.
  for (size_t i = 0; i != ChildrenCount; ++i) {
    auto elem = *Type.getChild(i);
    if (auto ty = elem.getAs<TypeSyntax>())
      (void)generate(*ty, Loc);
  }

  return nullptr;
}

void ASTGen::addType(TypeRepr *E, const SourceLoc Loc) {
  assert(!hasType(Loc));
  Types[Loc] = E;
}

bool ASTGen::hasType(const SourceLoc Loc) const {
  return Types.find(Loc) != Types.end();
}

TypeRepr *ASTGen::takeType(const SourceLoc Loc) {
  auto I = Types.find(Loc);
  assert(I != Types.end());
  auto expr = I->second;
  Types.erase(I);
  return expr;
}
  
//===--------------------------------------------------------------------===//
// MARK: - Private

void ASTGen::generateGenericArgs(
    const GenericArgumentClauseSyntax &ClauseSyntax, const SourceLoc Loc,
    SourceLoc &LAngleLoc, SourceLoc &RAngleLoc,
    SmallVectorImpl<TypeRepr *> &Args) {
  LAngleLoc = advanceLocBegin(Loc, ClauseSyntax);
  RAngleLoc = advanceLocEnd(Loc, ClauseSyntax);

  assert(Args.empty());
  for (auto arg : ClauseSyntax.getArguments()) {
    auto typeRepr = generate(arg.getArgumentType(), Loc);
    if (!typeRepr) {
      typeRepr = new (Context) ErrorTypeRepr(advanceLocBegin(Loc, arg));
    }
    Args.push_back(typeRepr);
  }
}

template <typename T>
ComponentIdentTypeRepr *ASTGen::generateTypeIdentifier(const T &TypeSyntax,
                                                       const SourceLoc Loc) {
  auto declNameLoc = DeclNameLoc(advanceLocBegin(Loc, TypeSyntax.getName()));
  auto declNameRef = DeclNameRef(
      Context.getIdentifier(TypeSyntax.getName().getIdentifierText()));
  if (auto clause = TypeSyntax.getGenericArgumentClause()) {
    SourceLoc lAngleLoc, rAngleLoc;
    SmallVector<TypeRepr *, 4> args;
    generateGenericArgs(*clause, Loc, lAngleLoc, rAngleLoc, args);
    if (!args.empty()) {
      return GenericIdentTypeRepr::create(Context, declNameLoc, declNameRef,
                                          args, {lAngleLoc, rAngleLoc});
    }
  }
  return new (Context) SimpleIdentTypeRepr(declNameLoc, declNameRef);
}

void ASTGen::gatherTypeIdentifierComponents(
    const TypeSyntax &Component, const SourceLoc Loc,
    SmallVectorImpl<ComponentIdentTypeRepr *> &Components) {
  if (auto simpleIdentifier = Component.getAs<SimpleTypeIdentifierSyntax>()) {
    auto componentType = generateTypeIdentifier(*simpleIdentifier, Loc);
    Components.push_back(componentType);
  } else if (auto memberIdentifier =
                 Component.getAs<MemberTypeIdentifierSyntax>()) {
    gatherTypeIdentifierComponents(memberIdentifier->getBaseType(), Loc,
                                   Components);
    auto ComponentType = generateTypeIdentifier(*memberIdentifier, Loc);
    Components.push_back(ComponentType);
  } else {
    llvm_unreachable("unexpected type identifier component");
  }
}

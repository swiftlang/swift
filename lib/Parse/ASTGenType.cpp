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
#include "swift/Parse/Parser.h"

using namespace swift;
using namespace swift::syntax;

TypeRepr *ASTGen::generate(const syntax::TypeSyntax &Type,
                           const SourceLoc Loc) {
  // Check if we have recorded a type that hasn't been migrated to
  // libSyntax-parsing yet at this location
  if (hasType(Loc)) {
    return takeType(Loc);
  }

  // Otherwise, generate the AST node for the type.
  if (auto SimpleIdentifier = Type.getAs<SimpleTypeIdentifierSyntax>()) {
    return generate(*SimpleIdentifier, Loc);
  } else if (auto Array = Type.getAs<ArrayTypeSyntax>()) {
    return generate(*Array, Loc);
  } else if (auto Dictionary = Type.getAs<DictionaryTypeSyntax>()) {
    return generate(*Dictionary, Loc);
  } else if (auto Unknown = Type.getAs<UnknownTypeSyntax>()) {
    return generate(*Unknown, Loc);
  }

  llvm_unreachable("ASTGen hasn't been tought how to generate this type");
}

TypeRepr *ASTGen::generate(const syntax::ArrayTypeSyntax &Type,
                           const SourceLoc Loc) {
  SourceLoc LBracketLoc = advanceLocBegin(Loc, Type);
  SourceLoc ElementLoc = advanceLocBegin(Loc, Type.getElementType());
  SourceLoc RBracketLoc = advanceLocEnd(Loc, Type);

  TypeRepr *ElementType = generate(Type.getElementType(), ElementLoc);
  if (!ElementType) {
    return nullptr;
  }
  return new (Context) ArrayTypeRepr(ElementType, {LBracketLoc, RBracketLoc});
}

TypeRepr *ASTGen::generate(const syntax::DictionaryTypeSyntax &Type,
                           const SourceLoc Loc) {
  SourceLoc LBracketLoc = advanceLocBegin(Loc, Type);
  SourceLoc KeyLoc = advanceLocBegin(Loc, Type.getKeyType());
  SourceLoc ColonLoc = advanceLocBegin(Loc, Type.getColon());
  SourceLoc ValueLoc = advanceLocBegin(Loc, Type.getValueType());
  SourceLoc RBracketLoc = advanceLocEnd(Loc, Type);

  TypeRepr *KeyType = generate(Type.getKeyType(), KeyLoc);
  TypeRepr *ValueType = generate(Type.getValueType(), ValueLoc);
  if (!KeyType || !ValueType) {
    return nullptr;
  }

  SourceRange Range{LBracketLoc, RBracketLoc};
  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const SimpleTypeIdentifierSyntax &Type,
                           const SourceLoc Loc) {
  auto typeLoc = advanceLocBegin(Loc, Type);
  if (hasType(typeLoc)) {
    return takeType(typeLoc);
  }

  if (Type.getName().getTokenKind() == tok::kw_Any) {
    auto AnyLoc = advanceLocBegin(Loc, Type.getName());
    return CompositionTypeRepr::createEmptyComposition(Context, AnyLoc);
  }

  llvm_unreachable("Only the Any type has been implemented so far");
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

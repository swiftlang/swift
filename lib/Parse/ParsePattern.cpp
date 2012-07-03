//===--- ParsePattern.cpp - Swift Language Parser for Patterns ------------===//
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
// Pattern Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "Parser.h"
using namespace swift;

/// Check that the given type is fully-typed.
/// FIXME: this is *terrible* for source locations.
bool Parser::checkFullyTyped(Type type) {
  switch (type->getKind()) {
  // Any sort of non-structural type can be ignored here.
  // Many of these are not actually possible to encounter in the
  // parser, but it's okay.
  case TypeKind::Error:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::BuiltinObjCPointer:
  case TypeKind::NameAlias: // FIXME: underlying type could be non-fully-typed!
  case TypeKind::Identifier:
  case TypeKind::Protocol:
  case TypeKind::OneOf:
  case TypeKind::Struct:
  case TypeKind::Class:
  case TypeKind::MetaType:
  case TypeKind::Module:
  case TypeKind::UnstructuredUnresolved:
  case TypeKind::Archetype:
  case TypeKind::Substituted:
  case TypeKind::ProtocolComposition:
    return false;

  case TypeKind::Paren:
    return checkFullyTyped(cast<ParenType>(type)->getUnderlyingType());

  case TypeKind::LValue:
    return checkFullyTyped(cast<LValueType>(type)->getObjectType());

  case TypeKind::Array:
    return checkFullyTyped(cast<ArrayType>(type)->getBaseType());

  case TypeKind::ArraySlice:
    return checkFullyTyped(cast<ArraySliceType>(type)->getBaseType());

  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    AnyFunctionType *fn = cast<AnyFunctionType>(type);
    return checkFullyTyped(fn->getInput())
         | checkFullyTyped(fn->getResult());
  }

  case TypeKind::Tuple: {
    TupleType *tuple = cast<TupleType>(type);
    bool isInvalid = false;
    for (auto &elt : tuple->getFields()) {
      if (elt.getType().isNull()) {
        assert(elt.hasInit());
        diagnose(elt.getInit()->getLoc(),
                 diag::untyped_tuple_elt_in_function_signature)
          << elt.getInit()->getSourceRange();
        isInvalid = true;
      } else {
        isInvalid |= checkFullyTyped(elt.getType());
      }
    }
    return isInvalid;
  }
  }
  llvm_unreachable("bad type kind");
}

/// Check that the given pattern is fully-typed.
bool Parser::checkFullyTyped(Pattern *pattern, Type &funcTy) {
  switch (pattern->getKind()) {
  // Any type with an explicit annotation is okay, as long as the
  // annotation is fully-typed.
  case PatternKind::Typed:
    if (checkFullyTyped(pattern->getType()))
      return true;
    funcTy = pattern->getType();
    return false;

  // Paren types depend on their parenthesized pattern.
  case PatternKind::Paren: {
    Pattern *sub = cast<ParenPattern>(pattern)->getSubPattern();
    if (checkFullyTyped(sub, funcTy)) return true;
    pattern->setType(sub->getType());
    return false;
  }

  // Tuple types can be built up from their components.
  case PatternKind::Tuple: {
    TuplePattern *tuple = cast<TuplePattern>(pattern);
    SmallVector<TupleTypeElt, 8> typeElts;
    SmallVector<TupleTypeElt, 8> typeEltsWithInits;
    typeElts.reserve(tuple->getNumFields());
    bool HadInit = false;
    for (TuplePatternElt &elt : tuple->getFields()) {
      Pattern *subpattern = elt.getPattern();
      Type eltTy;
      if (checkFullyTyped(subpattern, eltTy))
        return true;
      typeElts.push_back(TupleTypeElt(subpattern->getType(),
                                      subpattern->getBoundName(), nullptr,
                                      elt.getVarargBaseType()));
      typeEltsWithInits.push_back(TupleTypeElt(eltTy,
                                               subpattern->getBoundName(),
                                               elt.getInit(),
                                               elt.getVarargBaseType()));
      HadInit |= (elt.getInit() != 0);
      // The grammar allows default values in general patterns, but
      // they aren't ever allowed semantically.  However, function
      // declarations use a syntactic shortcut of sorts: the argument list
      // declares both the pattern for the argument declarations, and the type
      // of the function itself.  Default values are part of the type, not the
      // pattern, so we set the pattern's initializer to null when we build
      // the type.
      elt.setInit(nullptr);
    }
    TupleType *patternTT = TupleType::get(typeElts, Context);
    TupleType *funcTT = TupleType::get(typeEltsWithInits, Context);
    if (HadInit)
      TypesWithDefaultValues.emplace_back(funcTT, CurDeclContext);

    tuple->setType(patternTT);
    funcTy = funcTT;
    return false;
  }

  // Everything else is uninferrable.
  case PatternKind::Named:
  case PatternKind::Any:
    diagnose(pattern->getLoc(), diag::untyped_pattern_in_function_signature)
      << pattern->getSourceRange();
    return true;
  }
  llvm_unreachable("bad pattern kind");
}

/// parseFunctionSignature - Parse a function definition signature.
///   func-signature:
///     pattern-tuple+ func-signature-result?
///   func-signature-result:
///     '->' type
bool Parser::parseFunctionSignature(SmallVectorImpl<Pattern*> &params,
                                    Type &type, TypeLoc *&loc) {
  // Parse curried function argument clauses as long as we can.
  do {
    NullablePtr<Pattern> pattern = parsePatternTuple();
    if (pattern.isNull())
      return true;
    else
      params.push_back(pattern.get());
  } while (Tok.isAnyLParen());

  // If there's a trailing arrow, parse the rest as the result type.
  if (consumeIf(tok::arrow)) {
    if (parseType(type, loc))
      return true;

    checkFullyTyped(type);

  // Otherwise, we implicitly return ().
  } else {
    type = TupleType::getEmpty(Context);
    loc = nullptr;
  }

  // Now build up the function type.  We require all function
  // signatures to be fully-typed: that is, all top-down paths to a
  // leaf pattern must pass through a TypedPattern.
  return buildFunctionSignature(params, type, loc);
}

bool Parser::buildFunctionSignature(SmallVectorImpl<Pattern*> &params,
                                    Type &type, TypeLoc *&loc) {
  // Now build up the function type.  We require all function
  // signatures to be fully-typed: that is, all top-down paths to a
  // leaf pattern must pass through a TypedPattern.
  for (unsigned i = params.size(); i != 0; --i) {
    Pattern *param = params[i - 1];
    
    Type paramType;
    if (checkFullyTyped(param, paramType)) {
      // Recover by ignoring everything.
      paramType = TupleType::getEmpty(Context);
    }
    type = FunctionType::get(paramType, type, Context);
  }
  
  return false;  
}


/// Parse a pattern.
///   pattern ::= pattern-atom
///   pattern ::= pattern-atom ':' type-annotation
NullablePtr<Pattern> Parser::parsePattern() {
  // First, parse the pattern atom.
  NullablePtr<Pattern> pattern = parsePatternAtom();
  if (pattern.isNull()) return nullptr;

  // Now parse an optional type annotation.
  if (consumeIf(tok::colon)) {
    Type type;
    TypeLoc *loc;
    if (parseTypeAnnotation(type, loc))
      return nullptr;

    pattern = new (Context) TypedPattern(pattern.get(), type);
  }

  return pattern;
}

/// Parse a pattern "atom", meaning the part that precedes the
/// optional type annotation.
///
///   pattern-atom ::= identifier
///   pattern-atom ::= pattern-tuple
NullablePtr<Pattern> Parser::parsePatternAtom() {
  switch (Tok.getKind()) {
  case tok::l_paren:
  case tok::l_paren_space:
    return parsePatternTuple();

  case tok::identifier: {
    SourceLoc loc = Tok.getLoc();
    StringRef text = Tok.getText();
    consumeToken(tok::identifier);

    // '_' is a special case which means 'ignore this'.
    if (text == "_") {
      return new (Context) AnyPattern(loc);
    } else {
      Identifier ident = Context.getIdentifier(text);
      VarDecl *var = new (Context) VarDecl(loc, ident, Type(), nullptr);
      return new (Context) NamedPattern(var);
    }
  }

  default:
    diagnose(Tok, diag::expected_pattern);
    return nullptr;
  }
}

/// Parse a tuple pattern.
///
///   pattern-tuple:
////    '(' pattern-tuple-body? ')'
///   pattern-tuple-body:
///     pattern-tuple-element (',' pattern-tuple-body)*
///   pattern-tuple-element:
///     pattern ('=' expr)?
NullablePtr<Pattern> Parser::parsePatternTuple() {
  assert(Tok.isAnyLParen());

  // We're looking at the left parenthesis; consume it.
  SourceLoc lp = consumeToken();

  // Parse all the elements.
  SmallVector<TuplePatternElt, 8> elts;
  bool hadEllipsis = false;
  if (Tok.isNot(tok::r_paren)) {
    do {
      NullablePtr<Pattern> pattern = parsePattern();
      Expr *init = nullptr;

      if (pattern.isNull()) {
        skipUntil(tok::r_paren);
        return nullptr;
      } else if (consumeIf(tok::equal)) {
        NullablePtr<Expr> initR = parseExpr(diag::expected_initializer_expr);
        if (initR.isNull()) {
          skipUntil(tok::r_paren);
          return nullptr;
        }
        
        init = initR.get();
      }

      elts.push_back(TuplePatternElt(pattern.get(), init));
    } while (consumeIf(tok::comma));

    if (Tok.is(tok::ellipsis)) {
      if (elts.back().getInit()) {
        diagnose(Tok.getLoc(), diag::tuple_ellipsis_init);
        skipUntil(tok::r_paren);
        return nullptr;
      }
      hadEllipsis = true;
      consumeToken(tok::ellipsis);

      TypedPattern *subpattern =
          dyn_cast<TypedPattern>(elts.back().getPattern());
      if (!subpattern) {
        diagnose(elts.back().getPattern()->getLoc(),
                 diag::untyped_pattern_ellipsis);
        skipUntil(tok::r_paren);
        return nullptr;
      }
      Type subTy = subpattern->getType();
      elts.back().setVarargBaseType(subTy);
      subpattern->overwriteType(ArraySliceType::get(subTy, SourceLoc(),
                                                    Context));
    }

    if (Tok.isNot(tok::r_paren)) {
      diagnose(Tok, diag::expected_rparen_tuple_pattern_list);
      skipUntil(tok::r_paren);
      return nullptr;
    }
  }

  // Consume the right parenthesis.
  SourceLoc rp = consumeToken(tok::r_paren);

  // A pattern which wraps a single anonymous pattern is not a tuple.
  if (elts.size() == 1 &&
      elts[0].getInit() == nullptr &&
      elts[0].getPattern()->getBoundName().empty() &&
      !hadEllipsis)
    return new (Context) ParenPattern(lp, elts[0].getPattern(), rp);

  return TuplePattern::create(Context, lp, elts, rp);
}    

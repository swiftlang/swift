//===--- ParseType.cpp - Swift Language Parser for Types ------------------===//
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
// Type Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "Parser.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SaveAndRestore.h"
using namespace swift;

bool Parser::parseTypeAnnotation(TypeLoc &result) {
  return parseTypeAnnotation(result, diag::expected_type);
}

/// parseTypeAnnotation
///   type-annotation:
///     attribute-list type
bool Parser::parseTypeAnnotation(TypeLoc &result, Diag<> message) {
  // Parse attributes.
  DeclAttributes attrs;
  parseAttributeList(attrs);

  // Parse the type.
  if (parseType(result, message))
    return true;

  // Apply those attributes that do apply.
  if (attrs.empty()) return false;

  if (attrs.isByref()) {
    LValueType::Qual quals;
    Type resultType = LValueType::get(result.getType(), quals, Context);
    SourceRange resultRange = { attrs.LSquareLoc,
                                result.getSourceRange().End };
    result = { resultType, resultRange };
    attrs.Byref = false; // so that the empty() check below works
  }

  // Handle the auto_closure, cc, and objc_block attributes for function types.
  if (attrs.isAutoClosure() || attrs.hasCC() || attrs.isObjCBlock() ||
      attrs.isThin()) {
    FunctionType *FT = dyn_cast<FunctionType>(result.getType().getPointer());
    TupleType *InputTy = 0;
    if (FT) InputTy = dyn_cast<TupleType>(FT->getInput().getPointer());
    if (FT == 0) {
      // auto_closures and objc_blocks require a syntactic function type.
      if (attrs.isAutoClosure())
        diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                 "auto_closure");
      if (attrs.isObjCBlock())
        diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                 "objc_block");
      if (attrs.hasCC())
        diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                 "cc");
      if (attrs.isThin())
        diagnose(attrs.LSquareLoc, diag::attribute_requires_function_type,
                 "thin");
    } else if (attrs.isAutoClosure() &&
               (InputTy == 0 || !InputTy->getFields().empty())) {
      // auto_closures must take () syntactically.
      diagnose(attrs.LSquareLoc, diag::autoclosure_function_input_nonunit,
               FT->getInput());
    } else {
      // Otherwise, we're ok, rebuild type, adding the AutoClosure and ObjcBlock
      // bit.
      Type resultType = FunctionType::get(FT->getInput(), FT->getResult(),
                                          attrs.isAutoClosure(),
                                          attrs.isObjCBlock(),
                                          attrs.isThin(),
                                          attrs.hasCC()
                                           ? attrs.getAbstractCC()
                                           : AbstractCC::Freestanding,
                                          Context);
      SourceRange resultRange = { attrs.LSquareLoc,
                                  result.getSourceRange().End };
      result = { resultType, resultRange };
    }
    attrs.AutoClosure = false;
    attrs.ObjCBlock = false;
    attrs.Thin = false;
    attrs.cc = Nothing;
  }
  
  // FIXME: this is lame.
  if (!attrs.empty())
    diagnose(attrs.LSquareLoc, diag::attribute_does_not_apply_to_type);

  return false;
}

bool Parser::parseType(TypeLoc &Result) {
  return parseType(Result, diag::expected_type);
}

/// parseType
///   type:
///     type-simple
///     type-function
///     type-array
///
///   type-function:
///     type-tuple '->' type 
///
///   type-simple:
///     type-identifier
///     type-tuple
///     type-composition
///
bool Parser::parseType(TypeLoc &Result, Diag<> MessageID) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::kw_This:
  case tok::identifier:
    if (parseTypeIdentifier(Result))
      return true;
    break;
  case tok::kw_protocol:
    if (parseTypeComposition(Result))
      return true;
    break;
  case tok::l_paren: {
    if (parseTypeTupleBody(Result))
      return true;
    break;
  }
  default:
    diagnose(Tok.getLoc(), MessageID);
    return true;
  }

  // '.metatype' still leaves us with type-simple.
  while ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
         peekToken().is(tok::kw_metatype)) {
    consumeToken();
    SourceLoc metatypeLoc = consumeToken(tok::kw_metatype);

    Type metatypeType = MetaTypeType::get(Result.getType(), Context);

    SourceRange metatypeTypeRange{ Result.getSourceRange().Start,
                                   metatypeLoc };
    Result = { metatypeType, metatypeTypeRange };
  }

  // Handle type-function if we have an arrow.
  if (consumeIf(tok::arrow)) {
    TypeLoc SecondHalf;
    if (parseType(SecondHalf,
                  diag::expected_type_function_result))
      return true;
    Type FnType = FunctionType::get(Result.getType(), SecondHalf.getType(),
                                    Context);
    SourceRange FnTypeRange{ Result.getSourceRange().Start,
                             SecondHalf.getSourceRange().End };
    Result = { FnType, FnTypeRange };
    return false;
  }

  // If there is a square bracket without a newline, we have an array.
  if (Tok.isFollowingLSquare())
    return parseTypeArray(Result);
  
  return false;
}

bool Parser::parseGenericArguments(MutableArrayRef<TypeLoc> &Args,
                                   SourceLoc &LAngleLoc,
                                   SourceLoc &RAngleLoc) {
  // Parse the opening '<'.
  assert(startsWithLess(Tok) && "Generic parameter list must start with '<'");
  LAngleLoc = consumeStartingLess();

  SmallVector<TypeLoc, 4> GenericArgs;
  
  do {
    TypeLoc Result;
    if (parseType(Result, diag::expected_type)) {
      // Skip until we hit the '>'.
      skipUntilAnyOperator();
      if (startsWithGreater(Tok))
        consumeStartingGreater();
      return true;
    }

    GenericArgs.push_back(Result);
    // Parse the comma, if the list continues.
  } while (consumeIf(tok::comma));

  if (!startsWithGreater(Tok)) {
    diagnose(Tok.getLoc(), diag::expected_rangle_generic_arg_list);
    diagnose(LAngleLoc, diag::opening_angle);

    // Skip until we hit the '>'.
    skipUntilAnyOperator();
    if (startsWithGreater(Tok))
      RAngleLoc = consumeStartingGreater();
    return true;
  } else {
    RAngleLoc = consumeStartingGreater();
  }

  Args = Context.AllocateCopy(GenericArgs);
  return false;
}

/// parseTypeIdentifier
///   
///   type-identifier:
///     identifier generic-args? ('.' identifier generic-args?)*
///
bool Parser::parseTypeIdentifier(TypeLoc &Result) {
  SourceLoc StartLoc = Tok.getLoc();
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_This)) {
    diagnose(Tok.getLoc(), diag::expected_identifier_for_type);
    return true;
  }

  SmallVector<IdentifierType::Component, 4> Components;
  SourceLoc EndLoc;
  while (true) {
    SourceLoc Loc;
    Identifier Name;
    if (parseIdentifier(Name, Loc, diag::expected_identifier_in_dotted_type))
      return true;
    SourceLoc LAngle, RAngle;
    MutableArrayRef<TypeLoc> GenericArgs;
    if (startsWithLess(Tok)) {
      if (parseGenericArguments(GenericArgs, LAngle, RAngle))
        return true;
    }
    Components.push_back(IdentifierType::Component(Loc, Name, GenericArgs,
                                                   CurDeclContext));
    EndLoc = Loc;

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'metatype' or '('.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        (peekToken().isNot(tok::kw_metatype) && 
         peekToken().isNot(tok::l_paren))) {
      consumeToken();
    } else {
      break;
    }
  }

  // Lookup element #0 through our current scope chains in case it is some thing
  // local (this returns null if nothing is found).
  if (auto Entry = ScopeInfo.lookupValueName(Components[0].Id))
    Components[0].setValue(Entry);

  auto Ty = IdentifierType::getNew(Context, Components);
  Result = { Ty, SourceRange(StartLoc, EndLoc) };
  return false;
}

/// parseTypeComposition
///   
///   type-composition:
///     'protocol' '<' type-composition-list? '>'
///
///   type-composition-list:
///     type-identifier (',' type-identifier)*
///
bool Parser::parseTypeComposition(TypeLoc &Result) {
  SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
 
  // Check for the starting '<'.
  if (!startsWithLess(Tok)) {
    diagnose(Tok.getLoc(), diag::expected_langle_protocol);
    return true;
  }
  SourceLoc LAngleLoc = consumeStartingLess();
  
  // Check for empty protocol composition.
  if (startsWithGreater(Tok)) {
    SourceLoc RAngleLoc = consumeStartingGreater();
    Type ResultType = ProtocolCompositionType::get(Context, ArrayRef<Type>());
    Result = { ResultType, SourceRange(ProtocolLoc, RAngleLoc) };
    return false;
  }
  
  // Parse the type-composition-list.
  bool Invalid = false;
  SmallVector<TypeLoc, 4> Protocols;
  do {
    // Parse the type-identifier.
    TypeLoc Protocol;
    if (parseTypeIdentifier(Protocol)) {
      Invalid = true;
      break;
    }
    
    Protocols.push_back(Protocol);
  } while (consumeIf(tok::comma));
  
  // Check for the terminating '>'.
  SourceLoc EndLoc = Tok.getLoc();
  if (!startsWithGreater(Tok)) {
    if (!Invalid) {
      diagnose(Tok.getLoc(), diag::expected_rangle_protocol);
      diagnose(LAngleLoc, diag::opening_angle);
    }

    // Skip until we hit the '>'.
    skipUntilAnyOperator();
    if (startsWithGreater(Tok))
      EndLoc = consumeStartingGreater();    
  } else {
    EndLoc = consumeStartingGreater();
  }

  SmallVector<Type, 4> ProtocolTypes;
  for (TypeLoc T : Protocols)
    ProtocolTypes.push_back(T.getType());
  Result =  { ProtocolCompositionType::get(Context, ProtocolTypes),
              SourceRange(ProtocolLoc, EndLoc) };
  return false;
}

/// parseTypeTupleBody
///   type-tuple:
///     lparen-any type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)* '...'?
///   type-tuple-element:
///     identifier value-specifier
///     type-annotation
bool Parser::parseTypeTupleBody(TypeLoc &Result) {
  SourceLoc RPLoc, LPLoc = consumeToken(tok::l_paren);
  SourceLoc EllipsisLoc;
  SmallVector<TupleTypeElt, 8> Elements;
  bool HadExpr = false;
  bool HadEllipsis = false;

  bool Invalid = parseList(tok::r_paren, LPLoc, RPLoc,
                           tok::comma, /*OptionalSep=*/false,
                           diag::expected_rparen_tuple_type_list,
                           [&] () -> bool {
    // If the tuple element starts with "ident :" or "ident =", then
    // the identifier is an element tag, and it is followed by a
    // value-specifier.
    if (Tok.is(tok::identifier) &&
        (peekToken().is(tok::colon) || peekToken().is(tok::equal))) {
      Identifier name = Context.getIdentifier(Tok.getText());
      consumeToken(tok::identifier);

      NullablePtr<Expr> init;
      TypeLoc type;
      if (parseValueSpecifier(type, init))
        return true;

      ExprHandle *initHandle = nullptr;
      if (init.isNonNull()) {
        Invalid = true;
        initHandle = ExprHandle::get(Context, init.get());
      }
      HadExpr |= init.isNonNull();
      Elements.push_back(TupleTypeElt(type.getType(), name, initHandle));
    } else {
      // Otherwise, this has to be a type.
      TypeLoc type;
      if (parseTypeAnnotation(type))
        return true;
      Elements.push_back(type.getType());
    }
    if (Tok.is(tok::ellipsis)) {
      EllipsisLoc = consumeToken(tok::ellipsis);
      if (Tok.is(tok::r_paren)) {
        HadEllipsis = true;
      } else {
        diagnose(EllipsisLoc, diag::unexpected_ellipsis_in_tuple);
        Invalid = true;
      }
    }
    return false;
  });


  // A "tuple" with one anonymous element is actually not a tuple.
  if (Elements.size() == 1 && !Elements.back().hasName() && !HadEllipsis) {
    assert(!HadExpr && "Only TupleTypes have default values");
    Result = { ParenType::get(Context, Elements.back().getType()),
               SourceRange(LPLoc, Tok.getLoc()) };
    return Invalid;
  }

  if (HadEllipsis) {
    if (Elements.empty()) {
      diagnose(EllipsisLoc, diag::empty_tuple_ellipsis);
      return true;
    }
    if (Elements.back().getInit()) {
      diagnose(EllipsisLoc, diag::tuple_ellipsis_init);
      return true;
    }
    Type BaseTy = Elements.back().getType();
    Type FullTy = ArraySliceType::get(BaseTy, Context);
    Identifier Name = Elements.back().getName();
    ExprHandle *Init = Elements.back().getInit();
    Elements.back() = TupleTypeElt(FullTy, Name, Init, BaseTy);
  }

  TupleType *TT = TupleType::get(Elements, Context)->castTo<TupleType>();
  if (HadExpr)
    TypesWithDefaultValues.emplace_back(TT, CurDeclContext);

  Result = { TT, SourceRange(LPLoc, RPLoc) };
  return Invalid;
}


/// parseTypeArray - Parse the type-array production, given that we
/// are looking at the initial l_square.  Note that this index
/// clause is actually the outermost (first-indexed) clause.
///
///   type-array:
///     type-simple
///     type-array '[' ']'
///     type-array '[' expr ']'
///
bool Parser::parseTypeArray(TypeLoc &result) {
  assert(Tok.isFollowingLSquare());
  SourceLoc lsquareLoc = consumeToken();

  // Handle the [] production, meaning an array slice.
  if (Tok.is(tok::r_square)) {
    SourceLoc rsquareLoc = consumeToken(tok::r_square);

    // If we're starting another square-bracket clause, recur.
    if (Tok.isFollowingLSquare() && parseTypeArray(result))
      return true;

    // Just build a normal array slice type.
    SourceRange arrayRange{ result.getSourceRange().Start, rsquareLoc };
    result = { ArraySliceType::get(result.getType(), Context),
               arrayRange };
    return false;
  }

  // Pipe is not a delimiter within an array bound.
  llvm::SaveAndRestore<bool> pipeIsNotDelimiter(PipeIsDelimiter, false);

  NullablePtr<Expr> sizeEx = parseExpr(diag::expected_expr_array_type);
  if (sizeEx.isNull()) return true;

  SourceLoc rsquareLoc;
  if (parseMatchingToken(tok::r_square, rsquareLoc,
                         diag::expected_rbracket_array_type, lsquareLoc))
    return true;

  // If we're starting another square-bracket clause, recur.
  if (Tok.isFollowingLSquare() && parseTypeArray(result))
    return true;
  
  // FIXME: We don't supported fixed-length arrays yet.
  diagnose(lsquareLoc, diag::unsupported_fixed_length_array)
    .highlight(sizeEx.get()->getSourceRange());
  
  return true;
}

//===--------------------------------------------------------------------===//
// Speculative type list parsing
//===--------------------------------------------------------------------===//

static bool isGenericTypeDisambiguatingToken(Token &tok) {
  switch (tok.getKind()) {
  default:
    return false;
  case tok::r_paren:
  case tok::r_square:
  case tok::l_brace:
  case tok::r_brace:
  case tok::period:
  case tok::comma:
  case tok::semi:
  case tok::eof:
    return true;
  
  case tok::period_prefix:
    // These will be turned into following tokens if they appear unspaced after
    // a generic angle bracket.
    return tok.getText().data()[-1] == '>';
      
  case tok::l_paren:
  case tok::l_square:
    // These only apply to the generic type if they don't start a new line.
    return !tok.isAtStartOfLine();
  }
}

bool Parser::canParseAsGenericArgumentList() {
  if (!Tok.isAnyOperator() || !Tok.getText().equals("<"))
    return false;

  BacktrackingScope backtrack(*this);

  if (canParseGenericArguments())
    return isGenericTypeDisambiguatingToken(Tok);

  return false;
}

bool Parser::canParseGenericArguments() {
  // Parse the opening '<'.
  if (!startsWithLess(Tok))
    return false;
  consumeStartingLess();
  
  do {
    if (!canParseType())
      return false;
    // Parse the comma, if the list continues.
  } while (consumeIf(tok::comma));
  
  if (!startsWithGreater(Tok)) {
    return false;
  } else {
    consumeStartingGreater();
    return true;
  }
}

bool Parser::canParseType() {
  switch (Tok.getKind()) {
  case tok::kw_This:
  case tok::identifier:
    if (!canParseTypeIdentifier())
      return false;
    break;
  case tok::kw_protocol:
    if (!canParseTypeComposition())
      return false;
    break;
  case tok::l_paren: {
    consumeToken();
    if (!canParseTypeTupleBody())
      return false;
    break;
  }
  default:
    return false;
  }
  
  // '.metatype' still leaves us with type-simple.
  while ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
         peekToken().is(tok::kw_metatype)) {
    consumeToken();
    consumeToken(tok::kw_metatype);
  }
  
  // Handle type-function if we have an arrow.
  if (consumeIf(tok::arrow)) {
    if (!canParseType())
      return false;
    return true;
  }
  
  // If there is a square bracket without a newline, we have an array.
  if (Tok.isFollowingLSquare())
    return canParseTypeArray();
  
  return true;
}

bool Parser::canParseTypeIdentifier() {
  if (Tok.isNot(tok::identifier) && Tok.isNot(tok::kw_This)) {
    return false;
  }
  
  while (true) {
    switch (Tok.getKind()) {
#define IDENTIFIER_KEYWORD(kw) case tok::kw_##kw:
#include "swift/Parse/Tokens.def"
    case tok::identifier:
      consumeToken();
      break;
    default:
      return false;
    }
    
    if (startsWithLess(Tok)) {
      if (!canParseGenericArguments())
        return false;
    }

    // Treat 'Foo.<anything>' as an attempt to write a dotted type
    // unless <anything> is 'metatype'.
    if ((Tok.is(tok::period) || Tok.is(tok::period_prefix)) &&
        peekToken().isNot(tok::kw_metatype)) {
      consumeToken();
    } else {
      return true;
    }
  }
}

bool Parser::canParseTypeComposition() {
  consumeToken(tok::kw_protocol);
  
  // Check for the starting '<'.
  if (!startsWithLess(Tok)) {
    return false;
  }
  consumeStartingLess();
  
  // Check for empty protocol composition.
  if (startsWithGreater(Tok)) {
    consumeStartingGreater();
    return true;
  }
  
  // Parse the type-composition-list.
  do {
    if (!canParseTypeIdentifier()) {
      return false;
    }
  } while (consumeIf(tok::comma));
  
  // Check for the terminating '>'.
  if (!startsWithGreater(Tok)) {
    return false;
  }
  consumeStartingGreater();
  
  return true;
}

bool Parser::canParseTypeTupleBody() {
  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace) &&
      Tok.isNot(tok::ellipsis) && !isStartOfDecl(Tok, peekToken())) {
    do {
      // If the tuple element starts with "ident :", then
      // the identifier is an element tag, and it is followed by a
      // value-specifier.
      // FIXME: This doesn't catch type attrs or default initializing
      //   expressions!
      if (Tok.is(tok::identifier) && peekToken().is(tok::colon)) {
        consumeToken(tok::identifier);
        consumeToken(tok::colon);

        if (!canParseType())
          return false;
        continue;
      }
      
      // Otherwise, this has to be a type.
      // FIXME: Type attributes!
      if (!canParseType())
        return false;
    } while (consumeIf(tok::comma));
  }
  
  if (Tok.is(tok::ellipsis)) {
    consumeToken();
  }
  
  if (Tok.is(tok::r_paren)) {
    consumeToken();
    return true;
  } else {
    return false;
  }
}


bool Parser::canParseTypeArray() {
  assert(Tok.isFollowingLSquare());
  consumeToken();
  
  // Handle the [] production, meaning an array slice.
  if (Tok.is(tok::r_square)) {
    consumeToken(tok::r_square);
    
    // If we're starting another square-bracket clause, recur.
    if (Tok.isFollowingLSquare())
      return canParseTypeArray();
    
    return true;
  }
  
  // FIXME: Size expressions!
  return false;
}


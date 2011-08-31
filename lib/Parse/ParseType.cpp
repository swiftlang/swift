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
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

bool Parser::parseType(Type &Result) {
  return parseType(Result, "expected type");
}

/// parseType
///   type:
///     type-simple
///     type-function
///     type-array
///
///   type-function:
///     type-simple '->' type 
///
///   type-simple:
///     '__builtin_int32_type'
///     identifier
///     type-tuple
///     type-oneof
///     type-protocol
///
bool Parser::parseType(Type &Result, const Twine &Message) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::identifier: {
    Identifier Name = Context.getIdentifier(Tok.getText());
    Result = ScopeInfo.lookupOrInsertTypeName(Name, Tok.getLoc());
    consumeToken(tok::identifier);
    break;
  }
  case tok::kw___builtin_float32_type:
    Result = Context.TheFloat32Type;
    consumeToken(tok::kw___builtin_float32_type);
    break;
  case tok::kw___builtin_float64_type:
    Result = Context.TheFloat64Type;
    consumeToken(tok::kw___builtin_float64_type);
    break;
  case tok::kw___builtin_int1_type:
    Result = Context.TheInt1Type;
    consumeToken(tok::kw___builtin_int1_type);
    break;
  case tok::kw___builtin_int8_type:
    Result = Context.TheInt8Type;
    consumeToken(tok::kw___builtin_int8_type);
    break;
  case tok::kw___builtin_int16_type:
    Result = Context.TheInt16Type;
    consumeToken(tok::kw___builtin_int16_type);
    break;
  case tok::kw___builtin_int32_type:
    Result = Context.TheInt32Type;
    consumeToken(tok::kw___builtin_int32_type);
    break;
  case tok::kw___builtin_int64_type:
    Result = Context.TheInt64Type;
    consumeToken(tok::kw___builtin_int64_type);
    break;
  case tok::l_paren:
  case tok::l_paren_space: {
    SMLoc LPLoc = consumeToken();
    if (parseTypeTupleBody(LPLoc, Result))
      return true;

    if (parseToken(tok::r_paren, "expected ')' at end of tuple list",
                   tok::r_paren)) {
      note(LPLoc, "to match this opening '('");
      return true;
    }
    break;
  }
  case tok::kw_oneof:
    if (parseTypeOneOf(Result)) return true;
    break;
  case tok::kw_protocol:
    if (parseTypeProtocol(Result))
      return true;
    break;
  default:
    error(Tok.getLoc(), Message);
    return true;
  }
  
  while (1) {
    // If there is an arrow, parse the rest of the type.
    SMLoc TokLoc = Tok.getLoc();
    if (consumeIf(tok::arrow)) {
      Type SecondHalf;
      if (parseType(SecondHalf, "expected type in result of function type"))
        return true;
      Result = FunctionType::get(Result, SecondHalf, Context);
      continue;
    }
    
    // If there is a square bracket, we have an array.
    if (consumeIf(tok::l_square)) {
      if (parseTypeArray(TokLoc, Result)) return true;
      continue;
    }
    
    break;
  }
        
  
  return false;
}


/// parseTypeTupleBody
///   type-tuple:
///     '(' type-tuple-body? ')'
///   type-tuple-body:
///     type-tuple-element (',' type-tuple-element)*
///   type-tuple-element:
///     identifier value-specifier
///     type ('=' expr)?
bool Parser::parseTypeTupleBody(SMLoc LPLoc, Type &Result) {
  SmallVector<TupleTypeElt, 8> Elements;

  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::r_brace)) {
    bool HadError = false;
    do {
      Elements.push_back(TupleTypeElt());
      TupleTypeElt &Result = Elements.back();

      // If the tuple element starts with "ident :" or "ident =", then
      // the identifier is an element tag, and it is followed by a
      // value-specifier.
      if (Tok.is(tok::identifier) &&
          (peekToken().is(tok::colon) || peekToken().is(tok::equal))) {
        parseIdentifier(Result.Name, "");

        NullablePtr<Expr> Init;
        if ((HadError = parseValueSpecifier(Result.Ty, Init, /*single*/ true)))
          break;
        Result.Init = Init.getPtrOrNull();
        continue;
      }

      // Otherwise, this has to be a type.
      if ((HadError = parseType(Result.Ty)))
        break;

      // Parse the optional default value expression.
      if (Tok.is(tok::colon)) {
        ParseResult<Expr> Init =
          parseSingleExpr("expected initializer expression after '='");

        // Die if there was a parse error.
        if (Init) {
          HadError = true;
          break;
        }

        if (!Init.isSemaError()) {
          Result.Init = Init.get();
        }
      }
    } while (consumeIf(tok::comma));
    
    if (HadError) {
      skipUntil(tok::r_paren);
      if (Tok.is(tok::r_paren))
        consumeToken(tok::r_paren);
      return true;
    }
  }
  
  Result = TupleType::get(Elements, Context);
  return false;
}

///   type-oneof:
///     'oneof' attribute-list? oneof-body
///
bool Parser::parseTypeOneOf(Type &Result) {
  SMLoc OneOfLoc = consumeToken(tok::kw_oneof);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);
  
  return parseTypeOneOfBody(OneOfLoc, Attributes, Result);
}


///   oneof-body:
///      '{' oneof-element (',' oneof-element)* '}'
///   oneof-element:
///      identifier
///      identifier ':' type
///
/// If TypeName is specified, it is the type that the constructors should be
/// built with, so that they preserve the name of the oneof decl that contains
/// this.
bool Parser::parseTypeOneOfBody(SMLoc OneOfLoc, const DeclAttributes &Attrs,
                                Type &Result, TypeAliasDecl *TypeName) {
  if (parseToken(tok::l_brace, "expected '{' in oneof type"))
    return true;
  
  SmallVector<OneOfElementInfo, 8> ElementInfos;
  
  // Parse the comma separated list of oneof elements.
  while (Tok.is(tok::identifier)) {
    OneOfElementInfo ElementInfo;
    ElementInfo.Name = Tok.getText();
    ElementInfo.NameLoc = Tok.getLoc();
    ElementInfo.EltType = 0;
    
    consumeToken(tok::identifier);
    
    // See if we have a type specifier for this oneof element.  If so, parse it.
    if (consumeIf(tok::colon) &&
        parseType(ElementInfo.EltType,
                  "expected type while parsing oneof element '" +
                  ElementInfo.Name + "'")) {
      skipUntil(tok::r_brace);
      return true;
    }
    
    ElementInfos.push_back(ElementInfo);
    
    // Require comma separation.
    if (!consumeIf(tok::comma))
      break;
  }
  
  parseToken(tok::r_brace, "expected '}' at end of oneof");
  
  Result = actOnOneOfType(OneOfLoc, Attrs, ElementInfos, TypeName);
  return false;
}

OneOfType *Parser::actOnOneOfType(SMLoc OneOfLoc, const DeclAttributes &Attrs,
                                  ArrayRef<OneOfElementInfo> Elts,
                                  TypeAliasDecl *PrettyTypeName) {
  // No attributes are valid on oneof types at this time.
  if (!Attrs.empty())
    error(Attrs.LSquareLoc, "oneof types are not allowed to have attributes");
  
  llvm::SmallPtrSet<const char *, 16> SeenSoFar;
  SmallVector<OneOfElementDecl *, 16> EltDecls;
  
  // If we have a PrettyTypeName to use, use it.  Otherwise, just assign the
  // constructors a temporary dummy type.
  Type TmpTy = TupleType::getEmpty(Context);
  if (PrettyTypeName)
    TmpTy = PrettyTypeName->getAliasType(Context);
  
  for (const OneOfElementInfo &Elt : Elts) {
    Identifier NameI = Context.getIdentifier(Elt.Name);
    
    // If this was multiply defined, reject it.
    if (!SeenSoFar.insert(NameI.get())) {
      error(Elt.NameLoc, "element named '" + Elt.Name +
            "' defined multiple times");
      // Don't copy this element into NewElements.
      // TODO: QoI: add note for previous definition.
      continue;
    }
    
    Type EltTy = TmpTy;
    if (Type ArgTy = Elt.EltType)
      if (PrettyTypeName)
        EltTy = FunctionType::get(ArgTy, EltTy, Context);
    
    // Create a decl for each element, giving each a temporary type.
    EltDecls.push_back(new (Context) OneOfElementDecl(CurDeclContext,
                                                      Elt.NameLoc, NameI,
                                                      EltTy, Elt.EltType));
  }
  
  OneOfType *Result = OneOfType::getNew(CurDeclContext, OneOfLoc, EltDecls,
                                        Context);
  for (OneOfElementDecl *D : EltDecls)
    D->Context = Result;
  
  if (PrettyTypeName) {
    // If we have a pretty name for this, complete it to its actual type.
    assert(PrettyTypeName->UnderlyingTy.isNull() &&
           "Not an incomplete decl to complete!");
    PrettyTypeName->UnderlyingTy = Result;
  } else {
    // Now that the oneof type is created, we can go back and give proper types
    // to each element decl.
    for (OneOfElementDecl *Elt : EltDecls) {
      Type EltTy = Result;
      // If the OneOf Element takes a type argument, then it is actually a
      // function that takes the type argument and returns the OneOfType.
      if (Type ArgTy = Elt->ArgumentType)
        EltTy = FunctionType::get(ArgTy, EltTy, Context);
      Elt->Ty = EltTy;
    }
  }
  
  return Result;
}

/// parseTypeArray - The l_square has already been consumed.
///   type-array:
///     type '[' ']'
///     type '[' expr ']'
///
bool Parser::parseTypeArray(SMLoc LSquareLoc, Type &Result) {
  // Handle the [] production, and unsized array.
  if (consumeIf(tok::r_square)) {
    if (isa<ErrorType>(Result.getPointer()))
      return Result;
    return ArrayType::get(Result, 0, Context);
  }
  
  ParseResult<Expr> SizeEx;
  if ((SizeEx = parseSingleExpr("expected expression for array type size")))
    return true;
  
  SMLoc RArrayTok = Tok.getLoc();
  if (parseToken(tok::r_square, "expected ']' in array type")) {
    note(LSquareLoc, "to match this '['");
    return true;
  }
  
  // If we had a semantic error on the size or if the base type is invalid,
  // propagate up an error type.
  if (SizeEx.isSemaError() || isa<ErrorType>(Result.getPointer())) {
    Result = ErrorType::get(Context);
    return false;
  }
  
  // Semantic analysis.
  
  Expr *Size = SizeEx.get();
  
  // FIXME: Add real support for evaluating constant expressions for array
  // sizes.
  uint64_t SizeVal;
  if (IntegerLiteralExpr *IL = dyn_cast<IntegerLiteralExpr>(Size)) {
    SizeVal = IL->getValue();
  } else {
    error(Size->getLocStart(), "invalid type size, not a constant");
    return ErrorType::get(Context);
  }
  
  if (SizeVal == 0) {
    error(Size->getLocStart(), "array types must be larger than zero elements");
    return ErrorType::get(Context);
  }
  
  Result = ArrayType::get(Result, SizeVal, Context);
  return false;
}

///   type-protocol:
///      'protocol' attribute-list? '{'
///          protocol-element (',' protocol-element)*
///          '}'
///   protocol-element:
///      TODO.
///
bool Parser::parseTypeProtocol(Type &Result) {
  SMLoc ProtocolLoc = consumeToken(tok::kw_protocol);
  
  DeclAttributes Attributes;
  if (Tok.is(tok::l_square))
    parseAttributeList(Attributes);

  if (!Attributes.empty())
    error(Attributes.LSquareLoc,
          "protocol types are not allowed to have attributes yet");

  Result = ProtocolType::get(Context);
  return false;
}




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
#include "swift/Basic/Diagnostics.h"
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
///     type-identifier
///     type-tuple
///     type-oneof
///     type-protocol
///
///   type-identifier:
///     identifier
///     scope-qualifier identifier
///
bool Parser::parseType(Type &Result, const Twine &Message) {
  // Parse type-simple first.
  switch (Tok.getKind()) {
  case tok::identifier: {
    Identifier Name = Context.getIdentifier(Tok.getText());
    SMLoc NameLoc = Tok.getLoc();
    consumeToken(tok::identifier);

    if (Tok.isNot(tok::coloncolon)) {
      Result = ScopeInfo.lookupOrInsertTypeName(Name, NameLoc);
    } else {
      consumeToken(tok::coloncolon);
      SMLoc Loc2 = Tok.getLoc();
      Identifier Name2;
      if (parseIdentifier(Name2, "expected identifier after '" +
                          Name.str() + "' type"))
        return true;

      Result = ScopeInfo.getQualifiedTypeName(Name, NameLoc, Name2, Loc2);
    }

    break;
  }
  case tok::l_paren:
  case tok::l_paren_space: {
    SMLoc LPLoc = consumeToken();
    if (parseTypeTupleBody(LPLoc, Result))
      return true;

    if (parseToken(tok::r_paren, "expected ')' at end of tuple list",
                   tok::r_paren)) {
      diagnose(LPLoc, diags::opening_paren);
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
///     'oneof' attribute-list oneof-body
///
bool Parser::parseTypeOneOf(Type &Result) {
  SMLoc OneOfLoc = consumeToken(tok::kw_oneof);
  
  DeclAttributes Attributes;
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
  if (parseToken(tok::l_brace, "expected '{' in oneof"))
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
    diagnose(Attrs.LSquareLoc, diags::oneof_attributes);
  
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
      diagnose(Elt.NameLoc, diags::duplicate_oneof_element, Elt.Name);
      
      // FIXME: Do we care enough to make this efficient?
      for (unsigned I = 0, N = EltDecls.size(); I != N; ++I) {
        if (EltDecls[I]->Name == NameI) {
          diagnose(EltDecls[I]->getLocStart(), diags::previous_definition,
                   Elt.Name);
          break;
        }
      }
      
      // Don't copy this element into NewElements.
      continue;
    }
    
    Type EltTy = TmpTy;
    if (Type ArgTy = Elt.EltType)
      if (PrettyTypeName)
        EltTy = FunctionType::get(ArgTy, EltTy, Context);
    
    // Create a decl for each element, giving each a temporary type.
    EltDecls.push_back(new (Context) OneOfElementDecl(Elt.NameLoc, NameI,
                                                      EltTy, Elt.EltType,
                                                      CurDeclContext));
  }
  
  OneOfType *Result = OneOfType::getNew(OneOfLoc, EltDecls, CurDeclContext);
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
    diagnose(LSquareLoc, diags::opening_bracket);
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
    diagnose(Size->getLoc(), diags::non_constant_array);
    return ErrorType::get(Context);
  }
  
  if (SizeVal == 0) {
    diagnose(Size->getLoc(), diags::zero_length_array);
    return ErrorType::get(Context);
  }
  
  Result = ArrayType::get(Result, SizeVal, Context);
  return false;
}

///   type-protocol:
///      'protocol' attribute-list protocol-body
///
bool Parser::parseTypeProtocol(Type &Result) {
  SMLoc ProtocolLoc = consumeToken(tok::kw_protocol);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  return parseTypeProtocolBody(ProtocolLoc, Attributes, Result);
}


///   protocol-body:
///      '{' protocol-element* '}'
///   protocol-element:
///      decl-func
///      decl-var-simple
///      // 'typealias' identifier
///
bool Parser::parseTypeProtocolBody(SMLoc ProtocolLoc, 
                                   const DeclAttributes &Attributes,
                                   Type &Result, TypeAliasDecl *TypeName) {
  // Parse the body.
  if (parseToken(tok::l_brace, "expected '{' in protocol"))
    return true;
  
  SmallVector<ValueDecl*, 8> Elements;
  
  // Parse the list of protocol elements.
  do {
    switch (Tok.getKind()) {
    default:
      diagnose(Tok, diags::expected_protocol_member);
      return true;
    case tok::r_brace:  // End of protocol body.
      break;
        
    case tok::kw_func:
      Elements.push_back(parseDeclFunc(false));
      if (Elements.back() == 0) return true;
      break;
    case tok::kw_var:
      Elements.push_back(parseDeclVarSimple());
      if (Elements.back() == 0) return true;
      break;
    }
  } while (Tok.isNot(tok::r_brace));
  
  consumeToken(tok::r_brace);

  
  // Act on what we've parsed.
  if (!Attributes.empty())
    diagnose(Attributes.LSquareLoc, diags::protocol_attributes);
  
  ProtocolType *NewProto = ProtocolType::getNew(ProtocolLoc, Elements,
                                                CurDeclContext);
  
  TupleTypeElt ThisElt(NewProto, Context.getIdentifier("this"));
  if (TypeName) ThisElt.Ty = TypeName->getAliasType(Context);
  TupleType *ThisTy = TupleType::get(ThisElt, Context);
  
  // Install all of the members of protocol into the protocol's DeclContext, and
  // give each ValueDecl member an implicit "this" argument.
  // FIXME: This isn't quite right, 'this' should be found by name lookup when
  // parsing the body of each of these.
  for (Decl *D : Elements) {
    // Install the right DeclContext.
    D->Context = NewProto;
    
    // Change the type of the value decls to be functions that return whatever
    // they were declared as.
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      VD->Ty = FunctionType::get(ThisTy, VD->Ty, Context);
  }
    
  if (TypeName) {
    // If we have a pretty name for this, complete it to its actual type.
    assert(TypeName->UnderlyingTy.isNull() &&
           "Not an incomplete decl to complete!");
    TypeName->UnderlyingTy = NewProto;
  }

  Result = NewProto;
  return false;
}

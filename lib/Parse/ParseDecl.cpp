//===--- ParseDecl.cpp - Swift Language Parser for Declarations -----------===//
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
// Declaration Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Lexer.h"
#include "Parser.h"
#include "swift/Subsystems.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PathV2.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// parseTranslationUnit - Main entrypoint for the parser.
///   translation-unit:
///     stmt-brace-item*
void Parser::parseTranslationUnit(TranslationUnit *TU) {
  if (TU->ASTStage == TranslationUnit::Parsed) {
    // FIXME: This is a bit messy; need to figure out a better way to deal
    // with memory allocation for TranslationUnit.
    UnresolvedIdentifierTypes.insert(UnresolvedIdentifierTypes.end(),
                                     TU->getUnresolvedIdentifierTypes().begin(),
                                     TU->getUnresolvedIdentifierTypes().end());
  }

  TU->ASTStage = TranslationUnit::Parsing;

  // Prime the lexer.
  consumeToken();
  SourceLoc FileStartLoc;
  if (TU->Body)
    FileStartLoc = TU->Body->getStartLoc();
  else
    FileStartLoc = Tok.getLoc();

  CurDeclContext = TU;
  
  // Parse the body of the file.
  SmallVector<ExprStmtOrDecl, 128> Items;

  // FIXME: Recreating the BraceStmt from scratch for each chunk wastes
  // a bunch of memory.
  if (TU->Body)
    Items.append(TU->Body->getElements().begin(),
                 TU->Body->getElements().end());

  if (Tok.is(tok::r_brace)) {
    diagnose(Tok.getLoc(), diag::extra_rbrace);
    consumeToken();
  }
  
  parseBraceItemList(Items, true);

  // Process the end of the translation unit.
  SourceLoc FileEnd = Tok.getLoc();

  // First thing, we transform the body into a brace expression.
  TU->Body = BraceStmt::create(Context, FileStartLoc, Items, FileEnd);
  
  TU->setUnresolvedIdentifierTypes(
          Context.AllocateCopy(llvm::makeArrayRef(UnresolvedIdentifierTypes)));

  UnresolvedIdentifierTypes.clear();

  // Note that the translation unit is fully parsed and verify it.
  TU->ASTStage = TranslationUnit::Parsed;
  verify(TU);
}

namespace {
#define MAKE_ENUMERATOR(id) id,
  enum class AttrName {
    none,
#define ATTR(X) X,
#include "swift/AST/Attr.def"
  };
}

static AttrName getAttrName(StringRef text) {
  return llvm::StringSwitch<AttrName>(text)
#define ATTR(X) .Case(#X, AttrName::X)
#include "swift/AST/Attr.def"
    .Default(AttrName::none);
}

static Associativity getAssociativity(AttrName attr) {
  switch (attr) {
  case AttrName::infix: return Associativity::None;
  case AttrName::infix_left: return Associativity::Left;
  case AttrName::infix_right: return Associativity::Right;
  default: llvm_unreachable("bad associativity");
  }
}

static Resilience getResilience(AttrName attr) {
  switch (attr) {
  case AttrName::resilient: return Resilience::Resilient;
  case AttrName::fragile: return Resilience::Fragile;
  case AttrName::born_fragile: return Resilience::InherentlyFragile;
  default: llvm_unreachable("bad resilience");
  }
}

/// parseAttribute
///   attribute:
///     'infix' '=' numeric_constant
///     'infix_left' '=' numeric_constant
///     'infix_right' '=' numeric_constant
///     'unary'
bool Parser::parseAttribute(DeclAttributes &Attributes) {
  if (!Tok.is(tok::identifier)) {
    diagnose(Tok, diag::expected_attribute_name);
    skipUntil(tok::r_square);
    return true;
  }

  switch (AttrName attr = getAttrName(Tok.getText())) {
  case AttrName::none:
    diagnose(Tok, diag::unknown_attribute, Tok.getText());
    skipUntil(tok::r_square);
    return true;

  // Infix attributes.
  case AttrName::infix:
  case AttrName::infix_left:
  case AttrName::infix_right: {
    if (Attributes.isInfix())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);

    Associativity Assoc = getAssociativity(attr);

    // The default precedence is 100.
    Attributes.Infix = InfixData(100, Assoc);
    
    if (consumeIf(tok::equal)) {
      SourceLoc PrecLoc = Tok.getLoc();
      StringRef Text = Tok.getText();
      if (!parseToken(tok::integer_literal, diag::expected_precedence_value)){
        long long Value;
        if (Text.getAsInteger(10, Value) || Value > 255 || Value < 0)
          diagnose(PrecLoc, diag::invalid_precedence, Text);
        else
          Attributes.Infix = InfixData(Value, Assoc);
      } else {
        // FIXME: I'd far rather that we describe this in terms of some
        // list structure in the caller. This feels too ad hoc.
        skipUntil(tok::r_square, tok::comma);
      }
    }

    return false;
  }

  // Resilience attributes.
  case AttrName::resilient:
  case AttrName::fragile:
  case AttrName::born_fragile: {
    if (Attributes.Resilience.isValid())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);

    Resilience resil = getResilience(attr);
    
    // TODO: 'fragile' should allow deployment versioning.
    Attributes.Resilience = ResilienceData(resil);
    return false;
  }

  // 'byref' attribute.
  // FIXME: only permit this in specific contexts.
  case AttrName::byref: {
    SourceLoc TokLoc = Tok.getLoc();
    if (Attributes.Byref)
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);

    Attributes.Byref = true;
    Attributes.ByrefImplicit = false;
    Attributes.ByrefHeap = false;

    // Permit "qualifiers" on the byref.
    SourceLoc beginLoc = Tok.getLoc();
    if (consumeIf(tok::l_paren) || consumeIf(tok::l_paren_space)) {
      if (!Tok.is(tok::identifier)) {
        diagnose(Tok, diag::byref_attribute_expected_identifier);
        skipUntil(tok::r_paren);
      } else if (Tok.getText() == "implicit") {
        Attributes.ByrefImplicit = true;
        consumeToken(tok::identifier);
      } else if (Tok.getText() == "heap") {
        Attributes.ByrefHeap = true;
        consumeToken(tok::identifier);
      } else {
        diagnose(Tok, diag::byref_attribute_unknown_qualifier);
        consumeToken(tok::identifier);
      }
      SourceLoc endLoc;
      parseMatchingToken(tok::r_paren, endLoc,
                         diag::byref_attribute_expected_rparen,
                         beginLoc,
                         diag::opening_paren);
    }
    
    // Verify that we're not combining this attribute incorrectly.  Cannot be
    // both byref and auto_closure.
    if (Attributes.isAutoClosure()) {
      diagnose(TokLoc, diag::cannot_combine_attribute, "auto_closure");
      Attributes.AutoClosure = false;
    }
    
    return false;
  }
      
  // FIXME: Only valid on var and tuple elements, not on func's, typealias, etc.
  case AttrName::auto_closure: {
    SourceLoc TokLoc = Tok.getLoc();
    if (Attributes.isAutoClosure())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);
    
    // Verify that we're not combining this attribute incorrectly.  Cannot be
    // both byref and auto_closure.
    if (Attributes.isByref()) {
      diagnose(TokLoc, diag::cannot_combine_attribute, "byref");
      return false;
    }
    
    Attributes.AutoClosure = true;
    return false;
  }
  }
  llvm_unreachable("bad attribute kind");
}

/// parsePresentAttributeList - This is the internal implementation of
/// parseAttributeList, which we expect to be inlined to handle the common case
/// of an absent attribute list.
///   attribute-list:
///     /*empty*/
///     '[' ']'
///     '[' attribute (',' attribute)* ']'
void Parser::parseAttributeListPresent(DeclAttributes &Attributes) {
  assert(Tok.is(tok::l_square) || Tok.is(tok::l_square_space));
  Attributes.LSquareLoc = consumeToken();
  
  // If this is an empty attribute list, consume it and return.
  if (Tok.is(tok::r_square)) {
    Attributes.RSquareLoc = consumeToken(tok::r_square);
    return;
  }
  
  bool HadError = parseAttribute(Attributes);
  while (Tok.is(tok::comma)) {
    consumeToken(tok::comma);
    HadError |= parseAttribute(Attributes);
  }

  Attributes.RSquareLoc = Tok.getLoc();
  if (consumeIf(tok::r_square))
    return;
  
  // Otherwise, there was an error parsing the attribute list.  If we already
  // reported an error, skip to a ], otherwise report the error.
  if (!HadError)
    parseMatchingToken(tok::r_square, Attributes.RSquareLoc,
                       diag::expected_in_attribute_list, 
                       Attributes.LSquareLoc, diag::opening_bracket);
  skipUntil(tok::r_square);
  consumeIf(tok::r_square);
}

/// parseDecl - Parse a single syntactic declaration and return a list of decl
/// ASTs.  This can return multiple results for var decls that bind to multiple
/// values, structs that define a struct decl and a constructor, etc.
///
/// This method returns true on a parser error that requires recovery.
///
///   decl:
///     decl-typealias
///     decl-extension
///     decl-var
///     decl-func
///     decl-oneof
///     decl-struct
///     decl-import
///
bool Parser::parseDecl(SmallVectorImpl<Decl*> &Entries, unsigned Flags) {
  unsigned EntryStart = Entries.size();
  bool HadParseError = false;
  switch (Tok.getKind()) {
  default:
  ParseError:
    diagnose(Tok, diag::expected_decl);
    HadParseError = true;
    break;
  case tok::kw_import:
    Entries.push_back(parseDeclImport());
    break;
  case tok::kw_extension:
    Entries.push_back(parseDeclExtension());
    break;
  case tok::kw_var:
    HadParseError = parseDeclVar(Flags & PD_HasContainerType, Entries);
    break;
  case tok::kw_typealias:
    Entries.push_back(parseDeclTypeAlias());
    break;
  case tok::kw_oneof:
    HadParseError = parseDeclOneOf(Entries);
    break;
  case tok::kw_struct:
    HadParseError = parseDeclStruct(Entries);
    break;
  case tok::kw_protocol:
    Entries.push_back(parseDeclProtocol());
    break;
  case tok::kw_static:
    if (peekToken().isNot(tok::kw_func))
      goto ParseError;
    // FALL THROUGH.
  case tok::kw_func:
    Entries.push_back(parseDeclFunc(Flags & PD_HasContainerType));
    break;
  case tok::kw_subscript:
    HadParseError = parseDeclSubscript(Flags & PD_HasContainerType, Entries);
    break;
  }
  
  // If we got back a null pointer, then a parse error happened.
  if (Entries.empty())
    HadParseError = true;
  else if (Entries.back() == 0) {
    Entries.pop_back();
    HadParseError = true;
  }

  // Validate the new entries.
  for (unsigned i = EntryStart, e = Entries.size(); i != e; ++i) {
    Decl *D = Entries[i];

    // FIXME: Mark decls erroneous.
    if (isa<ImportDecl>(D) && !(Flags & PD_AllowTopLevel))
      diagnose(D->getLocStart(), diag::decl_inner_scope);
    if (isa<VarDecl>(D) && (Flags & PD_DisallowVar) &&
        !cast<VarDecl>(D)->isProperty()) {
      diagnose(D->getLocStart(), diag::disallowed_var_decl);
    } else if (NamedDecl *ND = dyn_cast<NamedDecl>(D)) {
      if (ND->isOperator() && (Flags & PD_DisallowOperators))
        diagnose(ND->getLocStart(), diag::operator_in_decl);
    }
    // FIXME: Diagnose top-level subscript
  }
  
  return HadParseError;
}


/// parseDeclImport - Parse an 'import' declaration, returning null (and doing
/// no token skipping) on error.
///
///   decl-import:
///      'import' attribute-list any-identifier ('.' any-identifier)*
///
Decl *Parser::parseDeclImport() {
  SourceLoc ImportLoc = consumeToken(tok::kw_import);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  SmallVector<std::pair<Identifier, SourceLoc>, 8> ImportPath(1);
  ImportPath.back().second = Tok.getLoc();
  if (parseAnyIdentifier(ImportPath.back().first,
                         diag::decl_expected_module_name))
    return 0;
  
  while (consumeIf(tok::period)) {
    ImportPath.push_back(std::make_pair(Identifier(), Tok.getLoc()));
    if (parseAnyIdentifier(ImportPath.back().first,
                        diag::expected_identifier_in_decl, "import"))
      return 0;
  }
  
  if (!Attributes.empty())
    diagnose(Attributes.LSquareLoc, diag::import_attributes);
  
  return ImportDecl::create(Context, CurDeclContext, ImportLoc, ImportPath);
}


/// parseDeclExtension - Parse an 'extension' declaration.
///   extension:
///    'extension' type-identifier '{' decl* '}'
///
Decl *Parser::parseDeclExtension() {
  SourceLoc ExtensionLoc = consumeToken(tok::kw_extension);

  Type Ty;
  SourceLoc LBLoc, RBLoc;
  if (parseTypeIdentifier(Ty) ||
      parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_oneof_type))
    return 0;
  
  // Parse the body as a series of decls.
  SmallVector<Decl*, 8> MemberDecls;
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    if (parseDecl(MemberDecls,
                  PD_HasContainerType|PD_DisallowVar|PD_DisallowOperators))
      skipUntilDeclRBrace();
  }

  parseMatchingToken(tok::r_brace, RBLoc, diag::expected_rbrace_extension,
                     LBLoc, diag::opening_brace);

  
  return actOnDeclExtension(ExtensionLoc, Ty, MemberDecls);
}

/// actOnDeclExtension - Given a list of declarations in an 'extension',
/// 'struct', 'oneof', etc, create an ExtensionDecl and register them as
/// members.
Decl *Parser::actOnDeclExtension(SourceLoc ExtensionLoc, Type Ty,
                                 ArrayRef<Decl*> MemberDecls) {
  ExtensionDecl *ED = new (Context) ExtensionDecl(ExtensionLoc, Ty,
                                              Context.AllocateCopy(MemberDecls),
                                                  CurDeclContext);
  // Install all of the members into the Extension's DeclContext.
  for (Decl *D : MemberDecls)
    D->setDeclContext(ED);
  
  return ED;
}

/// parseDeclTypeAlias
///   decl-typealias:
///     'typealias' identifier ':' type
TypeAliasDecl *Parser::parseDeclTypeAlias() {
  SourceLoc TypeAliasLoc = consumeToken(tok::kw_typealias);
  
  Identifier Id;
  Type Ty;
  if (parseIdentifier(Id, diag::expected_identifier_in_decl, "typealias") ||
      parseToken(tok::colon, diag::expected_colon_in_typealias) ||
      parseType(Ty, diag::expected_type_in_typealias))
    return 0;

  TypeAliasDecl *TAD =
    new (Context) TypeAliasDecl(TypeAliasLoc, Id, Ty, CurDeclContext,
                                ScopeInfo.isModuleScope());
  ScopeInfo.addToScope(TAD);
  return TAD;
}

static void addVarsToScope(Parser &P, Pattern *Pat,
                           SmallVectorImpl<Decl*> &Decls,
                           DeclAttributes &Attributes) {
  switch (Pat->getKind()) {
  // Recurse into patterns.
  case PatternKind::Tuple:
    for (auto &field : cast<TuplePattern>(Pat)->getFields())
      addVarsToScope(P, field.getPattern(), Decls, Attributes);
    return;
  case PatternKind::Paren:
    return addVarsToScope(P, cast<ParenPattern>(Pat)->getSubPattern(), Decls,
                          Attributes);
  case PatternKind::Typed:
    return addVarsToScope(P, cast<TypedPattern>(Pat)->getSubPattern(), Decls,
                          Attributes);

  // Handle vars.
  case PatternKind::Named: {
    VarDecl *VD = cast<NamedPattern>(Pat)->getDecl();
    VD->setDeclContext(P.CurDeclContext);
    VD->setModuleScope(P.ScopeInfo.isModuleScope());
    if (!VD->hasType())
      VD->setType(UnstructuredDependentType::get(P.Context));
    if (Attributes.isValid())
      VD->getMutableAttrs() = Attributes;

    if (VD->isProperty()) {
      // FIXME: Order of get/set not preserved.
      if (FuncDecl *Get = VD->getGetter()) {
        Get->setDeclContext(P.CurDeclContext);
        Get->setModuleScope(P.ScopeInfo.isModuleScope());
        Decls.push_back(Get);
      }
      if (FuncDecl *Set = VD->getSetter()) {
        Set->setDeclContext(P.CurDeclContext);
        Set->setModuleScope(P.ScopeInfo.isModuleScope());
        Decls.push_back(Set);
      }
    }
    
    Decls.push_back(VD);
    P.ScopeInfo.addToScope(VD);
    return;
  }

  // Handle non-vars.
  case PatternKind::Any:
    return;
  }
  llvm_unreachable("bad pattern kind!");
}

/// clonePattern - Clone the given pattern.
static Pattern *clonePattern(ASTContext &Context, Pattern *Pat);

/// cloneTuplePatternElts - Clone the given tuple pattern elements into the
/// 'to' list.
static void cloneTuplePatternElts(ASTContext &Context, Pattern &From,
                                  SmallVectorImpl<TuplePatternElt> &To) {
  if (TuplePattern *FromTuple = dyn_cast<TuplePattern>(&From)) {
    for (auto &Elt : FromTuple->getFields())
      // FIXME: Do we have to clone the initializer?
      To.push_back(TuplePatternElt(clonePattern(Context, Elt.getPattern()),
                                   Elt.getInit()));
    return;
  }
  
  ParenPattern *FromParen = cast<ParenPattern>(&From);
  To.push_back(TuplePatternElt(clonePattern(Context,
                                            FromParen->getSubPattern())));
}

/// clonePattern - Clone the given pattern.
static Pattern *clonePattern(ASTContext &Context, Pattern *Pat) {
  switch (Pat->getKind()) {
  case PatternKind::Any:
    return new (Context) AnyPattern(cast<AnyPattern>(Pat)->getLoc());
      
  case PatternKind::Named: {
    NamedPattern *Named = cast<NamedPattern>(Pat);
    VarDecl *Var = new (Context) VarDecl(Named->getLoc(),
                                         Named->getBoundName(),
                                         Named->hasType()? Named->getType()
                                                         : Type(),
                                         Named->getDecl()->getDeclContext(),
                                         Named->getDecl()->isModuleScope());
    return new (Context) NamedPattern(Var);
  }
    
  case PatternKind::Paren: {
    ParenPattern *Paren = cast<ParenPattern>(Pat);
    return new (Context) ParenPattern(Paren->getLParenLoc(),
                                      clonePattern(Context,
                                                   Paren->getSubPattern()),
                                      Paren->getRParenLoc());
  }
    
  case PatternKind::Tuple: {
    TuplePattern *Tuple = cast<TuplePattern>(Pat);
    SmallVector<TuplePatternElt, 2> Elts;
    Elts.reserve(Tuple->getNumFields());
    cloneTuplePatternElts(Context, *Tuple, Elts);
    return TuplePattern::create(Context, Tuple->getLParenLoc(), Elts,
                                Tuple->getRParenLoc());
  }
    
  case PatternKind::Typed: {
    TypedPattern *Typed = cast<TypedPattern>(Pat);
    return new (Context) TypedPattern(clonePattern(Context,
                                                   Typed->getSubPattern()),
                                      Typed->getType());
  }
  }
}


/// parseSetGet - Parse a get-set clause, containing a getter and (optionally)
/// a setter.
///
///   get-set:
///      get var-set?
///      set var-get
///
///   get:
///     'get' stmt-brace
///
///   set:
///     'set' set-name? stmt-brace
///
///   set-name:
///     '(' identifier ')'
bool Parser::parseGetSet(bool HasContainerType, Pattern *Indices,
                         Type ElementTy, FuncDecl *&Get, FuncDecl *&Set,
                         SourceLoc &LastValidLoc) {
  if (GetIdent.empty()) {
    GetIdent = Context.getIdentifier("get");
    SetIdent = Context.getIdentifier("set");
  }

  bool Invalid = false;
  Get = 0;
  Set = 0;
  
  while (true) {
    if (!Tok.is(tok::identifier))
      break;
    
    Identifier Id = Context.getIdentifier(Tok.getText());
    
    if (Id == GetIdent) {
      //   get         ::= 'get' stmt-brace
      
      // Have we already parsed a get clause?
      if (Get) {
        diagnose(Tok.getLoc(), diag::duplicate_getset, false);
        diagnose(Get->getLocStart(), diag::previous_getset, false);
        
        // Forget the previous version.
        Get = 0;
      }
      
      SourceLoc GetLoc = consumeToken();
      
      // It's easy to imagine someone writing redundant parentheses here;
      // diagnose this directly.
      if ((Tok.is(tok::l_paren) || Tok.is(tok::l_paren_space)) &&
          peekToken().is(tok::r_paren)) {
        SourceLoc StartLoc = consumeToken();
        SourceLoc EndLoc = consumeToken();
        diagnose(StartLoc, diag::empty_parens_getsetname, false)
          << SourceRange(StartLoc, EndLoc);
      }
      
      // Set up a function declaration for the getter and parse its body.
      
      // Create the parameter list(s) for the getter.
      llvm::SmallVector<Pattern *, 2> Params;
      
      // Add the implicit 'this' to Params, if needed.
      if (HasContainerType)
        Params.push_back(buildImplicitThisParameter());
      
      // Add a no-parameters clause.
      SmallVector<TuplePatternElt, 2> TupleElts;
      if (Indices)
        cloneTuplePatternElts(Context, *Indices, TupleElts);
      Params.push_back(TuplePattern::create(Context, SourceLoc(), TupleElts,
                                            SourceLoc()));
      
      // Getter has type: () -> T.
      Type FuncTy = ElementTy;
      if (buildFunctionSignature(Params, FuncTy)) {
        skipUntilDeclRBrace();
        Invalid = true;
        break;
      }
      
      Scope FnBodyScope(this);
      
      // Start the function.
      FuncExpr *GetFn = actOnFuncExprStart(GetLoc, FuncTy, Params);
      
      // Establish the new context.
      ContextChange CC(*this, GetFn);
      
      NullablePtr<BraceStmt> Body = parseStmtBrace(diag::expected_lbrace_get);
      if (Body.isNull()) {
        GetLoc = SourceLoc();
        skipUntilDeclRBrace();
        Invalid = true;
        break;
      }
      
      GetFn->setBody(Body.get());
      LastValidLoc = Body.get()->getRBraceLoc();
      
      Get = new (Context) FuncDecl(/*StaticLoc=*/SourceLoc(), GetLoc,
                                   Identifier(), FuncTy, GetFn, CurDeclContext,
                                   ScopeInfo.isModuleScope());
      continue;
    }
    
    if (Id != SetIdent) {
      diagnose(Tok.getLoc(), diag::expected_getset);
      skipUntilDeclRBrace();
      Invalid = true;
      break;
    }
    
    //   var-set         ::= 'set' var-set-name? stmt-brace
    
    // Have we already parsed a var-set clause?
    if (Set) {
      diagnose(Tok.getLoc(), diag::duplicate_getset, true);
      diagnose(Set->getLocStart(), diag::previous_getset, true);
      
      // Forget the previous setter.
      Set = 0;
    }
    
    SourceLoc SetLoc = consumeToken();
    
    //   var-set-name    ::= '(' identifier ')'
    Identifier SetName;
    SourceLoc SetNameLoc;
    SourceRange SetNameParens;
    if (Tok.is(tok::l_paren) || Tok.is(tok::l_paren_space)) {
      SourceLoc StartLoc = consumeToken();
      if (Tok.is(tok::identifier)) {
        // We have a name.
        SetName = Context.getIdentifier(Tok.getText());
        SetNameLoc = consumeToken();
        
        // Look for the closing ')'.
        SourceLoc EndLoc;
        if (parseMatchingToken(tok::r_paren, EndLoc,
                               diag::expected_rparen_setname,
                               StartLoc, diag::opening_paren))
          EndLoc = SetNameLoc;
        SetNameParens = SourceRange(StartLoc, EndLoc);
      } else if (Tok.is(tok::r_paren)) {
        diagnose(StartLoc, diag::empty_parens_getsetname, true)
        << SourceRange(StartLoc, consumeToken());
      } else {
        diagnose(Tok.getLoc(), diag::expected_setname);
        skipUntil(tok::r_paren, tok::l_brace);
        if (Tok.is(tok::r_paren))
          consumeToken();
      }
    }
    
    // Set up a function declaration for the setter and parse its body.
    
    // Create the parameter list(s) for the setter.
    llvm::SmallVector<Pattern *, 2> Params;
    
    // Add the implicit 'this' to Params, if needed.
    if (HasContainerType)
      Params.push_back(buildImplicitThisParameter());
    
    // Add the parameter. If no name was specified, the name defaults to
    // 'value'.
    if (SetName.empty())
      SetName = Context.getIdentifier("value");
    
    {
      SmallVector<TuplePatternElt, 2> TupleElts;
      if (Indices)
        cloneTuplePatternElts(Context, *Indices, TupleElts);
      
      VarDecl *Value = new (Context) VarDecl(SetNameLoc, SetName, ElementTy,
                                             CurDeclContext,
                                             /*IsModuleScope=*/false);
      
      Pattern *ValuePattern
        = new (Context) TypedPattern(new (Context) NamedPattern(Value),
                                     ElementTy);
      
      TupleElts.push_back(TuplePatternElt(ValuePattern, /*Init=*/nullptr));
      Pattern *ValueParamsPattern
      = TuplePattern::create(Context, SetNameParens.Start, TupleElts,
                             SetNameParens.End);
      Params.push_back(ValueParamsPattern);
    }
    
    // Getter has type: (value : T) -> ()
    Type FuncTy = TupleType::getEmpty(Context);
    if (buildFunctionSignature(Params, FuncTy)) {
      skipUntilDeclRBrace();
      Invalid = true;
      break;
    }
    
    Scope FnBodyScope(this);
    
    // Start the function.
    FuncExpr *SetFn = actOnFuncExprStart(SetLoc, FuncTy, Params);
    
    // Establish the new context.
    ContextChange CC(*this, SetFn);
    
    // Parse the body.
    NullablePtr<BraceStmt> Body = parseStmtBrace(diag::expected_lbrace_set);
    if (Body.isNull()) {
      skipUntilDeclRBrace();
      Invalid = true;
      break;
    }
    
    SetFn->setBody(Body.get());
    LastValidLoc = Body.get()->getRBraceLoc();
    
    Set = new (Context) FuncDecl(/*StaticLoc=*/SourceLoc(), SetLoc,
                                 Identifier(), FuncTy, SetFn, CurDeclContext,
                                 ScopeInfo.isModuleScope());
  }
  
  return Invalid;
}

/// parseDeclVarGetSet - Parse the brace-enclosed getter and setter for a variable.
///
///   decl-var:
///      'var' attribute-list identifier : type-annotation { get-set }
void Parser::parseDeclVarGetSet(Pattern &pattern, bool hasContainerType) {
  assert(!GetIdent.empty() && "No 'get' identifier?");
  assert(!SetIdent.empty() && "No 'set' identifier?");
  bool Invalid = false;
    
  // The grammar syntactically requires a simple identifier for the variable
  // name. Complain if that isn't what we got.
  VarDecl *PrimaryVar = 0;
  {
    Pattern *PrimaryPattern = &pattern;
    if (TypedPattern *Typed = dyn_cast<TypedPattern>(PrimaryPattern))
      PrimaryPattern = Typed->getSubPattern();
    if (NamedPattern *Named = dyn_cast<NamedPattern>(PrimaryPattern)) {
      PrimaryVar = Named->getDecl();
    }
  }

  if (!PrimaryVar)
    diagnose(pattern.getLoc(), diag::getset_nontrivial_pattern);

  // The grammar syntactically requires a type annotation. Complain if
  // our pattern does not have one.
  Type Ty;
  if (pattern.hasType())
    Ty = pattern.getType();
  else {
    if (PrimaryVar)
      diagnose(pattern.getLoc(), diag::getset_missing_type);
    Ty = ErrorType::get(Context);
  }
  
  SourceLoc LBLoc = consumeToken(tok::l_brace);
    
  // Parse getter and setter.
  FuncDecl *Get = 0;
  FuncDecl *Set = 0;
  SourceLoc LastValidLoc = LBLoc;
  if (parseGetSet(hasContainerType, /*Indices=*/0, Ty, Get, Set, LastValidLoc))
    Invalid = true;
  
  // Parse the final '}'.
  SourceLoc RBLoc;
  if (Invalid) {
    skipUntilDeclRBrace();
    RBLoc = LastValidLoc;
  } else if (parseMatchingToken(tok::r_brace, RBLoc,
                                diag::expected_rbrace_in_getset,
                                LBLoc, diag::opening_brace)) {
    RBLoc = LastValidLoc;
  }
  
  if (Set && !Get) {
    if (!Invalid)
      diagnose(Set->getLocStart(), diag::var_set_without_get);
    
    Set = nullptr;
    Invalid = true;
  }

  // If things went well, turn this variable into a property.
  if (!Invalid && PrimaryVar && (Set || Get))
    PrimaryVar->setProperty(Context, LBLoc, Get, Set, RBLoc);
}

/// parseDeclVar - Parse a 'var' declaration, returning true (and doing no
/// token skipping) on error.
///
///   decl-var:
///      'var' attribute-list pattern initializer?
///      'var' attribute-list identifier : type-annotation { get-set }
bool Parser::parseDeclVar(bool hasContainerType, SmallVectorImpl<Decl*> &Decls){
  SourceLoc VarLoc = consumeToken(tok::kw_var);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  NullablePtr<Pattern> pattern = parsePattern();
  if (pattern.isNull()) return true;

  // If we syntactically match the second decl-var production, with a
  // var-get-set clause, parse the var-get-set clause.
  bool HasGetSet = false;
  if (Tok.is(tok::l_brace)) {
    // Check whether the next token is 'get' or 'set'.
    const Token &NextTok = peekToken();
    if (NextTok.is(tok::identifier)) {
      Identifier Name = Context.getIdentifier(NextTok.getText());
      
      // Get the identifiers for both 'get' and 'set'.
      if (GetIdent.empty()) {
        GetIdent = Context.getIdentifier("get");
        SetIdent = Context.getIdentifier("set");
      }
      if (Name == GetIdent || Name == SetIdent) {
        parseDeclVarGetSet(*pattern.get(), hasContainerType);
        HasGetSet = true;
      }
    }
  }
  
  Type Ty;
  NullablePtr<Expr> Init;
  if (consumeIf(tok::equal)) {
    Init = parseExpr(diag::expected_initializer_expr);
    if (Init.isNull())
      return true;
    
    if (HasGetSet) {
      diagnose(pattern.get()->getLoc(), diag::getset_init)
        << Init.get()->getSourceRange();
      Init = nullptr;
    }
  }
  
  addVarsToScope(*this, pattern.get(), Decls, Attributes);

  PatternBindingDecl *PBD =
      new (Context) PatternBindingDecl(VarLoc, pattern.get(),
                                       Init.getPtrOrNull(), CurDeclContext);
  Decls.push_back(PBD);
  return false;
}

/// parseDeclVarSimple - This just parses a reduced case of decl-var.
/// FIXME: This is only used by protocol elements.  It seems that there should
/// be a better way to handle these.
///
///   decl-var-simple:
///      'var' identifier ':' type-annotation
///
VarDecl *Parser::parseDeclVarSimple() {
  SourceLoc VarLoc = consumeToken(tok::kw_var);

  if (Tok.isNot(tok::identifier))
    diagnose(Tok, diag::expected_lparen_var_name);

  Identifier ident = Context.getIdentifier(Tok.getText());
  consumeToken();

  if (!consumeIf(tok::colon)) {
    diagnose(Tok, diag::expected_type_or_init);
    return 0;
  }

  Type Ty;
  if (parseTypeAnnotation(Ty, diag::expected_type))
    return 0;

  return new (Context) VarDecl(VarLoc, ident, Ty, CurDeclContext,
                               ScopeInfo.isModuleScope());
}

/// addImplicitThisParameter - Add an implicit 'this' parameter to the given
/// set of parameter clauses.
Pattern *Parser::buildImplicitThisParameter() {
  VarDecl *D
    = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("this"),
                            Type(), CurDeclContext, /*isModuleScope*/false);
  Pattern *P = new (Context) NamedPattern(D);
  return new (Context) TypedPattern(P, UnstructuredDependentType::get(Context));
}

/// parseDeclFunc - Parse a 'func' declaration, returning null on error.  The
/// caller handles this case and does recovery as appropriate.  If AllowScoped
/// is true, we parse both productions.
///
///   decl-func:
///     'static'? 'func' attribute-list any-identifier func-signature 
///               stmt-brace?
///
/// NOTE: The caller of this method must ensure that the token sequence is
/// either 'func' or 'static' 'func'.
///
FuncDecl *Parser::parseDeclFunc(bool hasContainerType) {
  SourceLoc StaticLoc;
  if (Tok.is(tok::kw_static)) {
    StaticLoc = consumeToken(tok::kw_static);

    // Reject 'static' functions at global scope.
    if (!hasContainerType) {
      diagnose(Tok, diag::static_func_decl_global_scope);
      StaticLoc = SourceLoc();
    }
  }
  
  SourceLoc FuncLoc = consumeToken(tok::kw_func);

  DeclAttributes Attributes;
  // FIXME: Implicitly add immutable attribute.
  parseAttributeList(Attributes);

  Identifier Name;
  if (parseAnyIdentifier(Name, diag::expected_identifier_in_decl, "func"))
    return 0;

  // We force first type of a func declaration to be a tuple for consistency.
  if (Tok.isNot(tok::l_paren) && Tok.isNot(tok::l_paren_space)) {
    diagnose(Tok, diag::func_decl_without_paren);
    return 0;
  }

  SmallVector<Pattern*, 8> Params;

  // If we're within a container and this isn't a static method, add an
  // implicit first pattern to match the container type as an element
  // named 'this'.  This turns "(int)->int" on FooTy into "(this :
  // [byref] FooTy)->((int)->int)".  Note that we can't actually compute the
  // type here until Sema.
  if (hasContainerType && !StaticLoc.isValid())
    Params.push_back(buildImplicitThisParameter());

  Type FuncTy;
  if (parseFunctionSignature(Params, FuncTy))
    return 0;
  
  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  FuncExpr *FE = 0;
  {
    Scope FnBodyScope(this);
    
    FE = actOnFuncExprStart(FuncLoc, FuncTy, Params);

    // Establish the new context.
    ContextChange CC(*this, FE);
    
    // Then parse the expression.
    NullablePtr<Stmt> Body;
    
    // Check to see if we have a "{" to start a brace statement.
    if (Tok.is(tok::l_brace)) {
      NullablePtr<BraceStmt> Body = parseStmtBrace(diag::invalid_diagnostic);
      if (Body.isNull())
        FE = 0; // FIXME: Should do some sort of error recovery here.
      else
        FE->setBody(Body.get());
      
    } else {
      // Note, we just discard FE here.  It is bump pointer allocated, so this
      // is fine (if suboptimal).
      FE = 0;
    }
  }
  
  // Create the decl for the func and add it to the parent scope.
  FuncDecl *FD = new (Context) FuncDecl(StaticLoc, FuncLoc, Name,
                                        FuncTy, FE, CurDeclContext,
                                        ScopeInfo.isModuleScope());
  if (Attributes.isValid()) FD->getMutableAttrs() = Attributes;
  ScopeInfo.addToScope(FD);
  return FD;
}

/// parseDeclOneOf - Parse a 'oneof' declaration, returning true (and doing no
/// token skipping) on error.
///
///   decl-oneof:
///      'oneof' attribute-list identifier oneof-body
///   oneof-body:
///      '{' oneof-element (',' oneof-element)* decl* '}'
///   oneof-element:
///      identifier
///      identifier ':' type-annotation
///      
bool Parser::parseDeclOneOf(SmallVectorImpl<Decl*> &Decls) {
  SourceLoc OneOfLoc = consumeToken(tok::kw_oneof);

  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  SourceLoc NameLoc = Tok.getLoc();
  Identifier OneOfName;
  if (parseIdentifier(OneOfName, diag::expected_identifier_in_decl, "oneof"))
    return true;
  
  TypeAliasDecl *TAD =
    new (Context) TypeAliasDecl(NameLoc, OneOfName, Type(), CurDeclContext,
                                ScopeInfo.isModuleScope());
  Decls.push_back(TAD);

  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_oneof_type))
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
        parseTypeAnnotation(ElementInfo.EltType,
                            diag::expected_type_oneof_element)) {
      skipUntil(tok::r_brace);
      return true;
    }
    
    ElementInfos.push_back(ElementInfo);
    
    // Require comma separation.
    if (!consumeIf(tok::comma))
      break;
  }

  OneOfType *Result = actOnOneOfType(OneOfLoc, Attributes, ElementInfos, TAD);

  // Parse the body as a series of decls.
  SmallVector<Decl*, 8> MemberDecls;
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    if (parseDecl(MemberDecls,
                  PD_HasContainerType|PD_DisallowVar|PD_DisallowOperators))
      skipUntilDeclRBrace();
  }
  
  parseMatchingToken(tok::r_brace, RBLoc, diag::expected_rbrace_oneof_type,
                     LBLoc, diag::opening_brace);
  
  // If there were members, create an 'extension' to hold them.
  if (!MemberDecls.empty())
    Decls.push_back(actOnDeclExtension(SourceLoc(), Result, MemberDecls));
  
  return false;
}

/// actOnOneOfType - Generate a oneof type and wire it into the scope tree.
/// This is functionality shared by the different sugared forms of oneof types.
///
OneOfType *Parser::actOnOneOfType(SourceLoc OneOfLoc,
                                  const DeclAttributes &Attrs,
                                  ArrayRef<OneOfElementInfo> Elts,
                                  TypeAliasDecl *PrettyTypeName) {
  // No attributes are valid on oneof types at this time.
  if (!Attrs.empty())
    diagnose(Attrs.LSquareLoc, diag::oneof_attributes);
  
  llvm::SmallPtrSet<const char *, 16> SeenSoFar;
  SmallVector<OneOfElementDecl *, 16> EltDecls;
  
  // If we have a PrettyTypeName to use, use it.  Otherwise, just assign the
  // constructors a temporary dummy type.
  Type AliasTy = PrettyTypeName->getAliasType();
  
  for (const OneOfElementInfo &Elt : Elts) {
    Identifier NameI = Context.getIdentifier(Elt.Name);
    
    // If this was multiply defined, reject it.
    if (!SeenSoFar.insert(NameI.get())) {
      diagnose(Elt.NameLoc, diag::duplicate_oneof_element, Elt.Name);
      
      // FIXME: Do we care enough to make this efficient?
      for (unsigned I = 0, N = EltDecls.size(); I != N; ++I) {
        if (EltDecls[I]->getName() == NameI) {
          diagnose(EltDecls[I]->getLocStart(), diag::previous_definition,
                   NameI);
          break;
        }
      }
      
      // Don't copy this element into NewElements.
      continue;
    }
    
    Type EltTy = AliasTy;
    if (Type ArgTy = Elt.EltType)
      EltTy = FunctionType::get(ArgTy, EltTy, Context);
    
    // Create a decl for each element, giving each a temporary type.
    EltDecls.push_back(new (Context) OneOfElementDecl(Elt.NameLoc, NameI,
                                                  EltTy, Elt.EltType,
                                                  CurDeclContext,
                                                  ScopeInfo.isModuleScope()));
  }
  
  OneOfType *Result = OneOfType::getNew(OneOfLoc, EltDecls, PrettyTypeName);
  for (OneOfElementDecl *D : EltDecls)
    D->setDeclContext(Result);
  
  // Complete the type alias to its actual type.
  PrettyTypeName->setUnderlyingType(Result);
  
  // Inject the type name into the containing scope so that it can be found as a
  // metatype.
  ScopeInfo.addToScope(PrettyTypeName);

  return Result;
}


/// parseDeclStruct - Parse a 'struct' declaration, returning null (and doing no
/// token skipping) on error.  A 'struct' is just syntactic sugar for a oneof
/// with a single element.
///
///   decl-struct:
///      'struct' attribute-list identifier '{' decl-struct-body '}
///   decl-struct-body:
///      type-tuple-body? decl*
///
bool Parser::parseDeclStruct(SmallVectorImpl<Decl*> &Decls) {
  SourceLoc StructLoc = consumeToken(tok::kw_struct);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  Identifier StructName;
  SourceLoc LBLoc, RBLoc;
  if (parseIdentifier(StructName, diag::expected_identifier_in_decl, "struct")||
      parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_struct))
    return true;

  // Get the TypeAlias for the name that we'll eventually have.
  TypeAliasDecl *TAD =
    new (Context) TypeAliasDecl(StructLoc, StructName, Type(), CurDeclContext,
                                ScopeInfo.isModuleScope());

  // Parse elements of the body as a tuple body.
  Type BodyTy;
  if (parseTypeTupleBody(LBLoc, BodyTy))
    return true;
  assert(isa<TupleType>(BodyTy.getPointer()));
  
  // Reject any unnamed members.
  for (auto Elt : BodyTy->castTo<TupleType>()->getFields())
    if (!Elt.hasName()) {
      // FIXME: Mark erroneous, terrible location info.  Probably should just
      // have custom parsing logic instead of reusing type-tuple-body.
      diagnose(LBLoc, diag::struct_unnamed_member);
    }

  
  // Parse the body as a series of decls.
  SmallVector<Decl*, 8> MemberDecls;
  while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
    if (parseDecl(MemberDecls,
                  PD_HasContainerType|PD_DisallowVar|PD_DisallowOperators))
      skipUntilDeclRBrace();
  }
  
  if (parseMatchingToken(tok::r_brace, RBLoc, diag::expected_rbrace_struct,
                         LBLoc, diag::opening_brace))
    return true;
          
  Decls.push_back(TAD);
  
  // The 'struct' is syntactically fine, invoke the semantic actions for the
  // syntactically expanded oneof type.  Struct declarations are just sugar for
  // other existing constructs.
  Parser::OneOfElementInfo ElementInfo;
  ElementInfo.Name = StructName.str();
  ElementInfo.NameLoc = StructLoc;
  ElementInfo.EltType = BodyTy;
  OneOfType *OneOfTy = actOnOneOfType(StructLoc, Attributes, ElementInfo, TAD);
  assert(OneOfTy->isTransparentType() && "Somehow isn't a struct?");
  
  // If there were members, create an 'extension' to hold them.
  if (!MemberDecls.empty())
    Decls.push_back(actOnDeclExtension(StructLoc, OneOfTy, MemberDecls));
  return false;
}


/// parseDeclProtocol - Parse a 'protocol' declaration, returning null (and
/// doing no token skipping) on error.
///
///   decl-protocol:
///      'protocol' attribute-list identifier protocol-body
///      
Decl *Parser::parseDeclProtocol() {
  SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  SourceLoc NameLoc = Tok.getLoc();
  Identifier ProtocolName;
  if (parseIdentifier(ProtocolName,
                      diag::expected_identifier_in_decl, "protocol"))
    return 0;
  
  TypeAliasDecl *TAD =
    new (Context) TypeAliasDecl(NameLoc, ProtocolName, Type(), CurDeclContext,
                                ScopeInfo.isModuleScope());
  if (parseProtocolBody(ProtocolLoc, Attributes, TAD))
    return 0;
  return TAD;
}

///   protocol-body:
///      '{' protocol-member* '}'
///   protocol-member:
///      decl-func
///      decl-var-simple
///      // 'typealias' identifier
///
bool Parser::parseProtocolBody(SourceLoc ProtocolLoc, 
                               const DeclAttributes &Attributes,
                               TypeAliasDecl *TypeName) {
  // Parse the body.
  if (parseToken(tok::l_brace, diag::expected_lbrace_protocol_type))
    return true;
  
  // Parse the list of protocol elements.
  SmallVector<ValueDecl*, 8> Elements;
  do {
    switch (Tok.getKind()) {
    default:
      diagnose(Tok, diag::expected_protocol_member);
      return true;
    case tok::r_brace:  // End of protocol body.
      break;
      
    // FIXME: use standard parseDecl loop.
    case tok::kw_static:
    case tok::kw_func:
      Elements.push_back(parseDeclFunc(Tok.is(tok::kw_func)));
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
    diagnose(Attributes.LSquareLoc, diag::protocol_attributes);
  
  ProtocolType *NewProto = ProtocolType::getNew(ProtocolLoc, Elements,TypeName);
  
  // Install all of the members of protocol into the protocol's DeclContext.
  for (Decl *D : Elements)
    D->setDeclContext(NewProto);
  
  // Complete the pretty name for this type.
  TypeName->setUnderlyingType(NewProto);
  
  return false;
}

/// parseDeclSubscript - Parse a 'subscript' declaration, returning true
/// on error.
///
///   decl-subscript:
///     'subscript' attribute-list pattern-tuple '->' type '{' get-set '}'
///
bool Parser::parseDeclSubscript(bool HasContainerType,
                                SmallVectorImpl<Decl *> &Decls) {
  bool Invalid = false;
  SourceLoc SubscriptLoc = consumeToken(tok::kw_subscript);
  
  // attribute-list
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  // pattern-tuple
  if (!Tok.is(tok::l_paren) && !Tok.is(tok::l_paren_space)) {
    diagnose(Tok.getLoc(), diag::expected_lparen_subscript);
    return true;
  }
  
  NullablePtr<Pattern> Indices = parsePatternTuple();
  if (Indices.isNull())
    return true;
  if (checkFullyTyped(Indices.get()))
    Invalid = true;
  
  // '->'
  if (!Tok.is(tok::arrow)) {
    diagnose(Tok.getLoc(), diag::expected_arrow_subscript);
    return true;
  }
  SourceLoc ArrowLoc = consumeToken();
  
  // type
  Type ElementTy;
  if (parseTypeAnnotation(ElementTy, diag::expected_type_subscript))
    return true;
  if (checkFullyTyped(ElementTy))
    Invalid = true;
  
  // '{'
  if (!Tok.is(tok::l_brace)) {
    diagnose(Tok.getLoc(), diag::expected_lbrace_subscript);
    return true;
  }
  SourceLoc LBLoc = consumeToken();
  
  // Parse getter and setter.
  FuncDecl *Get = 0;
  FuncDecl *Set = 0;
  SourceLoc LastValidLoc = LBLoc;
  if (parseGetSet(HasContainerType, Indices.get(), ElementTy, Get, Set,
                  LastValidLoc))
    Invalid = true;

  // Parse the final '}'.
  SourceLoc RBLoc;
  if (Invalid) {
    skipUntilDeclRBrace();
    RBLoc = LastValidLoc;
  } else if (parseMatchingToken(tok::r_brace, RBLoc,
                                diag::expected_rbrace_in_getset,
                                LBLoc, diag::opening_brace)) {
    RBLoc = LastValidLoc;
  }

  if (Set && !Get) {
    if (!Invalid)
      diagnose(Set->getLocStart(), diag::set_without_get_subscript);
    
    Set = nullptr;
    Invalid = true;
  }
  
  if (!Invalid && (Set || Get)) {
    // FIXME: We should build the declarations even if they are invalid.

    // Build an AST for the subscript declaration.
    SubscriptDecl *Subscript
      = new (Context) SubscriptDecl(SubscriptLoc, Indices.get(), ArrowLoc,
                                    ElementTy, SourceRange(LBLoc, RBLoc),
                                    Get, Set, CurDeclContext);
    Decls.push_back(Subscript);

    // FIXME: Order of get/set not preserved.
    if (Set) {
      Set->setDeclContext(CurDeclContext);
      Set->setModuleScope(ScopeInfo.isModuleScope());
      Set->makeSetter(Subscript);
      Decls.push_back(Set);
    }

    if (Get) {
      Get->setDeclContext(CurDeclContext);
      Get->setModuleScope(ScopeInfo.isModuleScope());
      Get->makeGetter(Subscript);
      Decls.push_back(Get);
    }    
  }
  return Invalid;
}




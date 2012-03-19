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

Identifier Parser::getModuleIdentifier() {
  StringRef moduleName = Buffer->getBufferIdentifier();

  // As a special case, recognize <stdin>.
  if (moduleName == "<stdin>")
    return Context.getIdentifier("stdin");

  // Find the stem of the filename.
  moduleName = llvm::sys::path::stem(moduleName);

  // Complain about non-identifier characters in the module name.
  if (!Lexer::isIdentifier(moduleName)) {
    diagnose(L.getLocForStartOfBuffer(), diag::bad_module_name);
    moduleName = "bad";
  }

  return Context.getIdentifier(moduleName);
}

/// parseTranslationUnit - Main entrypoint for the parser.
///   translation-unit:
///     stmt-brace-item*
TranslationUnit *Parser::parseTranslationUnit() {
  // Prime the lexer.
  consumeToken();
  SourceLoc FileStartLoc = Tok.getLoc();

  TranslationUnit *TU =
    new (Context) TranslationUnit(getModuleIdentifier(), Component, Context);
  CurDeclContext = TU;
  
  // Parse the body of the file.
  SmallVector<ExprStmtOrDecl, 128> Items;
  parseBraceItemList(Items, true);

  // Process the end of the translation unit.
  SourceLoc FileEnd = Tok.getLoc();
  
  // First thing, we transform the body into a brace expression.
  TU->Body = BraceStmt::create(Context, FileStartLoc, Items, FileEnd);
  
  // Turn our little catalog of unresolved types and identifier types into a
  // list on the TranslationUnit, so NameBinding can bind them.
  SmallVector<TypeAliasDecl*, 8> UnresolvedTypeList;
  for (TypeAliasDecl *Decl : UnresolvedTypeNames) {
    if (!Decl->hasUnderlyingType())
      UnresolvedTypeList.push_back(Decl);
  }
  
  TU->setUnresolvedTypes(Context.AllocateCopy(UnresolvedTypeList));
  TU->setUnresolvedIdentifierTypes(
          Context.AllocateCopy(llvm::makeArrayRef(UnresolvedIdentifierTypes)));

  UnresolvedTypeNames.clear();
  UnresolvedIdentifierTypes.clear();

  // Note that the translation unit is fully parsed and verify it.
  TU->ASTStage = TranslationUnit::Parsed;
  verify(TU);
  return TU;
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
  Attributes.LSquareLoc = consumeToken(tok::l_square);
  
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
    HadParseError = parseDeclVar(Entries);
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
  case tok::kw_plus:
    if (peekToken().isNot(tok::kw_func))
      goto ParseError;
    // FALL THROUGH.
  case tok::kw_func:
    Entries.push_back(parseDeclFunc(Flags & PD_HasContainerType));
    break;
  }
  
  // If we got back a null pointer, then a parse error happened.
  if (Entries.back() == 0) {
    Entries.pop_back();
    HadParseError = true;
  }

  // Validate the new entries.
  for (unsigned i = EntryStart, e = Entries.size(); i != e; ++i) {
    Decl *D = Entries[i];

    // FIXME: Mark decls erroneous.
    if ((isa<ImportDecl>(D) || isa<ExtensionDecl>(D)) &&
        !(Flags & PD_AllowTopLevel))
      diagnose(D->getLocStart(), diag::decl_inner_scope);
    if (isa<VarDecl>(D) && (Flags & PD_DisallowVar)) {
      diagnose(D->getLocStart(), diag::disallowed_var_decl);
    } else if (NamedDecl *ND = dyn_cast<NamedDecl>(D)) {
      if (ND->isOperator() && (Flags & PD_DisallowOperators))
        diagnose(ND->getLocStart(), diag::operator_in_decl);
    }
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


/// parseVarName
///   var-name:
///     identifier
///     lparen-any ')'
///     lparen-any var-name (',' var-name)* ')'
bool Parser::parseVarName(DeclVarName &Name) {
  if (Tok.is(tok::oper)) {
    diagnose(Tok, diag::operator_not_func);
    return true;
  }
  
  // Single name case.
  if (Tok.is(tok::identifier)) {
    SourceLoc IdLoc = Tok.getLoc();
    Identifier Id = Context.getIdentifier(Tok.getText());
    consumeToken();
    Name = DeclVarName(Id, IdLoc);
    return false;
  }
  
  if (Tok.isNot(tok::l_paren) && Tok.isNot(tok::l_paren_space)) {
    diagnose(Tok, diag::expected_lparen_var_name);
    return true;
  }
  
  SourceLoc LPLoc = consumeToken();
  
  SmallVector<DeclVarName*, 8> ChildNames;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      DeclVarName *Elt = new (Context) DeclVarName();
      if (parseVarName(*Elt)) return true;
      ChildNames.push_back(Elt);
    } while (consumeIf(tok::comma));
  }

  SourceLoc RPLoc;
  parseMatchingToken(tok::r_paren, RPLoc, diag::expected_rparen_var_name,
                     LPLoc, diag::opening_paren);

  Name = DeclVarName(LPLoc, Context.AllocateCopy(ChildNames), RPLoc);
  return false;
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
    new (Context) TypeAliasDecl(TypeAliasLoc, Id, Ty, CurDeclContext);
  ScopeInfo.addToScope(TAD);
  return TAD;
}


/// AddElementNamesForVarDecl - This recursive function walks a name specifier
/// adding ElementRefDecls for the named subcomponents and checking that types
/// match up correctly.
void Parser::actOnVarDeclName(const DeclVarName *Name,
                              SmallVectorImpl<unsigned> &AccessPath,
                              VarDecl *VD, SmallVectorImpl<Decl*> &Decls) {
  if (Name->isSimple()) {
    // If this is a leaf name, create a ElementRefDecl with the specified
    // access path.
    Type Ty = ElementRefDecl::getTypeForPath(VD->getType(), AccessPath);
    
    // If the type of the path is obviously invalid, diagnose it now and refuse
    // to create the decl.  The most common result here is DependentType, which
    // allows type checking to resolve this later.
    if (Ty.isNull()) {
      diagnose(Name->getLocation(), diag::invalid_index_in_var_name_path,
               Name->getIdentifier(), VD->getType());
      return;
    }
    
    // Create the decl for this name and add it to the current scope.
    ElementRefDecl *ERD =
      new (Context) ElementRefDecl(VD, Name->getLocation(),
                                   Name->getIdentifier(),
                                   Context.AllocateCopy(AccessPath), Ty,
                                   CurDeclContext);
    Decls.push_back(ERD);
    ScopeInfo.addToScope(ERD);
    return;
  }
  
  AccessPath.push_back(0);
  unsigned Index = 0;
  for (auto Element : Name->getElements()) {
    AccessPath.back() = Index++;
    actOnVarDeclName(Element, AccessPath, VD, Decls);
  }
  AccessPath.pop_back();
}

/// parseDeclVar - Parse a 'var' declaration, returning null (and doing no
/// token skipping) on error.
///
///   decl-var:
///      'var' attribute-list var-name value-specifier
bool Parser::parseDeclVar(SmallVectorImpl<Decl*> &Decls) {
  SourceLoc VarLoc = consumeToken(tok::kw_var);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  DeclVarName VarName;
  if (parseVarName(VarName)) return true;
  
  Type Ty;
  NullablePtr<Expr> Init;
  if (parseValueSpecifier(Ty, Init))
    return true;

  if (Ty.isNull())
    Ty = DependentType::get(Context);

  // Note that we enter the declaration into the current scope.  Since var's are
  // not allowed to be recursive, they are entered after its initializer is
  // parsed.  This does mean that stuff like this is different than C:
  //    var x = 1; { var x = x+1; assert(x == 2); }
  if (VarName.isSimple()) {
    VarDecl *VD = new (Context) VarDecl(VarLoc,  VarName.getIdentifier(), Ty,
                                        Init.getPtrOrNull(), CurDeclContext);
    if (Attributes.isValid())
      VD->getMutableAttrs() = Attributes;
    ScopeInfo.addToScope(VD);
    Decls.push_back(VD);
    return false;
  }
  
  // Copy the name into the ASTContext heap.
  DeclVarName *TmpName = new (Context) DeclVarName(VarName);
  VarDecl *VD = new (Context) VarDecl(VarLoc, TmpName, Ty, Init.getPtrOrNull(),
                                      CurDeclContext);
  if (Attributes.isValid())
    VD->getMutableAttrs() = Attributes;
  Decls.push_back(VD);
  
  // If there is a more interesting name presented here, then we need to walk
  // through it and synthesize the decls that reference the var elements as
  // appropriate.
  SmallVector<unsigned, 8> AccessPath;
  actOnVarDeclName(VD->getNestedName(), AccessPath, VD, Decls);
  return false;
}

/// parseDeclVarSimple - This just parses a reduced case of decl-var.
/// FIXME: This is only used by protocol elements.  It seems that there should
/// be a better way to handle these.
///
///   decl-var-simple:
///      'var' attribute-list identifier value-specifier
///
VarDecl *Parser::parseDeclVarSimple() {
  SourceLoc CurLoc = Tok.getLoc();
  SmallVector<Decl*, 2> Decls;
  if (parseDeclVar(Decls)) return 0;
  
  if (Decls.size() == 1)
    if (VarDecl *VD = dyn_cast_or_null<VarDecl>(Decls[0]))
      return VD;
  
  // FIXME: "here" requires a lot more context.
  diagnose(CurLoc, diag::non_simple_var);
  return 0;
}

/// parseDeclFunc - Parse a 'func' declaration, returning null on error.  The
/// caller handles this case and does recovery as appropriate.  If AllowScoped
/// is true, we parse both productions.
///
///   decl-func:
///     'plus'? 'func' attribute-list any-identifier func-signature stmt-brace?
///
/// NOTE: The caller of this method must ensure that the token sequence is
/// either 'func' or 'plus' 'func'.
///
FuncDecl *Parser::parseDeclFunc(bool hasContainerType) {
  SourceLoc PlusLoc;
  if (Tok.is(tok::kw_plus)) {
    PlusLoc = consumeToken(tok::kw_plus);

    // Reject 'plus' functions at global scope.
    if (!hasContainerType) {
      diagnose(Tok, diag::plus_func_decl_global_scope);
      PlusLoc = SourceLoc();
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

  // If we're within a container and this isn't a plus method, add an
  // implicit first pattern to match the container type as an element
  // named 'this'.  This turns "(int)->int" on FooTy into "(this :
  // [byref] FooTy)->((int)->int)".  Note that we can't actually compute the
  // type here until Sema.
  if (hasContainerType && !PlusLoc.isValid()) {
    VarDecl *D =
      new (Context) VarDecl(SourceLoc(), Context.getIdentifier("this"),
                            Type(), nullptr, CurDeclContext);
    Pattern *P = new (Context) NamedPattern(D);
    P = new (Context) TypedPattern(P, Context.TheDependentType);
    Params.push_back(P);
  }
  

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
  FuncDecl *FD = new (Context) FuncDecl(PlusLoc, FuncLoc, Name,
                                        FuncTy, FE, CurDeclContext);
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
    new (Context) TypeAliasDecl(NameLoc, OneOfName, Type(), CurDeclContext);
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
                                                      CurDeclContext));
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
    new (Context) TypeAliasDecl(StructLoc, StructName, Type(), CurDeclContext);

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
    Decls.push_back(actOnDeclExtension(SourceLoc(), OneOfTy, MemberDecls));
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
    new (Context) TypeAliasDecl(NameLoc, ProtocolName, Type(), CurDeclContext);
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
    case tok::kw_plus:
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


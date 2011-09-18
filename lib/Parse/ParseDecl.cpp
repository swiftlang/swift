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

#include "Lexer.h"
#include "Parser.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
using namespace swift;


/// parseTranslationUnit - Main entrypoint for the parser.
///   translation-unit:
///     stmt-brace-item*
TranslationUnit *Parser::parseTranslationUnit() {
  // Prime the lexer.
  consumeToken();
  SMLoc FileStartLoc = Tok.getLoc();

  TranslationUnit *TU =
    new (Context) TranslationUnit(L.getModuleName(), Context);
  CurDeclContext = TU;
  
  // Parse the body of the file.
  SmallVector<ExprStmtOrDecl, 128> Items;
  parseBraceItemList(Items, true);

  // Process the end of the translation unit.
  SMLoc FileEnd = Tok.getLoc();
  
  // First thing, we transform the body into a brace expression.
  ExprStmtOrDecl *NewElements = 
    Context.AllocateCopy<ExprStmtOrDecl>(Items.begin(), Items.end());
  TU->Body = new (Context) BraceStmt(FileStartLoc, NewElements, Items.size(),
                                     FileEnd);
    
  // Do a prepass over the declarations to make sure they have basic sanity and
  // to find the list of top-level value declarations.
  for (unsigned i = 0, e = TU->Body->NumElements; i != e; ++i) {
    if (!TU->Body->Elements[i].is<Decl*>()) continue;
    
    Decl *D = TU->Body->Elements[i].get<Decl*>();
    
    // If any top-level value decl has an unresolved type, then it is erroneous.
    // It is not valid to have something like "var x = 4" at the top level, all
    // types must be explicit here.
    ValueDecl *VD = dyn_cast<ValueDecl>(D);
    if (VD == 0) continue;
    
    // FIXME: This can be better handled in the various ActOnDecl methods when
    // they get passed in a parent context decl.
    
    // Verify that values have a type specified.
    if (false && VD->Ty->is<DependentType>()) {
      error(VD->getLocStart(),
            "top level declarations require a type specifier");
      // FIXME: Should mark the decl as invalid.
      VD->Ty = TupleType::getEmpty(Context);
    }
  }
  
  // Verify that any forward declared types were ultimately defined.
  // TODO: Move this to name binding!
  SmallVector<TypeAliasDecl*, 8> UnresolvedTypeList;
  for (TypeAliasDecl *Decl : ScopeInfo.getUnresolvedTypeList()) {
    if (Decl->UnderlyingTy.isNull())
      UnresolvedTypeList.push_back(Decl);
  }
  
  TU->UnresolvedTypesForParser = Context.AllocateCopy(UnresolvedTypeList);
  TU->UnresolvedScopedTypesForParser =
    Context.AllocateCopy(ScopeInfo.getUnresolvedScopedTypeList());
  return TU;
}

/// parseAttribute
///   attribute:
///     'infix_left' '=' numeric_constant
///     'unary'
bool Parser::parseAttribute(DeclAttributes &Attributes) {
  // infix_left attribute.
  if (Tok.is(tok::identifier) && Tok.getText() == "infix_left") {
    if (Attributes.InfixPrecedence != -1)
      error(Tok.getLoc(), "infix_left precedence repeatedly specified");
    consumeToken(tok::identifier);

    // The default infix_left precedence is 100.
    Attributes.InfixPrecedence = 100;
    
    if (consumeIf(tok::equal)) {
      SMLoc PrecLoc = Tok.getLoc();
      StringRef Text = Tok.getText();
      if (!parseToken(tok::numeric_constant,
                      "expected precedence number in 'infix_left' attribute")) {
        long long Value;
        if (Text.getAsInteger(10, Value) || Value > 255 || Value < 0)
          error(PrecLoc, "invalid precedence: value must be between 0 and 255");
        else
          Attributes.InfixPrecedence = Value;
      }
    }

    return false;
  }
  
  error(Tok.getLoc(), "unknown declaration attribute");
  skipUntil(tok::r_square);
  return true;
}

/// parsePresentAttributeList
///   attribute-list:
///     attribute-list-present?
///
///   attribute-list-present:
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
    parseToken(tok::r_square, "expected ']' or ',' in attribute list",
               tok::r_square);
  else {
    skipUntil(tok::r_square);
    consumeIf(tok::r_square);
  }
}

/// parseDeclImport - Parse an 'import' declaration, returning null (and doing
/// no token skipping) on error.
///
///   decl-import:
///      'import' attribute-list? identifier ('.' identifier)*
///
Decl *Parser::parseDeclImport() {
  SMLoc ImportLoc = consumeToken(tok::kw_import);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  SmallVector<std::pair<Identifier, SMLoc>, 8> ImportPath(1);
  ImportPath.back().second = Tok.getLoc();
  if (parseIdentifier(ImportPath.back().first,
                      "expected module name in import declaration"))
    return 0;
  
  while (consumeIf(tok::period)) {
    ImportPath.push_back(std::make_pair(Identifier(), Tok.getLoc()));
    if (parseIdentifier(ImportPath.back().first,
                        "expected name in import declaration"))
      return 0;
  }
  
  if (!Attributes.empty())
    error(Attributes.LSquareLoc, "invalid attributes specified for import");
  
  return new (Context) ImportDecl(ImportLoc, Context.AllocateCopy(ImportPath),
                                  CurDeclContext);
}


/// parseVarName
///   var-name:
///     identifier
///     '(' ')'
///     '(' name (',' name)* ')'
bool Parser::parseVarName(DeclVarName &Name) {
  // Single name case.
  if (Tok.is(tok::identifier) || Tok.is(tok::oper)) {
    SMLoc IdLoc = Tok.getLoc();
    Identifier Id;
    parseIdentifier(Id, "");
    Name = DeclVarName(Id, IdLoc);
    return false;
  }
  
  if (Tok.isNot(tok::l_paren) && Tok.isNot(tok::l_paren_space)) {
    error(Tok.getLoc(), "expected identifier or '(' in var name");
    return true;
  }
  
  SMLoc LPLoc = consumeToken();
  
  SmallVector<DeclVarName*, 8> ChildNames;
  
  if (Tok.isNot(tok::r_paren)) {
    do {
      DeclVarName *Elt = new (Context) DeclVarName();
      if (parseVarName(*Elt)) return true;
      ChildNames.push_back(Elt);
    } while (consumeIf(tok::comma));
  }

  SMLoc RPLoc = Tok.getLoc();
  if (parseToken(tok::r_paren, "expected ')' at end of var name"))
    note(LPLoc, "to match this '('");

  Name = DeclVarName(LPLoc, Context.AllocateCopy(ChildNames), RPLoc);
  return false;
}


/// parseDeclTypeAlias
///   decl-typealias:
///     'typealias' identifier ':' type
TypeAliasDecl *Parser::parseDeclTypeAlias() {
  SMLoc TypeAliasLoc = consumeToken(tok::kw_typealias);
  
  Identifier Id;
  Type Ty;
  if (parseIdentifier(Id, "expected identifier in var declaration") ||
      parseToken(tok::colon, "expected ':' in typealias declaration") ||
      parseType(Ty, "expected type in var declaration"))
    return 0;

  return ScopeInfo.addTypeAliasToScope(TypeAliasLoc, Id, Ty);
}


/// AddElementNamesForVarDecl - This recursive function walks a name specifier
/// adding ElementRefDecls for the named subcomponents and checking that types
/// match up correctly.
void Parser::actOnVarDeclName(const DeclVarName *Name,
                              SmallVectorImpl<unsigned> &AccessPath,
                              VarDecl *VD,
                              SmallVectorImpl<Parser::ExprStmtOrDecl> &Decls) {
  if (Name->isSimple()) {
    // If this is a leaf name, create a ElementRefDecl with  the specified
    // access path.
    Type Ty = ElementRefDecl::getTypeForPath(VD->Ty, AccessPath);
    
    // If the type of the path is obviously invalid, diagnose it now and refuse
    // to create the decl.  The most common result here is DependentType, which
    // allows type checking to resolve this later.
    if (Ty.isNull()) {
      error(Name->getLocation(), "'" + Name->getIdentifier().str() + 
            "' is an invalid index for '" + VD->Ty->getString() + "'");
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
///      'var' attribute-list? var-name value-specifier
bool Parser::parseDeclVar(SmallVectorImpl<ExprStmtOrDecl> &Decls) {
  SMLoc VarLoc = consumeToken(tok::kw_var);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  DeclVarName VarName;
  if (parseVarName(VarName)) return true;
  
  Type Ty;
  NullablePtr<Expr> Init;
  if (parseValueSpecifier(Ty, Init, /*single*/ false))
    return true;

  if (Ty.isNull())
    Ty = DependentType::get(Context);

  // Note that we enter the declaration into the current scope.  Since var's are
  // not allowed to be recursive, they are entered after its initializer is
  // parsed.  This does mean that stuff like this is different than C:
  //    var x = 1; { var x = x+1; assert(x == 2); }
  if (VarName.isSimple()) {
    VarDecl *VD = new (Context) VarDecl(VarLoc,  VarName.getIdentifier(), Ty,
                                        Init.getPtrOrNull(), Attributes,
                                        CurDeclContext);
    ScopeInfo.addToScope(VD);
    Decls.push_back(VD);
    return false;
  }
  
  // Copy the name into the ASTContext heap.
  DeclVarName *TmpName = new (Context) DeclVarName(VarName);
  VarDecl *VD = new (Context) VarDecl(VarLoc, TmpName, Ty, Init.getPtrOrNull(),
                                      Attributes, CurDeclContext);
  Decls.push_back(VD);
  
  // If there is a more interesting name presented here, then we need to walk
  // through it and synthesize the decls that reference the var elements as
  // appropriate.
  SmallVector<unsigned, 8> AccessPath;
  actOnVarDeclName(VD->NestedName, AccessPath, VD, Decls);
  return false;
}

/// parseDeclVarSimple - This just parses a reduced case of decl-var.
///
///   decl-var-simple:
///      'var' attribute-list? any-identifier value-specifier
///
VarDecl *Parser::parseDeclVarSimple() {
  SMLoc CurLoc = Tok.getLoc();
  SmallVector<ExprStmtOrDecl, 2> Decls;
  if (parseDeclVar(Decls)) return 0;
  
  if (Decls.size() == 1 && Decls[0].is<Decl*>())
    if (Decl *D = Decls[0].get<Decl*>())
      if (VarDecl *VD = dyn_cast<VarDecl>(D))
        return VD;
  
  error(CurLoc, "complex 'var' declaration not allowed here");
  return 0;
}


/// parseDeclFunc - Parse a 'func' declaration, returning null on error.  The
/// caller handles this case and does recovery as appropriate.  If AllowScoped
/// is true, we parse both productions.
///
///   decl-func:
///     'func' attribute-list? identifier type stmt-brace?
///   decl-func-scoped:
///     'func' attribute-list? type-identifier '::' identifier type stmt-brace?
///
FuncDecl *Parser::parseDeclFunc(bool AllowScoped) {
  SMLoc FuncLoc = consumeToken(tok::kw_func);

  DeclAttributes Attributes;
  // FIXME: Implicitly add immutable attribute.
  parseAttributeList(Attributes);

  Type ReceiverTy;
  Identifier Name;
  SMLoc TypeNameLoc = Tok.getLoc();
  if (parseIdentifier(Name, "expected identifier in func declaration"))
    return 0;

  // If this is method syntax, the first name is the receiver type.  Parse the
  // actual function name.
  if (AllowScoped && consumeIf(tok::coloncolon)) {
    // Look up the type name.
    ReceiverTy = ScopeInfo.lookupOrInsertTypeName(Name, TypeNameLoc);
    if (parseIdentifier(Name, "expected identifier in 'func' declaration"))
      return 0;
  }
  
  // We force first type of a func declaration to be a tuple for consistency.
  if (Tok.isNot(tok::l_paren) && Tok.isNot(tok::l_paren_space)) {
    error(Tok.getLoc(), "expected '(' in argument list of func declaration");
    return 0;
  }
    
  Type FuncTy;
  if (parseType(FuncTy))
    return 0;
  
  // If the parsed type is not spelled as a function type (i.e., has no '->' in
  // it), then it is implicitly a function that returns ().
  if (!isa<FunctionType>(FuncTy.getPointer()))
    FuncTy = FunctionType::get(FuncTy, TupleType::getEmpty(Context), Context);
  
  // If a receiver type was specified, install the first type as the receiver,
  // as a tuple with element named 'this'.  This turns "int->int" on FooTy into
  // "(this : FooTy)->(int->int)".
  if (!ReceiverTy.isNull()) {
    TupleTypeElt ReceiverElt(ReceiverTy, Context.getIdentifier("this"));
    FuncTy = FunctionType::get(TupleType::get(ReceiverElt, Context),
                               FuncTy, Context);
  }
  
  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  FuncExpr *FE = 0;
  {
    Scope FnBodyScope(this);
    
    FE = actOnFuncExprStart(FuncLoc, FuncTy);

    // Establish the new context.
    ContextChange CC(*this, FE);
    
    // Then parse the expression.
    NullablePtr<Stmt> Body;
    
    // Check to see if we have a "{" which is a brace expr.
    if (Tok.is(tok::l_brace)) {
      ParseResult<BraceStmt> Body = parseStmtBrace();
      if (Body.isSuccess())
        FE->Body = Body.get();
      else  // FIXME: Should do some sort of error recovery here.
        FE = 0;
      
    } else {
      // Note, we just discard FE here.  It is bump pointer allocated, so this
      // is fine (if suboptimal).
      FE = 0;
    }
  }
  
  // Create the decl for the func and add it to the parent scope.
  FuncDecl *FD = new (Context) FuncDecl(FuncLoc, Name, FuncTy, FE, Attributes,
                                        CurDeclContext);
  ScopeInfo.addToScope(FD);
  return FD;
}

/// parseDeclOneOf - Parse a 'oneof' declaration, returning null (and doing no
/// token skipping) on error.
///
///   decl-oneof:
///      'oneof' attribute-list identifier oneof-body
///      
Decl *Parser::parseDeclOneOf() {
  SMLoc OneOfLoc = consumeToken(tok::kw_oneof);

  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  SMLoc NameLoc = Tok.getLoc();
  Identifier OneOfName;
  Type OneOfType;
  if (parseIdentifier(OneOfName, "expected identifier in oneof declaration"))
    return 0;
  
  TypeAliasDecl *TAD = ScopeInfo.addTypeAliasToScope(NameLoc, OneOfName,Type());
  if (parseTypeOneOfBody(OneOfLoc, Attributes, OneOfType, TAD))
    return 0;
  return TAD;
}


/// parseDeclStruct - Parse a 'struct' declaration, returning null (and doing no
/// token skipping) on error.  A 'struct' is just syntactic sugar for a oneof
/// with a single element.
///
///   decl-struct:
///      'struct' attribute-list identifier { type-tuple-body? }
///
bool Parser::parseDeclStruct(SmallVectorImpl<ExprStmtOrDecl> &Decls) {
  SMLoc StructLoc = consumeToken(tok::kw_struct);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  Identifier StructName;
  if (parseIdentifier(StructName, "expected identifier in struct declaration"))
    return true;

  SMLoc LBLoc = Tok.getLoc();
  if (parseToken(tok::l_brace, "expected '{' in struct"))
    return true;
  
  Type BodyTy;
  if (parseTypeTupleBody(LBLoc, BodyTy)) return true;

  if (parseToken(tok::r_brace, "expected '{' in struct")) {
    note(LBLoc, "to match this opening '{'");
    return true;
  }

  // The type is required to be syntactically a tuple type.
  if (!isa<TupleType>(BodyTy.getPointer())) {
    error(StructLoc, "element type of struct is not a tuple");
    // FIXME: Should set this as an erroroneous decl.
    return true;
  }
          
  // Get the TypeAlias for the name that we'll eventually have.  This ensures
  // that the constructors generated have the pretty name for the type instead
  // of the raw oneof.
  TypeAliasDecl *TAD = ScopeInfo.addTypeAliasToScope(StructLoc, StructName,
                                                     Type());
  Decls.push_back(TAD);
  
  // The 'struct' is syntactically fine, invoke the semantic actions for the
  // syntactically expanded oneof type.  Struct declarations are just sugar for
  // other existing constructs.
  Parser::OneOfElementInfo ElementInfo;
  ElementInfo.Name = StructName.str();
  ElementInfo.NameLoc = StructLoc;
  ElementInfo.EltType = BodyTy;
  OneOfType *OneOfTy = actOnOneOfType(StructLoc, Attributes, ElementInfo, TAD);
  assert(OneOfTy->hasSingleElement() && "Somehow isn't a struct?");
  
  // In addition to defining the oneof declaration, structs also inject their
  // constructor into the global scope.
  assert(OneOfTy->Elements.size() == 1 && "Struct has exactly one element");
  ScopeInfo.addToScope(OneOfTy->getElement(0));
  Decls.push_back(OneOfTy->getElement(0));
  return false;
}


/// parseDeclProtocol - Parse a 'protocol' declaration, returning null (and
/// doing no token skipping) on error.
///
///   decl-protocol:
///      'protocol' attribute-list identifier protocol-body
///      
Decl *Parser::parseDeclProtocol() {
  SMLoc ProtocolLoc = consumeToken(tok::kw_protocol);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  SMLoc NameLoc = Tok.getLoc();
  Identifier ProtocolName;
  if (parseIdentifier(ProtocolName,
                      "expected identifier in protocol declaration"))
    return 0;
  
  TypeAliasDecl *TAD = ScopeInfo.addTypeAliasToScope(NameLoc, ProtocolName,
                                                     Type());
  Type ProtocolType;
  if (parseTypeProtocolBody(ProtocolLoc, Attributes, ProtocolType, TAD))
    return 0;
  return TAD;
}


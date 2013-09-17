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

#include "swift/Parse/Parser.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/DelayedParsingCallbacks.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Diagnostics.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// \brief Main entrypoint for the parser.
///
/// \verbatim
///   translation-unit:
///     stmt-brace-item*
///     decl-sil       [[only in SIL mode]
///     decl-sil-stage [[only in SIL mode]
/// \endverbatim
bool Parser::parseTranslationUnit(TranslationUnit *TU) {
  TU->ASTStage = TranslationUnit::Parsing;

  // Prime the lexer.
  if (Tok.is(tok::NUM_TOKENS))
    consumeToken();

  CurDeclContext = TU;
  
  // Parse the body of the file.
  SmallVector<ExprStmtOrDecl, 128> Items;

  if (Tok.is(tok::r_brace)) {
    diagnose(Tok, diag::extra_rbrace)
      .fixItRemove(SourceRange(Tok.getLoc()));
    consumeToken();
  }

  // If we are in SIL mode, and if the first token is the start of a sil
  // declaration, parse that one SIL function and return to the top level.  This
  // allows type declarations and other things to be parsed, name bound, and
  // type checked in batches, similar to immediate mode.  This also enforces
  // that SIL bodies can only be at the top level.
  if (Tok.is(tok::kw_sil)) {
    assert(isInSILMode() && "'sil' should only be a keyword in SIL mode");
    parseDeclSIL();
  } else if (Tok.is(tok::kw_sil_stage)) {
    assert(isInSILMode() && "'sil' should only be a keyword in SIL mode");
    parseDeclSILStage();
  } else {
    parseBraceItems(Items, true,
                    allowTopLevelCode() ? BraceItemListKind::TopLevelCode
                                        : BraceItemListKind::Brace);
  }


  // If this is a MainModule, determine if we found code that needs to be
  // executed (this is used by the repl to know whether to compile and run the
  // newly parsed stuff).
  bool FoundTopLevelCodeToExecute = false;
  if (allowTopLevelCode()) {
    for (auto V : Items)
      if (isa<TopLevelCodeDecl>(V.get<Decl*>()))
        FoundTopLevelCodeToExecute = true;
  }

  // Add newly parsed decls to the translation unit.
  for (auto Item : Items)
    TU->Decls.push_back(Item.get<Decl*>());

  // Note that the translation unit is fully parsed and verify it.
  TU->ASTStage = TranslationUnit::Parsed;
  verify(TU);

  State->markParserPosition(Tok.getLoc(), PreviousLoc);

  return FoundTopLevelCodeToExecute;
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

static Resilience getResilience(AttrName attr) {
  switch (attr) {
  case AttrName::resilient: return Resilience::Resilient;
  case AttrName::fragile: return Resilience::Fragile;
  case AttrName::born_fragile: return Resilience::InherentlyFragile;
  default: llvm_unreachable("bad resilience");
  }
}

/// \verbatim
///   attribute:
///     'asmname' '=' identifier  (FIXME: This is a temporary hack until we
///                                       can import C modules.)
///     'infix' '=' numeric_constant
///     'infix_left' '=' numeric_constant
///     'infix_right' '=' numeric_constant
///     'unary'
///     'stdlib'
///     'weak'
///     'unowned'
///     'noreturn'
/// \endverbatim
bool Parser::parseAttribute(DeclAttributes &Attributes) {
  if (Tok.is(tok::kw_weak)) {
    if (Attributes.hasOwnership()) {
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    } else {
      Attributes.Weak = true;
    }
    consumeToken(tok::kw_weak);
    return false;
  }

  if (Tok.is(tok::kw_unowned)) {
    if (Attributes.hasOwnership()) {
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    } else {
      Attributes.Unowned = true;
    }
    consumeToken(tok::kw_unowned);
    return false;
  }

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
  case AttrName::infix: {
    if (Attributes.isInfix())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);
    Attributes.ExplicitInfix = true;

    return false;
  }

  // SIL's 'local_storage' type attribute.
  case AttrName::local_storage: {
    if (Attributes.isLocalStorage())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    else if (!isInSILMode())
      diagnose(Tok, diag::only_allowed_in_sil, Tok.getText());

    consumeToken(tok::identifier);
    Attributes.LocalStorage = true;
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

    // Permit "qualifiers" on the byref.
    SourceLoc beginLoc = Tok.getLoc();
    if (consumeIfNotAtStartOfLine(tok::l_paren)) {
      diagnose(Tok, diag::byref_attribute_unknown_qualifier);
      SourceLoc endLoc;
      parseMatchingToken(tok::r_paren, endLoc,
                         diag::byref_attribute_expected_rparen,
                         beginLoc);
    }
    
    // Verify that we're not combining this attribute incorrectly.  Cannot be
    // both byref and auto_closure.
    if (Attributes.isAutoClosure()) {
      diagnose(TokLoc, diag::cannot_combine_attribute, "auto_closure");
      Attributes.AutoClosure = false;
    }
    
    return false;
  }
      
  // 'cc' attribute.
  // FIXME: only permit this in type contexts.
  case AttrName::cc: {
    if (Attributes.hasCC())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    
    consumeToken(tok::identifier);
    
    // Parse the cc name in parens.
    SourceLoc beginLoc = Tok.getLoc(), nameLoc, endLoc;
    StringRef name;
    if (consumeIfNotAtStartOfLine(tok::l_paren)) {
      if (Tok.is(tok::identifier)) {
        nameLoc = Tok.getLoc();
        name = Tok.getText();
        consumeToken();
      } else {
        diagnose(Tok, diag::cc_attribute_expected_name);
      }
      if (parseMatchingToken(tok::r_paren, endLoc,
                             diag::cc_attribute_expected_rparen,
                             beginLoc)) {
        // If the name isn't immediately followed by a closing paren, recover
        // by trying to find some closing paren.
        skipUntil(tok::r_paren);
        consumeIf(tok::r_paren);
      }
    } else {
      diagnose(Tok, diag::cc_attribute_expected_lparen);
    }
    
    if (!name.empty()) {
      Attributes.cc = llvm::StringSwitch<Optional<AbstractCC>>(name)
        .Case("freestanding", AbstractCC::Freestanding)
        .Case("method", AbstractCC::Method)
        .Case("cdecl", AbstractCC::C)
        .Case("objc_method", AbstractCC::ObjCMethod)
        .Default(Nothing);
      if (!Attributes.cc) {
        diagnose(nameLoc, diag::cc_attribute_unknown_cc_name, name);
        Attributes.cc = AbstractCC::Freestanding;
      }
    }
    return false;
  }
   
  case AttrName::class_protocol: {
    if (Attributes.isClassProtocol())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    
    consumeToken(tok::identifier);
    
    Attributes.ClassProtocol = true;
    return false;
  }

  // 'objc_block' attribute.
  // FIXME: only permit this in type contexts.
  case AttrName::objc_block: {
    if (Attributes.Byref)
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);
    
    Attributes.ObjCBlock = true;
    
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
  case AttrName::thin: {
    if (Attributes.isThin())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);
    
    Attributes.Thin = true;
    return false;
  }
  case AttrName::noreturn: {
    if (Attributes.isNoReturn())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);

    Attributes.NoReturn = true;
    return false;
  }

  case AttrName::assignment: {
    if (Attributes.isAssignment())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);
    
    Attributes.Assignment = true;
    return false;    
  }

  case AttrName::prefix: {
    SourceLoc TokLoc = Tok.getLoc();
    if (Attributes.isPrefix())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());

    consumeToken(tok::identifier);

    if (Attributes.isPostfix()) {
      diagnose(TokLoc, diag::cannot_combine_attribute, "postfix");
      return false;
    }
    Attributes.ExplicitPrefix = true;
    return false;
  }

  case AttrName::postfix: {
    SourceLoc TokLoc = Tok.getLoc();
    if (Attributes.isPostfix())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());

    consumeToken(tok::identifier);

    if (Attributes.isPrefix()) {
      diagnose(TokLoc, diag::cannot_combine_attribute, "prefix");
      return false;
    }
    Attributes.ExplicitPostfix = true;
    return false;
  }

  case AttrName::conversion: {
    if (Attributes.isConversion())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);
    
    Attributes.Conversion = true;
    return false;    
  }

  case AttrName::exported: {
    if (Attributes.isExported())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);

    Attributes.Exported = true;
    return false;
  }
      
  case AttrName::transparent: {
    if (Attributes.isTransparent())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);
    
    Attributes.Transparent = true;
    return false;
  }

  case AttrName::iboutlet: {
    if (Attributes.isIBOutlet())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());

    consumeToken(tok::identifier);
    Attributes.IBOutlet = true;
    return false;
  }

  case AttrName::ibaction: {
    if (Attributes.isIBAction())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());

    consumeToken(tok::identifier);
    Attributes.IBAction = true;
    return false;
  }

  case AttrName::objc: {
    if (Attributes.isObjC())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());

    consumeToken(tok::identifier);
    Attributes.ObjC = true;
    return false;
  }

  /// FIXME: This is a temporary hack until we can import C modules.
  case AttrName::asmname: {
    SourceLoc TokLoc = Tok.getLoc();
    if (!Attributes.AsmName.empty())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);

    if (!consumeIf(tok::equal)) {
      diagnose(TokLoc, diag::asmname_expected_equals);
      return false;
    }

    if (!Tok.is(tok::string_literal)) {
      diagnose(TokLoc, diag::asmname_expected_string_literal);
      return false;
    }

    SmallVector<Lexer::StringSegment, 1> Segments;
    L->getStringLiteralSegments(Tok, Segments);
    if (Segments.size() != 1 ||
        Segments.front().Kind == Lexer::StringSegment::Expr) {
      diagnose(TokLoc, diag::asmname_interpolated_string);
    } else {
      Attributes.AsmName = StringRef(
          SourceMgr->getMemoryBuffer(BufferID)->getBufferStart() +
              SourceMgr.getLocOffsetInBuffer(Segments.front().Loc, BufferID),
          Segments.front().Length);
    }
    consumeToken(tok::string_literal);
    return false;
  }
  }
  llvm_unreachable("bad attribute kind");
}

/// \brief This is the internal implementation of \c parseAttributeList, which
/// we expect to be inlined to handle the common case of an absent attribute
/// list.
///
/// \verbatim
///   attribute-list:
///     attribute-list-clause*
///   attribute-list-clause
///     '[' ']'
///     '[' attribute (',' attribute)* ']'
/// \endverbatim
bool Parser::parseAttributeListPresent(DeclAttributes &Attributes) {
  SourceLoc leftLoc = consumeToken(tok::l_square);
  Attributes.LSquareLoc = leftLoc;

  do {
    if (parseList(tok::r_square, leftLoc, Attributes.RSquareLoc,
                  tok::comma, /*OptionalSep=*/false,
                  diag::expected_in_attribute_list,
                  [&] () -> bool {
          return parseAttribute(Attributes);
        }))
      return true;

    leftLoc = Tok.getLoc();

    // A square bracket here begins another attribute-list-clause;
    // consume it and continue.  Note that we'll overwrite
    // Attributes.RSquareLoc so that it encompasses the entire range.
  } while (consumeIf(tok::l_square));

  return false;
}

bool Parser::isStartOfOperatorDecl(const Token &Tok, const Token &Tok2) {
  return Tok.isContextualKeyword("operator")
    && (Tok2.isContextualKeyword("prefix")
        || Tok2.isContextualKeyword("postfix")
        || Tok2.isContextualKeyword("infix"));
}

void Parser::consumeDecl(ParserPosition BeginParserPosition, unsigned Flags) {
  backtrackToPosition(BeginParserPosition);
  SourceLoc BeginLoc = Tok.getLoc();
  // Consume tokens up to code completion token.
  while (Tok.isNot(tok::code_complete)) {
    consumeToken();
  }
  // Consume the code completion token, if there is one.
  consumeIf(tok::code_complete);
  SourceLoc EndLoc = Tok.getLoc();
  State->delayDecl(PersistentParserState::DelayedDeclKind::Decl, Flags,
                   CurDeclContext, { BeginLoc, EndLoc },
                   BeginParserPosition.PreviousLoc);
}

/// \brief Parse a single syntactic declaration and return a list of decl
/// ASTs.  This can return multiple results for var decls that bind to multiple
/// values, structs that define a struct decl and a constructor, etc.
///
/// \verbatim
///   decl:
///     decl-typealias
///     decl-extension
///     decl-var
///     decl-func
///     decl-union
///     decl-struct
///     decl-import
///     decl-operator
/// \endverbatim
ParserStatus Parser::parseDecl(SmallVectorImpl<Decl*> &Entries,
                               unsigned Flags) {
  ParserPosition BeginParserPosition;
  if (isCodeCompletionFirstPass())
    BeginParserPosition = getParserPosition();

  // If we see the 'static' keyword, parse it now.
  SourceLoc StaticLoc;
  bool UnhandledStatic = false;
  if (Tok.is(tok::kw_static)) {
    StaticLoc = consumeToken();
    UnhandledStatic = true;
  }

  ParserResult<Decl> DeclResult;
  ParserStatus Status;
  switch (Tok.getKind()) {
  case tok::kw_import:
    DeclResult = parseDeclImport(Flags);
    Status = DeclResult;
    break;
  case tok::kw_extension:
    DeclResult = parseDeclExtension(Flags);
    Status = DeclResult;
    break;
  case tok::kw_var:
    if (StaticLoc.isValid()) {
      diagnose(Tok, diag::unimplemented_static_var)
        .highlight(SourceRange(StaticLoc));
      UnhandledStatic = false;
    }
    Status = parseDeclVar(Flags, Entries);
    break;
  case tok::kw_typealias:
    DeclResult = parseDeclTypeAlias(!(Flags & PD_DisallowTypeAliasDef),
                                     Flags & PD_InProtocol);
    Status = DeclResult;
    break;
  case tok::kw_union:
    DeclResult = parseDeclUnion(Flags);
    Status = DeclResult;
    break;
  case tok::kw_case:
    DeclResult = parseDeclUnionElement(Flags);
    Status = DeclResult;
    break;
  case tok::kw_struct:
    DeclResult = parseDeclStruct(Flags);
    Status = DeclResult;
    break;
  case tok::kw_class:
    DeclResult = parseDeclClass(Flags);
    Status = DeclResult;
    break;
  case tok::kw_constructor:
    DeclResult = parseDeclConstructor(Flags);
    Status = DeclResult;
    break;
  case tok::kw_destructor:
    DeclResult = parseDeclDestructor(Flags);
    Status = DeclResult;
    break;
  case tok::kw_protocol:
    DeclResult = parseDeclProtocol(Flags);
    Status = DeclResult;
    break;

  case tok::kw_func:
    DeclResult = parseDeclFunc(StaticLoc, Flags);
    Status = DeclResult;
    UnhandledStatic = false;
    break;

  case tok::kw_subscript:
    if (StaticLoc.isValid()) {
      diagnose(Tok, diag::subscript_static)
        .fixItRemove(SourceRange(StaticLoc));
      UnhandledStatic = false;
    }
    Status = parseDeclSubscript(Flags & PD_HasContainerType,
                                !(Flags & PD_DisallowFuncDef),
                                Entries);
    break;
  
  case tok::identifier:
    if (isStartOfOperatorDecl(Tok, peekToken())) {
      DeclResult = parseDeclOperator(Flags & PD_AllowTopLevel);
      break;
    }
    SWIFT_FALLTHROUGH;

  default:
    diagnose(Tok, diag::expected_decl);
    DeclResult = makeParserErrorResult<Decl>();
    Status = DeclResult;
    break;
  }

  if (Status.hasCodeCompletion() && isCodeCompletionFirstPass() &&
      !CurDeclContext->isModuleContext()) {
    // Only consume non-toplevel decls.
    consumeDecl(BeginParserPosition, Flags);

    // Pretend that there was no error.
    return makeParserSuccess();
  }

  if (DeclResult.isNonNull())
    Entries.push_back(DeclResult.get());

  if (Status.isSuccess() && Tok.is(tok::semi))
    Entries.back()->TrailingSemiLoc = consumeToken(tok::semi);

  // If we parsed 'static' but didn't handle it above, complain about it.
  if (Status.isSuccess() && UnhandledStatic) {
    diagnose(Entries.back()->getLoc(), diag::decl_not_static)
      .fixItRemove(SourceRange(StaticLoc));
  }

  return Status;
}

void Parser::parseDeclDelayed() {
  auto DelayedState = State->takeDelayedDeclState();
  assert(DelayedState.get() && "should have delayed state");

  auto BeginParserPosition = getParserPosition(DelayedState->BodyPos);
  auto EndLexerState = L->getStateForEndOfTokenLoc(DelayedState->BodyEnd);

  // ParserPositionRAII needs a primed parser to restore to.
  if (Tok.is(tok::NUM_TOKENS))
    consumeToken();

  // Ensure that we restore the parser state at exit.
  ParserPositionRAII PPR(*this);

  // Create a lexer that can not go past the end state.
  Lexer LocalLex(*L, BeginParserPosition.LS, EndLexerState);

  // Temporarily swap out the parser's current lexer with our new one.
  llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

  // Rewind to the beginning of the decl.
  restoreParserPosition(BeginParserPosition);

  // Re-enter the lexical scope.
  Scope S(this, DelayedState->takeScope());
  ContextChange CC(*this, DelayedState->ParentContext);

  SmallVector<Decl *, 2> Entries;
  parseDecl(Entries, DelayedState->Flags);
}

/// \brief Parse an 'import' declaration, doing no token skipping on error.
///
/// \verbatim
///   decl-import:
///     'import' attribute-list import-kind? import-path
///   import-kind:
///     'typealias'
///     'struct'
///     'class'
///     'union'
///     'protocol'
///     'var'
///     'func'
///   import-path:
///     any-identifier ('.' any-identifier)*
/// \endverbatim
ParserResult<ImportDecl> Parser::parseDeclImport(unsigned Flags) {
  SourceLoc ImportLoc = consumeToken(tok::kw_import);
  
  bool Exported;
  {
    DeclAttributes Attributes;
    parseAttributeList(Attributes);

    Exported = Attributes.isExported();
    Attributes.Exported = false;
    if (!Attributes.empty())
      diagnose(Attributes.LSquareLoc, diag::import_attributes);
  }

  if (!(Flags & PD_AllowTopLevel)) {
    diagnose(ImportLoc, diag::decl_inner_scope);
    return nullptr;
  }

  ImportKind Kind = ImportKind::Module;
  SourceLoc KindLoc;
  if (Tok.isKeyword()) {
    switch (Tok.getKind()) {
    case tok::kw_typealias:
      Kind = ImportKind::Type;
      break;
    case tok::kw_struct:
      Kind = ImportKind::Struct;
      break;
    case tok::kw_class:
      Kind = ImportKind::Class;
      break;
    case tok::kw_union:
      Kind = ImportKind::Union;
      break;
    case tok::kw_protocol:
      Kind = ImportKind::Protocol;
      break;
    case tok::kw_var:
      Kind = ImportKind::Var;
      break;
    case tok::kw_func:
      Kind = ImportKind::Func;
      break;
    default:
      diagnose(Tok, diag::expected_identifier_in_decl, "import");
      return nullptr;
    }
    KindLoc = consumeToken();
  }

  SmallVector<std::pair<Identifier, SourceLoc>, 8> ImportPath;
  do {
    ImportPath.push_back(std::make_pair(Identifier(), Tok.getLoc()));
    if (parseAnyIdentifier(ImportPath.back().first,
                        diag::expected_identifier_in_decl, "import"))
      return nullptr;
  } while (consumeIf(tok::period));

  if (Kind != ImportKind::Module && ImportPath.size() == 1) {
    diagnose(ImportPath.front().second, diag::decl_expected_module_name);
    return nullptr;
  }

  return makeParserResult(ImportDecl::create(
      Context, CurDeclContext, ImportLoc, Kind, KindLoc, Exported, ImportPath));
}

/// \brief Parse an inheritance clause.
///
/// \verbatim
///   inheritance:
///      ':' type-identifier (',' type-identifier)*
/// \endverbatim
ParserStatus Parser::parseInheritance(SmallVectorImpl<TypeLoc> &Inherited) {
  consumeToken(tok::colon);

  ParserStatus Status;
  do {
    // Parse the inherited type (which must be a protocol).
    ParserResult<TypeRepr> Ty = parseTypeIdentifier();
    Status |= Ty;

    // Record the type.
    if (Ty.isNonNull())
      Inherited.push_back(Ty.get());

    // Check for a ',', which indicates that there are more protocols coming.
  } while (consumeIf(tok::comma));

  return Status;
}

enum class TokenProperty {
  None,
  StartsWithLess,
};

static ParserStatus parseIdentifierDeclName(Parser &P, Identifier &Result,
                                            SourceLoc &Loc, tok ResyncT1,
                                            tok ResyncT2, tok ResyncT3,
                                            TokenProperty ResyncP1,
                                            const Diagnostic &D) {
  switch (P.Tok.getKind()) {
  case tok::identifier:
    Result = P.Context.getIdentifier(P.Tok.getText());
    Loc = P.Tok.getLoc();
    P.consumeToken();
    return makeParserSuccess();

  default:
    if (D.getID() != DiagID::invalid_diagnostic)
      P.diagnose(P.Tok, D);
    if (P.Tok.isKeyword() &&
        (P.peekToken().is(ResyncT1) || P.peekToken().is(ResyncT2) ||
         P.peekToken().is(ResyncT3) ||
         (ResyncP1 != TokenProperty::None &&
          P.startsWithLess(P.peekToken())))) {
      llvm::SmallString<32> Name(P.Tok.getText());
      // Append an invalid character so that nothing can resolve to this name.
      Name += "#";
      Result = P.Context.getIdentifier(Name.str());
      Loc = P.Tok.getLoc();
      P.consumeToken();
      // Return success because we recovered.
      return makeParserSuccess();
    }
    return makeParserError();
  }
}

template <typename... DiagArgTypes, typename... ArgTypes>
static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        tok ResyncT1, tok ResyncT2, Diag<DiagArgTypes...> ID,
                        ArgTypes... Args) {
  return parseIdentifierDeclName(P, Result, L, ResyncT1, ResyncT2,
                                 tok::unknown, TokenProperty::None,
                                 Diagnostic(ID, Args...));
}

template <typename... DiagArgTypes, typename... ArgTypes>
static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        tok ResyncT1, tok ResyncT2, tok ResyncT3,
                        Diag<DiagArgTypes...> ID, ArgTypes... Args) {
  return parseIdentifierDeclName(P, Result, L, ResyncT1, ResyncT2, ResyncT3,
                                 TokenProperty::None, Diagnostic(ID, Args...));
}

template <typename... DiagArgTypes, typename... ArgTypes>
static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        tok ResyncT1, tok ResyncT2, TokenProperty ResyncP1,
                        Diag<DiagArgTypes...> ID, ArgTypes... Args) {
  return parseIdentifierDeclName(P, Result, L, ResyncT1, ResyncT2, tok::unknown,
                                 ResyncP1, Diagnostic(ID, Args...));
}

/// \brief Parse an 'extension' declaration.
///
/// \verbatim
///   extension:
///    'extension' type-identifier inheritance? '{' decl* '}'
/// \endverbatim
ParserResult<ExtensionDecl> Parser::parseDeclExtension(unsigned Flags) {
  SourceLoc ExtensionLoc = consumeToken(tok::kw_extension);

  ParserResult<TypeRepr> Ty = parseTypeIdentifierWithRecovery(
      diag::expected_type, diag::expected_ident_type_in_extension);
  if (Ty.hasCodeCompletion())
    return makeParserCodeCompletionResult<ExtensionDecl>();
  if (Ty.isNull() && Tok.isKeyword()) {
    // We failed to parse the type, but we could try recovering by parsing a
    // keyword if the lookahead token looks promising.
    Identifier ExtensionName;
    SourceLoc NameLoc;
    if (parseIdentifierDeclName(*this, ExtensionName, NameLoc, tok::colon,
                                tok::l_brace,
                                diag::invalid_diagnostic).isError())
      return nullptr;
    Ty = makeParserErrorResult(
        IdentTypeRepr::createSimple(Context, NameLoc, ExtensionName,
                                    CurDeclContext));
  }
  if (Ty.isNull())
    return nullptr;

  ParserStatus Status;

  // Parse optional inheritance clause.
  SmallVector<TypeLoc, 2> Inherited;
  if (Tok.is(tok::colon))
    Status |= parseInheritance(Inherited);

  ExtensionDecl *ED
    = new (Context) ExtensionDecl(ExtensionLoc, Ty.get(),
                                  Context.AllocateCopy(Inherited),
                                  CurDeclContext);

  SmallVector<Decl*, 8> MemberDecls;
  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_extension)) {
    LBLoc = Tok.getLoc();
    RBLoc = LBLoc;
    Status.setIsParseError();
  } else {
    // Parse the body.
    ContextChange CC(*this, ED);
    Scope S(this, ScopeKind::Extension);

    ParserStatus BodyStatus =
        parseList(tok::r_brace, LBLoc, RBLoc, tok::semi, /*OptionalSep=*/true,
                  diag::expected_rbrace_extension, [&]()->ParserStatus{
      return parseDecl(MemberDecls, PD_HasContainerType | PD_DisallowVar);
    });
    // Don't propagate the code completion bit from members: we can not help
    // code completion inside a member decl, and our callers can not do
    // anything about it either.  But propagate the error bit.
    if (BodyStatus.isError())
      Status.setIsParseError();
  }

  if (MemberDecls.empty())
    ED->setMembers({}, { LBLoc, RBLoc });
  else
    ED->setMembers(Context.AllocateCopy(MemberDecls), { LBLoc, RBLoc });

  if (!(Flags & PD_AllowTopLevel)) {
    diagnose(ExtensionLoc, diag::decl_inner_scope);
    Status.setIsParseError();

    // Tell the type checker not to touch this extension.
    ED->setInvalid();
  }

  return makeParserResult(Status, ED);
}

/// \brief Parse a typealias decl.
///
/// \verbatim
///   decl-typealias:
///     'typealias' identifier inheritance? '=' type
/// \endverbatim
ParserResult<TypeDecl> Parser::parseDeclTypeAlias(bool WantDefinition,
                                                  bool isAssociatedType) {
  SourceLoc TypeAliasLoc = consumeToken(tok::kw_typealias);
  
  Identifier Id;
  SourceLoc IdLoc;
  ParserStatus Status;

  Status |=
      parseIdentifierDeclName(*this, Id, IdLoc, tok::colon, tok::equal,
                              diag::expected_identifier_in_decl, "typealias");
  if (Status.isError())
    return nullptr;

  // Parse optional inheritance clause.
  SmallVector<TypeLoc, 2> Inherited;
  if (Tok.is(tok::colon))
    Status |= parseInheritance(Inherited);

  ParserResult<TypeRepr> UnderlyingTy;
  if (WantDefinition || Tok.is(tok::equal)) {
    if (parseToken(tok::equal, diag::expected_equal_in_typealias)) {
      Status.setIsParseError();
      return Status;
    }
    UnderlyingTy = parseType(diag::expected_type_in_typealias);
    Status |= UnderlyingTy;
    if (UnderlyingTy.isNull())
      return Status;
    
    if (!WantDefinition) {
      diagnose(IdLoc, diag::associated_type_def, Id);
      UnderlyingTy = nullptr;
    }
  }

  // If this is an associated type, build the AST for it.
  if (isAssociatedType) {
    auto assocType = new (Context) AssociatedTypeDecl(CurDeclContext,
                                                      TypeAliasLoc, Id, IdLoc);
    if (!Inherited.empty())
      assocType->setInherited(Context.AllocateCopy(Inherited));
    addToScope(assocType);
    return makeParserResult(Status, assocType);
  }

  // Otherwise, build a typealias.
  TypeAliasDecl *TAD =
    new (Context) TypeAliasDecl(TypeAliasLoc, Id, IdLoc,
                                UnderlyingTy.getPtrOrNull(),
                                CurDeclContext,
                                Context.AllocateCopy(Inherited));
  addToScope(TAD);
  return makeParserResult(Status, TAD);
}

namespace {
  class AddVarsToScope : public ASTWalker {
  public:
    Parser &TheParser;
    ASTContext &Context;
    DeclContext *CurDeclContext;
    SmallVectorImpl<Decl*> &Decls;
    DeclAttributes &Attributes;
    
    AddVarsToScope(Parser &P,
                   ASTContext &Context,
                   DeclContext *CurDeclContext,
                   SmallVectorImpl<Decl*> &Decls,
                   DeclAttributes &Attributes)
      : TheParser(P),
        Context(Context),
        CurDeclContext(CurDeclContext),
        Decls(Decls),
        Attributes(Attributes)
    {}
    
    Pattern *walkToPatternPost(Pattern *P) override {
      // Handle vars.
      if (auto *Named = dyn_cast<NamedPattern>(P)) {
        VarDecl *VD = Named->getDecl();
        VD->setDeclContext(CurDeclContext);
        if (Attributes.isValid())
          VD->getMutableAttrs() = Attributes;
        
        if (VD->isProperty()) {
          // FIXME: Order of get/set not preserved.
          if (FuncDecl *Get = VD->getGetter()) {
            Get->setDeclContext(CurDeclContext);
            Decls.push_back(Get);
          }
          if (FuncDecl *Set = VD->getSetter()) {
            Set->setDeclContext(CurDeclContext);
            Decls.push_back(Set);
          }
        }
        
        Decls.push_back(VD);
        TheParser.addToScope(VD);
      }
      return P;
    }
  };
}

void Parser::addVarsToScope(Pattern *Pat,
                            SmallVectorImpl<Decl*> &Decls,
                            DeclAttributes &Attributes) {
  Pat->walk(AddVarsToScope(*this, Context, CurDeclContext,
                           Decls, Attributes));
}

/// \brief Parse a get-set clause, containing a getter and (optionally)
/// a setter.
///
/// \verbatim
///   get-set:
///      get var-set?
///      set var-get
///
///   get:
///     'get:' stmt-brace-item*
///
///   set:
///     'set' set-name? ':' stmt-brace-item*
///
///   set-name:
///     '(' identifier ')'
/// \endverbatim
bool Parser::parseGetSet(bool HasContainerType, Pattern *Indices,
                         TypeLoc ElementTy, FuncDecl *&Get, FuncDecl *&Set,
                         SourceLoc &LastValidLoc) {
  bool Invalid = false;
  Get = nullptr;
  Set = nullptr;
  
  while (Tok.isNot(tok::r_brace)) {
    if (Tok.is(tok::eof)) {
      Invalid = true;
      break;
    }
    if (Tok.isContextualKeyword("get") || !Tok.isContextualKeyword("set")) {
      //   get         ::= 'get' stmt-brace
      
      // Have we already parsed a get clause?
      if (Get) {
        diagnose(Tok, diag::duplicate_getset, false);
        diagnose(Get->getLoc(), diag::previous_getset, false);
        
        // Forget the previous version.
        Get = nullptr;
      }
      
      SourceLoc GetLoc = Tok.getLoc(), ColonLoc = Tok.getLoc();
      if (Tok.isContextualKeyword("get")) {
        GetLoc = consumeToken();
        if (Tok.isNot(tok::colon)) {
          diagnose(Tok, diag::expected_colon_get);
          Invalid = true;
          break;
        }
        ColonLoc = consumeToken(tok::colon);
      }

      // Set up a function declaration for the getter and parse its body.
      
      // Create the parameter list(s) for the getter.
      SmallVector<Pattern *, 3> Params;
      
      // Add the implicit 'self' to Params, if needed.
      if (HasContainerType)
        Params.push_back(buildImplicitSelfParameter());

      // Add the index clause if necessary.
      if (Indices) {
        Params.push_back(Indices->clone(Context));
      }
      
      // Add a no-parameters clause.
      Params.push_back(TuplePattern::create(Context, SourceLoc(),
                                            ArrayRef<TuplePatternElt>(),
                                            SourceLoc()));

      Scope S(this, ScopeKind::FunctionBody);

      // Start the function.
      Get = FuncDecl::create(Context, /*StaticLoc=*/SourceLoc(), GetLoc,
                             Identifier(), GetLoc, /*GenericParams=*/nullptr,
                             Type(), Params, Params, ElementTy,
                             CurDeclContext);
      addFunctionParametersToScope(Get->getBodyParamPatterns(), Get);

      // Establish the new context.
      ContextChange CC(*this, Get);

      SmallVector<ExprStmtOrDecl, 16> Entries;
      parseBraceItems(Entries, false /*NotTopLevel*/,
                      BraceItemListKind::Property);
      BraceStmt *Body = BraceStmt::create(Context, ColonLoc,
                                          Entries, Tok.getLoc());
      Get->setBody(Body);

      LastValidLoc = Body->getRBraceLoc();
      continue;
    }

    //   var-set         ::= 'set' var-set-name? stmt-brace
    
    // Have we already parsed a var-set clause?
    if (Set) {
      diagnose(Tok, diag::duplicate_getset, true);
      diagnose(Set->getLoc(), diag::previous_getset, true);

      // Forget the previous setter.
      Set = nullptr;
    }
    
    SourceLoc SetLoc = consumeToken();
    
    //   var-set-name    ::= '(' identifier ')'
    Identifier SetName;
    SourceLoc SetNameLoc;
    SourceRange SetNameParens;
    if (Tok.is(tok::l_paren)) {
      SourceLoc StartLoc = consumeToken();
      if (Tok.is(tok::identifier)) {
        // We have a name.
        SetName = Context.getIdentifier(Tok.getText());
        SetNameLoc = consumeToken();
        
        // Look for the closing ')'.
        SourceLoc EndLoc;
        if (parseMatchingToken(tok::r_paren, EndLoc,
                               diag::expected_rparen_setname, StartLoc))
          EndLoc = SetNameLoc;
        SetNameParens = SourceRange(StartLoc, EndLoc);
      } else {
        diagnose(Tok, diag::expected_setname);
        skipUntil(tok::r_paren, tok::l_brace);
        if (Tok.is(tok::r_paren))
          consumeToken();
      }
    }
    if (Tok.isNot(tok::colon)) {
      diagnose(Tok, diag::expected_colon_set);
      Invalid = true;
      break;
    }
    SourceLoc ColonLoc = consumeToken(tok::colon);

    // Set up a function declaration for the setter and parse its body.
    
    // Create the parameter list(s) for the setter.
    SmallVector<Pattern *, 3> Params;
    
    // Add the implicit 'self' to Params, if needed.
    if (HasContainerType)
      Params.push_back(buildImplicitSelfParameter());

    // Add the index parameters, if necessary.
    if (Indices) {
      Params.push_back(Indices->clone(Context));
    }
    
    // Add the parameter. If no name was specified, the name defaults to
    // 'value'.
    if (SetName.empty())
      SetName = Context.getIdentifier("value");
    {
      VarDecl *Value = new (Context) VarDecl(SetNameLoc, SetName,
                                             Type(), CurDeclContext);
      
      Pattern *ValuePattern
        = new (Context) TypedPattern(new (Context) NamedPattern(Value),
                                     ElementTy);
      TuplePatternElt ValueElt(ValuePattern);
      Pattern *ValueParamsPattern
        = TuplePattern::create(Context, SetNameParens.Start, ValueElt,
                               SetNameParens.End);
      Params.push_back(ValueParamsPattern);
    }

    Scope S(this, ScopeKind::FunctionBody);

    // Start the function.
    Type SetterRetTy = TupleType::getEmpty(Context);
    Set = FuncDecl::create(Context, /*StaticLoc=*/SourceLoc(), SetLoc,
                           Identifier(), SetLoc, /*generic=*/nullptr, Type(),
                           Params, Params, TypeLoc::withoutLoc(SetterRetTy),
                           CurDeclContext);
    addFunctionParametersToScope(Set->getBodyParamPatterns(), Set);

    // Establish the new context.
    ContextChange CC(*this, Set);
    
    // Parse the body.
    SmallVector<ExprStmtOrDecl, 16> Entries;
    parseBraceItems(Entries, false /*NotTopLevel*/,
                    BraceItemListKind::Property);
    BraceStmt *Body = BraceStmt::create(Context, ColonLoc,
                                        Entries, Tok.getLoc());
    Set->setBody(Body);

    LastValidLoc = Body->getRBraceLoc();
  }
  
  return Invalid;
}

/// \brief Parse the brace-enclosed getter and setter for a variable.
///
/// \verbatim
///   decl-var:
///      'var' attribute-list identifier : type-annotation { get-set }
/// \endverbatim
void Parser::parseDeclVarGetSet(Pattern &pattern, bool HasContainerType) {
  bool Invalid = false;
    
  // The grammar syntactically requires a simple identifier for the variable
  // name. Complain if that isn't what we got.
  VarDecl *PrimaryVar = nullptr;
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
  TypeLoc TyLoc;
  if (TypedPattern *TP = dyn_cast<TypedPattern>(&pattern)) {
    TyLoc = TP->getTypeLoc();
  } else {
    if (PrimaryVar)
      diagnose(pattern.getLoc(), diag::getset_missing_type);
    TyLoc = TypeLoc::withoutLoc(ErrorType::get(Context));
  }
  
  SourceLoc LBLoc = consumeToken(tok::l_brace);
    
  // Parse getter and setter.
  FuncDecl *Get = nullptr;
  FuncDecl *Set = nullptr;
  SourceLoc LastValidLoc = LBLoc;
  if (parseGetSet(HasContainerType, /*Indices=*/0, TyLoc, Get, Set, LastValidLoc))
    Invalid = true;
  
  // Parse the final '}'.
  SourceLoc RBLoc;
  if (Invalid) {
    skipUntilDeclRBrace();
    RBLoc = LastValidLoc;
  }

  if (parseMatchingToken(tok::r_brace, RBLoc, diag::expected_rbrace_in_getset,
                         LBLoc)) {
    RBLoc = LastValidLoc;
  }
  
  if (Set && !Get) {
    if (!Invalid)
      diagnose(Set->getLoc(), diag::var_set_without_get);
    
    Set = nullptr;
    Invalid = true;
  }

  // If things went well, turn this variable into a property.
  if (!Invalid && PrimaryVar && (Set || Get))
    PrimaryVar->setProperty(Context, LBLoc, Get, Set, RBLoc);
}

/// \brief Parse a 'var' declaration, doing no token skipping on error.
///
/// \verbatim
///   decl-var:
///      'var' attribute-list pattern initializer? (',' pattern initializer? )*
///      'var' attribute-list identifier : type-annotation { get-set }
/// \endverbatim
ParserStatus Parser::parseDeclVar(unsigned Flags,
                                  SmallVectorImpl<Decl *> &Decls) {
  SourceLoc VarLoc = consumeToken(tok::kw_var);

  SmallVector<PatternBindingDecl*, 4> PBDs;
  bool HasGetSet = false;
  ParserStatus Status;

  unsigned FirstDecl = Decls.size();

  do {
    DeclAttributes Attributes;
    parseAttributeList(Attributes);

    ParserResult<Pattern> pattern = parsePattern();
    if (pattern.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (pattern.isNull())
      return makeParserError();

    // If we syntactically match the second decl-var production, with a
    // var-get-set clause, parse the var-get-set clause.
    if (Tok.is(tok::l_brace)) {
      parseDeclVarGetSet(*pattern.get(), Flags & PD_HasContainerType);
      HasGetSet = true;
    }

    ParserResult<Expr> Init;
    if (Tok.is(tok::equal)) {
      // Record the variables that we're trying to initialize.
      SmallVector<VarDecl *, 4> Vars;
      Vars.append(CurVars.second.begin(), CurVars.second.end());
      pattern.get()->collectVariables(Vars);
      using RestoreVarsRAII = llvm::SaveAndRestore<decltype(CurVars)>;
      RestoreVarsRAII RestoreCurVars(CurVars, {CurDeclContext, Vars});

      SourceLoc EqualLoc = consumeToken(tok::equal);
      Init = parseExpr(diag::expected_initializer_expr);
      if (Init.hasCodeCompletion())
        return makeParserCodeCompletionStatus();
      if (Init.isNull()) {
        Status.setIsParseError();
        break;
      }
    
      if (HasGetSet) {
        diagnose(pattern.get()->getLoc(), diag::getset_init)
          .highlight(Init.get()->getSourceRange());
        Init = nullptr;
      }
      if (Flags & PD_DisallowInit) {
        diagnose(EqualLoc, diag::disallowed_init);
        Status.setIsParseError();
      }
    }

    addVarsToScope(pattern.get(), Decls, Attributes);

    // In the normal case, just add PatternBindingDecls to our DeclContext.
    PatternBindingDecl *PBD =
      new (Context) PatternBindingDecl(VarLoc, pattern.get(),
                                       Init.getPtrOrNull(), CurDeclContext);
    Decls.push_back(PBD);

    // Propagate back types for simple patterns, like "var A, B : T".
    if (TypedPattern *TP = dyn_cast<TypedPattern>(PBD->getPattern())) {
      if (isa<NamedPattern>(TP->getSubPattern()) && !PBD->hasInit()) {
        for (unsigned i = PBDs.size(); i != 0; --i) {
          PatternBindingDecl *PrevPBD = PBDs[i-1];
          Pattern *PrevPat = PrevPBD->getPattern();
          if (!isa<NamedPattern>(PrevPat) || PrevPBD->hasInit())
            break;
          if (HasGetSet) {
            // FIXME -- offer a fixit to explicitly specify the type
            diagnose(PrevPat->getLoc(), diag::getset_cannot_be_implied);
            Status.setIsParseError();
          }

          TypedPattern *NewTP = new (Context) TypedPattern(PrevPat,
                                                           TP->getTypeLoc());
          PrevPBD->setPattern(NewTP);
        }
      }
    }
    PBDs.push_back(PBD);
  } while (consumeIf(tok::comma));

  if (HasGetSet) {
    if (PBDs.size() > 1) {
      diagnose(VarLoc, diag::disallowed_property_multiple_getset);
      Status.setIsParseError();
    }
    if (Flags & PD_DisallowProperty) {
      diagnose(VarLoc, diag::disallowed_property_decl);
      Status.setIsParseError();
    }
  } else if (Flags & PD_DisallowVar) {
    diagnose(VarLoc, diag::disallowed_var_decl);
    Status.setIsParseError();
    return Status;
  }

  // If this is a var in the top-level of script/repl translation unit, then
  // wrap the PatternBindingDecls in TopLevelCodeDecls, since they represent
  // executable code.
  if (allowTopLevelCode() && isa<TranslationUnit>(CurDeclContext)) {
    for (unsigned i = FirstDecl; i != Decls.size(); ++i) {
      auto *PBD = dyn_cast<PatternBindingDecl>(Decls[i]);
      if (PBD == 0) continue;
      auto *Brace = BraceStmt::create(Context, PBD->getStartLoc(),
                                      ExprStmtOrDecl(PBD), PBD->getEndLoc());

      auto *TLCD = new (Context) TopLevelCodeDecl(CurDeclContext, Brace);
      PBD->setDeclContext(TLCD);
      Decls[i] = TLCD;
    }
  }

  return Status;
}

/// \brief Build an implicit 'self' parameter for the current DeclContext.
Pattern *Parser::buildImplicitSelfParameter() {
  VarDecl *D
    = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("self"),
                            Type(), CurDeclContext);
  Pattern *P = new (Context) NamedPattern(D);
  return new (Context) TypedPattern(P, TypeLoc());
}

void Parser::consumeAbstractFunctionBody(AbstractFunctionDecl *AFD,
                                         const DeclAttributes &Attrs) {
  auto BeginParserPosition = getParserPosition();
  SourceRange BodyRange;
  BodyRange.Start = Tok.getLoc();

  // Consume the '{', and find the matching '}'.
  consumeToken(tok::l_brace);
  unsigned OpenBraces = 1;
  while (OpenBraces != 0 && Tok.isNot(tok::eof)) {
    if (consumeIf(tok::l_brace)) {
      OpenBraces++;
      continue;
    }
    if (consumeIf(tok::r_brace)) {
      OpenBraces--;
      continue;
    }
    consumeToken();
  }
  if (OpenBraces != 0 && Tok.isNot(tok::code_complete)) {
    assert(Tok.is(tok::eof));
    // We hit EOF, and not every brace has a pair.  Recover by searching
    // for the next decl except variable decls and cutting off before
    // that point.
    backtrackToPosition(BeginParserPosition);
    consumeToken(tok::l_brace);
    while (Tok.is(tok::kw_var) ||
           (Tok.isNot(tok::eof) && !isStartOfDecl(Tok, peekToken()))) {
      consumeToken();
    }
  }

  BodyRange.End = PreviousLoc;

  if (DelayedParseCB->shouldDelayFunctionBodyParsing(*this, AFD, Attrs,
                                                     BodyRange)) {
    State->delayFunctionBodyParsing(AFD, BodyRange,
                                    BeginParserPosition.PreviousLoc);
    AFD->setBodyDelayed(BodyRange.End);
  } else {
    AFD->setBodySkipped(BodyRange.End);
  }
}

/// \brief Parse a 'func' declaration, returning null on error.  The caller
/// handles this case and does recovery as appropriate.
///
/// \verbatim
///   decl-func:
///     'static'? 'func' attribute-list any-identifier generic-params?
///               func-signature stmt-brace?
/// \endverbatim
///
/// \note The caller of this method must ensure that the next token is 'func'.
ParserResult<FuncDecl>
Parser::parseDeclFunc(SourceLoc StaticLoc, unsigned Flags) {
  bool HasContainerType = Flags & PD_HasContainerType;

  // Reject 'static' functions at global scope.
  if (StaticLoc.isValid() && !HasContainerType) {
    diagnose(Tok, diag::static_func_decl_global_scope)
      .fixItRemoveChars(StaticLoc, Tok.getLoc());
    StaticLoc = SourceLoc();
  }
  
  SourceLoc FuncLoc = consumeToken(tok::kw_func);

  DeclAttributes Attributes;
  // FIXME: Implicitly add immutable attribute.
  parseAttributeList(Attributes);

  Identifier Name;
  SourceLoc NameLoc = Tok.getLoc();
  if (!(Flags & PD_AllowTopLevel) && !(Flags & PD_DisallowFuncDef) &&
      Tok.isAnyOperator()) {
    diagnose(Tok, diag::func_decl_nonglobal_operator);
    return nullptr;
  }
  if (parseAnyIdentifier(Name, diag::expected_identifier_in_decl, "func")) {
    ParserStatus NameStatus =
        parseIdentifierDeclName(*this, Name, NameLoc, tok::l_paren, tok::arrow,
                                tok::l_brace, diag::invalid_diagnostic);
    if (NameStatus.isError())
      return nullptr;
  }
  
  // Parse the generic-params, if present.
  Optional<Scope> GenericsScope;
  GenericsScope.emplace(this, ScopeKind::Generics);
  GenericParamList *GenericParams;

  // If the name is an operator token that ends in '<' and the following token
  // is an identifier, split the '<' off as a separate token. This allows things
  // like 'func ==<T>(x:T, y:T) {}' to parse as '==' with generic type variable
  // '<T>' as expected.
  if (Name.str().size() > 1 && Name.str().back() == '<'
      && Tok.is(tok::identifier)) {
    Name = Context.getIdentifier(Name.str().slice(0, Name.str().size() - 1));
    SourceLoc LAngleLoc = NameLoc.getAdvancedLoc(Name.str().size());
    GenericParams = parseGenericParameters(LAngleLoc);
  } else {
    GenericParams = maybeParseGenericParams();
  }

  SmallVector<Pattern*, 8> ArgParams;
  SmallVector<Pattern*, 8> BodyParams;
  
  // If we're within a container, add an implicit first pattern to match the
  // container type as an element named 'self'.
  //
  // This turns an instance function "(int)->int" on FooTy into
  // "(this: [byref] FooTy)->(int)->int", and a static function
  // "(int)->int" on FooTy into "(this: [byref] FooTy.metatype)->(int)->int".
  // Note that we can't actually compute the type here until Sema.
  if (HasContainerType) {
    Pattern *SelfPattern = buildImplicitSelfParameter();
    ArgParams.push_back(SelfPattern);
    BodyParams.push_back(SelfPattern);
  }

  bool HadSignatureParseError = false;
  TypeRepr *FuncRetTy = nullptr;
  {
    ParserStatus SignatureStatus =
        parseFunctionSignature(ArgParams, BodyParams, FuncRetTy);

    HadSignatureParseError = SignatureStatus.isError();

    if (SignatureStatus.hasCodeCompletion()) {
      if (!CodeCompletion)
        // Trigger delayed parsing, no need to continue.
        return SignatureStatus;

      // Create function AST nodes.
      FuncDecl *FD = FuncDecl::create(Context, StaticLoc, FuncLoc, Name,
                                      NameLoc, GenericParams, Type(), ArgParams,
                                      BodyParams, FuncRetTy, CurDeclContext);
      FD->setBodySkipped(Tok.getLoc());

      addFunctionParametersToScope(FD->getBodyParamPatterns(), FD);

      // Pass the function signature to code completion.
      CodeCompletion->setDelayedParsedDecl(FD);
    }
  }

  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  FuncDecl *FD;
  {
    Scope S(this, ScopeKind::FunctionBody);

    // Create the decl for the func and add it to the parent scope.
    FD = FuncDecl::create(Context, StaticLoc, FuncLoc, Name, NameLoc,
                          GenericParams, Type(), ArgParams, BodyParams,
                          FuncRetTy, CurDeclContext);

    addFunctionParametersToScope(FD->getBodyParamPatterns(), FD);

    // Now that we have a context, update the generic parameters with that
    // context.
    if (GenericParams) {
      for (auto Param : *GenericParams) {
        Param.setDeclContext(FD);
      }
    }

    // Establish the new context.
    ContextChange CC(*this, FD);

    // Check to see if we have a "{" to start a brace statement.
    if (Tok.is(tok::l_brace)) {
      if (Flags & PD_DisallowFuncDef) {
        diagnose(Tok, diag::disallowed_func_def);
        consumeToken();
        skipUntil(tok::r_brace);
        consumeToken();
        // FIXME: don't just drop the body.
      } else if (!isDelayedParsingEnabled()) {
        ParserResult<BraceStmt> Body =
            parseBraceItemList(diag::func_decl_without_brace);
        if (Body.isNull()) {
          // FIXME: Should do some sort of error recovery here?
        } else {
          FD->setBody(Body.get());
        }
      } else {
        consumeAbstractFunctionBody(FD, Attributes);
      }
    } else if (Attributes.AsmName.empty() && !(Flags & PD_DisallowFuncDef) &&
               !HadSignatureParseError) {
      diagnose(Tok.getLoc(), diag::func_decl_without_brace);
    }
  }

  // Exit the scope introduced for the generic parameters.
  GenericsScope.reset();

  if (Attributes.isValid())
    FD->getMutableAttrs() = Attributes;
  addToScope(FD);
  return makeParserResult(FD);
}

bool Parser::parseAbstractFunctionBodyDelayed(AbstractFunctionDecl *AFD) {
  assert(!AFD->getBody() && "function should not have a parsed body");
  assert(AFD->getBodyKind() == AbstractFunctionDecl::BodyKind::Unparsed &&
         "function body should be delayed");

  auto FunctionParserState = State->takeBodyState(AFD);
  assert(FunctionParserState.get() && "should have a valid state");

  auto BeginParserPosition = getParserPosition(FunctionParserState->BodyPos);
  auto EndLexerState = L->getStateForEndOfTokenLoc(AFD->getEndLoc());

  // ParserPositionRAII needs a primed parser to restore to.
  if (Tok.is(tok::NUM_TOKENS))
    consumeToken();

  // Ensure that we restore the parser state at exit.
  ParserPositionRAII PPR(*this);

  // Create a lexer that can not go past the end state.
  Lexer LocalLex(*L, BeginParserPosition.LS, EndLexerState);

  // Temporarily swap out the parser's current lexer with our new one.
  llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

  // Rewind to '{' of the function body.
  restoreParserPosition(BeginParserPosition);

  // Re-enter the lexical scope.
  Scope S(this, FunctionParserState->takeScope());
  ContextChange CC(*this, AFD);

  ParserResult<BraceStmt> Body =
      parseBraceItemList(diag::func_decl_without_brace);
  if (Body.isNull()) {
    // FIXME: Should do some sort of error recovery here?
    return true;
  } else {
    AFD->setBody(Body.get());
  }

  return false;
}

/// \brief Parse a 'union' declaration, returning true (and doing no token
/// skipping) on error.
///
/// \verbatim
///   decl-union:
///      'union' attribute-list identifier generic-params? inheritance?
///          '{' decl-union-body '}'
///   decl-union-body:
///      decl*
/// \endverbatim
ParserResult<UnionDecl> Parser::parseDeclUnion(unsigned Flags) {
  SourceLoc UnionLoc = consumeToken(tok::kw_union);

  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  Identifier UnionName;
  SourceLoc UnionNameLoc;
  ParserStatus Status;

  Status |=
      parseIdentifierDeclName(*this, UnionName, UnionNameLoc, tok::colon,
                              tok::l_brace, TokenProperty::StartsWithLess,
                              diag::expected_identifier_in_decl, "union");
  if (Status.isError())
    return nullptr;

  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    Scope S(this, ScopeKind::Generics);
    GenericParams = maybeParseGenericParams();
  }

  UnionDecl *UD = new (Context) UnionDecl(UnionLoc,
                                          /*isEnum*/ false,
                                          UnionName, UnionNameLoc,
                                          { },
                                          GenericParams, CurDeclContext);

  if (Attributes.isValid())
    UD->getMutableAttrs() = Attributes;

  // Now that we have a context, update the generic parameters with that
  // context.
  if (GenericParams)
    for (auto Param : *GenericParams)
      Param.setDeclContext(UD);

  // Parse optional inheritance clause within the context of the union.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, UD);
    SmallVector<TypeLoc, 2> Inherited;
    Status |= parseInheritance(Inherited);
    UD->setInherited(Context.AllocateCopy(Inherited));
  }

  SmallVector<Decl*, 8> MemberDecls;
  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_union)) {
    LBLoc = Tok.getLoc();
    RBLoc = LBLoc;
    Status.setIsParseError();
  } else {
    ContextChange CC(*this, UD);
    Scope S(this, ScopeKind::ClassBody);
    if (parseNominalDeclMembers(MemberDecls, LBLoc, RBLoc,
                                diag::expected_rbrace_union,
                                PD_HasContainerType | PD_AllowUnionElement |
                                PD_DisallowVar))
      Status.setIsParseError();
  }

  if (MemberDecls.empty())
    UD->setMembers({}, { LBLoc, RBLoc });
  else
    UD->setMembers(Context.AllocateCopy(MemberDecls), { LBLoc, RBLoc });
  addToScope(UD);

  if (Flags & PD_DisallowNominalTypes) {
    diagnose(UnionLoc, diag::disallowed_type);
    Status.setIsParseError();
  }

  return makeParserResult(Status, UD);
}

/// \brief Parse a 'case' of a union.
///
/// \verbatim
///   decl-union-element:
///      'case' identifier type-tuple? ('->' type)?
/// \endverbatim
ParserResult<UnionElementDecl> Parser::parseDeclUnionElement(unsigned Flags) {
  SourceLoc CaseLoc = consumeToken(tok::kw_case);
  
  // TODO: Accept attributes here?
  
  Identifier Name;
  SourceLoc NameLoc;

  const bool NameIsNotIdentifier = Tok.isNot(tok::identifier);
  if (parseIdentifierDeclName(*this, Name, NameLoc, tok::l_paren,
                              tok::kw_case, tok::colon,
                              diag::invalid_diagnostic).isError()) {
    // For recovery, see if the user typed something resembling a switch "case"
    // label.
    parseMatchingPattern();
  }
  if (NameIsNotIdentifier) {
    if (consumeIf(tok::colon)) {
      diagnose(CaseLoc, diag::case_outside_of_switch, "case");
      return nullptr;
    }
    diagnose(CaseLoc, diag::expected_identifier_in_decl, "union case");
  }

  // See if there's a following argument type.
  ParserResult<TypeRepr> ArgType;
  if (Tok.isFollowingLParen()) {
    ArgType = parseTypeTupleBody();
    if (ArgType.hasCodeCompletion())
      return makeParserCodeCompletionResult<UnionElementDecl>();
    if (ArgType.isNull())
      return nullptr;
  }
  
  // See if there's a result type.
  SourceLoc ArrowLoc;
  ParserResult<TypeRepr> ResultType;
  if (Tok.is(tok::arrow)) {
    ArrowLoc = consumeToken();
    ResultType = parseType(diag::expected_type_union_element_result);
    if (ResultType.hasCodeCompletion())
      return makeParserCodeCompletionResult<UnionElementDecl>();
    if (ResultType.isNull())
      return nullptr;
  }
  
  // For recovery, again make sure the the user didn't try to spell a switch
  // case label:
  // 'case Identifier:',
  // 'case Identifier, ...:', or
  // 'case Identifier where ...:'
  if (Tok.is(tok::colon) || Tok.is(tok::comma) || Tok.is(tok::kw_where)) {
    diagnose(CaseLoc, diag::case_outside_of_switch, "case");
    skipUntilDeclRBrace();
    return nullptr;
  }
  
  // Create the element.
  auto *result = new (Context) UnionElementDecl(CaseLoc, NameLoc, Name,
                                                ArgType.getPtrOrNull(),
                                                ArrowLoc,
                                                ResultType.getPtrOrNull(),
                                                CurDeclContext);
  if (!(Flags & PD_AllowUnionElement)) {
    diagnose(CaseLoc, diag::disallowed_union_element);
    // Don't return the UnionElementDecl unless it is allowed to have
    // a UnionElementDecl in the current context.
    return nullptr;
  }

  return makeParserResult(result);
}

/// \brief Parse the members in a struct/class/protocol definition.
///
/// \verbatim
///    decl*
/// \endverbatim
bool Parser::parseNominalDeclMembers(SmallVectorImpl<Decl *> &memberDecls,
                                     SourceLoc LBLoc, SourceLoc &RBLoc,
                                     Diag<> ErrorDiag, unsigned flags) {
  bool previousHadSemi = true;
  return parseList(tok::r_brace, LBLoc, RBLoc, tok::semi, /*OptionalSep=*/true,
                   ErrorDiag, [&] () -> bool {
    // If the previous declaration didn't have a semicolon and this new
    // declaration doesn't start a line, complain.
    if (!previousHadSemi && !Tok.isAtStartOfLine()) {
      SourceLoc endOfPrevious = getEndOfPreviousLoc();
      diagnose(endOfPrevious, diag::declaration_same_line_without_semi)
        .fixItInsert(endOfPrevious, ";");
      // FIXME: Add semicolon to the AST?
    }

    previousHadSemi = false;
    if (parseDecl(memberDecls, flags).isError())
      return true;

    // Check whether the previous declaration had a semicolon after it.
    if (!memberDecls.empty() && memberDecls.back()->TrailingSemiLoc.isValid())
      previousHadSemi = true;

    return false;
  });
}

/// \brief Parse a 'struct' declaration, returning true (and doing no token
/// skipping) on error.
///
/// \verbatim
///   decl-struct:
///      'struct' attribute-list identifier generic-params? inheritance?
///          '{' decl-struct-body '}
///   decl-struct-body:
///      decl*
/// \endverbatim
ParserResult<StructDecl> Parser::parseDeclStruct(unsigned Flags) {
  SourceLoc StructLoc = consumeToken(tok::kw_struct);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  Identifier StructName;
  SourceLoc StructNameLoc;
  ParserStatus Status;

  Status |=
      parseIdentifierDeclName(*this, StructName, StructNameLoc, tok::colon,
                              tok::l_brace, TokenProperty::StartsWithLess,
                              diag::expected_identifier_in_decl, "struct");
  if (Status.isError())
    return nullptr;

  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    Scope S(this, ScopeKind::Generics);
    GenericParams = maybeParseGenericParams();
  }

  StructDecl *SD = new (Context) StructDecl(StructLoc, StructName,
                                            StructNameLoc,
                                            { },
                                            GenericParams,
                                            CurDeclContext);

  if (Attributes.isValid())
    SD->getMutableAttrs() = Attributes;

  // Now that we have a context, update the generic parameters with that
  // context.
  if (GenericParams) {
    for (auto Param : *GenericParams) {
      Param.setDeclContext(SD);
    }
  }

  // Parse optional inheritance clause within the context of the struct.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, SD);
    SmallVector<TypeLoc, 2> Inherited;
    Status |= parseInheritance(Inherited);
    SD->setInherited(Context.AllocateCopy(Inherited));
  }

  SmallVector<Decl*, 8> MemberDecls;
  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_struct)) {
    LBLoc = Tok.getLoc();
    RBLoc = LBLoc;
    Status.setIsParseError();
  } else {
    // Parse the body.
    ContextChange CC(*this, SD);
    Scope S(this, ScopeKind::StructBody);
    if (parseNominalDeclMembers(MemberDecls, LBLoc, RBLoc,
                                diag::expected_rbrace_struct,
                                PD_HasContainerType))
      Status.setIsParseError();
  }

  if (MemberDecls.empty())
    SD->setMembers({}, { LBLoc, RBLoc });
  else
    SD->setMembers(Context.AllocateCopy(MemberDecls), { LBLoc, RBLoc });
  addToScope(SD);

  if (Flags & PD_DisallowNominalTypes) {
    diagnose(StructLoc, diag::disallowed_type);
    Status.setIsParseError();
  }

  return makeParserResult(Status, SD);
}

/// \brief Parse a 'class' declaration, doing no token skipping on error.
///
/// \verbatim
///   decl-class:
///      'class' attribute-list identifier generic-params? inheritance?
///          '{' decl-class-body '}
///   decl-class-body:
///      decl*
/// \endverbatim
ParserResult<ClassDecl> Parser::parseDeclClass(unsigned Flags) {
  SourceLoc ClassLoc = consumeToken(tok::kw_class);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  Identifier ClassName;
  SourceLoc ClassNameLoc;
  ParserStatus Status;

  Status |=
      parseIdentifierDeclName(*this, ClassName, ClassNameLoc, tok::colon,
                              tok::l_brace, TokenProperty::StartsWithLess,
                              diag::expected_identifier_in_decl, "class");
  if (Status.isError())
    return nullptr;

  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    Scope S(this, ScopeKind::Generics);
    GenericParams = maybeParseGenericParams();
  }

  // Create the class.
  ClassDecl *CD = new (Context) ClassDecl(ClassLoc, ClassName, ClassNameLoc,
                                          { }, GenericParams, CurDeclContext);

  // Attach attributes.
  if (Attributes.isValid())
    CD->getMutableAttrs() = Attributes;

  // Now that we have a context, update the generic parameters with that
  // context.
  if (GenericParams) {
    for (auto Param : *GenericParams) {
      Param.setDeclContext(CD);
    }
  }

  // Parse optional inheritance clause within the context of the class.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, CD);
    SmallVector<TypeLoc, 2> Inherited;
    Status |= parseInheritance(Inherited);
    CD->setInherited(Context.AllocateCopy(Inherited));
  }

  SmallVector<Decl*, 8> MemberDecls;
  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_class)) {
    LBLoc = Tok.getLoc();
    RBLoc = LBLoc;
    Status.setIsParseError();
  } else {
    // Parse the body.
    ContextChange CC(*this, CD);
    Scope S(this, ScopeKind::ClassBody);
    if (parseNominalDeclMembers(MemberDecls, LBLoc, RBLoc,
                                diag::expected_rbrace_class,
                                PD_HasContainerType | PD_AllowDestructor))
      Status.setIsParseError();
  }

  bool hasConstructor = false;
  for (Decl *Member : MemberDecls) {
    if (isa<ConstructorDecl>(Member))
      hasConstructor = true;
  }

  if (!hasConstructor) {
    VarDecl *SelfDecl
      = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("self"),
                              Type(), CD);
    Pattern *Arguments = TuplePattern::create(Context, SourceLoc(),
                                              ArrayRef<TuplePatternElt>(),
                                              SourceLoc());
    ConstructorDecl *Constructor =
        new (Context) ConstructorDecl(Context.getIdentifier("constructor"),
                                     SourceLoc(), Arguments, SelfDecl,
                                     nullptr, CD);
    Constructor->setImplicit();
    SelfDecl->setDeclContext(Constructor);
    MemberDecls.push_back(Constructor);
  }

  CD->setMembers(Context.AllocateCopy(MemberDecls), { LBLoc, RBLoc });
  addToScope(CD);

  if (Flags & PD_DisallowNominalTypes) {
    diagnose(ClassLoc, diag::disallowed_type);
    Status.setIsParseError();
  }

  return makeParserResult(Status, CD);
}

/// \brief Parse a 'protocol' declaration, doing no token skipping on error.
///
/// \verbatim
///   decl-protocol:
///      protocol-head '{' protocol-member* '}'
///
///   protocol-head:
///     'protocol' attribute-list identifier inheritance? 
///
///   protocol-member:
///      decl-func
///      decl-var-simple
///      decl-typealias
/// \endverbatim
ParserResult<ProtocolDecl> Parser::parseDeclProtocol(unsigned Flags) {
  SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  SourceLoc NameLoc;
  Identifier ProtocolName;
  ParserStatus Status;

  Status |=
      parseIdentifierDeclName(*this, ProtocolName, NameLoc, tok::colon,
                              tok::l_brace, diag::expected_identifier_in_decl,
                              "protocol");
  if (Status.isError())
    return nullptr;

  // Parse optional inheritance clause.
  SmallVector<TypeLoc, 4> InheritedProtocols;
  if (Tok.is(tok::colon))
    Status |= parseInheritance(InheritedProtocols);

  ProtocolDecl *Proto
    = new (Context) ProtocolDecl(CurDeclContext, ProtocolLoc, NameLoc,
                                 ProtocolName,
                                 Context.AllocateCopy(InheritedProtocols));

  if (Attributes.isValid())
    Proto->getMutableAttrs() = Attributes;

  ContextChange CC(*this, Proto);
  Scope ProtocolBodyScope(this, ScopeKind::ProtocolBody);

  // Parse the body.
  {
    // The list of protocol elements.
    SmallVector<Decl*, 8> Members;

    // Add the implicit 'Self' associated type.
    Members.push_back(new (Context) AssociatedTypeDecl(
                                      CurDeclContext,
                                      Proto->getLoc(),
                                      Context.getIdentifier("Self"),
                                      Proto->getLoc()));
    Members.back()->setImplicit();

    SourceLoc LBraceLoc;
    SourceLoc RBraceLoc;
    if (parseToken(tok::l_brace, LBraceLoc, diag::expected_lbrace_protocol)) {
      LBraceLoc = Tok.getLoc();
      RBraceLoc = LBraceLoc;
      Status.setIsParseError();
    } else {
      // Parse the members.
      if (parseNominalDeclMembers(Members, LBraceLoc, RBraceLoc,
                                  diag::expected_rbrace_protocol,
                                  PD_HasContainerType | PD_DisallowProperty |
                                  PD_DisallowFuncDef | PD_DisallowNominalTypes |
                                  PD_DisallowInit | PD_DisallowTypeAliasDef |
                                  PD_InProtocol))
        Status.setIsParseError();
    }

    // Install the protocol elements.
    Proto->setMembers(Context.AllocateCopy(Members), { LBraceLoc, RBraceLoc });
  }
  
  if (Flags & PD_DisallowNominalTypes) {
    diagnose(ProtocolLoc, diag::disallowed_type);
    Status.setIsParseError();
  } else if (!(Flags & PD_AllowTopLevel)) {
    diagnose(ProtocolLoc, diag::decl_inner_scope);
    Status.setIsParseError();
  }

  return makeParserResult(Status, Proto);
}

namespace {
  /// Recursively walks a pattern and sets all variables' decl contexts to the
  /// given context.
  class SetVarContext : public ASTWalker {
    DeclContext *CurDeclContext;

  public:
    SetVarContext(DeclContext *Context) : CurDeclContext(Context) {}

    Pattern *walkToPatternPost(Pattern *P) override {
      // Handle vars.
      if (auto *Named = dyn_cast<NamedPattern>(P))
        Named->getDecl()->setDeclContext(CurDeclContext);
      return P;
    }
  };
}

/// \brief Parse a 'subscript' declaration.
///
/// \verbatim
///   decl-subscript:
///     subscript-head get-set
///   subscript-head
///     'subscript' attribute-list pattern-tuple '->' type
/// \endverbatim
ParserStatus Parser::parseDeclSubscript(bool HasContainerType,
                                        bool NeedDefinition,
                                        SmallVectorImpl<Decl *> &Decls) {
  ParserStatus Status;
  SourceLoc SubscriptLoc = consumeToken(tok::kw_subscript);

  // attribute-list
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  // pattern-tuple
  if (Tok.isNot(tok::l_paren)) {
    diagnose(Tok, diag::expected_lparen_subscript);
    return makeParserError();
  }

  ParserResult<Pattern> Indices = parsePatternTuple(/*AllowInitExpr=*/false);
  if (Indices.isNull() || Indices.hasCodeCompletion())
    return Indices;
  Indices.get()->walk(SetVarContext(CurDeclContext));

  // '->'
  if (!Tok.is(tok::arrow)) {
    diagnose(Tok, diag::expected_arrow_subscript);
    return makeParserError();
  }
  SourceLoc ArrowLoc = consumeToken();
  
  // type
  ParserResult<TypeRepr> ElementTy =
      parseTypeAnnotation(diag::expected_type_subscript);
  if (ElementTy.isNull() || ElementTy.hasCodeCompletion())
    return ElementTy;
  
  if (!NeedDefinition) {
    SubscriptDecl *Subscript
      = new (Context) SubscriptDecl(Context.getIdentifier("__subscript"),
                                    SubscriptLoc, Indices.get(), ArrowLoc,
                                    ElementTy.get(), SourceRange(),
                                    0, 0, CurDeclContext);
    Decls.push_back(Subscript);
    return makeParserSuccess();
  }
  
  // '{'
  if (!Tok.is(tok::l_brace)) {
    diagnose(Tok, diag::expected_lbrace_subscript);
    return makeParserError();
  }
  SourceLoc LBLoc = consumeToken();
  
  // Parse getter and setter.
  FuncDecl *Get = nullptr;
  FuncDecl *Set = nullptr;
  SourceLoc LastValidLoc = LBLoc;
  if (parseGetSet(HasContainerType, Indices.get(), ElementTy.get(),
                  Get, Set, LastValidLoc))
    Status.setIsParseError();

  // Parse the final '}'.
  SourceLoc RBLoc;
  if (Status.isError()) {
    skipUntilDeclRBrace();
    RBLoc = LastValidLoc;
  }

  if (parseMatchingToken(tok::r_brace, RBLoc, diag::expected_rbrace_in_getset,
                         LBLoc)) {
    RBLoc = LastValidLoc;
  }

  if (!Get) {
    if (Status.isSuccess())
      diagnose(SubscriptLoc, diag::subscript_without_get);
    Status.setIsParseError();
  }

  // Reject 'subscript' functions outside of type decls
  if (!HasContainerType) {
    diagnose(SubscriptLoc, diag::subscript_decl_wrong_scope);
    Status.setIsParseError();
  }

  if (Status.isSuccess()) {
    // FIXME: We should build the declarations even if they are invalid.

    // Build an AST for the subscript declaration.
    SubscriptDecl *Subscript
      = new (Context) SubscriptDecl(Context.getIdentifier("__subscript"),
                                    SubscriptLoc, Indices.get(), ArrowLoc,
                                    ElementTy.get(), SourceRange(LBLoc, RBLoc),
                                    Get, Set, CurDeclContext);

    // FIXME: Order of get/set not preserved.
    if (Set) {
      Set->setDeclContext(CurDeclContext);
      Set->makeSetter(Subscript);
      Decls.push_back(Set);
    }

    if (Get) {
      Get->setDeclContext(CurDeclContext);
      Get->makeGetter(Subscript);
      Decls.push_back(Get);
    }

    Decls.push_back(Subscript);
  }
  return Status;
}

ParserResult<ConstructorDecl>
Parser::parseDeclConstructor(unsigned Flags) {
  SourceLoc ConstructorLoc = consumeToken(tok::kw_constructor);

  const bool ConstructorsNotAllowed =
      !(Flags & PD_HasContainerType) || (Flags & PD_InProtocol);

  // Reject 'constructor' functions outside of types
  if (ConstructorsNotAllowed) {
    diagnose(Tok, diag::constructor_decl_wrong_scope);
  }

  // attribute-list
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  // Parse the generic-params, if present.
  Scope S(this, ScopeKind::Generics);
  GenericParamList *GenericParams = maybeParseGenericParams();

  // pattern-tuple
  ParserResult<Pattern> Arguments;
  if (!Tok.is(tok::l_paren))
    diagnose(Tok, diag::expected_lparen_constructor);
  else
    Arguments = parsePatternTuple(/*AllowInitExpr=*/true);

  // FIXME: handle code completion in Arguments.

  if (Arguments.isNull())
    // Recover by creating an empty parameter tuple.
    Arguments = makeParserErrorResult(
        TuplePattern::createSimple(Context, Tok.getLoc(), {}, Tok.getLoc()));

  // '{'
  if (!Tok.is(tok::l_brace)) {
    if (!Arguments.isParseError()) {
      // Don't emit this diagnostic if we already complained about this
      // constructor decl.
      diagnose(Tok, diag::expected_lbrace_constructor);
    }
    return nullptr;
  }

  VarDecl *SelfDecl
    = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("self"),
                            Type(), CurDeclContext);

  Scope S2(this, ScopeKind::ConstructorBody);
  ConstructorDecl *CD =
      new (Context) ConstructorDecl(Context.getIdentifier("constructor"),
                                    ConstructorLoc, Arguments.get(), SelfDecl,
                                    GenericParams, CurDeclContext);
  SelfDecl->setDeclContext(CD);
  if (ConstructorsNotAllowed) {
    // Tell the type checker not to touch this constructor.
    CD->setInvalid();
  }
  if (GenericParams) {
    for (auto Param : *GenericParams)
      Param.setDeclContext(CD);
  }
  addFunctionParametersToScope(Arguments.get(), CD);
  addToScope(SelfDecl);
  ContextChange CC(*this, CD);

  if (!isDelayedParsingEnabled()) {
    ParserResult<BraceStmt> Body = parseBraceItemList(diag::invalid_diagnostic);

    if (!Body.isNull())
      CD->setBody(Body.get());
  } else {
    consumeAbstractFunctionBody(CD, Attributes);
  }

  if (Attributes.isValid())
    CD->getMutableAttrs() = Attributes;

  return makeParserResult(CD);
}

ParserResult<DestructorDecl> Parser::parseDeclDestructor(unsigned Flags) {
  SourceLoc DestructorLoc = consumeToken(tok::kw_destructor);

  // attribute-list
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  // '{'
  if (!Tok.is(tok::l_brace)) {
    if (Tok.is(tok::l_paren)) {
      // Parse the parameter tuple for recovery.
      SourceLoc LParenLoc = Tok.getLoc();
      ParserResult<Pattern> Params = parsePatternTuple(/*AllowInitExpr=*/true);
      if (Params.isParseError()) {
        diagnose(LParenLoc, diag::destructor_parameter_tuple);
      } else {
        diagnose(LParenLoc, diag::destructor_parameter_tuple)
            .fixItRemove(Params.get()->getSourceRange());
      }
    }
    if (!Tok.is(tok::l_brace)) {
      diagnose(Tok, diag::expected_lbrace_destructor);
      return nullptr;
    }
  }

  VarDecl *SelfDecl
    = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("self"),
                            Type(), CurDeclContext);

  Scope S(this, ScopeKind::DestructorBody);
  DestructorDecl *DD =
      new (Context) DestructorDecl(Context.getIdentifier("destructor"),
                                   DestructorLoc, SelfDecl, CurDeclContext);
  SelfDecl->setDeclContext(DD);
  addToScope(SelfDecl);
  ContextChange CC(*this, DD);

  ParserResult<BraceStmt> Body = parseBraceItemList(diag::invalid_diagnostic);

  if (!Body.isNull())
    DD->setBody(Body.get());

  if (Attributes.isValid())
    DD->getMutableAttrs() = Attributes;

  // Reject 'destructor' functions outside of classes
  if (!(Flags & PD_AllowDestructor)) {
    diagnose(DestructorLoc, diag::destructor_decl_outside_class);
    return nullptr;
  }

  return makeParserResult(DD);
}

ParserResult<OperatorDecl> Parser::parseDeclOperator(bool AllowTopLevel) {
  assert(Tok.isContextualKeyword("operator") &&
         "no 'operator' at start of operator decl?!");

  SourceLoc OperatorLoc = consumeToken(tok::identifier);

  auto kind = llvm::StringSwitch<Optional<DeclKind>>(Tok.getText())
    .Case("prefix", DeclKind::PrefixOperator)
    .Case("postfix", DeclKind::PostfixOperator)
    .Case("infix", DeclKind::InfixOperator)
    .Default(Nothing);
  
  assert(kind && "no fixity after 'operator'?!");

  SourceLoc KindLoc = consumeToken(tok::identifier);
  
  if (!Tok.isAnyOperator()) {
    diagnose(Tok, diag::expected_operator_name_after_operator);
    return nullptr;
  }
  
  Identifier Name = Context.getIdentifier(Tok.getText());
  SourceLoc NameLoc = consumeToken();
  
  if (!Tok.is(tok::l_brace)) {
    diagnose(Tok, diag::expected_lbrace_after_operator);
    return nullptr;
  }
  
  ParserResult<OperatorDecl> Result;
  
  switch (*kind) {
  case DeclKind::PrefixOperator:
    Result = parseDeclPrefixOperator(OperatorLoc, KindLoc, Name, NameLoc);
    break;
  case DeclKind::PostfixOperator:
    Result = parseDeclPostfixOperator(OperatorLoc, KindLoc, Name, NameLoc);
    break;
  case DeclKind::InfixOperator:
    Result = parseDeclInfixOperator(OperatorLoc, KindLoc, Name, NameLoc);
    break;
  default:
    llvm_unreachable("impossible");
  }
  
  if (Tok.is(tok::r_brace))
    consumeToken();
  
  if (!AllowTopLevel) {
    diagnose(OperatorLoc, diag::operator_decl_inner_scope);
    return nullptr;
  }
  
  return Result;
}

ParserResult<OperatorDecl>
Parser::parseDeclPrefixOperator(SourceLoc OperatorLoc, SourceLoc PrefixLoc,
                                Identifier Name, SourceLoc NameLoc) {
  SourceLoc LBraceLoc = consumeToken(tok::l_brace);
  
  while (!Tok.is(tok::r_brace)) {
    // Currently there are no operator attributes for prefix operators.
    if (Tok.is(tok::identifier))
      diagnose(Tok, diag::unknown_prefix_operator_attribute, Tok.getText());
    else
      diagnose(Tok, diag::expected_operator_attribute);
    skipUntilDeclRBrace();
    return nullptr;
  }
  
  SourceLoc RBraceLoc = Tok.getLoc();

  return makeParserResult(
      new (Context) PrefixOperatorDecl(CurDeclContext, OperatorLoc, PrefixLoc,
                                       Name, NameLoc, LBraceLoc, RBraceLoc));
}

ParserResult<OperatorDecl>
Parser::parseDeclPostfixOperator(SourceLoc OperatorLoc, SourceLoc PostfixLoc,
                                 Identifier Name, SourceLoc NameLoc) {
  SourceLoc LBraceLoc = consumeToken(tok::l_brace);
  
  while (!Tok.is(tok::r_brace)) {
    // Currently there are no operator attributes for postfix operators.
    if (Tok.is(tok::identifier))
      diagnose(Tok, diag::unknown_postfix_operator_attribute, Tok.getText());
    else
      diagnose(Tok, diag::expected_operator_attribute);
    skipUntilDeclRBrace();
    return nullptr;
  }
  
  SourceLoc RBraceLoc = Tok.getLoc();
  
  return makeParserResult(
      new (Context) PostfixOperatorDecl(CurDeclContext, OperatorLoc,
                                        PostfixLoc, Name, NameLoc, LBraceLoc,
                                        RBraceLoc));
}

ParserResult<OperatorDecl>
Parser::parseDeclInfixOperator(SourceLoc OperatorLoc, SourceLoc InfixLoc,
                               Identifier Name, SourceLoc NameLoc) {
  SourceLoc LBraceLoc = consumeToken(tok::l_brace);

  // Initialize InfixData with default attributes:
  // precedence 100, associativity none
  unsigned char precedence = 100;
  Associativity associativity = Associativity::None;
  
  SourceLoc AssociativityLoc, AssociativityValueLoc,
    PrecedenceLoc, PrecedenceValueLoc;
  
  while (!Tok.is(tok::r_brace)) {
    if (!Tok.is(tok::identifier)) {
      diagnose(Tok, diag::expected_operator_attribute);
      skipUntilDeclRBrace();
      return nullptr;
    }
    
    if (Tok.getText().equals("associativity")) {
      if (AssociativityLoc.isValid()) {
        diagnose(Tok, diag::operator_associativity_redeclared);
        skipUntilDeclRBrace();
        return nullptr;
      }
      AssociativityLoc = consumeToken();
      if (!Tok.is(tok::identifier)) {
        diagnose(Tok, diag::expected_infix_operator_associativity);
        skipUntilDeclRBrace();
        return nullptr;
      }
      auto parsedAssociativity
        = llvm::StringSwitch<Optional<Associativity>>(Tok.getText())
          .Case("none", Associativity::None)
          .Case("left", Associativity::Left)
          .Case("right", Associativity::Right)
          .Default(Nothing);
      if (!parsedAssociativity) {
        diagnose(Tok, diag::unknown_infix_operator_associativity, Tok.getText());
        skipUntilDeclRBrace();
        return nullptr;
      }
      associativity = *parsedAssociativity;

      AssociativityValueLoc = consumeToken();
      continue;
    }
    
    if (Tok.getText().equals("precedence")) {
      if (PrecedenceLoc.isValid()) {
        diagnose(Tok, diag::operator_precedence_redeclared);
        skipUntilDeclRBrace();
        return nullptr;
      }
      PrecedenceLoc = consumeToken();
      if (!Tok.is(tok::integer_literal)) {
        diagnose(Tok, diag::expected_infix_operator_precedence);
        skipUntilDeclRBrace();
        return nullptr;
      }
      if (Tok.getText().getAsInteger(0, precedence)) {
        diagnose(Tok, diag::invalid_infix_operator_precedence);
        precedence = 255;
      }
      
      PrecedenceValueLoc = consumeToken();
      continue;
    }
    
    diagnose(Tok, diag::unknown_infix_operator_attribute, Tok.getText());
    skipUntilDeclRBrace();
    return nullptr;
  }
  
  SourceLoc RBraceLoc = Tok.getLoc();
  
  return makeParserResult(new (Context) InfixOperatorDecl(
      CurDeclContext, OperatorLoc, InfixLoc, Name, NameLoc, LBraceLoc,
      AssociativityLoc, AssociativityValueLoc, PrecedenceLoc,
      PrecedenceValueLoc, RBraceLoc, InfixData(precedence, associativity)));
}

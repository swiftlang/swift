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

/// parseTranslationUnit - Main entrypoint for the parser.
///   translation-unit:
///     stmt-brace-item*
///     decl-sil       [[only in SIL mode]
///     decl-sil-stage [[only in SIL mode]
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

/// parseAttribute
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
    if (Attributes.isPrefix())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());

    consumeToken(tok::identifier);
    Attributes.ExplicitPrefix = true;
    return false;
  }

  case AttrName::postfix: {
    if (Attributes.isPostfix())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());

    consumeToken(tok::identifier);
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
      
  case AttrName::force_inline: {
    if (Attributes.isForceInline())
      diagnose(Tok, diag::duplicate_attribute, Tok.getText());
    consumeToken(tok::identifier);
    
    Attributes.ForceInline = true;
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

/// parseAttributeListPresent - This is the internal implementation of
/// parseAttributeList, which we expect to be inlined to handle the common case
/// of an absent attribute list.
///   attribute-list:
///     attribute-list-clause*
///   attribute-list-clause
///     '[' ']'
///     '[' attribute (',' attribute)* ']'
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
///     decl-union
///     decl-struct
///     decl-import
///     decl-operator
///
bool Parser::parseDecl(SmallVectorImpl<Decl*> &Entries, unsigned Flags) {
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

  bool HadParseError = false;
  switch (Tok.getKind()) {
  case tok::kw_import:
    if (Decl *D = parseDeclImport(Flags).getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;
  case tok::kw_extension:
    if (Decl *D = parseDeclExtension(Flags).getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;
  case tok::kw_var:
    if (StaticLoc.isValid()) {
      diagnose(Tok, diag::unimplemented_static_var)
        .highlight(SourceRange(StaticLoc));
      UnhandledStatic = false;
    }
    HadParseError = parseDeclVar(Flags, Entries);
    break;
  case tok::kw_typealias:
    if (Decl *D = parseDeclTypeAlias(!(Flags & PD_DisallowTypeAliasDef),
                                     Flags & PD_InProtocol)
            .getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;
  case tok::kw_union:
    if (Decl *D = parseDeclUnion(Flags).getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;
  case tok::kw_case:
    HadParseError = parseDeclUnionElement(Flags, Entries);
    break;
  case tok::kw_struct:
    if (Decl *D = parseDeclStruct(Flags).getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;
  case tok::kw_class:
    if (Decl *D = parseDeclClass(Flags).getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;
  case tok::kw_constructor:
    if (Decl *D = parseDeclConstructor(Flags & PD_HasContainerType)
            .getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;
  case tok::kw_destructor:
    if (Decl *D = parseDeclDestructor(Flags).getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;
  case tok::kw_protocol:
    if (Decl *D = parseDeclProtocol(Flags).getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    break;

  case tok::kw_func:
    if (Decl *D = parseDeclFunc(StaticLoc, Flags).getPtrOrNull())
      Entries.push_back(D);
    else
      HadParseError = true;
    UnhandledStatic = false;
    break;

  case tok::kw_subscript:
    if (StaticLoc.isValid()) {
      diagnose(Tok, diag::subscript_static)
        .fixItRemove(SourceRange(StaticLoc));
      UnhandledStatic = false;
    }
    HadParseError = parseDeclSubscript(Flags & PD_HasContainerType,
                                       !(Flags & PD_DisallowFuncDef),
                                       Entries);
    break;
  
  case tok::identifier:
    if (isStartOfOperatorDecl(Tok, peekToken())) {
      if (Decl *D = parseDeclOperator(Flags & PD_AllowTopLevel).getPtrOrNull())
        Entries.push_back(D);
      else
        HadParseError = true;
      break;
    }
    SWIFT_FALLTHROUGH;

  default:
    diagnose(Tok, diag::expected_decl);
    HadParseError = true;
    break;
  }

  if (!HadParseError && Tok.is(tok::semi))
    Entries.back()->TrailingSemiLoc = consumeToken(tok::semi);

  if (HadParseError &&
      Tok.is(tok::code_complete) && isCodeCompletionFirstPass() &&
      !CurDeclContext->isModuleContext()) {
    // Only consume non-toplevel decls.
    consumeDecl(BeginParserPosition, Flags);

    // Pretend that there was no error.
    return false;
  }

  // If we parsed 'static' but didn't handle it above, complain about it.
  if (!HadParseError && UnhandledStatic) {
    diagnose(Entries.back()->getLoc(), diag::decl_not_static)
      .fixItRemove(SourceRange(StaticLoc));
  }

  return HadParseError;
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

/// Parse an 'import' declaration, returning true (and doing no token skipping)
/// on error.
///
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
///
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

/// parseInheritance - Parse an inheritance clause.
///
///   inheritance:
///      ':' type-identifier (',' type-identifier)*
bool Parser::parseInheritance(SmallVectorImpl<TypeLoc> &Inherited) {
  consumeToken(tok::colon);
  
  do {
    // Parse the inherited type (which must be a protocol).
    ParserResult<TypeRepr> Ty = parseTypeIdentifier();
    if (Ty.isNull() || Ty.hasCodeCompletion())
      return true;
    
    // Record the type.
    Inherited.push_back(Ty.get());
    
    // Check for a ',', which indicates that there are more protocols coming.
  } while (consumeIf(tok::comma));
  
  return false;
}

/// parseDeclExtension - Parse an 'extension' declaration.
///   extension:
///    'extension' type-identifier inheritance? '{' decl* '}'
///
ParserResult<ExtensionDecl> Parser::parseDeclExtension(unsigned Flags) {
  SourceLoc ExtensionLoc = consumeToken(tok::kw_extension);

  // The grammar allows only type-identifier here, but we parse type-simple for
  // recovery purposes and let the type checker reject types that can not be
  // extended.
  ParserResult<TypeRepr> Ty = parseTypeSimple();
  if (Ty.hasCodeCompletion())
    return makeParserCodeCompletionResult<ExtensionDecl>();
  if (Ty.isNull())
    return nullptr;
  // Diagnose extensions for paren types in the parser because a ParenType is
  // canonically equivalent to the wrapped type, and we are using syntactic
  // information to differentiate between them.
  if (auto *TTR = dyn_cast<TupleTypeRepr>(Ty.get())) {
    if (TTR->isParenType()) {
      diagnose(TTR->getStartLoc(), diag::paren_type_in_extension)
        .highlight(TTR->getSourceRange());
    }
  }
  SourceLoc LBLoc, RBLoc;
  
  // Parse optional inheritance clause.
  SmallVector<TypeLoc, 2> Inherited;
  if (Tok.is(tok::colon))
    parseInheritance(Inherited);

  ExtensionDecl *ED
    = new (Context) ExtensionDecl(ExtensionLoc, Ty.get(),
                                  Context.AllocateCopy(Inherited),
                                  CurDeclContext);
  ContextChange CC(*this, ED);
  Scope S(this, ScopeKind::Extension);

  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_extension)) {
    ED->setMembers({}, Tok.getLoc());
    return makeParserErrorResult(ED);
  }

  SmallVector<Decl*, 8> MemberDecls;

  parseList(tok::r_brace, LBLoc, RBLoc, tok::semi, /*OptionalSep=*/ true,
            diag::expected_rbrace_extension, [&]()->bool {
    return parseDecl(MemberDecls, PD_HasContainerType | PD_DisallowVar);
  });

  ED->setMembers(Context.AllocateCopy(MemberDecls), { LBLoc, RBLoc });

  if (!(Flags & PD_AllowTopLevel)) {
    diagnose(ExtensionLoc, diag::decl_inner_scope);
    return nullptr;
  }

  return makeParserResult(ED);
}

/// parseDeclTypeAlias
///   decl-typealias:
///     'typealias' identifier inheritance? '=' type
///
ParserResult<TypeDecl> Parser::parseDeclTypeAlias(bool WantDefinition,
                                                  bool isAssociatedType) {
  SourceLoc TypeAliasLoc = consumeToken(tok::kw_typealias);
  
  Identifier Id;
  ParserResult<TypeRepr> UnderlyingTy;
  SourceLoc IdLoc;
  if (parseIdentifier(Id, IdLoc, diag::expected_identifier_in_decl,"typealias"))
    return nullptr;

  // Parse optional inheritance clause.
  SmallVector<TypeLoc, 2> Inherited;
  if (Tok.is(tok::colon))
    parseInheritance(Inherited);

  if (WantDefinition || Tok.is(tok::equal)) {
    if (parseToken(tok::equal, diag::expected_equal_in_typealias))
      return nullptr;
    UnderlyingTy = parseType(diag::expected_type_in_typealias);
    if (UnderlyingTy.hasCodeCompletion())
      return makeParserCodeCompletionResult<TypeDecl>();
    if (UnderlyingTy.isNull())
      return nullptr;
    
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
    return makeParserResult(assocType);
  }

  // Otherwise, build a typealias.
  TypeAliasDecl *TAD =
    new (Context) TypeAliasDecl(TypeAliasLoc, Id, IdLoc,
                                UnderlyingTy.getPtrOrNull(),
                                CurDeclContext,
                                Context.AllocateCopy(Inherited));
  addToScope(TAD);
  return makeParserResult(TAD);
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

/// parseSetGet - Parse a get-set clause, containing a getter and (optionally)
/// a setter.
///
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
bool Parser::parseGetSet(bool HasContainerType, Pattern *Indices,
                         TypeLoc ElementTy, FuncDecl *&Get, FuncDecl *&Set,
                         SourceLoc &LastValidLoc) {
  bool Invalid = false;
  Get = 0;
  Set = 0;
  
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
        Get = 0;
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
      
      // Add the implicit 'this' to Params, if needed.
      if (HasContainerType)
        Params.push_back(buildImplicitThisParameter());

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
      FuncExpr *GetFn = actOnFuncExprStart(GetLoc, ElementTy, Params,Params);
      
      // Establish the new context.
      ContextChange CC(*this, GetFn);

      SmallVector<ExprStmtOrDecl, 16> Entries;
      parseBraceItems(Entries, false /*NotTopLevel*/,
                      BraceItemListKind::Property);
      NullablePtr<BraceStmt> Body = BraceStmt::create(Context, ColonLoc,
                                                      Entries, Tok.getLoc());

      if (Body.isNull()) {
        GetLoc = SourceLoc();
        skipUntilDeclRBrace();
        Invalid = true;
        break;
      }
      
      GetFn->setBody(Body.get());

      LastValidLoc = Body.get()->getRBraceLoc();
      
      Get = new (Context) FuncDecl(/*StaticLoc=*/SourceLoc(), GetLoc,
                                   Identifier(), GetLoc, /*generic=*/nullptr,
                                   Type(), GetFn, CurDeclContext);
      GetFn->setDecl(Get);
      continue;
    }

    //   var-set         ::= 'set' var-set-name? stmt-brace
    
    // Have we already parsed a var-set clause?
    if (Set) {
      diagnose(Tok, diag::duplicate_getset, true);
      diagnose(Set->getLoc(), diag::previous_getset, true);

      // Forget the previous setter.
      Set = 0;
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
    
    // Add the implicit 'this' to Params, if needed.
    if (HasContainerType)
      Params.push_back(buildImplicitThisParameter());

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
    FuncExpr *SetFn = actOnFuncExprStart(SetLoc,
                                         TypeLoc::withoutLoc(SetterRetTy),
                                         Params, Params);
    
    // Establish the new context.
    ContextChange CC(*this, SetFn);
    
    // Parse the body.
    SmallVector<ExprStmtOrDecl, 16> Entries;
    parseBraceItems(Entries, false /*NotTopLevel*/,
                    BraceItemListKind::Property);
    NullablePtr<BraceStmt> Body = BraceStmt::create(Context, ColonLoc,
                                                    Entries, Tok.getLoc());

    if (Body.isNull()) {
      skipUntilDeclRBrace();
      Invalid = true;
      break;
    }
    
    SetFn->setBody(Body.get());

    LastValidLoc = Body.get()->getRBraceLoc();
    
    Set = new (Context) FuncDecl(/*StaticLoc=*/SourceLoc(), SetLoc,
                                 Identifier(), SetLoc, /*generic=*/nullptr,
                                 Type(), SetFn, CurDeclContext);
    SetFn->setDecl(Set);
  }
  
  return Invalid;
}

/// parseDeclVarGetSet - Parse the brace-enclosed getter and setter for a variable.
///
///   decl-var:
///      'var' attribute-list identifier : type-annotation { get-set }
void Parser::parseDeclVarGetSet(Pattern &pattern, bool HasContainerType) {
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
  FuncDecl *Get = 0;
  FuncDecl *Set = 0;
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

/// parseDeclVar - Parse a 'var' declaration, returning true (and doing no
/// token skipping) on error.
///
///   decl-var:
///      'var' attribute-list pattern initializer? (',' pattern initializer? )*
///      'var' attribute-list identifier : type-annotation { get-set }
bool Parser::parseDeclVar(unsigned Flags, SmallVectorImpl<Decl*> &Decls){
  SourceLoc VarLoc = consumeToken(tok::kw_var);

  SmallVector<PatternBindingDecl*, 4> PBDs;
  bool HasGetSet = false;
  bool isInvalid = false;

  unsigned FirstDecl = Decls.size();

  do {
    DeclAttributes Attributes;
    parseAttributeList(Attributes);

    ParserResult<Pattern> pattern = parsePattern();
    if (pattern.isNull()) return true;

    // If we syntactically match the second decl-var production, with a
    // var-get-set clause, parse the var-get-set clause.
    if (Tok.is(tok::l_brace)) {
      parseDeclVarGetSet(*pattern.get(), Flags & PD_HasContainerType);
      HasGetSet = true;
    }

    NullablePtr<Expr> Init;
    if (Tok.is(tok::equal)) {
      // Record the variables that we're trying to initialize.
      SmallVector<VarDecl *, 4> Vars;
      Vars.append(CurVars.second.begin(), CurVars.second.end());
      pattern.get()->collectVariables(Vars);
      using RestoreVarsRAII = llvm::SaveAndRestore<decltype(CurVars)>;
      RestoreVarsRAII RestoreCurVars(CurVars, {CurDeclContext, Vars});

      SourceLoc EqualLoc = consumeToken(tok::equal);
      Init = parseExpr(diag::expected_initializer_expr);
      if (Init.isNull()) {
        isInvalid = true;
        break;
      }
    
      if (HasGetSet) {
        diagnose(pattern.get()->getLoc(), diag::getset_init)
          .highlight(Init.get()->getSourceRange());
        Init = nullptr;
      }
      if (Flags & PD_DisallowInit) {
        diagnose(EqualLoc, diag::disallowed_init);
        isInvalid = true;
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
            isInvalid = true;
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
      isInvalid = true;
    }
    if (Flags & PD_DisallowProperty) {
      diagnose(VarLoc, diag::disallowed_property_decl);
      isInvalid = true;
    }
  } else if (Flags & PD_DisallowVar) {
    diagnose(VarLoc, diag::disallowed_var_decl);
    return true;
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

  return isInvalid;
}

/// addImplicitThisParameter - Add an implicit 'this' parameter to the given
/// set of parameter clauses.
Pattern *Parser::buildImplicitThisParameter() {
  VarDecl *D
    = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("this"),
                            Type(), CurDeclContext);
  Pattern *P = new (Context) NamedPattern(D);
  return new (Context) TypedPattern(P, TypeLoc());
}

void Parser::consumeFunctionBody(FuncExpr *FE) {
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

  if (DelayedParseCB->shouldDelayFunctionBodyParsing(*this, FE, BodyRange)) {
    State->delayFunctionBodyParsing(FE, BodyRange,
                                    BeginParserPosition.PreviousLoc);
    FE->setBodyDelayed(BodyRange.End);
  } else {
    FE->setBodySkipped(BodyRange.End);
  }
}

/// parseDeclFunc - Parse a 'func' declaration, returning null on error.  The
/// caller handles this case and does recovery as appropriate.
///
///   decl-func:
///     'static'? 'func' attribute-list any-identifier generic-params?
///               func-signature stmt-brace?
///
/// NOTE: The caller of this method must ensure that the token sequence is
/// either 'func' or 'static' 'func'.
///
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
  if (parseAnyIdentifier(Name, diag::expected_identifier_in_decl, "func"))
    return nullptr;
  
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

  // We force first type of a func declaration to be a tuple for consistency.
  //
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::func_decl_without_paren);
    return nullptr;
  }

  SmallVector<Pattern*, 8> ArgParams;
  SmallVector<Pattern*, 8> BodyParams;
  
  // If we're within a container, add an implicit first pattern to match the
  // container type as an element named 'this'.
  //
  // This turns an instance function "(int)->int" on FooTy into
  // "(this: [byref] FooTy)->(int)->int", and a static function
  // "(int)->int" on FooTy into "(this: [byref] FooTy.metatype)->(int)->int".
  // Note that we can't actually compute the type here until Sema.
  if (HasContainerType) {
    Pattern *thisPattern = buildImplicitThisParameter();
    ArgParams.push_back(thisPattern);
    BodyParams.push_back(thisPattern);
  }

  bool HadSignatureParseError = false;
  TypeRepr *FuncRetTy = nullptr;
  {
    ParserStatus SignatureStatus =
        parseFunctionSignature(ArgParams, BodyParams, FuncRetTy);

    if (SignatureStatus.isError()) {
      HadSignatureParseError = true;
      // Try to recover.  Create a function signature with as much information
      // as possible.
      //
      // FIXME: right now creates a '() -> ()' signature.
      ArgParams.clear();
      BodyParams.clear();
      if (HasContainerType) {
        Pattern *thisPattern = buildImplicitThisParameter();
        ArgParams.push_back(thisPattern);
        BodyParams.push_back(thisPattern);
      }
      auto *VoidPattern =
          TuplePattern::create(Context, Tok.getLoc(), {}, Tok.getLoc());
      ArgParams.push_back(VoidPattern);
      BodyParams.push_back(VoidPattern);
      // FuncRetTy is always initialized by parseFunctionSignature().
    }

    if (SignatureStatus.hasCodeCompletion()) {
      if (!CodeCompletion)
        // Trigger delayed parsing, no need to continue.
        return SignatureStatus;

      // Create function AST nodes.
      FuncExpr *FE =
          actOnFuncExprStart(FuncLoc, FuncRetTy, ArgParams, BodyParams);
      FuncDecl *FD = new (Context) FuncDecl(StaticLoc, FuncLoc, Name, NameLoc,
                                            GenericParams, Type(), FE,
                                            CurDeclContext);
      FE->setDecl(FD);
      FE->setBodySkipped(Tok.getLoc());

      // Pass the function signature to code completion.
      CodeCompletion->setDelayedParsedDecl(FD);
    }
  }

  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  FuncExpr *FE = 0;
  {
    Scope S(this, ScopeKind::FunctionBody);
    
    FE = actOnFuncExprStart(FuncLoc, FuncRetTy, ArgParams, BodyParams);

    // Now that we have a context, update the generic parameters with that
    // context.
    if (GenericParams) {
      for (auto Param : *GenericParams) {
        Param.setDeclContext(FE);
      }
    }
    
    // Establish the new context.
    ContextChange CC(*this, FE);

    // Check to see if we have a "{" to start a brace statement.
    if (Tok.is(tok::l_brace)) {
      if (Flags & PD_DisallowFuncDef) {
        diagnose(Tok, diag::disallowed_func_def);
        consumeToken();
        skipUntil(tok::r_brace);
        consumeToken();
        // FIXME: don't just drop the body.
      } else if (!isDelayedParsingEnabled()) {
        NullablePtr<BraceStmt> Body =
            parseBraceItemList(diag::func_decl_without_brace);
        if (Body.isNull()) {
          // FIXME: Should do some sort of error recovery here?
        } else {
          FE->setBody(Body.get());
        }
      } else {
        consumeFunctionBody(FE);
      }
    } else if (Attributes.AsmName.empty() && !(Flags & PD_DisallowFuncDef) &&
               !HadSignatureParseError) {
      diagnose(NameLoc, diag::func_decl_without_brace);
    }
  }

  // Exit the scope introduced for the generic parameters.
  GenericsScope.reset();

  // Create the decl for the func and add it to the parent scope.
  FuncDecl *FD = new (Context) FuncDecl(StaticLoc, FuncLoc, Name, NameLoc,
                                        GenericParams, Type(), FE,
                                        CurDeclContext);
  if (FE)
    FE->setDecl(FD);
  if (Attributes.isValid()) FD->getMutableAttrs() = Attributes;
  addToScope(FD);
  return makeParserResult(FD);
}

bool Parser::parseDeclFuncBodyDelayed(FuncDecl *FD) {
  auto FE = FD->getBody();
  assert(!FE->getBody() && "function should not have a parsed body");
  assert(FE->getBodyKind() == FuncExpr::BodyKind::Unparsed &&
         "function body should be delayed");

  auto FunctionParserState = State->takeBodyState(FE);
  assert(FunctionParserState.get() && "should have a valid state");

  auto BeginParserPosition = getParserPosition(FunctionParserState->BodyPos);
  auto EndLexerState = L->getStateForEndOfTokenLoc(FE->getEndLoc());

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
  ContextChange CC(*this, FE);

  NullablePtr<BraceStmt> Body =
      parseBraceItemList(diag::func_decl_without_brace);
  if (Body.isNull()) {
    // FIXME: Should do some sort of error recovery here?
    return true;
  } else {
    FE->setBody(Body.get());
  }

  return false;
}

/// parseDeclUnion - Parse a 'union' declaration, returning true (and doing no
/// token skipping) on error.
///
///   decl-union:
///      'union' attribute-list identifier generic-params? inheritance?
///          '{' decl-union-body '}'
///   decl-union-body:
///      decl*
///
ParserResult<UnionDecl> Parser::parseDeclUnion(unsigned Flags) {
  SourceLoc UnionLoc = consumeToken(tok::kw_union);

  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  Identifier UnionName;
  SourceLoc UnionNameLoc;
  if (parseIdentifier(UnionName, UnionNameLoc,
                      diag::expected_identifier_in_decl, "union"))
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

  // Now that we have a context, update the generic parameters with that
  // context.
  if (GenericParams)
    for (auto Param : *GenericParams)
      Param.setDeclContext(UD);

  if (Attributes.isValid())
    UD->getMutableAttrs() = Attributes;

  // Parse optional inheritance clause.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, UD);
    SmallVector<TypeLoc, 2> Inherited;
    parseInheritance(Inherited);
    UD->setInherited(Context.AllocateCopy(Inherited));
  }

  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_union_type)) {
    UD->setMembers({}, Tok.getLoc());
    return makeParserErrorResult(UD);
  }

  bool Invalid = false;
  // Parse the body.
  SmallVector<Decl*, 8> MemberDecls;
  {
    ContextChange CC(*this, UD);
    Scope S(this, ScopeKind::ClassBody);
    Invalid |= parseNominalDeclMembers(MemberDecls, LBLoc, RBLoc,
                                       diag::expected_rbrace_union_type,
                                       PD_HasContainerType
                                         | PD_AllowUnionElement
                                         | PD_DisallowVar);
  }

  UD->setMembers(Context.AllocateCopy(MemberDecls), {LBLoc, RBLoc});
  addToScope(UD);

  if (Flags & PD_DisallowNominalTypes) {
    diagnose(UnionLoc, diag::disallowed_type);
    return makeParserErrorResult(UD);
  }

  if (Invalid)
    return makeParserErrorResult(UD);
  else
    return makeParserResult(UD);
}

/// parseDeclUnionElement - Parse a 'case' of a union, returning true on error.
///
///   decl-union-element:
///      'case' identifier type-tuple? ('->' type)?
bool Parser::parseDeclUnionElement(unsigned Flags,
                                   SmallVectorImpl<Decl*> &Decls) {
  SourceLoc CaseLoc = consumeToken(tok::kw_case);
  
  // TODO: Accept attributes here?
  
  Identifier Name;
  SourceLoc NameLoc;
  // For recovery, see if the user typed something resembling a switch "case"
  // label.
  if (!Tok.is(tok::identifier)) {
    ParserResult<Pattern> pattern = parseMatchingPattern();
    if (pattern.isNull())
      return true;
    diagnose(CaseLoc, diag::case_outside_of_switch, "case");
    skipUntil(tok::colon);
    consumeIf(tok::colon);
    return true;
  }
  
  if (parseIdentifier(Name, NameLoc,
                      diag::expected_identifier_in_decl, "union case"))
    return true;
  
  // See if there's a following argument type.
  ParserResult<TypeRepr> ArgType;
  if (Tok.isFollowingLParen()) {
    ArgType = parseTypeTupleBody();
    if (ArgType.isNull() || ArgType.hasCodeCompletion())
      return true;
  }
  
  // See if there's a result type.
  SourceLoc ArrowLoc;
  ParserResult<TypeRepr> ResultType;
  if (Tok.is(tok::arrow)) {
    ArrowLoc = consumeToken();
    ResultType = parseType(diag::expected_type_union_element_result);
    if (ResultType.isNull() || ResultType.hasCodeCompletion())
      return true;
  }
  
  // For recovery, again make sure the the user didn't try to spell a switch
  // case label:
  // 'case Identifier:',
  // 'case Identifier, ...:', or
  // 'case Identifier where ...:'
  if (Tok.is(tok::colon) || Tok.is(tok::comma) || Tok.is(tok::kw_where)) {
    diagnose(CaseLoc, diag::case_outside_of_switch, "case");
    skipUntilDeclRBrace();
    return true;
  }
  
  // Create the element.
  auto *result = new (Context) UnionElementDecl(CaseLoc, NameLoc, Name,
                                                ArgType.getPtrOrNull(),
                                                ArrowLoc,
                                                ResultType.getPtrOrNull(),
                                                CurDeclContext);
  if (!(Flags & PD_AllowUnionElement)) {
    diagnose(CaseLoc, diag::disallowed_union_element);
    return true;
  }
  // Don't add the UnionElementDecl to a DeclContext unless it is allowed to
  // have a UnionElementDecl in that context.
  Decls.push_back(result);

  return false;
}

/// \brief Parse the members in a struct/class/protocol definition.
///
///    decl*
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
    if (parseDecl(memberDecls, flags))
      return true;

    // Check whether the previous declaration had a semicolon after it.
    if (!memberDecls.empty() && memberDecls.back()->TrailingSemiLoc.isValid())
      previousHadSemi = true;

    return false;
  });
}

/// parseDeclStruct - Parse a 'struct' declaration, returning true (and doing no
/// token skipping) on error.
///
///   decl-struct:
///      'struct' attribute-list identifier generic-params? inheritance?
///          '{' decl-struct-body '}
///   decl-struct-body:
///      decl*
///
ParserResult<StructDecl> Parser::parseDeclStruct(unsigned Flags) {
  SourceLoc StructLoc = consumeToken(tok::kw_struct);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  Identifier StructName;
  SourceLoc StructNameLoc;
  if (parseIdentifier(StructName, StructNameLoc,
                      diag::expected_identifier_in_decl, "struct"))
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

  if (Attributes.isValid()) SD->getMutableAttrs() = Attributes;

  // Now that we have a context, update the generic parameters with that
  // context.
  if (GenericParams) {
    for (auto Param : *GenericParams) {
      Param.setDeclContext(SD);
    }
  }

  // Parse optional inheritance clause.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, SD);
    SmallVector<TypeLoc, 2> Inherited;
    parseInheritance(Inherited);
    SD->setInherited(Context.AllocateCopy(Inherited));
  }

  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_struct)) {
    SD->setMembers({}, Tok.getLoc());
    return makeParserErrorResult(SD);
  }

  bool Invalid = false;

  // Parse the body.
  SmallVector<Decl*, 8> MemberDecls;
  {
    ContextChange CC(*this, SD);
    Scope S(this, ScopeKind::StructBody);
    Invalid |= parseNominalDeclMembers(MemberDecls, LBLoc, RBLoc,
                                       diag::expected_rbrace_struct,
                                       PD_HasContainerType);
  }

  SD->setMembers(Context.AllocateCopy(MemberDecls), { LBLoc, RBLoc });
  addToScope(SD);

  if (Flags & PD_DisallowNominalTypes) {
    diagnose(StructLoc, diag::disallowed_type);
    return makeParserErrorResult(SD);
  }

  if (Invalid)
    return makeParserErrorResult(SD);
  else
    return makeParserResult(SD);
}

/// parseDeclClass - Parse a 'class' declaration, returning true (and doing no
/// token skipping) on error.  
///
///   decl-class:
///      'class' attribute-list identifier generic-params? inheritance?
///          '{' decl-class-body '}
///   decl-class-body:
///      decl*
///
ParserResult<ClassDecl> Parser::parseDeclClass(unsigned Flags) {
  SourceLoc ClassLoc = consumeToken(tok::kw_class);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  Identifier ClassName;
  SourceLoc ClassNameLoc;
  SourceLoc LBLoc, RBLoc;
  if (parseIdentifier(ClassName, ClassNameLoc,
                      diag::expected_identifier_in_decl, "class"))
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

  // Now that we have a context, update the generic parameters with that
  // context.
  if (GenericParams) {
    for (auto Param : *GenericParams) {
      Param.setDeclContext(CD);
    }
  }

  // Attach attributes.
  if (Attributes.isValid()) CD->getMutableAttrs() = Attributes;

  // Parse optional inheritance clause within the context of the class.
  SmallVector<TypeLoc, 2> Inherited;
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, CD);    
    parseInheritance(Inherited);
    CD->setInherited(Context.AllocateCopy(Inherited));
  }

  // Parse the class body.
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_class)) {
    CD->setMembers({}, Tok.getLoc());
    return makeParserErrorResult(CD);
  }

  bool Invalid = false;

  // Parse the body.
  SmallVector<Decl*, 8> MemberDecls;
  {
    ContextChange CC(*this, CD);
    Scope S(this, ScopeKind::ClassBody);
    Invalid |= parseNominalDeclMembers(MemberDecls, LBLoc, RBLoc,
                                       diag::expected_rbrace_class,
                                       PD_HasContainerType|PD_AllowDestructor);
  }

  bool hasConstructor = false;
  for (Decl *Member : MemberDecls) {
    if (isa<ConstructorDecl>(Member))
      hasConstructor = true;
  }

  if (!hasConstructor) {
    VarDecl *ThisDecl
      = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("this"),
                              Type(), CD);
    Pattern *Arguments = TuplePattern::create(Context, SourceLoc(),
                                              ArrayRef<TuplePatternElt>(),
                                              SourceLoc());
    ConstructorDecl *Constructor =
        new (Context) ConstructorDecl(Context.getIdentifier("constructor"),
                                     SourceLoc(), Arguments, ThisDecl,
                                     nullptr, CD);
    Constructor->setImplicit();
    ThisDecl->setDeclContext(Constructor);
    MemberDecls.push_back(Constructor);
  }

  CD->setMembers(Context.AllocateCopy(MemberDecls), { LBLoc, RBLoc });
  addToScope(CD);

  if (Flags & PD_DisallowNominalTypes) {
    diagnose(ClassLoc, diag::disallowed_type);
    return makeParserErrorResult(CD);
  }

  if (Invalid)
    return makeParserErrorResult(CD);
  else
    return makeParserResult(CD);
}

/// parseDeclProtocol - Parse a 'protocol' declaration, returning null (and
/// doing no token skipping) on error.
///
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
///
ParserResult<ProtocolDecl> Parser::parseDeclProtocol(unsigned Flags) {
  SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
  
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  SourceLoc NameLoc;
  Identifier ProtocolName;
  if (parseIdentifier(ProtocolName, NameLoc,
                      diag::expected_identifier_in_decl, "protocol"))
    return nullptr;
  
  SmallVector<TypeLoc, 4> InheritedProtocols;
  if (Tok.is(tok::colon))
    parseInheritance(InheritedProtocols);

  ProtocolDecl *Proto
    = new (Context) ProtocolDecl(CurDeclContext, ProtocolLoc, NameLoc,
                                 ProtocolName,
                                 Context.AllocateCopy(InheritedProtocols));

  if (Attributes.isValid()) Proto->getMutableAttrs() = Attributes;

  ContextChange CC(*this, Proto);
  Scope ProtocolBodyScope(this, ScopeKind::ProtocolBody);

  // Parse the body.
  {
    // The list of protocol elements.
    SmallVector<Decl*, 8> Members;

    // Add the implicit 'This' associated type.
    Members.push_back(new (Context) AssociatedTypeDecl(
                                      CurDeclContext,
                                      SourceLoc(),
                                      Context.getIdentifier("This"),
                                      SourceLoc()));
    Members.back()->setImplicit();

    SourceLoc LBraceLoc = Tok.getLoc();
    if (parseToken(tok::l_brace, diag::expected_lbrace_protocol_type)) {
      Proto->setMembers(Context.AllocateCopy(Members), Tok.getLoc());
      return makeParserErrorResult(Proto);
    }

    SourceLoc RBraceLoc;
    // Parse the members.
    if (parseNominalDeclMembers(Members, LBraceLoc, RBraceLoc,
                                diag::expected_rbrace_protocol,
                                PD_HasContainerType|PD_DisallowProperty|
                                PD_DisallowFuncDef|PD_DisallowNominalTypes|
                                PD_DisallowInit|PD_DisallowTypeAliasDef|
                                PD_InProtocol))
      return nullptr;

    // Install the protocol elements.
    Proto->setMembers(Context.AllocateCopy(Members),
                      SourceRange(LBraceLoc, RBraceLoc));
  }
  
  if (Flags & PD_DisallowNominalTypes) {
    diagnose(ProtocolLoc, diag::disallowed_type);
    return nullptr;
  } else if (!(Flags & PD_AllowTopLevel)) {
    diagnose(ProtocolLoc, diag::decl_inner_scope);
    return nullptr;
  }

  return makeParserResult(Proto);
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

/// parseDeclSubscript - Parse a 'subscript' declaration, returning true
/// on error.
///
///   decl-subscript:
///     subscript-head get-set
///   subscript-head
///     'subscript' attribute-list pattern-tuple '->' type
///
bool Parser::parseDeclSubscript(bool HasContainerType,
                                bool NeedDefinition,
                                SmallVectorImpl<Decl *> &Decls) {
  bool Invalid = false;
  SourceLoc SubscriptLoc = consumeToken(tok::kw_subscript);

  // attribute-list
  DeclAttributes Attributes;
  parseAttributeList(Attributes);
  
  // pattern-tuple
  if (Tok.isNot(tok::l_paren)) {
    diagnose(Tok, diag::expected_lparen_subscript);
    return true;
  }

  ParserResult<Pattern> Indices = parsePatternTuple(/*AllowInitExpr=*/false);
  if (Indices.isNull())
    return true;
  Indices.get()->walk(SetVarContext(CurDeclContext));

  // '->'
  if (!Tok.is(tok::arrow)) {
    diagnose(Tok, diag::expected_arrow_subscript);
    return true;
  }
  SourceLoc ArrowLoc = consumeToken();
  
  // type
  ParserResult<TypeRepr> ElementTy =
      parseTypeAnnotation(diag::expected_type_subscript);
  if (ElementTy.isNull() || ElementTy.hasCodeCompletion())
    return true;
  
  if (!NeedDefinition) {
    SubscriptDecl *Subscript
      = new (Context) SubscriptDecl(Context.getIdentifier("__subscript"),
                                    SubscriptLoc, Indices.get(), ArrowLoc,
                                    ElementTy.get(), SourceRange(),
                                    0, 0, CurDeclContext);
    Decls.push_back(Subscript);
    return false;
  }
  
  // '{'
  if (!Tok.is(tok::l_brace)) {
    diagnose(Tok, diag::expected_lbrace_subscript);
    return true;
  }
  SourceLoc LBLoc = consumeToken();
  
  // Parse getter and setter.
  FuncDecl *Get = 0;
  FuncDecl *Set = 0;
  SourceLoc LastValidLoc = LBLoc;
  if (parseGetSet(HasContainerType, Indices.get(), ElementTy.get(),
                  Get, Set, LastValidLoc))
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

  if (!Get) {
    if (!Invalid)
      diagnose(SubscriptLoc, diag::subscript_without_get);
    Invalid = true;
  }

  // Reject 'subscript' functions outside of type decls
  if (!HasContainerType) {
    diagnose(SubscriptLoc, diag::subscript_decl_wrong_scope);
    Invalid = true;
  }

  if (!Invalid) {
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
  return Invalid;
}

static void AddConstructorArgumentsToScope(const Pattern *pat,
                                           ConstructorDecl *CD,
                                           Parser &P) {
  switch (pat->getKind()) {
  case PatternKind::Named: {
    // Reparent the decl and add it to the scope.
    VarDecl *var = cast<NamedPattern>(pat)->getDecl();
    var->setDeclContext(CD);
    P.addToScope(var);
    return;
  }

  case PatternKind::Any:
    return;

  case PatternKind::Paren:
    AddConstructorArgumentsToScope(cast<ParenPattern>(pat)->getSubPattern(),
                                   CD, P);
    return;

  case PatternKind::Typed:
    AddConstructorArgumentsToScope(cast<TypedPattern>(pat)->getSubPattern(),
                                   CD, P);
    return;

  case PatternKind::Tuple:
    for (const TuplePatternElt &field : cast<TuplePattern>(pat)->getFields())
      AddConstructorArgumentsToScope(field.getPattern(), CD, P);
    return;

#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
    llvm_unreachable("pattern can't appear as a constructor argument!");
  }
  llvm_unreachable("bad pattern kind!");
}


ParserResult<ConstructorDecl>
Parser::parseDeclConstructor(bool HasContainerType) {
  SourceLoc ConstructorLoc = consumeToken(tok::kw_constructor);
  
  // Reject 'constructor' functions outside of types
  if (!HasContainerType) {
    diagnose(Tok, diag::constructor_decl_wrong_scope);
    return nullptr;
  }

  // attribute-list
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  // Parse the generic-params, if present.
  Scope S(this, ScopeKind::Generics);
  GenericParamList *GenericParams = maybeParseGenericParams();

  // pattern-tuple
  if (!Tok.is(tok::l_paren)) {
    diagnose(Tok, diag::expected_lparen_constructor);
    return nullptr;
  }

  ParserResult<Pattern> Arguments = parsePatternTuple(/*AllowInitExpr=*/true);
  if (Arguments.isNull())
    return nullptr;

  // '{'
  if (!Tok.is(tok::l_brace)) {
    diagnose(Tok, diag::expected_lbrace_constructor);
    return nullptr;
  }

  VarDecl *ThisDecl
    = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("this"),
                            Type(), CurDeclContext);

  Scope S2(this, ScopeKind::ConstructorBody);
  ConstructorDecl *CD =
      new (Context) ConstructorDecl(Context.getIdentifier("constructor"),
                                    ConstructorLoc, Arguments.get(), ThisDecl,
                                    GenericParams, CurDeclContext);
  ThisDecl->setDeclContext(CD);
  if (GenericParams) {
    for (auto Param : *GenericParams)
      Param.setDeclContext(CD);
  }
  AddConstructorArgumentsToScope(Arguments.get(), CD, *this);
  addToScope(ThisDecl);
  ContextChange CC(*this, CD);

  NullablePtr<BraceStmt> Body = parseBraceItemList(diag::invalid_diagnostic);

  if (!Body.isNull())
    CD->setBody(Body.get());

  if (Attributes.isValid()) CD->getMutableAttrs() = Attributes;

  return makeParserResult(CD);
}

ParserResult<DestructorDecl> Parser::parseDeclDestructor(unsigned Flags) {
  SourceLoc DestructorLoc = consumeToken(tok::kw_destructor);

  // attribute-list
  DeclAttributes Attributes;
  parseAttributeList(Attributes);

  // '{'
  if (!Tok.is(tok::l_brace)) {
    diagnose(Tok, diag::expected_lbrace_destructor);
    return nullptr;
  }

  VarDecl *ThisDecl
    = new (Context) VarDecl(SourceLoc(), Context.getIdentifier("this"),
                            Type(), CurDeclContext);

  Scope S(this, ScopeKind::DestructorBody);
  DestructorDecl *DD =
      new (Context) DestructorDecl(Context.getIdentifier("destructor"),
                                   DestructorLoc, ThisDecl, CurDeclContext);
  ThisDecl->setDeclContext(DD);
  addToScope(ThisDecl);
  ContextChange CC(*this, DD);

  NullablePtr<BraceStmt> Body = parseBraceItemList(diag::invalid_diagnostic);

  if (!Body.isNull())
    DD->setBody(Body.get());

  if (Attributes.isValid()) DD->getMutableAttrs() = Attributes;

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

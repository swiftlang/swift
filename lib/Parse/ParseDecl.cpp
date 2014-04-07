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
#include "swift/AST/Attr.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include <algorithm>

using namespace swift;

/// \brief Build an implicit 'self' parameter for the specified DeclContext.
static Pattern *buildImplicitSelfParameter(SourceLoc Loc,
                                           DeclContext *CurDeclContext,
                                           VarDecl **SelfDeclRet = nullptr) {
  ASTContext &Ctx = CurDeclContext->getASTContext();
  auto *SelfDecl = new (Ctx) VarDecl(/*static*/ false, /*IsLet*/ true,
                                     Loc, Ctx.Id_self,
                                     Type(), CurDeclContext);
  // FIXME: Remove SelfDeclRet when we don't need it anymore.
  if (SelfDeclRet)
    *SelfDeclRet = SelfDecl;

  SelfDecl->setImplicit();
  Pattern *P = new (Ctx) NamedPattern(SelfDecl, /*Implicit=*/true);
  return new (Ctx) TypedPattern(P, TypeLoc());
}


/// \brief Main entrypoint for the parser.
///
/// \verbatim
///   top-level:
///     stmt-brace-item*
///     decl-sil       [[only in SIL mode]
///     decl-sil-stage [[only in SIL mode]
/// \endverbatim
bool Parser::parseTopLevel() {
  SF.ASTStage = SourceFile::Parsing;

  // Prime the lexer.
  if (Tok.is(tok::NUM_TOKENS))
    consumeToken();

  CurDeclContext = &SF;

  // Parse the body of the file.
  SmallVector<ASTNode, 128> Items;

  skipExtraTopLevelRBraces();

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
  } else if (Tok.is(tok::kw_sil_vtable)) {
    assert(isInSILMode() && "'sil' should only be a keyword in SIL mode");
    parseSILVTable();
  } else if (Tok.is(tok::kw_sil_global)) {
    assert(isInSILMode() && "'sil' should only be a keyword in SIL mode");
    parseSILGlobal();
  } else if (Tok.is(tok::kw_sil_witness_table)) {
    assert(isInSILMode() && "'sil' should only be a keyword in SIL mode");
    parseSILWitnessTable();
  } else {
    parseBraceItems(Items,
                    allowTopLevelCode() ? BraceItemListKind::TopLevelCode
                                        : BraceItemListKind::TopLevelLibrary);
  }
  
  // In the case of a catastrophic parse error, consume any trailing
  // #else or #endif and move on to the next statement or declaration
  // block.
  if (Tok.is(tok::pound_else) || Tok.is(tok::pound_endif)) {
    diagnose(Tok.getLoc(), diag::unexpected_config_block_terminator);
    consumeToken();
  }

  // If this is a Main source file, determine if we found code that needs to be
  // executed (this is used by the repl to know whether to compile and run the
  // newly parsed stuff).
  bool FoundTopLevelCodeToExecute = false;
  if (allowTopLevelCode()) {
    for (auto V : Items)
      if (isa<TopLevelCodeDecl>(V.get<Decl*>()))
        FoundTopLevelCodeToExecute = true;
  }

  // Add newly parsed decls to the module.
  for (auto Item : Items)
    if (Decl *D = Item.dyn_cast<Decl*>())
      SF.Decls.push_back(D);

  // Note that the source file is fully parsed and verify it.
  SF.ASTStage = SourceFile::Parsed;
  verify(SF);

  // Next time start relexing from the beginning of the comment so that we can
  // attach it to the token.
  State->markParserPosition(Tok.getCommentRange().getStart(), PreviousLoc);

  return FoundTopLevelCodeToExecute;
}

bool Parser::skipExtraTopLevelRBraces() {
  if (!Tok.is(tok::r_brace))
    return false;
  while (Tok.is(tok::r_brace)) {
    diagnose(Tok, diag::extra_rbrace)
        .fixItRemove(Tok.getLoc());
    consumeToken();
  }
  return true;
}

/// getTypeAttrFromString - If the specified string is a valid type attribute,
/// return the kind.  Otherwise, return TAK_Count as a sentinel.
static TypeAttrKind getTypeAttrFromString(StringRef Str) {
  return llvm::StringSwitch<TypeAttrKind>(Str)
#define TYPE_ATTR(X) .Case(#X, TAK_##X)
#include "swift/AST/Attr.def"
  .Default(TAK_Count);
}

/// If the specified string is a valid declaration attribute,
/// return the kind.  Otherwise, return DAK_Count as a sentinel.
static DeclAttrKind getDeclAttrFromString(StringRef Str) {
  return llvm::StringSwitch<DeclAttrKind>(Str)
#define DECL_ATTR(X, ...) .Case(#X, DAK_##X)
#include "swift/AST/Attr.def"
  .Default(DAK_Count);
}

static StringRef getStringLiteralIfNotInterpolated(Parser &P,
                                                   SourceLoc Loc,
                                                   const Token &Tok,
                                                   StringRef DiagText) {
  SmallVector<Lexer::StringSegment, 1> Segments;
  P.L->getStringLiteralSegments(Tok, Segments);
  if (Segments.size() != 1 ||
      Segments.front().Kind == Lexer::StringSegment::Expr) {
   P.diagnose(Loc, diag::attr_interpolated_string, DiagText);
   return StringRef();
  }

  SourceManager &SourceMgr = P.SourceMgr;
  return StringRef(SourceMgr->getMemoryBuffer(P.BufferID)->getBufferStart() +
              SourceMgr.getLocOffsetInBuffer(Segments.front().Loc, P.BufferID),
              Segments.front().Length);
}

bool Parser::parseNewDeclAttribute(DeclAttributes &Attributes,
                                   SourceLoc AtLoc,
                                   SourceLoc InversionLoc,
                                   StringRef AttrName,
                                   DeclAttrKind DK) {
  // Ok, it is a valid attribute, eat it, and then process it.
  SourceLoc Loc = consumeToken();
  bool DiscardAttribute = false;

  // Diagnose duplicated attributes.
  const DeclAttribute *DuplicateAttribute = nullptr;
  if (!DeclAttribute::allowMultipleAttributes(DK))
    if ((DuplicateAttribute = Attributes.getAttribute(DK))) {
      // Delay issuing the diagnostic until we parse the attribute.
      DiscardAttribute = true;
    }

  if (InversionLoc.isValid())
    diagnose(InversionLoc, diag::invalid_attribute_inversion);

  // Filled in during parsing.  If there is a duplicate
  // diagnostic this can be used for better error presentation.
  SourceRange AttrRange;

  switch (DK) {
  case DAK_Count:
    llvm_unreachable("DAK_Count should not appear in parsing switch");
  case DAK_asmname: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName);
      return false;
    }

    if (Tok.isNot(tok::string_literal)) {
      diagnose(Loc, diag::attr_expected_string_literal, AttrName);
      return false;
    }

    StringRef AsmName =
      getStringLiteralIfNotInterpolated(*this, Loc, Tok, "asmname");

    if (!AsmName.empty())
      AttrRange = SourceRange(Loc, Tok.getRange().getStart());
    else
      DiscardAttribute = true;

    consumeToken(tok::string_literal);

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName);
      return false;
    }

    if (!DiscardAttribute)
      Attributes.add(new (Context) AsmnameAttr(AsmName, AtLoc, AttrRange,
                                               /*Implicit=*/false));

    break;
  }
  case DAK_availability: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName);
      return false;
    }

    // platform:
    //   *
    //   identifier

    StringRef Platform;

    if (Tok.is(tok::identifier)) {
      // FIXME: This is a hard coded list probably doesn't belong
      // here.  Putting here now for bringup.
      bool IsValid =
        llvm::StringSwitch<bool>(Tok.getText())
          .Case("ios", true)
          .Case("macosx", true)
          .Case("ios_app_extension", true)
          .Case("macosx_app_extension", true)
          .Default(false);
      if (!IsValid) {
        diagnose(Loc, diag::attr_availability_unknown_platform,
                 Tok.getText(), AttrName)
          .highlight(SourceRange(Tok.getLoc()));
        return false;
      }
      Platform = Tok.getText();
    }
    else if (!(Tok.isAnyOperator() && Tok.getText() == "*")) {
      diagnose(Tok.getLoc(), diag::attr_availability_platform, AttrName)
        .highlight(SourceRange(Tok.getLoc()));
      return false;
    }

    consumeToken();

    // Parse the kind, looking for 'unavailable'.  This needs to be
    // relaxed later, but this is strict now for bringup.

    if (!consumeIf(tok::comma)) {
      diagnose(Tok.getLoc(), diag::attr_expected_comma, AttrName);
      return false;
    }

    if (!Tok.is(tok::identifier) || Tok.getText() != "unavailable") {
      diagnose(Tok.getLoc(), diag::attr_availability_expected_option,
               AttrName)
        .highlight(SourceRange(Tok.getLoc()));
      return false;
    }

    consumeToken();

    StringRef Message;

    if (consumeIf(tok::comma)) {
      if (!Tok.is(tok::identifier) || Tok.getText() != "message") {
        diagnose(Tok.getLoc(), diag::attr_availability_expected_option,
                 AttrName)
        .highlight(SourceRange(Tok.getLoc()));
        return false;
      }

      consumeToken();

      if (!consumeIf(tok::equal)) {
        diagnose(Tok.getLoc(), diag::attr_availability_expected_equal,
                 AttrName, "message");
        return false;
      }

      if (Tok.isNot(tok::string_literal)) {
        diagnose(Loc, diag::attr_expected_string_literal, AttrName);
        return false;
      }

      Message =
        getStringLiteralIfNotInterpolated(*this, Loc, Tok, "message");

      // FIXME: an empty message is still possible if parsing was valid.
      // We need to updategetStringLiteralIfNotInterpolated().
      if (Message.empty())
        return false;

      consumeToken(tok::string_literal);

    }

    SourceRange R(Loc, Tok.getLoc());

    if (!consumeIf(tok::r_paren)) {
      diagnose(Tok.getLoc(), diag::attr_expected_rparen, AttrName);
      return false;
    }

    if (!DiscardAttribute)
      Attributes.add(new (Context)
                     AvailabilityAttr(AtLoc, R, Platform, Message, true,
                                      /*Implicit=*/false));
    break;
  }

  case DAK_class_protocol:
    if (!DiscardAttribute)
      Attributes.add(new (Context) ClassProtocolAttr(AtLoc, Loc));
    break;
  case DAK_final:
    if (!DiscardAttribute)
      Attributes.add(new (Context) FinalAttr(AtLoc, Loc));
    break;
  case DAK_noreturn:
    if (!DiscardAttribute)
      Attributes.add(new (Context) NoReturnAttr(AtLoc, Loc));
    break;
  case DAK_objc: {
    // Unnamed @objc attribute.
    if (Tok.isNot(tok::l_paren)) {
      Attributes.add(ObjCAttr::createUnnamed(Context, AtLoc, Loc));
      break;
    }

    // Parse the leading '('.
    SourceLoc LParenLoc = consumeToken(tok::l_paren);

    // Parse the names, with trailing colons (if there are present).
    SmallVector<Identifier, 4> Names;
    SmallVector<SourceLoc, 4> NameLocs;
    bool sawColon = false;
    while (true) {
      // Empty selector piece.
      if (Tok.is(tok::colon)) {
        Names.push_back(Identifier());
        NameLocs.push_back(Tok.getLoc());
        sawColon = true;
        consumeToken();
        continue;
      }

      // Name.
      if (Tok.is(tok::identifier) || Tok.isKeyword()) {
        Names.push_back(Context.getIdentifier(Tok.getText()));
        NameLocs.push_back(Tok.getLoc());
        consumeToken();

        // If we have a colon, consume it.
        if (Tok.is(tok::colon)) {
          consumeToken();
          sawColon = true;
          continue;
        } 
        
        // If we see a closing parentheses, we're done.
        if (Tok.is(tok::r_paren)) {
          // If we saw more than one identifier, there's a ':'
          // missing here. Complain and pretend we saw it.
          if (Names.size() > 1) {
            SourceLoc afterLast
              = Lexer::getLocForEndOfToken(Context.SourceMgr, 
                                           NameLocs.back());
            diagnose(Tok, diag::attr_objc_missing_colon)
              .fixItInsert(afterLast, ":");
            sawColon = true;
          }

          break;
        }

        // If we see another identifier or keyword, complain about
        // the missing colon and keep going.
        if (Tok.is(tok::identifier) || Tok.isKeyword()) {
          SourceLoc afterLast
            = Lexer::getLocForEndOfToken(Context.SourceMgr, NameLocs.back());
          diagnose(Tok, diag::attr_objc_missing_colon)
            .fixItInsert(afterLast, ":");
          sawColon = true;
          continue;
        }

        // We don't know what happened. Break out.
        break;
      }

      break;
    }
    
    // Parse the matching ')'.
    SourceLoc RParenLoc;
    bool Invalid = false;
    if (parseMatchingToken(tok::r_paren, RParenLoc, 
                           diag::attr_objc_expected_rparen, LParenLoc)) {
      RParenLoc = Tok.getLoc();
      Invalid = true;
    }

    if (Names.empty()) {
      // When there are no names, recover as if there were no parentheses.
      if (!Invalid)
        diagnose(LParenLoc, diag::attr_objc_empty_name);
      Attributes.add(ObjCAttr::createUnnamed(Context, AtLoc, Loc));
    } else if (!sawColon) {
      // When we didn't see a colon, this is a nullary name.
      assert(Names.size() == 1 && "Forgot to set sawColon?");
      Attributes.add(ObjCAttr::createNullary(Context, AtLoc, Loc, LParenLoc,
                                             NameLocs.front(), Names.front(),
                                             RParenLoc));
    } else {
      // When we did see a colon, this is a selector.
      Attributes.add(ObjCAttr::createSelector(Context, AtLoc, Loc,
                                              LParenLoc, NameLocs, Names,
                                              RParenLoc));
    }
    break;
  }
  case DAK_required:
    if (!DiscardAttribute)
      Attributes.add(new (Context) RequiredAttr(AtLoc, Loc));
    break;
  }

  if (DuplicateAttribute) {
    diagnose(Loc, diag::duplicate_attribute)
      .highlight(AttrRange);
    diagnose(DuplicateAttribute->getLocation(), diag::previous_attribute)
      .highlight(DuplicateAttribute->getRange());
  }

  return false;
}

/// \verbatim
///   attribute:
///     'asmname' '(' identifier ')'
///     'infix' '=' numeric_constant
///     'unary'
///     'stdlib'
///     'weak'
///     'inout'
///     'unowned'
///     'noreturn'
///     'optional'
///     '!'? 'mutating'
///     'requires_stored_property_inits'
/// \endverbatim
bool Parser::parseDeclAttribute(DeclAttributes &Attributes, SourceLoc AtLoc) {
  SourceLoc InversionLoc;
  bool isInverted = false;
  if (consumeIf(tok::exclaim_postfix)) {
    InversionLoc = PreviousLoc;
    isInverted = true;
  }

  // If this not an identifier, the attribute is malformed.
  if (Tok.isNot(tok::identifier) &&
      Tok.isNot(tok::kw_in) &&
      Tok.isNot(tok::kw_weak) &&
      Tok.isNot(tok::kw_unowned)) {
    diagnose(Tok, diag::expected_attribute_name);
    return true;
  }

  // FIXME: This is bogus to only honor the first '@', but this
  // will be fixed once the attribute refactoring completes for
  // all existing declaration attributes.
  if (Attributes.AtLoc.isInvalid())
    Attributes.AtLoc = AtLoc;

  // Determine which attribute it is, and diagnose it if unknown.
  AttrKind attr = llvm::StringSwitch<AttrKind>(Tok.getText())
#define ATTR(X) .Case(#X, AK_##X)
#define VIRTUAL_ATTR(X)
#include "swift/AST/Attr.def"
               .Default(AK_Count);
  
  if (attr == AK_Count) {
    // If the attribute follows the new representation, switch
    // over to the alternate parsing path.
    DeclAttrKind DK = getDeclAttrFromString(Tok.getText());
    if (DK != DAK_Count)
      return parseNewDeclAttribute(Attributes, AtLoc, InversionLoc,
                                   Tok.getText(), DK);

    if (getTypeAttrFromString(Tok.getText()) != TAK_Count)
      diagnose(Tok, diag::type_attribute_applied_to_decl);
    else
      diagnose(Tok, diag::unknown_attribute, Tok.getText());
    // Recover by eating @foo when foo is not known.
    consumeToken();
      
    // Recovery by eating "@foo=bar" if present.
    if (consumeIf(tok::equal)) {
      if (Tok.is(tok::identifier) ||
          Tok.is(tok::integer_literal) ||
          Tok.is(tok::floating_literal) ||
          Tok.is(tok::string_literal))
        consumeToken();
    }
    return true;
  }
  
  // Ok, it is a valid attribute, eat it, and then process it.
  SourceLoc Loc = consumeToken();
  
  // Diagnose duplicated attributes.
  if (Attributes.has(attr)) {
    diagnose(Loc, diag::duplicate_attribute);
    return false;
  }

  Attributes.setAttr(attr, Loc);

  // If this is an inverted attribute like "@!mutating", verify that inversion
  // is ok.
  if (isInverted) {
    if (attr == AK_mutating) {
      Attributes.MutatingInverted = true;
    } else {
      diagnose(InversionLoc, diag::invalid_attribute_inversion);
      isInverted = false;
    }
  }

  // Handle any attribute-specific processing logic.
  switch (attr) {
  default: break;
  // Ownership attributes.
  case AK_weak:
  case AK_unowned:
    // Test for duplicate entries by temporarily removing this one.
    Attributes.clearAttribute(attr);
    if (Attributes.hasOwnership()) {
      diagnose(Loc, diag::duplicate_attribute);
      break;
    }
    Attributes.setAttr(attr, Loc);
    break;
      
      
  case AK_prefix:
    if (Attributes.isPostfix()) {
      diagnose(Loc, diag::cannot_combine_attribute, "postfix");
      Attributes.clearAttribute(attr);
    }
    break;
    
  case AK_postfix:
    if (Attributes.isPrefix()) {
      diagnose(Loc, diag::cannot_combine_attribute, "prefix");
      Attributes.clearAttribute(attr);
    }
    break;
  }

  return false;
}

bool Parser::canParseTypeAttribute() {
  TypeAttributes attrs; // ignored
  return !parseTypeAttribute(attrs, /*justChecking*/ true);
}

/// \verbatim
///   attribute-type:
///     'noreturn'
/// \endverbatim
///
/// \param justChecking - if true, we're just checking whether we
///   canParseTypeAttribute; don't emit any diagnostics, and there's
///   no need to actually record the attribute
bool Parser::parseTypeAttribute(TypeAttributes &Attributes, bool justChecking) {
  // If this not an identifier, the attribute is malformed.
  if (Tok.isNot(tok::identifier) && !Tok.is(tok::kw_in)) {
    if (!justChecking)
      diagnose(Tok, diag::expected_attribute_name);
    return true;
  }
  
  // Determine which attribute it is, and diagnose it if unknown.
  TypeAttrKind attr = getTypeAttrFromString(Tok.getText());

  if (attr == TAK_Count) {
    if (justChecking) return true;

    StringRef Text = Tok.getText();
    bool isDeclAttribute = false
#define ATTR(X) || Text == #X
#define VIRTUAL_ATTR(X)
#define DECL_ATTR(X, ...) || Text == #X
#include "swift/AST/Attr.def"
    ;
    
    if (isDeclAttribute)
      diagnose(Tok, diag::decl_attribute_applied_to_type);
    else
      diagnose(Tok, diag::unknown_attribute, Tok.getText());

    // Recover by eating @foo when foo is not known.
    consumeToken();
      
    // Recovery by eating "@foo=bar" if present.
    if (consumeIf(tok::equal)) {
      if (Tok.is(tok::identifier) ||
          Tok.is(tok::integer_literal) ||
          Tok.is(tok::floating_literal))
        consumeToken();
    }
    return true;
  }
  
  // Ok, it is a valid attribute, eat it, and then process it.
  StringRef Text = Tok.getText();
  SourceLoc Loc = consumeToken();
  
  // Diagnose duplicated attributes.
  if (justChecking) {
    // do nothing
  } else if (Attributes.has(attr)) {
    diagnose(Loc, diag::duplicate_attribute);
  } else {
    Attributes.setAttr(attr, Loc);
  }
  
  // Handle any attribute-specific processing logic.

  // In just-checking mode, we only need additional parsing for the "cc"
  // attribute.  (Note that we're never in just-checking mode in SIL mode.)
  if (justChecking && attr != TAK_cc)
    return false;

  switch (attr) {
  default: break;
  case TAK_local_storage:
  case TAK_sil_self:
  case TAK_out:
  case TAK_in:
  case TAK_owned:
  case TAK_guaranteed:
  case TAK_autoreleased:
  case TAK_callee_owned:
  case TAK_callee_guaranteed:
  case TAK_objc_metatype:
    if (!isInSILMode()) {
      diagnose(Loc, diag::only_allowed_in_sil, Text);
      Attributes.clearAttribute(attr);
    }
    break;
    
  // Ownership attributes.
  case TAK_sil_weak:
  case TAK_sil_unowned:
    Attributes.clearAttribute(attr);
    if (!isInSILMode()) {
      diagnose(Loc, diag::only_allowed_in_sil, "local_storage");
      return false;
    }
      
    if (Attributes.hasOwnership()) {
      diagnose(Loc, diag::duplicate_attribute);
      break;
    }
    if (!justChecking)
      Attributes.setAttr(attr, Loc);
    break;

  // 'inout' attribute.
  case TAK_inout:
    if (!isInSILMode()) {
      diagnose(Loc, diag::inout_not_attribute);
      return false;
    }
    break;
      
  case TAK_opened: {
    // Parse the opened existential ID in parens
    SourceLoc beginLoc = Tok.getLoc(), idLoc, endLoc;
    Attributes.setAttr(TAK_opened, beginLoc);
    if (consumeIfNotAtStartOfLine(tok::l_paren)) {
      if (Tok.is(tok::integer_literal)) {
        unsigned openedID = 0;
        idLoc = Tok.getLoc();
        if (Tok.getText().getAsInteger(0, openedID)) {
          diagnose(Tok, diag::opened_attribute_id_value);
        } else {
          Attributes.OpenedID = openedID;
        }
        consumeToken();
      } else {
        diagnose(Tok, diag::opened_attribute_id_value);
      }
      parseMatchingToken(tok::r_paren, endLoc,
                         diag::opened_attribute_expected_rparen,
                         beginLoc);
    } else {
      diagnose(Tok, diag::opened_attribute_expected_lparen);
    }

    if (!isInSILMode()) {
      diagnose(Loc, diag::only_allowed_in_sil, "opened");
      Attributes.clearAttribute(TAK_opened);
    }

    break;
  }

    // 'cc' attribute.
  case TAK_cc: {
    // Parse the cc name in parens.
    SourceLoc beginLoc = Tok.getLoc(), nameLoc, endLoc;
    StringRef name;
    if (consumeIfNotAtStartOfLine(tok::l_paren)) {
      if (Tok.is(tok::identifier)) {
        nameLoc = Tok.getLoc();
        name = Tok.getText();
        consumeToken();
      } else if (!justChecking) {
        diagnose(Tok, diag::cc_attribute_expected_name);
      }

      // Parse the ')'.  We can't use parseMatchingToken if we're in
      // just-checking mode.
      if (!justChecking) {
        if (parseMatchingToken(tok::r_paren, endLoc,
                               diag::cc_attribute_expected_rparen,
                               beginLoc))
          return true;
      } else if (!consumeIf(tok::r_paren)) {
        return true;
      }
    } else if (!justChecking) {
      diagnose(Tok, diag::cc_attribute_expected_lparen);
    }

    // Don't validate the CC in just-checking mode.
    if (justChecking) return false;
    
    if (!name.empty()) {
      Attributes.cc = llvm::StringSwitch<Optional<AbstractCC>>(name)
        .Case("freestanding", AbstractCC::Freestanding)
        .Case("method", AbstractCC::Method)
        .Case("cdecl", AbstractCC::C)
        .Case("objc_method", AbstractCC::ObjCMethod)
        .Case("witness_method", AbstractCC::WitnessMethod)
        .Default(Nothing);
      if (!Attributes.cc) {
        diagnose(nameLoc, diag::cc_attribute_unknown_cc_name, name);
        Attributes.clearAttribute(attr);
      }
    }
    return false;
  }
  }
  
  return false;
}

/// \verbatim
///   attribute-list:
///     /*empty*/
///     attribute-list-clause attribute-list
///   attribute-list-clause:
///     '@' attribute
/// \endverbatim
bool Parser::parseDeclAttributeList(DeclAttributes &Attributes) {
  while (Tok.is(tok::at_sign)) {
    SourceLoc AtLoc = Tok.getLoc();
    consumeToken();
    if (parseDeclAttribute(Attributes, AtLoc))
      return true;
  }
  return false;
}

/// \brief This is the internal implementation of \c parseTypeAttributeList,
/// which we expect to be inlined to handle the common case of an absent
/// attribute list.
///
/// \verbatim
///   attribute-list:
///     /*empty*/
///     attribute-list-clause attribute-list
///   attribute-list-clause:
///     '@' attribute
///     '@' attribute ','? attribute-list-clause
/// \endverbatim
bool Parser::parseTypeAttributeListPresent(TypeAttributes &Attributes) {
  Attributes.AtLoc = Tok.getLoc();
  do {
    if (parseToken(tok::at_sign, diag::expected_in_attribute_list) ||
        parseTypeAttribute(Attributes))
      return true;
    
    // Attribute lists don't require separating commas.
  } while (Tok.is(tok::at_sign) || consumeIf(tok::comma));
  
  return false;
}

static bool isStartOfOperatorDecl(const Token &Tok, const Token &Tok2) {
  return Tok.isContextualKeyword("operator")
    && (Tok2.isContextualKeyword("prefix")
        || Tok2.isContextualKeyword("postfix")
        || Tok2.isContextualKeyword("infix"));
}

static bool isStartOfModifiedDecl(const Token &Tok, const Token &Tok2,
                                  bool &IsMutating) {
  if (Tok.is(tok::kw_static)) {
    IsMutating = false;
    // Declarations beginning with 'static' are always modified decls.
    return true;
  }
  if (Tok.is(tok::kw_class)) {
    IsMutating = false;
    // By looking at 'Tok' only, we don't know yet if it is a class function or
    // property, or a class decl.
  } else if (Tok.isContextualKeyword("mutating"))
    IsMutating = true;
  else
    return false;
  
  return
       (Tok2.is(tok::kw_func) || Tok2.is(tok::kw_let) || Tok2.is(tok::kw_var) ||
        Tok2.is(tok::kw_init) || Tok2.is(tok::kw_destructor) ||
        Tok2.is(tok::kw_deinit) ||
        Tok2.is(tok::kw_subscript) || Tok2.is(tok::kw_struct) ||
        Tok2.is(tok::kw_enum) || Tok2.is(tok::kw_class) ||
        Tok2.is(tok::kw_protocol) || Tok2.is(tok::kw_typealias));
}

/// isStartOfDecl - Return true if this is the start of a decl or decl-import.
bool Parser::isStartOfDecl(const Token &Tok, const Token &Tok2) {
  switch (Tok.getKind()) {
  case tok::at_sign:
  case tok::kw_static:
  case tok::kw_extension:
  case tok::kw_let:
  case tok::kw_var:
  case tok::kw_typealias:
  case tok::kw_enum:
  case tok::kw_case:
  case tok::kw_struct:
  case tok::kw_class:
  case tok::kw_import:
  case tok::kw_subscript:
  case tok::kw_init:
  case tok::kw_deinit:
  case tok::kw_destructor:
  case tok::kw_func:
  case tok::pound_if:
    return true;
  case tok::kw_protocol:
    return !Tok2.isAnyOperator() || !Tok2.getText().equals("<");
  default:
    if (isStartOfOperatorDecl(Tok, Tok2)) return true;
      
    bool isMutating = false;
    return isStartOfModifiedDecl(Tok, Tok2, isMutating);
  }
}


void Parser::consumeDecl(ParserPosition BeginParserPosition,
                         ParseDeclOptions Flags,
                         bool IsTopLevel) {
  backtrackToPosition(BeginParserPosition);
  SourceLoc BeginLoc = Tok.getLoc();
  // Consume tokens up to code completion token.
  while (Tok.isNot(tok::code_complete))
    consumeToken();

  // Consume the code completion token, if there is one.
  consumeIf(tok::code_complete);
  SourceLoc EndLoc = Tok.getLoc();
  State->delayDecl(PersistentParserState::DelayedDeclKind::Decl, Flags.toRaw(),
                   CurDeclContext, { BeginLoc, EndLoc },
                   BeginParserPosition.PreviousLoc);

  if (IsTopLevel) {
    // Skip the rest of the file to prevent the parser from constructing the
    // AST for it.  Forward references are not allowed at the top level.
    skipUntil(tok::eof);
  }
}

void Parser::setLocalDiscriminator(ValueDecl *D) {
  // If we're not in a local context, this is unnecessary.
  if (!CurLocalContext || !D->getDeclContext()->isLocalContext())
    return;

  Identifier name = D->getName();
  unsigned discriminator = CurLocalContext->claimNextNamedDiscriminator(name);
  D->setLocalDiscriminator(discriminator);
}

/// \brief Parse a single syntactic declaration and return a list of decl
/// ASTs.  This can return multiple results for var decls that bind to multiple
/// values, structs that define a struct decl and a constructor, etc.
///
/// \verbatim
///   decl:
///     decl-typealias
///     decl-extension
///     decl-let
///     decl-var
///     decl-class
///     decl-func
///     decl-enum
///     decl-struct
///     decl-import
///     decl-operator
/// \endverbatim
ParserStatus Parser::parseDecl(SmallVectorImpl<Decl*> &Entries,
                               ParseDeclOptions Flags) {
  ParserPosition BeginParserPosition;
  if (isCodeCompletionFirstPass())
    BeginParserPosition = getParserPosition();

  // Note that we're parsing a declaration.
  StructureMarkerRAII ParsingDecl(*this, Tok.getLoc(),
                                  StructureMarkerKind::Declaration);

  DeclAttributes Attributes;
  if (Tok.hasComment()) {
    Attributes.CommentRange = Tok.getCommentRange();
    Attributes.setAttr(AK_raw_doc_comment, SourceLoc());
  }
  parseDeclAttributeList(Attributes);

  // If we see the 'static', 'class' or 'mutating' followed by a declaration
  // keyword, parse it now.
  SourceLoc StaticLoc, MutatingLoc;
  bool UnhandledStatic = false, UnhandledMutating = false;
  StaticSpellingKind StaticSpelling = StaticSpellingKind::None;
  bool IsMutating = false;
  if (isStartOfModifiedDecl(Tok, peekToken(), IsMutating)) {
    if (IsMutating) {
      MutatingLoc = consumeToken(tok::identifier);
      UnhandledMutating = true;
    } else {
      assert(Tok.is(tok::kw_static) || Tok.is(tok::kw_class));
      if (Tok.is(tok::kw_static))
        StaticSpelling = StaticSpellingKind::KeywordStatic;
      else
        StaticSpelling = StaticSpellingKind::KeywordClass;

      StaticLoc = consumeToken();
      UnhandledStatic = true;
    }
  }

  ParserResult<Decl> DeclResult;
  ParserStatus Status;
  switch (Tok.getKind()) {
  case tok::kw_import:
    DeclResult = parseDeclImport(Flags, Attributes);
    Status = DeclResult;
    break;
  case tok::kw_extension:
    DeclResult = parseDeclExtension(Flags, Attributes);
    Status = DeclResult;
    break;
  case tok::kw_let:
  case tok::kw_var:
    Status = parseDeclVar(Flags, Attributes, Entries, StaticLoc,
                          StaticSpelling);
    UnhandledStatic = false;
    break;
  case tok::kw_typealias:
    DeclResult = parseDeclTypeAlias(!(Flags & PD_DisallowTypeAliasDef),
                                     Flags.contains(PD_InProtocol), Attributes);
    Status = DeclResult;
    break;
  case tok::kw_enum:
    DeclResult = parseDeclEnum(Flags, Attributes);
    Status = DeclResult;
    break;
  case tok::kw_case:
    Status = parseDeclEnumCase(Flags, Attributes, Entries);
    break;
  case tok::kw_struct:
    DeclResult = parseDeclStruct(Flags, Attributes);
    Status = DeclResult;
    break;
  case tok::kw_class:
    DeclResult = parseDeclClass(Flags, Attributes);
    Status = DeclResult;
    break;
  case tok::kw_init:
    DeclResult = parseDeclConstructor(Flags, Attributes);
    Status = DeclResult;
    break;
  case tok::kw_deinit:
  case tok::kw_destructor:
    DeclResult = parseDeclDestructor(Flags, Attributes);
    Status = DeclResult;
    break;
  case tok::kw_protocol:
    DeclResult = parseDeclProtocol(Flags, Attributes);
    Status = DeclResult;
    break;
  case tok::pound_if: {
    auto IfConfigResult = parseDeclIfConfig(Flags);
    Status = IfConfigResult;

    if (auto ICD = IfConfigResult.getPtrOrNull()) {
      // The IfConfigDecl is ahead of its members in source order.
      Entries.push_back(ICD);
      // Copy the active members into the entries list.
      for (auto activeMember : ICD->getActiveMembers()) {
        Entries.push_back(activeMember);
      }
    }
    }
    break;

  case tok::kw_func:
    DeclResult = parseDeclFunc(StaticLoc, StaticSpelling, MutatingLoc, Flags,
                               Attributes);
    Status = DeclResult;
    UnhandledStatic = false;
    UnhandledMutating = false;
    break;

  case tok::kw_subscript:
    if (StaticLoc.isValid()) {
      diagnose(Tok, diag::subscript_static, StaticSpelling)
        .fixItRemove(SourceRange(StaticLoc));
      UnhandledStatic = false;
    }
    Status = parseDeclSubscript(Flags, Attributes, Entries);
    break;
  
  case tok::identifier:
    if (isStartOfOperatorDecl(Tok, peekToken())) {
      DeclResult = parseDeclOperator(Flags.contains(PD_AllowTopLevel),
                                     Attributes);
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
      !CurDeclContext->isModuleScopeContext()) {
    // Only consume non-toplevel decls.
    consumeDecl(BeginParserPosition, Flags, /*IsTopLevel=*/false);

    // Pretend that there was no error.
    return makeParserSuccess();
  }

  if (DeclResult.isNonNull())
    Entries.push_back(DeclResult.get());

  if (Status.isSuccess() && Tok.is(tok::semi))
    Entries.back()->TrailingSemiLoc = consumeToken(tok::semi);

  if (Status.isSuccess()) {
    // If we parsed 'class' or 'static', but didn't handle it above, complain
    // about it.
    if (UnhandledStatic)
      diagnose(Entries.back()->getLoc(), diag::decl_not_static,
               StaticSpelling)
        .fixItRemove(SourceRange(StaticLoc));
    // If we parsed 'mutating' but didn't handle it above, complain about it.
    if (UnhandledMutating)
      diagnose(Entries.back()->getLoc(), diag::mutating_invalid)
        .fixItRemove(SourceRange(MutatingLoc));
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
  parseDecl(Entries, ParseDeclOptions(DelayedState->Flags));
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
///     'enum'
///     'protocol'
///     'var'
///     'func'
///   import-path:
///     any-identifier ('.' any-identifier)*
/// \endverbatim
ParserResult<ImportDecl> Parser::parseDeclImport(ParseDeclOptions Flags,
                                                 DeclAttributes &Attributes) {
  SourceLoc ImportLoc = consumeToken(tok::kw_import);
  
  bool Exported = Attributes.isExported();
  Attributes.clearAttribute(AK_exported);
  if (Attributes.hasNonVirtualAttributes())
    diagnose(Attributes.AtLoc, diag::import_attributes);
    
  if (!Context.LangOpts.DebuggerSupport && !(Flags & PD_AllowTopLevel)) {
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
    case tok::kw_enum:
      Kind = ImportKind::Enum;
      break;
    case tok::kw_protocol:
      Kind = ImportKind::Protocol;
      break;
    case tok::kw_var:
    case tok::kw_let:
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
                                            tok ResyncT4,
                                            TokenProperty ResyncP1,
                                            const Diagnostic &D) {
  switch (P.Tok.getKind()) {
  case tok::identifier:
    Result = P.Context.getIdentifier(P.Tok.getText());
    Loc = P.Tok.getLoc();
    P.consumeToken();
    return makeParserSuccess();

  default:
    P.checkForInputIncomplete();
    if (!D.is(diag::invalid_diagnostic))
      P.diagnose(P.Tok, D);
    if (P.Tok.isKeyword() &&
        (P.peekToken().is(ResyncT1) || P.peekToken().is(ResyncT2) ||
         P.peekToken().is(ResyncT3) || P.peekToken().is(ResyncT4) ||
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
                                 tok::unknown, tok::unknown,
                                 TokenProperty::None,
                                 Diagnostic(ID, Args...));
}

template <typename... DiagArgTypes, typename... ArgTypes>
static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        tok ResyncT1, tok ResyncT2, tok ResyncT3,
                        Diag<DiagArgTypes...> ID, ArgTypes... Args) {
  return parseIdentifierDeclName(P, Result, L, ResyncT1, ResyncT2, ResyncT3,
                                 tok::unknown, TokenProperty::None,
                                 Diagnostic(ID, Args...));
}

template <typename... DiagArgTypes, typename... ArgTypes>
static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        tok ResyncT1, tok ResyncT2, tok ResyncT3, tok ResyncT4,
                        Diag<DiagArgTypes...> ID, ArgTypes... Args) {
  return parseIdentifierDeclName(P, Result, L, ResyncT1, ResyncT2, ResyncT3,
                                 ResyncT4, TokenProperty::None,
                                 Diagnostic(ID, Args...));
}


template <typename... DiagArgTypes, typename... ArgTypes>
static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        tok ResyncT1, tok ResyncT2, TokenProperty ResyncP1,
                        Diag<DiagArgTypes...> ID, ArgTypes... Args) {
  return parseIdentifierDeclName(P, Result, L, ResyncT1, ResyncT2, tok::unknown,
                                 tok::unknown,
                                 ResyncP1, Diagnostic(ID, Args...));
}

/// \brief Parse an 'extension' declaration.
///
/// \verbatim
///   extension:
///    'extension' attribute-list type-identifier inheritance? '{' decl* '}'
/// \endverbatim
ParserResult<ExtensionDecl>
Parser::parseDeclExtension(ParseDeclOptions Flags, DeclAttributes &Attributes) {
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
        new (Context) SimpleIdentTypeRepr(NameLoc, ExtensionName));
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
  if (Attributes.shouldSaveInAST())
    ED->getMutableAttrs() = Attributes;

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
                  /*AllowSepAfterLast=*/false, diag::expected_rbrace_extension,
                  [&]() -> ParserStatus {
      ParseDeclOptions Options(PD_HasContainerType |
                               PD_DisallowStoredInstanceVar |
                               PD_InExtension);

      return parseDecl(MemberDecls, Options);
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

ParserResult<IfConfigDecl> Parser::parseDeclIfConfig(
                                            ParseDeclOptions Flags) {
  if (peekToken().isAtStartOfLine()) {
    diagnose(Tok.getLoc(), diag::expected_build_configuration_expression);
  }
  
  SourceLoc IfLoc = consumeToken(tok::pound_if);
  SourceLoc ElseLoc;
  SourceLoc EndLoc;
  
  StructureMarkerRAII ParsingDecl(*this, Tok.getLoc(),
                                  StructureMarkerKind::IfConfig);
  
  // Evaluate the condition.
  ParserResult<Expr> Configuration = parseExprSequence(diag::expected_expr,
                                                       true,
                                                       true);
  if (Configuration.isNull()) {
    return makeParserError();
  }
  
  bool ifBlockIsActive = evaluateConfigConditionExpr(Configuration.get());

  SmallVector<Decl*, 8> IfDecls;
  SmallVector<Decl*, 8> ElseDecls;
  ParserStatus Status;
  while(Tok.isNot(tok::pound_else) && Tok.isNot(tok::pound_endif)) {
    Status = parseDecl(IfDecls, Flags);
    
    if (Status.isError()) {
      diagnose(Tok, diag::expected_close_to_config_stmt);
      skipUntilConfigBlockClose();
    }
  }
  
  if (Tok.is(tok::pound_else)) {
    ElseLoc = consumeToken(tok::pound_else);
    
    while(Tok.isNot(tok::pound_endif)) {
      Status = parseDecl(ElseDecls, Flags);
      
      if (Status.isError()) {
        skipUntilConfigBlockClose();
      }
    }
  }
  
  if (Tok.is(tok::pound_endif)) {
    EndLoc = consumeToken(tok::pound_endif);
  } else {
    diagnose(Tok, diag::expected_close_to_config_stmt);
    skipUntilConfigBlockClose();
  }
  
  IfConfigDecl *ICD = new (Context) IfConfigDecl(CurDeclContext,
                                                 ifBlockIsActive,
                                                 IfLoc,
                                                 ElseLoc,
                                                 EndLoc,
                                                 Configuration.getPtrOrNull(),
                                                 Context.AllocateCopy(IfDecls),
                                                 Context.AllocateCopy(ElseDecls));
  return makeParserResult(ICD);
}

/// \brief Parse a typealias decl.
///
/// \verbatim
///   decl-typealias:
///     'typealias' identifier inheritance? '=' type
/// \endverbatim
ParserResult<TypeDecl> Parser::parseDeclTypeAlias(bool WantDefinition,
                                                  bool isAssociatedType,
                                                  DeclAttributes &Attributes) {
  SourceLoc TypeAliasLoc = consumeToken(tok::kw_typealias);
  
  Identifier Id;
  SourceLoc IdLoc;
  ParserStatus Status;
  
  if (Attributes.hasNonVirtualAttributes())
    diagnose(Attributes.AtLoc, diag::typealias_attributes);
  

  Status |=
      parseIdentifierDeclName(*this, Id, IdLoc, tok::colon, tok::equal,
                              diag::expected_identifier_in_decl, "typealias");
  if (Status.isError())
    return nullptr;

  // Parse optional inheritance clause.
  SmallVector<TypeLoc, 2> Inherited;
  if (isAssociatedType && Tok.is(tok::colon))
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
  }

  // If this is an associated type, build the AST for it.
  if (isAssociatedType) {
    auto assocType = new (Context) AssociatedTypeDecl(
                                     CurDeclContext,
                                     TypeAliasLoc, Id, IdLoc,
                                     UnderlyingTy.getPtrOrNull());
    if (Attributes.shouldSaveInAST())
      assocType->getMutableAttrs() = Attributes;
    if (!Inherited.empty())
      assocType->setInherited(Context.AllocateCopy(Inherited));
    addToScope(assocType);
    return makeParserResult(Status, assocType);
  }

  // Otherwise, build a typealias.
  TypeAliasDecl *TAD =
    new (Context) TypeAliasDecl(TypeAliasLoc, Id, IdLoc,
                                UnderlyingTy.getPtrOrNull(),
                                CurDeclContext);
  if (Attributes.shouldSaveInAST())
    TAD->getMutableAttrs() = Attributes;
  addToScope(TAD);
  return makeParserResult(Status, TAD);
}

/// This function creates an accessor function (with no body) for a computed
/// property or subscript.
static FuncDecl *createAccessorFunc(SourceLoc DeclLoc,
                                    TypedPattern *NamePattern,
                                    TypeLoc ElementTy,
                                    Pattern *Indices, SourceLoc StaticLoc,
                                    Parser::ParseDeclOptions Flags,
                                    AccessorKind Kind, Parser *P) {
  // First task, set up the value argument pattern.  This is the NamePattern
  // (for setters) followed by the index list (for subscripts).  For
  // non-subscript getters, this degenerates down to "()".
  //
  // We put the 'value' argument before the subscript index list as a
  // micro-optimization for Objective-C thunk generation.
  Pattern *ValueArg;
  {
    SmallVector<TuplePatternElt, 2> ValueArgElements;
    SourceLoc StartLoc, EndLoc;
    if (NamePattern) {
      ValueArgElements.push_back(TuplePatternElt(NamePattern));
      StartLoc = NamePattern->getStartLoc();
      EndLoc = NamePattern->getEndLoc();
    }

    if (Indices) {
      Indices = Indices->clone(P->Context, Pattern::Implicit);
      if (auto *PP = dyn_cast<ParenPattern>(Indices)) {
        ValueArgElements.push_back(TuplePatternElt(PP->getSubPattern()));
      } else {
        auto *TP = cast<TuplePattern>(Indices);
        ValueArgElements.append(TP->getFields().begin(), TP->getFields().end());
      }

      StartLoc = Indices->getStartLoc();
      EndLoc = Indices->getEndLoc();
    }

    if (NamePattern && Indices) {
      StartLoc = Indices->getStartLoc();
      EndLoc = NamePattern->getEndLoc();
    }

    ValueArg = TuplePattern::create(P->Context, StartLoc,
                                    ValueArgElements, EndLoc);
    if (NamePattern && !NamePattern->isImplicit())
      ValueArg->setImplicit();
  }


  // Create the parameter list(s) for the getter.
  SmallVector<Pattern *, 4> Params;
  
  // Add the implicit 'self' to Params, if needed.
  if (Flags & Parser::PD_HasContainerType)
    Params.push_back(buildImplicitSelfParameter(DeclLoc, P->CurDeclContext));
  
  // Add the "(value)" and subscript indices parameter clause.
  Params.push_back(ValueArg);

  TypeLoc ReturnType;
  if (Kind == AccessorKind::IsGetter) // Getters return something
    ReturnType = ElementTy.clone(P->Context);
  else  // Nothing else does.
    ReturnType = TypeLoc::withoutLoc(TupleType::getEmpty(P->Context));

  // Start the function.
  auto *D = FuncDecl::create(P->Context, StaticLoc, StaticSpellingKind::None,
                             /* FIXME*/DeclLoc, Identifier(),
                             DeclLoc, /*GenericParams=*/nullptr, Type(), Params,
                             Params, ReturnType, P->CurDeclContext);

  // non-static set/willSet/didSet default to @mutating.
  if (!D->isStatic() && Kind != AccessorKind::IsGetter)
    D->setMutating();

  return D;
}

/// Parse a "(value)" specifier for "set" or "willSet" if present.  Create a
/// pattern to represent the spelled argument or the implicit one if it is
/// missing.
static TypedPattern *
parseOptionalAccessorArgument(SourceLoc SpecifierLoc, TypeLoc ElementTy,
                              Parser &P, AccessorKind Kind) {
  // 'set' and 'willSet' have a (value) parameter, 'didSet' takes an (oldValue)
  // paramter and 'get' and always takes a () parameter.
  if (Kind != AccessorKind::IsSetter && Kind != AccessorKind::IsWillSet &&
      Kind != AccessorKind::IsDidSet)
    return nullptr;

  SourceLoc StartLoc, NameLoc, EndLoc;
  Identifier Name;
  ASTContext &Context = P.Context;

  // If the SpecifierLoc is invalid, then the caller just wants us to synthesize
  // the default, not actually try to parse something.
  if (SpecifierLoc.isValid() && P.Tok.is(tok::l_paren)) {
    StartLoc = P.consumeToken(tok::l_paren);
    if (P.Tok.isNot(tok::identifier)) {
      P.diagnose(P.Tok, diag::expected_accessor_name,
                 Kind != AccessorKind::IsSetter);
      P.skipUntil(tok::r_paren, tok::l_brace);
      if (P.Tok.is(tok::r_paren))
        P.consumeToken();
    } else {
      // We have a name.
      Name = P.Context.getIdentifier(P.Tok.getText());
      NameLoc = P.consumeToken();

      auto DiagID =
         Kind == AccessorKind::IsSetter ? diag::expected_rparen_set_name :
         Kind == AccessorKind::IsWillSet ? diag::expected_rparen_willSet_name :
          diag::expected_rparen_didSet_name;
      
      // Look for the closing ')'.
      P.parseMatchingToken(tok::r_paren, EndLoc, DiagID, StartLoc);
    }
  }

  bool IsNameImplicit = EndLoc.isInvalid();

  // Add the parameter. If no name was specified, the name defaults to
  // 'value'.
  if (IsNameImplicit) {
    const char *ImplName =
      Kind == AccessorKind::IsDidSet ? "oldValue" : "newValue";
    Name = P.Context.getIdentifier(ImplName);
    NameLoc = SpecifierLoc;
    StartLoc = SourceLoc();
  }

  VarDecl *Value = new (Context) VarDecl(/*static*/false, /*IsLet*/true,
                                         NameLoc, Name,
                                         Type(), P.CurDeclContext);
  if (IsNameImplicit)
    Value->setImplicit();
  auto *namedPat = new (Context) NamedPattern(Value, IsNameImplicit);
  return new (Context) TypedPattern(namedPat, ElementTy.clone(Context),
                                    /*Implicit*/true);
}

static unsigned skipUntilMatchingRBrace(Parser &P) {
  unsigned OpenBraces = 1;
  while (OpenBraces != 0 && P.Tok.isNot(tok::eof)) {
    if (P.consumeIf(tok::l_brace)) {
      OpenBraces++;
      continue;
    }
    if (OpenBraces == 1 && P.Tok.is(tok::r_brace))
      break;
    if (P.consumeIf(tok::r_brace)) {
      OpenBraces--;
      continue;
    }
    P.consumeToken();
  }
  return OpenBraces;
}

static unsigned skipBracedBlock(Parser &P) {
  P.consumeToken(tok::l_brace);
  unsigned OpenBraces = skipUntilMatchingRBrace(P);
  if (P.consumeIf(tok::r_brace))
    OpenBraces--;
  return OpenBraces;
}

void Parser::consumeGetSetBody(AbstractFunctionDecl *AFD,
                               SourceLoc LBLoc) {
  SourceLoc SavedPreviousLoc = PreviousLoc;

  SourceRange BodyRange;
  BodyRange.Start = Tok.getLoc();

  // Skip until the next '}' at the correct nesting level.
  unsigned OpenBraces = skipUntilMatchingRBrace(*this);

  if (OpenBraces != 1) {
    // FIXME: implement some error recovery?
  }

  BodyRange.End = PreviousLoc;

  if (DelayedParseCB->shouldDelayFunctionBodyParsing(
          *this, AFD, AFD->getAttrs(), BodyRange)) {
    State->delayAccessorBodyParsing(AFD, BodyRange, SavedPreviousLoc, LBLoc);
    AFD->setBodyDelayed(BodyRange);
  } else {
    AFD->setBodySkipped(BodyRange);
  }
}

/// \brief Parse a get-set clause, optionally containing a getter, setter,
/// willSet, and/or didSet clauses.  'Indices' is a paren or tuple pattern,
/// specifying the index list for a subscript.
bool Parser::parseGetSetImpl(ParseDeclOptions Flags, Pattern *Indices,
                             TypeLoc ElementTy, FuncDecl *&Get, FuncDecl *&Set,
                             FuncDecl *&WillSet, FuncDecl *&DidSet,
                             SourceLoc &LastValidLoc, SourceLoc StaticLoc,
                             SmallVectorImpl<Decl *> &Decls) {
  Get = Set = WillSet = DidSet = nullptr;

  // Properties in protocols use sufficiently limited syntax that we have a
  // special parsing loop for them.  SIL mode uses the same syntax.
  if (Flags.contains(PD_InProtocol) || isInSILMode()) {
    while (Tok.isNot(tok::r_brace)) {
      if (Tok.is(tok::eof))
        return true;

      // Parse any leading attributes.
      DeclAttributes Attributes;
      parseDeclAttributeList(Attributes);

      AccessorKind Kind;
      FuncDecl **TheDeclPtr;
      if (Tok.isContextualKeyword("get")) {
        Kind = AccessorKind::IsGetter;
        TheDeclPtr = &Get;
      } else if (Tok.isContextualKeyword("set")) {
        Kind = AccessorKind::IsSetter;
        TheDeclPtr = &Set;
      } else {
        diagnose(Tok, diag::expected_getset_in_protocol);
        return true;
      }

      FuncDecl *&TheDecl = *TheDeclPtr;
      SourceLoc Loc = consumeToken();

      // Have we already parsed this kind of clause?
      if (TheDecl) {
        diagnose(Loc, diag::duplicate_property_accessor, (unsigned)Kind);
        diagnose(TheDecl->getLoc(), diag::previous_accessor, (unsigned)Kind);
        TheDecl = nullptr;  // Forget the previous decl.
      }

      // "set" could have a name associated with it.  This isn't valid in a
      // protocol, but we parse and then reject it, for better QoI.
      if (Tok.is(tok::l_paren))
        diagnose(Loc, diag::protocol_setter_name);

      auto *ValueNamePattern
        = parseOptionalAccessorArgument(Loc, ElementTy, *this, Kind);

      // Set up a function declaration.
      TheDecl = createAccessorFunc(Loc, ValueNamePattern, ElementTy, Indices,
                                   StaticLoc, Flags, Kind, this);
      if (Attributes.shouldSaveInAST())
        TheDecl->getMutableAttrs() = Attributes;
      
      Decls.push_back(TheDecl);
    }

    return false;
  }


  // Otherwise, we have a normal var or subscript declaration, parse the full
  // complement of specifiers, along with their bodies.

  // If the body is completely empty, reject it.  This is at best a getter with
  // an implicit fallthrough off the end.
  if (Tok.is(tok::r_brace)) {
    diagnose(Tok, diag::computed_property_no_accessors);
    return true;
  }

  bool IsFirstAccessor = true;
  while (Tok.isNot(tok::r_brace)) {
    if (Tok.is(tok::eof))
      return true;

    // If there are any attributes, we are going to parse them.  Because these
    // attributes might not be appertaining to the accessor, but to the first
    // declaration inside the implicit getter, we need to save the parser
    // position and restore it later.
    ParserPosition BeginParserPosition;
    if (Tok.is(tok::at_sign))
      BeginParserPosition = getParserPosition();

    // Parse any leading attributes.
    DeclAttributes Attributes;
    parseDeclAttributeList(Attributes);

    bool isImplicitGet = false;
    AccessorKind Kind;
    FuncDecl **TheDeclPtr;
    if (Tok.isContextualKeyword("get")) {
      Kind = AccessorKind::IsGetter;
      TheDeclPtr = &Get;
    } else if (Tok.isContextualKeyword("set")) {
      Kind = AccessorKind::IsSetter;
      TheDeclPtr = &Set;
    } else if (Tok.isContextualKeyword("willSet")) {
      Kind = AccessorKind::IsWillSet;
      TheDeclPtr = &WillSet;
    } else if (Tok.isContextualKeyword("didSet")) {
      Kind = AccessorKind::IsDidSet;
      TheDeclPtr = &DidSet;
    } else {
      // This is an implicit getter.  Might be not valid in this position,
      // though.  Anyway, go back to the beginning of the getter code to ensure
      // that the diagnostics point to correct tokens.
      if (BeginParserPosition.isValid()) {
        backtrackToPosition(BeginParserPosition);
        Attributes = DeclAttributes();
      }
      if (!IsFirstAccessor) {
        // Can not have an implicit getter after other accessor.
        diagnose(Tok, diag::expected_accessor_kw);
        skipUntil(tok::r_brace);
        // Don't signal an error since we recovered.
        return false;
      }
      Kind = AccessorKind::IsGetter;
      TheDeclPtr = &Get;
      isImplicitGet = true;
    }

    IsFirstAccessor = false;

    // Consume the contextual keyword, if present.
    SourceLoc Loc = isImplicitGet ? Tok.getLoc() : consumeToken();

    FuncDecl *&TheDecl = *TheDeclPtr;

    // Have we already parsed this kind of clause?
    if (TheDecl) {
      diagnose(Loc, diag::duplicate_property_accessor, (unsigned)Kind);
      diagnose(TheDecl->getLoc(), diag::previous_accessor, (unsigned)Kind);
      // Forget the previous decl.
      Decls.erase(std::find(Decls.begin(), Decls.end(), TheDecl));
      TheDecl = nullptr;
    }

    // 'set' and 'willSet' can have an optional name.
    //
    //     set-name    ::= '(' identifier ')'
    auto *ValueNamePattern =
      parseOptionalAccessorArgument(Loc, ElementTy, *this, Kind);

    SourceLoc LBLoc = Tok.getLoc();
    // FIXME: Use outer '{' loc if isImplicitGet.
    bool ExternalAsmName = false;
    if (!isImplicitGet && !consumeIf(tok::l_brace)) {
      // asmname'd accessors don't need bodies.
      if (!Attributes.hasAttribute<AsmnameAttr>()) {
        diagnose(Tok, diag::expected_lbrace_accessor, (unsigned)Kind);
        return true;
      }
      ExternalAsmName = true;
    }

    // Set up a function declaration.
    TheDecl = createAccessorFunc(Loc, ValueNamePattern, ElementTy, Indices,
                                 StaticLoc, Flags, Kind, this);

    if (Attributes.shouldSaveInAST())
      TheDecl->getMutableAttrs() = Attributes;

    // Parse the body, if any.
    if (ExternalAsmName) {
      LastValidLoc = Loc;
    } else {
      Scope S(this, ScopeKind::FunctionBody);
      addPatternVariablesToScope(TheDecl->getBodyParamPatterns());

      // Establish the new context.
      ParseFunctionBody CC(*this, TheDecl);

      // Parse the body.
      SmallVector<ASTNode, 16> Entries;
      if (!isDelayedParsingEnabled())
        parseBraceItems(Entries);
      else
        consumeGetSetBody(TheDecl, LBLoc);

      SourceLoc RBLoc = Tok.getLoc();
      if (!isImplicitGet &&
          parseMatchingToken(tok::r_brace, RBLoc, diag::expected_rbrace_in_getset,
                             LBLoc))
        RBLoc = PreviousLoc;

      if (!isDelayedParsingEnabled()) {
        BraceStmt *Body = BraceStmt::create(Context, LBLoc, Entries, RBLoc);
        TheDecl->setBody(Body);
      }
      LastValidLoc = RBLoc;
    }

    Decls.push_back(TheDecl);
  }

  return false;
}

bool Parser::parseGetSet(ParseDeclOptions Flags, Pattern *Indices,
                         TypeLoc ElementTy, FuncDecl *&Get, FuncDecl *&Set,
                         FuncDecl *&WillSet, FuncDecl *&DidSet,
                         SourceLoc &LBLoc, SourceLoc &RBLoc,
                         SourceLoc StaticLoc,
                         SmallVectorImpl<Decl *> &Decls) {
  LBLoc = consumeToken(tok::l_brace);
  SourceLoc LastValidLoc = LBLoc;
  bool Invalid = parseGetSetImpl(Flags, Indices, ElementTy, Get, Set, WillSet,
                                 DidSet, LastValidLoc, StaticLoc, Decls);

  // Parse the final '}'.
  if (Invalid)
    skipUntil(tok::r_brace);

  if (parseMatchingToken(tok::r_brace, RBLoc, diag::expected_rbrace_in_getset,
                         LBLoc)) {
    Invalid = true;
    RBLoc = LastValidLoc;
  }
  return Invalid;
}

void Parser::parseAccessorBodyDelayed(AbstractFunctionDecl *AFD) {
  assert(!AFD->getBody() && "function should not have a parsed body");
  assert(AFD->getBodyKind() == AbstractFunctionDecl::BodyKind::Unparsed &&
         "function body should be delayed");

  auto AccessorParserState = State->takeAccessorBodyState(AFD);
  assert(AccessorParserState.get() && "should have a valid state");

  auto BeginParserPosition = getParserPosition(AccessorParserState->BodyPos);
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

  // Rewind to the first token of the accessor body.
  restoreParserPosition(BeginParserPosition);

  // Re-enter the lexical scope.
  Scope S(this, AccessorParserState->takeScope());
  ParseFunctionBody CC(*this, AFD);

  SmallVector<ASTNode, 16> Entries;
  parseBraceItems(Entries);
  BraceStmt *Body =
      BraceStmt::create(Context, AccessorParserState->LBLoc, Entries,
                        Tok.getLoc());
  AFD->setBody(Body);
}

/// \brief Parse the brace-enclosed getter and setter for a variable.
VarDecl *Parser::parseDeclVarGetSet(Pattern *pattern, ParseDeclOptions Flags,
                                    SourceLoc StaticLoc,
                                    SmallVectorImpl<Decl *> &Decls) {
  bool Invalid = false;
  
  // The grammar syntactically requires a simple identifier for the variable
  // name. Complain if that isn't what we got.
  VarDecl *PrimaryVar = nullptr;
  {
    Pattern *PrimaryPattern = pattern;
    if (TypedPattern *Typed = dyn_cast<TypedPattern>(PrimaryPattern))
      PrimaryPattern = Typed->getSubPattern();
    if (NamedPattern *Named = dyn_cast<NamedPattern>(PrimaryPattern)) {
      PrimaryVar = Named->getDecl();
    }
  }

  if (!PrimaryVar) {
    diagnose(pattern->getLoc(), diag::getset_nontrivial_pattern);
    Invalid = true;
  } else {
    setLocalDiscriminator(PrimaryVar);

    // Reject getters and setters for 'val's, but keep parsing them, for better
    // recovery.
    if (PrimaryVar->isLet()) {
      diagnose(Tok, diag::let_cannot_be_computed_property);
      Invalid = true;
    }
  }

  // The grammar syntactically requires a type annotation. Complain if
  // our pattern does not have one.
  TypeLoc TyLoc;
  if (TypedPattern *TP = dyn_cast<TypedPattern>(pattern)) {
    TyLoc = TP->getTypeLoc();
  } else {
    if (PrimaryVar)
      diagnose(pattern->getLoc(), diag::getset_missing_type);
    TyLoc = TypeLoc::withoutLoc(ErrorType::get(Context));
  }

  // Parse getter and setter.
  FuncDecl *Get = nullptr, *Set = nullptr;
  FuncDecl *WillSet = nullptr, *DidSet = nullptr;
  SourceLoc LBLoc;
  SourceLoc RBLoc;
  if (parseGetSet(Flags, /*Indices=*/0, TyLoc, Get, Set, WillSet, DidSet,
                  LBLoc, RBLoc, StaticLoc, Decls))
    Invalid = true;

  // If we have an invalid case, bail out now.
  if (!PrimaryVar)
    return nullptr;
  
  // If this is a willSet/didSet observing property, record this and we're done.
  if (WillSet || DidSet) {
    if (Get || Set) {
      diagnose(Get ? Get->getLoc() : Set->getLoc(),
               diag::observingproperty_with_getset, bool(DidSet), bool(Set));
      if (Get) {
        Get->setType(ErrorType::get(Context));
        Get->setInvalid(); Get = nullptr;
      }
      if (Set) {
        Set->setType(ErrorType::get(Context));
        Set->setInvalid(); Set = nullptr;
      }
    }

    PrimaryVar->makeObserving(LBLoc, WillSet, DidSet, RBLoc);

    // Observing properties will have getters and setters synthesized by sema.
    // Create their prototypes now.
    Get = createAccessorFunc(SourceLoc(), /*ArgPattern*/nullptr, TyLoc, nullptr,
                             StaticLoc, Flags, AccessorKind::IsGetter, this);
    Get->setImplicit();
    Decls.push_back(Get);

    auto ArgPattern = parseOptionalAccessorArgument(SourceLoc(), TyLoc, *this,
                                                    AccessorKind::IsSetter);
    Set = createAccessorFunc(SourceLoc(), ArgPattern, TyLoc, nullptr,
                             StaticLoc, Flags, AccessorKind::IsSetter, this);
    Set->setImplicit();
    Decls.push_back(Set);
    PrimaryVar->setObservingAccessors(Get, Set);
    return PrimaryVar;
  }

  // If this decl is invalid, mark any parsed accessors as invalid to avoid
  // tripping up later invariants.
  if (Invalid) {
    if (Get) {
      Get->setType(ErrorType::get(Context));
      Get->setInvalid();
    }
    if (Set) {
      Set->setType(ErrorType::get(Context));
      Set->setInvalid();
    }
  }

  // Otherwise, this must be a get/set property.  The set is optional, but get
  // is not.
  if (!Invalid && Set && !Get) {
    diagnose(Set->getLoc(), diag::var_set_without_get);
  }

  // Turn this into a computed variable.
  if (Set || Get) {
    PrimaryVar->makeComputed(LBLoc, Get, Set, RBLoc);
    return PrimaryVar;
  }

  return nullptr;
}

/// \brief Parse a 'var' or 'val' declaration, doing no token skipping on error.
ParserStatus Parser::parseDeclVar(ParseDeclOptions Flags,
                                  DeclAttributes &Attributes,
                                  SmallVectorImpl<Decl *> &Decls,
                                  SourceLoc StaticLoc,
                                  StaticSpellingKind StaticSpelling) {
  assert(StaticLoc.isInvalid() || StaticSpelling != StaticSpellingKind::None);

  if (StaticLoc.isValid()) {
    if (!Flags.contains(PD_HasContainerType)) {
      diagnose(Tok, diag::static_var_decl_global_scope, StaticSpelling)
          .fixItRemoveChars(StaticLoc, Tok.getLoc());
      StaticLoc = SourceLoc();
    } else if (Flags.contains(PD_InProtocol) || Flags.contains(PD_InClass)) {
      if (StaticSpelling == StaticSpellingKind::KeywordStatic)
        diagnose(Tok, diag::static_var_in_class)
            .fixItReplace(StaticLoc, "class");
    } else if (!Flags.contains(PD_InExtension)) {
      if (StaticSpelling == StaticSpellingKind::KeywordClass)
        diagnose(Tok, diag::class_var_in_struct)
            .fixItReplace(StaticLoc, "static");
    }
  }

  bool isLet = Tok.is(tok::kw_let);
  assert(Tok.getKind() == tok::kw_let || Tok.getKind() == tok::kw_var);
  SourceLoc VarLoc = consumeToken();

  
  struct AllBindings {
    Parser &P;

    struct BindingInfo {
      PatternBindingDecl *Binding;
      TopLevelCodeDecl *TopLevelCode;
    };
    SmallVector<BindingInfo, 4> All;

    AllBindings(Parser &P) : P(P) {}
    ~AllBindings() {
      for (auto &info : All) {
        if (!info.TopLevelCode) continue;
        auto binding = info.Binding;
        auto range = binding->getSourceRange();
        info.TopLevelCode->setBody(BraceStmt::create(P.Context, range.Start,
                                               ASTNode(binding), range.End));
      }
    }
  } Bindings(*this);

  bool HasGetSet = false;
  ParserStatus Status;

  do {
    ParserResult<Pattern> pattern;

    { // In our recursive parse, remember that we're in a var/let pattern.
      llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
        T(InVarOrLetPattern, isLet ? IVOLP_InLet : IVOLP_InVar);

      pattern = parsePattern(isLet);
    }
    if (pattern.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
    if (pattern.isNull())
      return makeParserError();

    // If this is a var in the top-level of script/repl source file, wrap the
    // PatternBindingDecl in a TopLevelCodeDecl, since it represents executable
    // code.  The VarDecl and any accessor decls (for computed properties) go in
    // CurDeclContext.
    //
    // Note that, once we've built the TopLevelCodeDecl, we have to be
    // really cautious not to escape this scope in a way that doesn't
    // add it as a binding.
    TopLevelCodeDecl *topLevelDecl = nullptr;
    Optional<ContextChange> topLevelParser;
    if (allowTopLevelCode() && CurDeclContext->isModuleScopeContext()) {
      // The body of topLevelDecl will get set later.
      topLevelDecl = new (Context) TopLevelCodeDecl(CurDeclContext);
      topLevelParser.emplace(*this, topLevelDecl,
                             &State->getTopLevelContext());
    }

    // In the normal case, just add PatternBindingDecls to our DeclContext.
    auto PBD = new (Context) PatternBindingDecl(
        StaticLoc, StaticSpelling, VarLoc, pattern.get(), nullptr,
        /*conditional*/ false, CurDeclContext);

    Bindings.All.push_back({PBD, topLevelDecl});

    // Parse an initializer if present.
    if (Tok.is(tok::equal)) {
      // Record the variables that we're trying to initialize.
      SmallVector<VarDecl *, 4> Vars;
      Vars.append(CurVars.second.begin(), CurVars.second.end());
      pattern.get()->collectVariables(Vars);
      using RestoreVarsRAII = llvm::SaveAndRestore<decltype(CurVars)>;
      RestoreVarsRAII RestoreCurVars(CurVars, {CurDeclContext, Vars});
      
      // Enter an initializer context if we're not in a local context.
      PatternBindingInitializer *initContext = nullptr;
      Optional<ParseFunctionBody> initParser;
      if (!CurDeclContext->isLocalContext()) {
        initContext = Context.createPatternBindingContext(PBD);
        initParser.emplace(*this, initContext);
      }
      
      SourceLoc EqualLoc = consumeToken(tok::equal);
      ParserResult<Expr> init = parseExpr(diag::expected_init_value);
      
      // Leave the initializer context.
      if (initContext) {
        if (!initParser->hasClosures())
          Context.destroyPatternBindingContext(initContext);
        initParser.reset();
      }
      assert(!initParser.hasValue());
      
      if (init.hasCodeCompletion())
        return makeParserCodeCompletionStatus();
      if (init.isNull())
        return makeParserError();
      
      if (Flags & PD_DisallowInit) {
        diagnose(EqualLoc, diag::disallowed_init);
        Status.setIsParseError();
        init = nullptr;
      }
      
      PBD->setInit(init.getPtrOrNull(), false);
    }
    
    if (topLevelDecl) {
      Decls.push_back(topLevelDecl);
    } else {
      Decls.push_back(PBD);
    }
    
    // We need to revert CurDeclContext before parsing accessors.
    if (topLevelDecl)
      topLevelParser.getValue().pop();


    // If we syntactically match the second decl-var production, with a
    // var-get-set clause, parse the var-get-set clause.
    if (Tok.is(tok::l_brace)) {
      if (auto *boundVar =
            parseDeclVarGetSet(pattern.get(), Flags, StaticLoc, Decls)) {

        if (PBD->getInit() && !boundVar->hasStorage()) {
          diagnose(pattern.get()->getLoc(), diag::getset_init)
            .highlight(PBD->getInit()->getSourceRange());
          PBD->setInit(nullptr, false);
        }
      }

      if (isLet)
        return makeParserError();

      HasGetSet = true;
    }

    // Add all parsed vardecls to this scope.
    addPatternVariablesToScope(pattern.get());
    
    // Configure them properly with attributes and 'static'.
    pattern.get()->forEachVariable([&](VarDecl *VD) {
      VD->setStatic(StaticLoc.isValid());
      VD->setParentPattern(PBD);
      if (Attributes.shouldSaveInAST())
        VD->getMutableAttrs() = Attributes;
      
      Decls.push_back(VD);
    });
    
    // Propagate back types for simple patterns, like "var A, B : T".
    if (TypedPattern *TP = dyn_cast<TypedPattern>(PBD->getPattern())) {
      if (isa<NamedPattern>(TP->getSubPattern()) && !PBD->hasInit()) {
        for (unsigned i = Bindings.All.size() - 1; i != 0; --i) {
          PatternBindingDecl *PrevPBD = Bindings.All[i-1].Binding;
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
          NewTP->setPropagatedType();
          PrevPBD->setPattern(NewTP);
        }
      }
    }
  } while (consumeIf(tok::comma));

  if (HasGetSet) {
    if (Bindings.All.size() > 1) {
      diagnose(VarLoc, diag::disallowed_var_multiple_getset);
      Status.setIsParseError();
    }
  } else if (!StaticLoc.isValid() && (Flags & PD_DisallowStoredInstanceVar)) {
    diagnose(VarLoc, diag::disallowed_stored_var_decl);
    Status.setIsParseError();
    return Status;
  }

  return Status;
}

void Parser::consumeAbstractFunctionBody(AbstractFunctionDecl *AFD,
                                         const DeclAttributes &Attrs) {
  auto BeginParserPosition = getParserPosition();
  SourceRange BodyRange;
  BodyRange.Start = Tok.getLoc();

  // Consume the '{', and find the matching '}'.
  unsigned OpenBraces = skipBracedBlock(*this);
  if (OpenBraces != 0 && Tok.isNot(tok::code_complete)) {
    assert(Tok.is(tok::eof));
    // We hit EOF, and not every brace has a pair.  Recover by searching
    // for the next decl except variable decls and cutting off before
    // that point.
    backtrackToPosition(BeginParserPosition);
    consumeToken(tok::l_brace);
    while (Tok.is(tok::kw_var) || Tok.is(tok::kw_let) ||
           (Tok.isNot(tok::eof) && !isStartOfDecl(Tok, peekToken()))) {
      consumeToken();
    }
  }

  BodyRange.End = PreviousLoc;

  if (DelayedParseCB->shouldDelayFunctionBodyParsing(*this, AFD, Attrs,
                                                     BodyRange)) {
    State->delayFunctionBodyParsing(AFD, BodyRange,
                                    BeginParserPosition.PreviousLoc);
    AFD->setBodyDelayed(BodyRange);
  } else {
    AFD->setBodySkipped(BodyRange);
  }
}

/// \brief Parse a 'func' declaration, returning null on error.  The caller
/// handles this case and does recovery as appropriate.
///
/// \verbatim
///   decl-func:
///     ('static' | 'class')? 'mutating'? 'func' attribute-list
///               any-identifier generic-params? func-signature stmt-brace?
/// \endverbatim
///
/// \note The caller of this method must ensure that the next token is 'func'.
ParserResult<FuncDecl>
Parser::parseDeclFunc(SourceLoc StaticLoc, StaticSpellingKind StaticSpelling,
                      SourceLoc MutatingLoc,
                      ParseDeclOptions Flags, DeclAttributes &Attributes) {
  assert((StaticLoc.isInvalid() || MutatingLoc.isInvalid()) &&
         "Parser should only parse one of 'mutating' or 'class'/'static'");
  assert(StaticLoc.isInvalid() || StaticSpelling != StaticSpellingKind::None);

  bool HasContainerType = Flags.contains(PD_HasContainerType);

  if (StaticLoc.isValid()) {
    if (!HasContainerType) {
      // Reject static functions at global scope.
      diagnose(Tok, diag::static_func_decl_global_scope, StaticSpelling)
          .fixItRemoveChars(StaticLoc, Tok.getLoc());
      StaticLoc = SourceLoc();
    } else if (Flags.contains(PD_InProtocol) || Flags.contains(PD_InClass)) {
      if (StaticSpelling == StaticSpellingKind::KeywordStatic)
        diagnose(Tok, diag::static_func_in_class)
            .fixItReplace(StaticLoc, "class");
    } else if (!Flags.contains(PD_InExtension)) {
      if (StaticSpelling == StaticSpellingKind::KeywordClass)
        diagnose(Tok, diag::class_func_in_struct)
            .fixItReplace(StaticLoc, "static");
    }
  }

  // If the 'mutating' modifier was applied to the func, model it as if the
  // @mutating attribute were specified.
  if (MutatingLoc.isValid()) {
    if (!Attributes.AtLoc.isValid())
      Attributes.AtLoc = MutatingLoc;
    Attributes.setAttr(AK_mutating, MutatingLoc);
  }

  if (StaticLoc.isValid() && Attributes.hasMutating()) {
    diagnose(Tok, diag::static_functions_not_mutating);
    Attributes.clearAttribute(AK_mutating);
  }

  SourceLoc FuncLoc = consumeToken(tok::kw_func);

  // Forgive the lexer
  if (Tok.is(tok::amp_prefix)) {
    Tok.setKind(tok::oper_prefix);
  }
  Identifier SimpleName;
  SourceLoc NameLoc = Tok.getLoc();
  if (!(Flags & PD_AllowTopLevel) && 
      !(Flags & PD_InProtocol) &&
      Tok.isAnyOperator()) {
    // FIXME: Recovery here is awful.
    diagnose(Tok, diag::func_decl_nonglobal_operator);
    return nullptr;
  }
  if (parseAnyIdentifier(SimpleName, diag::expected_identifier_in_decl, "function")) {
    ParserStatus NameStatus =
        parseIdentifierDeclName(*this, SimpleName, NameLoc, tok::l_paren, tok::arrow,
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
  if (SimpleName.str().size() > 1 && SimpleName.str().back() == '<'
      && Tok.is(tok::identifier)) {
    SimpleName = Context.getIdentifier(SimpleName.str().slice(0, SimpleName.str().size() - 1));
    SourceLoc LAngleLoc = NameLoc.getAdvancedLoc(SimpleName.str().size());
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
  // "(inout self: FooTy)->(int)->int", and a static function
  // "(int)->int" on FooTy into "(self: FooTy.Type)->(int)->int".
  // Note that we can't actually compute the type here until Sema.
  if (HasContainerType) {
    Pattern *SelfPattern = buildImplicitSelfParameter(NameLoc, CurDeclContext);
    ArgParams.push_back(SelfPattern);
    BodyParams.push_back(SelfPattern);
  }

  DefaultArgumentInfo DefaultArgs;
  TypeRepr *FuncRetTy = nullptr;
  bool HasSelectorStyleSignature;
  DeclName FullName;
  ParserStatus SignatureStatus =
      parseFunctionSignature(SimpleName, FullName,
                             ArgParams, BodyParams, DefaultArgs,
                             FuncRetTy, HasSelectorStyleSignature);

  if (SignatureStatus.hasCodeCompletion() && !CodeCompletion) {
    // Trigger delayed parsing, no need to continue.
    return SignatureStatus;
  }
  
  // Protocol method arguments may not have default values.
  if (Flags.contains(PD_InProtocol) && DefaultArgs.HasDefaultArgument) {
    diagnose(FuncLoc, diag::protocol_method_argument_init);
    return nullptr;
  }
  
  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  FuncDecl *FD;
  {
    Scope S(this, ScopeKind::FunctionBody);

    // Create the decl for the func and add it to the parent scope.
    FD = FuncDecl::create(Context, StaticLoc, StaticSpelling,
                          FuncLoc, FullName, NameLoc, GenericParams,
                          Type(), ArgParams, BodyParams, FuncRetTy,
                          CurDeclContext);

    if (HasSelectorStyleSignature)
      FD->setHasSelectorStyleSignature();

    // Pass the function signature to code completion.
    if (SignatureStatus.hasCodeCompletion())
      CodeCompletion->setDelayedParsedDecl(FD);

    DefaultArgs.setFunctionContext(FD);
    addPatternVariablesToScope(FD->getBodyParamPatterns());
    setLocalDiscriminator(FD);
    
    // Establish the new context.
    ParseFunctionBody CC(*this, FD);

    // Check to see if we have a "{" to start a brace statement.
    if (Tok.is(tok::l_brace)) {
      if (Flags.contains(PD_InProtocol)) {
        diagnose(Tok, diag::protocol_method_with_body);
        skipUntilDeclRBrace();
      } else if (!isDelayedParsingEnabled()) {
        ParserResult<BraceStmt> Body =
            parseBraceItemList(diag::func_decl_without_brace);
        if (Body.isNull()) {
          // FIXME: Should do some sort of error recovery here?
        } else if (SignatureStatus.hasCodeCompletion()) {
          // Code completion was inside the signature, don't attach the body.
          FD->setBodySkipped(Body.get()->getSourceRange());
        } else {
          FD->setBody(Body.get());
        }
      } else {
        consumeAbstractFunctionBody(FD, Attributes);
      }
    } else {
      checkForInputIncomplete();
    }
  }

  // Exit the scope introduced for the generic parameters.
  GenericsScope.reset();

  if (Attributes.shouldSaveInAST())
    FD->getMutableAttrs() = Attributes;
  addToScope(FD);
  return makeParserResult(FD);
}

bool Parser::parseAbstractFunctionBodyDelayed(AbstractFunctionDecl *AFD) {
  assert(!AFD->getBody() && "function should not have a parsed body");
  assert(AFD->getBodyKind() == AbstractFunctionDecl::BodyKind::Unparsed &&
         "function body should be delayed");

  auto FunctionParserState = State->takeFunctionBodyState(AFD);
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
  ParseFunctionBody CC(*this, AFD);

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

/// \brief Parse a 'enum' declaration, returning true (and doing no token
/// skipping) on error.
///
/// \verbatim
///   decl-enum:
///      'enum' attribute-list identifier generic-params? inheritance?
///          '{' decl-enum-body '}'
///   decl-enum-body:
///      decl*
/// \endverbatim
ParserResult<EnumDecl> Parser::parseDeclEnum(ParseDeclOptions Flags,
                                             DeclAttributes &Attributes) {
  SourceLoc EnumLoc = consumeToken(tok::kw_enum);

  Identifier EnumName;
  SourceLoc EnumNameLoc;
  ParserStatus Status;

  Status |=
      parseIdentifierDeclName(*this, EnumName, EnumNameLoc, tok::colon,
                              tok::l_brace, TokenProperty::StartsWithLess,
                              diag::expected_identifier_in_decl, "enum");
  if (Status.isError())
    return nullptr;

  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    Scope S(this, ScopeKind::Generics);
    GenericParams = maybeParseGenericParams();
  }

  EnumDecl *UD = new (Context) EnumDecl(EnumLoc, EnumName, EnumNameLoc,
                                        { }, GenericParams, CurDeclContext);
  setLocalDiscriminator(UD);

  if (Attributes.shouldSaveInAST())
    UD->getMutableAttrs() = Attributes;

  // Parse optional inheritance clause within the context of the enum.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, UD);
    SmallVector<TypeLoc, 2> Inherited;
    Status |= parseInheritance(Inherited);
    UD->setInherited(Context.AllocateCopy(Inherited));
  }

  SmallVector<Decl*, 8> MemberDecls;
  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_enum)) {
    LBLoc = Tok.getLoc();
    RBLoc = LBLoc;
    Status.setIsParseError();
  } else {
    ContextChange CC(*this, UD);
    Scope S(this, ScopeKind::ClassBody);
    ParseDeclOptions Options(PD_HasContainerType | PD_AllowEnumElement |
                             PD_DisallowStoredInstanceVar);
    if (parseNominalDeclMembers(MemberDecls, LBLoc, RBLoc,
                                diag::expected_rbrace_enum,
                                Options))
      Status.setIsParseError();
  }

  if (MemberDecls.empty())
    UD->setMembers({}, { LBLoc, RBLoc });
  else
    UD->setMembers(Context.AllocateCopy(MemberDecls), { LBLoc, RBLoc });
  addToScope(UD);

  if (Flags & PD_DisallowNominalTypes) {
    diagnose(EnumLoc, diag::disallowed_type);
    Status.setIsParseError();
  }

  return makeParserResult(Status, UD);
}

/// \brief Parse a 'case' of an enum.
///
/// \verbatim
///   enum-case:
///      identifier type-tuple?
///   decl-enum-element:
///      'case' attribute-list enum-case (',' enum-case)*
/// \endverbatim
ParserStatus Parser::parseDeclEnumCase(ParseDeclOptions Flags,
                                       DeclAttributes &Attributes,
                                       llvm::SmallVectorImpl<Decl *> &Decls) {
  ParserStatus Status;
  SourceLoc CaseLoc = consumeToken(tok::kw_case);

  // Parse comma-separated enum elements.
  SmallVector<EnumElementDecl*, 4> Elements;
  
  SourceLoc CommaLoc;
  for (;;) {
    Identifier Name;
    SourceLoc NameLoc;

    const bool NameIsNotIdentifier = Tok.isNot(tok::identifier);
    if (parseIdentifierDeclName(*this, Name, NameLoc, tok::l_paren,
                                tok::kw_case, tok::colon, tok::r_brace,
                                diag::invalid_diagnostic).isError()) {
      NameLoc = CaseLoc;

      // Handle the likely case someone typed 'case X, case Y'.
      if (Tok.is(tok::kw_case) && CommaLoc.isValid()) {
        diagnose(Tok, diag::expected_identifier_after_case_comma);
        return Status;
      }
      
      // For recovery, see if the user typed something resembling a switch
      // "case" label.
      parseMatchingPattern();
    }
    if (NameIsNotIdentifier) {
      if (consumeIf(tok::colon)) {
        diagnose(CaseLoc, diag::case_outside_of_switch, "case");
        Status.setIsParseError();
        return Status;
      }
      if (CommaLoc.isValid()) {
        diagnose(Tok, diag::expected_identifier_after_case_comma);
        return Status;
      }
      diagnose(CaseLoc, diag::expected_identifier_in_decl, "enum case");
    }

    // See if there's a following argument type.
    ParserResult<TypeRepr> ArgType;
    if (Tok.isFollowingLParen()) {
      ArgType = parseTypeTupleBody();
      if (ArgType.hasCodeCompletion()) {
        Status.setHasCodeCompletion();
        return Status;
      }
      if (ArgType.isNull()) {
        Status.setIsParseError();
        return Status;
      }
    }
    
    // See if there's a raw value expression.
    SourceLoc EqualsLoc;
    ParserResult<Expr> RawValueExpr;
    LiteralExpr *LiteralRawValueExpr = nullptr;
    if (Tok.is(tok::equal)) {
      EqualsLoc = consumeToken();
      {
        CodeCompletionCallbacks::InEnumElementRawValueRAII
            InEnumElementRawValue(CodeCompletion);
        RawValueExpr = parseExpr(diag::expected_expr_enum_case_raw_value);
      }
      if (RawValueExpr.hasCodeCompletion()) {
        Status.setHasCodeCompletion();
        return Status;
      }
      if (RawValueExpr.isNull()) {
        Status.setIsParseError();
        return Status;
      }
      // The raw value must be syntactically a simple literal.
      LiteralRawValueExpr = dyn_cast<LiteralExpr>(RawValueExpr.getPtrOrNull());
      if (!LiteralRawValueExpr
          || isa<InterpolatedStringLiteralExpr>(LiteralRawValueExpr)) {
        diagnose(RawValueExpr.getPtrOrNull()->getLoc(),
                 diag::nonliteral_enum_case_raw_value);
        LiteralRawValueExpr = nullptr;
      }
    }
    
    // For recovery, again make sure the the user didn't try to spell a switch
    // case label:
    // 'case Identifier:' or
    // 'case Identifier where ...:'
    if (Tok.is(tok::colon) || Tok.is(tok::kw_where)) {
      diagnose(CaseLoc, diag::case_outside_of_switch, "case");
      skipUntilDeclRBrace();
      Status.setIsParseError();
      return Status;
    }
    
    // Create the element.
    auto *result = new (Context) EnumElementDecl(NameLoc, Name,
                                                 ArgType.getPtrOrNull(),
                                                 EqualsLoc,
                                                 LiteralRawValueExpr,
                                                 CurDeclContext);
    result->getMutableAttrs() = Attributes;
    Elements.push_back(result);
    
    // Continue through the comma-separated list.
    if (!Tok.is(tok::comma))
      break;
    CommaLoc = consumeToken(tok::comma);
  }
  
  if (!(Flags & PD_AllowEnumElement)) {
    diagnose(CaseLoc, diag::disallowed_enum_element);
    // Don't add the EnumElementDecls unless the current context
    // is allowed to have EnumElementDecls.
    Status.setIsParseError();
    return Status;
  }

  // Create and insert the EnumCaseDecl containing all the elements.
  auto TheCase = EnumCaseDecl::create(CaseLoc, Elements, CurDeclContext);
  Decls.push_back(TheCase);
  
  // Insert the element decls.
  std::copy(Elements.begin(), Elements.end(), std::back_inserter(Decls));
  return Status;
}

/// \brief Parse the members in a struct/class/protocol definition.
///
/// \verbatim
///    decl*
/// \endverbatim
bool Parser::parseNominalDeclMembers(SmallVectorImpl<Decl *> &memberDecls,
                                     SourceLoc LBLoc, SourceLoc &RBLoc,
                                     Diag<> ErrorDiag, ParseDeclOptions flags) {
  bool previousHadSemi = true;
  parseList(tok::r_brace, LBLoc, RBLoc, tok::semi, /*OptionalSep=*/true,
            /*AllowSepAfterLast=*/false, ErrorDiag, [&]() -> ParserStatus {
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
      return makeParserError();

    // Check whether the previous declaration had a semicolon after it.
    if (!memberDecls.empty() && memberDecls.back()->TrailingSemiLoc.isValid())
      previousHadSemi = true;

    return makeParserSuccess();
  });

  // If we found the closing brace, then the caller should not care if there
  // were errors while parsing inner decls, because we recovered.
  return !RBLoc.isValid();
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
ParserResult<StructDecl> Parser::parseDeclStruct(ParseDeclOptions Flags,
                                                 DeclAttributes &Attributes) {
  SourceLoc StructLoc = consumeToken(tok::kw_struct);
  
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
  setLocalDiscriminator(SD);

  if (Attributes.shouldSaveInAST())
    SD->getMutableAttrs() = Attributes;

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
ParserResult<ClassDecl> Parser::parseDeclClass(ParseDeclOptions Flags,
                                               DeclAttributes &Attributes) {
  SourceLoc ClassLoc = consumeToken(tok::kw_class);

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
  setLocalDiscriminator(CD);

  // Attach attributes.
  if (Attributes.shouldSaveInAST())
    CD->getMutableAttrs() = Attributes;

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
    ParseDeclOptions Options(PD_HasContainerType | PD_AllowDestructor |
                             PD_InClass);
    if (parseNominalDeclMembers(MemberDecls, LBLoc, RBLoc,
                                diag::expected_rbrace_class,
                                Options))
      Status.setIsParseError();
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
ParserResult<ProtocolDecl> Parser::
parseDeclProtocol(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  SourceLoc ProtocolLoc = consumeToken(tok::kw_protocol);
  
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
  // No need to setLocalDiscriminator: protocols can't appear in local contexts.

  if (Attributes.shouldSaveInAST())
    Proto->getMutableAttrs() = Attributes;

  ContextChange CC(*this, Proto);
  Scope ProtocolBodyScope(this, ScopeKind::ProtocolBody);

  // Parse the body.
  {
    // The list of protocol elements.
    SmallVector<Decl*, 8> Members;

    SourceLoc LBraceLoc;
    SourceLoc RBraceLoc;
    if (parseToken(tok::l_brace, LBraceLoc, diag::expected_lbrace_protocol)) {
      LBraceLoc = Tok.getLoc();
      RBraceLoc = LBraceLoc;
      Status.setIsParseError();
    } else {
      // Parse the members.
      ParseDeclOptions Options(PD_HasContainerType |
                               PD_DisallowNominalTypes |
                               PD_DisallowInit | PD_DisallowTypeAliasDef |
                               PD_InProtocol);
      if (parseNominalDeclMembers(Members, LBraceLoc, RBraceLoc,
                                  diag::expected_rbrace_protocol,
                                  Options))
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

/// \brief Parse a 'subscript' declaration.
///
/// \verbatim
///   decl-subscript:
///     subscript-head get-set
///   subscript-head
///     'subscript' attribute-list pattern-tuple '->' type
/// \endverbatim
ParserStatus Parser::parseDeclSubscript(ParseDeclOptions Flags,
                                        DeclAttributes &Attributes,
                                        SmallVectorImpl<Decl *> &Decls) {
  ParserStatus Status;
  SourceLoc SubscriptLoc = consumeToken(tok::kw_subscript);

  // pattern-tuple
  if (Tok.isNot(tok::l_paren)) {
    diagnose(Tok, diag::expected_lparen_subscript);
    return makeParserError();
  }

  ParserResult<Pattern> Indices =
    parsePatternTuple(/*IsLet*/true, /*IsArgList*/true,/*DefaultArgs=*/nullptr);
  if (Indices.isNull() || Indices.hasCodeCompletion())
    return Indices;
  
  // '->'
  if (!Tok.is(tok::arrow)) {
    diagnose(Tok, diag::expected_arrow_subscript);
    return makeParserError();
  }
  SourceLoc ArrowLoc = consumeToken();
  
  // type
  ParserResult<TypeRepr> ElementTy = parseType(diag::expected_type_subscript);
  if (ElementTy.isNull() || ElementTy.hasCodeCompletion())
    return ElementTy;

  
  // Build an AST for the subscript declaration.
  auto *Subscript = new (Context) SubscriptDecl(Context.Id_subscript,
                                                SubscriptLoc, Indices.get(),
                                                ArrowLoc, ElementTy.get(),
                                                CurDeclContext);
  if (Attributes.shouldSaveInAST())
    Subscript->getMutableAttrs() = Attributes;
  
  Decls.push_back(Subscript);

  
  // '{'
  // Parse getter and setter.
  SourceRange DefRange = SourceRange();
  FuncDecl *Get = nullptr;
  FuncDecl *Set = nullptr;
  if (Tok.isNot(tok::l_brace))  {
    // Subscript declarations must always have at least a getter, so they need
    // to be followed by a {.
    diagnose(Tok, diag::expected_lbrace_subscript);
    Status.setIsParseError();
  } else {
    FuncDecl *WillSet = nullptr, *DidSet = nullptr;
    SourceLoc LBLoc;
    SourceLoc RBLoc;
    if (parseGetSet(Flags, Indices.get(), ElementTy.get(),
                    Get, Set, WillSet, DidSet, LBLoc, RBLoc,
                    /*StaticLoc=*/SourceLoc(), Decls))
      Status.setIsParseError();

    if (Status.isSuccess()) {
      if (!Get)
        diagnose(SubscriptLoc, diag::subscript_without_get);
      if (WillSet || DidSet)
        diagnose(DidSet ? DidSet->getLoc() : WillSet->getLoc(),
                 diag::observingproperty_in_subscript, bool(DidSet));
    }

    DefRange = SourceRange(LBLoc, RBLoc);
  }

  bool Invalid = false;
  // Reject 'subscript' functions outside of type decls
  if (!(Flags & PD_HasContainerType)) {
    diagnose(SubscriptLoc, diag::subscript_decl_wrong_scope);
    Invalid = true;
  }

  // If we had no getter (e.g., because we're in SIL mode or because the
  // program isn't valid) create a stub here.
  if (!Get) {
    Get = createAccessorFunc(SubscriptLoc, /*ArgPattern*/ nullptr,
                             ElementTy.get(), Indices.get(),
                             /*StaticLoc*/ SourceLoc(), Flags,
                             AccessorKind::IsGetter, this);
    Get->setInvalid();
    Get->setType(ErrorType::get(Context));
    Decls.push_back(Get);
  }

  Subscript->setAccessors(DefRange, Get, Set);

  if (Invalid) {
    Subscript->setType(ErrorType::get(Context));
    Subscript->setInvalid();
  }

  // No need to setLocalDiscriminator because subscripts cannot
  // validly appear outside of type decls.
  return Status;
}

ParserResult<ConstructorDecl>
Parser::parseDeclConstructor(ParseDeclOptions Flags,
                             DeclAttributes &Attributes) {
  assert(Tok.is(tok::kw_init));
  SourceLoc ConstructorLoc = consumeToken();

  const bool ConstructorsNotAllowed = !(Flags & PD_HasContainerType);

  // Reject constructors outside of types.
  if (ConstructorsNotAllowed) {
    diagnose(Tok, diag::initializer_decl_wrong_scope);
  }

  // Parse the generic-params, if present.
  Scope S(this, ScopeKind::Generics);
  GenericParamList *GenericParams = maybeParseGenericParams();

  // Parse the parameters.
  // FIXME: handle code completion in Arguments.
  DefaultArgumentInfo DefaultArgs;
  Pattern *ArgPattern;
  Pattern *BodyPattern;
  bool HasSelectorStyleSignature;
  ParserStatus SignatureStatus =
      parseConstructorArguments(ArgPattern, BodyPattern, DefaultArgs,
                                HasSelectorStyleSignature);

  if (SignatureStatus.hasCodeCompletion() && !CodeCompletion) {
    // Trigger delayed parsing, no need to continue.
    return SignatureStatus;
  }

  // '->' 'Self'
  // FIXME: Parse as general type, verify semantically.
  bool isCompleteObjectInit = false;
  if (Tok.is(tok::arrow)) {
    consumeToken(tok::arrow);
    
    if (Tok.is(tok::kw_Self)) {
      consumeToken(tok::kw_Self);
      isCompleteObjectInit = true;
    } else {
      diagnose(Tok, diag::init_expected_self);
      while (!Tok.is(tok::eof) && !Tok.is(tok::l_brace) &&
             !isStartOfDecl(Tok, peekToken()))
        skipSingle();
    }
  }

  VarDecl *SelfDecl;
  auto *SelfPattern = buildImplicitSelfParameter(ConstructorLoc,
                                                 CurDeclContext, &SelfDecl);

  Scope S2(this, ScopeKind::ConstructorBody);
  auto *CD = new (Context) ConstructorDecl(Context.Id_init, ConstructorLoc,
                                           SelfPattern, ArgPattern,
                                           SelfPattern, BodyPattern,
                                           GenericParams, CurDeclContext);
  CD->setCompleteObjectInit(isCompleteObjectInit);

  // No need to setLocalDiscriminator.

  if (HasSelectorStyleSignature)
    CD->setHasSelectorStyleSignature();

  DefaultArgs.setFunctionContext(CD);

  // Pass the function signature to code completion.
  if (SignatureStatus.hasCodeCompletion())
    CodeCompletion->setDelayedParsedDecl(CD);

  if (ConstructorsNotAllowed || SignatureStatus.isError()) {
    // Tell the type checker not to touch this constructor.
    CD->setInvalid();
  }
  addPatternVariablesToScope(ArrayRef<Pattern*>{SelfPattern, BodyPattern} );

  // '{'
  if (Tok.is(tok::l_brace)) {
    // Parse the body.
    ParseFunctionBody CC(*this, CD);

    if (!isDelayedParsingEnabled()) {
      ParserResult<BraceStmt> Body = parseBraceItemList(diag::invalid_diagnostic);

      if (!Body.isNull())
        CD->setBody(Body.get());
    } else {
      consumeAbstractFunctionBody(CD, Attributes);
    }
  }

  if (Attributes.shouldSaveInAST())
    CD->getMutableAttrs() = Attributes;

  return makeParserResult(CD);
}

ParserResult<DestructorDecl> Parser::
parseDeclDestructor(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  assert(Tok.is(tok::kw_deinit) || Tok.is(tok::kw_destructor));
  bool hasDestructorKeyword = Tok.is(tok::kw_destructor);
  SourceLoc DestructorLoc = consumeToken();

  // Parse extraneous parentheses.
  SourceRange ParenRange;
  if (Tok.is(tok::l_paren)) {
    SourceLoc LParenLoc = consumeToken();
    SourceLoc RParenLoc;
    skipUntil(tok::r_paren);

    if (Tok.is(tok::r_paren)) {
      SourceLoc RParenLoc = consumeToken();
      ParenRange = SourceRange(LParenLoc, RParenLoc);
    } else {
      diagnose(Tok, diag::opened_destructor_expected_rparen);
      diagnose(LParenLoc, diag::opening_paren);
    }
  }

  // If we have 'destructor', replace it (and any extraneous parentheses) with
  // 'deinit'.
  if (hasDestructorKeyword) {
    SourceLoc StartLoc = DestructorLoc;
    SourceLoc EndLoc = ParenRange.isValid()? ParenRange.End : DestructorLoc;
    EndLoc = Lexer::getLocForEndOfToken(Context.SourceMgr, EndLoc);

    diagnose(Tok, diag::destructor_is_deinit)
      .fixItReplaceChars(StartLoc, EndLoc, "deinit");
  } else if (ParenRange.isValid()) {
    diagnose(ParenRange.Start, diag::destructor_params)
    .fixItRemoveChars(Lexer::getLocForEndOfToken(Context.SourceMgr,
                                                 DestructorLoc),
                      Lexer::getLocForEndOfToken(Context.SourceMgr,
                                                 ParenRange.End));
  }

  // '{'
  if (!Tok.is(tok::l_brace)) {
    if (!Tok.is(tok::l_brace) && !isInSILMode()) {
      diagnose(Tok, diag::expected_lbrace_destructor);
      return nullptr;
    }
  }

  VarDecl *SelfDecl;
  auto *SelfPattern = buildImplicitSelfParameter(DestructorLoc,
                                                 CurDeclContext, &SelfDecl);

  Scope S(this, ScopeKind::DestructorBody);
  auto *DD = new (Context) DestructorDecl(Context.Id_deinit, DestructorLoc,
                                          SelfPattern, CurDeclContext);
  // No need to setLocalDiscriminator.
  addToScope(SelfDecl);

  // Parse the body.
  if (Tok.is(tok::l_brace)) {
    ParseFunctionBody CC(*this, DD);
    if (!isDelayedParsingEnabled()) {
      ParserResult<BraceStmt> Body = parseBraceItemList(diag::invalid_diagnostic);

      if (!Body.isNull())
        DD->setBody(Body.get());
    } else {
      consumeAbstractFunctionBody(DD, Attributes);
    }
  }

  if (Attributes.shouldSaveInAST())
    DD->getMutableAttrs() = Attributes;

  // Reject 'destructor' functions outside of classes
  if (!(Flags & PD_AllowDestructor)) {
    diagnose(DestructorLoc, diag::destructor_decl_outside_class);

    // Tell the type checker not to touch this destructor.
    DD->setInvalid();
  }

  return makeParserResult(DD);
}

ParserResult<OperatorDecl> Parser::parseDeclOperator(bool AllowTopLevel,
                                                  DeclAttributes &Attributes) {
  assert(Tok.isContextualKeyword("operator") &&
         "no 'operator' at start of operator decl?!");

  SourceLoc OperatorLoc = consumeToken(tok::identifier);

  if (Attributes.hasNonVirtualAttributes())
    diagnose(Attributes.AtLoc, diag::operator_attributes);
  
  auto kind = llvm::StringSwitch<Optional<DeclKind>>(Tok.getText())
    .Case("prefix", DeclKind::PrefixOperator)
    .Case("postfix", DeclKind::PostfixOperator)
    .Case("infix", DeclKind::InfixOperator)
    .Default(Nothing);
  
  assert(kind && "no fixity after 'operator'?!");

  SourceLoc KindLoc = consumeToken(tok::identifier);

  if (!Tok.isAnyOperator() && !Tok.is(tok::exclaim_postfix)) {
    diagnose(Tok, diag::expected_operator_name_after_operator);
    return nullptr;
  }

  Identifier Name = Context.getIdentifier(Tok.getText());
  SourceLoc NameLoc = consumeToken();

  // Postfix operator '!' is reserved.
  if (*kind == DeclKind::PostfixOperator &&Name.str().equals("!")) {
    diagnose(NameLoc, diag::custom_operator_postfix_exclaim);
  }

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

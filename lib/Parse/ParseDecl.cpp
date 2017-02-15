//===--- ParseDecl.cpp - Swift Language Parser for Declarations -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Declaration Parsing and AST Building
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/DelayedParsingCallbacks.h"
#include "swift/Subsystems.h"
#include "swift/AST/Attr.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include <algorithm>

using namespace swift;

namespace {
  /// A RAII object for deciding whether this DeclKind needs special
  /// treatment when parsing in the "debugger context", and implementing
  /// that treatment.  The problem arises because, when lldb
  /// uses swift to parse expressions, it needs to emulate the current
  /// frame's scope. We do that, for instance, by making a class extension
  /// and running the code in a function in that extension.
  ///
  /// This causes two kinds of issues:
  /// 1) Some DeclKinds require to be parsed in TopLevel contexts only.
  /// 2) Sometimes the debugger wants a Decl to live beyond the current
  /// function invocation, in which case it should be parsed at the
  /// file scope level so it will be set up correctly for this purpose.
  ///
  /// Creating an instance of this object will cause it to figure out
  /// whether we are in the debugger function, whether it needs to swap 
  /// the Decl that is currently being parsed.
  /// If you have created the object, instead of returning the result
  /// with makeParserResult, use the object's fixupParserResult.  If
  /// no swap has occurred, these methods will work the same.  
  /// If the decl has been moved, then Parser::markWasHandled will be
  /// called on the Decl, and you should call declWasHandledAlready
  /// before you consume the Decl to see if you actually need to
  /// consume it.
  /// If you are making one of these objects to address issue 1, call
  /// the constructor that only takes a DeclKind, and it will be moved
  /// unconditionally.  Otherwise pass in the Name and DeclKind and the
  /// DebuggerClient will be asked whether to move it or not.
  class DebuggerContextChange {
  protected:
    Parser &P;
    Identifier Name;
    SourceFile *SF;
    Optional<Parser::ContextChange> CC;
  public:
    DebuggerContextChange (Parser &P)
      : P(P), SF(nullptr) {
      if (!inDebuggerContext())
        return;
      else
        switchContext();
    }
    
    DebuggerContextChange (Parser &P, Identifier &Name, DeclKind Kind)
      : P(P), Name(Name), SF(nullptr) {
      if (!inDebuggerContext())
        return;
      bool globalize = false;
        
      DebuggerClient *debug_client = getDebuggerClient();
      if (!debug_client)
        return;
      
      globalize = debug_client->shouldGlobalize(Name, Kind);
        
      if (globalize)
        switchContext();
    }
    
    bool movedToTopLevel() {
      return CC.hasValue();
    }
    
    template <typename T>
    ParserResult<T>
    fixupParserResult(ParserResult<T> &Result) {
      ParserStatus Status = Result;
      return fixupParserResult(Status, Result.getPtrOrNull());
    }
    
    template <typename T>
    ParserResult<T>
    fixupParserResult(T *D) {
      if (CC.hasValue()) {
        swapDecl(D);
      }
      return ParserResult<T>(D);
    }
    
    template <typename T>
    ParserResult<T>
    fixupParserResult(ParserStatus Status, T *D) {
      if (CC.hasValue() && !Status.isError()) {
        // If there is an error, don't do our splicing trick,
        // just return the Decl and the status for reporting.
        swapDecl(D);
      }
      return makeParserResult(Status, D);
    }

    // The destructor doesn't need to do anything, the CC's destructor will
    // pop the context if we set it.
    ~DebuggerContextChange () {}
  protected:
  
    DebuggerClient *getDebuggerClient()
    {
      ModuleDecl *PM = P.CurDeclContext->getParentModule();
      if (!PM)
          return nullptr;
      else
           return PM->getDebugClient();
    }
    
    bool inDebuggerContext() {
      if (!P.Context.LangOpts.DebuggerSupport)
        return false;
      if (!P.CurDeclContext)
        return false;
      FuncDecl *func_decl = dyn_cast<FuncDecl>(P.CurDeclContext);
      if (!func_decl)
        return false;
        
      if (!func_decl->getAttrs().hasAttribute<LLDBDebuggerFunctionAttr>())
        return false;
      
      return true;
    }
    
    void switchContext () {
      SF = P.CurDeclContext->getParentSourceFile();
      CC.emplace (P, SF);
    }
    
    void swapDecl (Decl *D)
    {
      assert (SF);
      DebuggerClient *debug_client = getDebuggerClient();
      assert (debug_client);
      debug_client->didGlobalize(D);
      SF->Decls.push_back(D);
      P.markWasHandled(D);
    }
  };
} // end anonymous namespace

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
  } else if (Tok.is(tok::kw_sil_default_witness_table)) {
    assert(isInSILMode() && "'sil' should only be a keyword in SIL mode");
    parseSILDefaultWitnessTable();
  } else if (Tok.is(tok::kw_sil_coverage_map)) {
    assert(isInSILMode() && "'sil' should only be a keyword in SIL mode");
    parseSILCoverageMap();
  } else if (Tok.is(tok::kw_sil_scope)) {
    assert(isInSILMode() && "'sil' should only be a keyword in SIL mode");
    parseSILScope();
  } else {
    parseBraceItems(Items,
                    allowTopLevelCode() ? BraceItemListKind::TopLevelCode
                                        : BraceItemListKind::TopLevelLibrary);
  }
  
  // In the case of a catastrophic parse error, consume any trailing
  // #else, #elseif, or #endif and move on to the next statement or declaration
  // block.
  if (Tok.is(tok::pound_else) || Tok.is(tok::pound_elseif) ||
      Tok.is(tok::pound_endif)) {
    diagnose(Tok.getLoc(),
             diag::unexpected_conditional_compilation_block_terminator);
    consumeToken();
  }

  // If this is a Main source file, determine if we found code that needs to be
  // executed (this is used by the repl to know whether to compile and run the
  // newly parsed stuff).
  bool FoundTopLevelCodeToExecute = false;
  if (allowTopLevelCode()) {
    for (auto V : Items) {
      if (isa<TopLevelCodeDecl>(V.get<Decl*>()))
        FoundTopLevelCodeToExecute = true;
    }
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
  State->markParserPosition(Tok.getCommentRange().getStart(), PreviousLoc,
                            InPoundLineEnvironment);

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



static Optional<StringRef>
getStringLiteralIfNotInterpolated(Parser &P, SourceLoc Loc, const Token &Tok,
                                  StringRef DiagText) {
  SmallVector<Lexer::StringSegment, 1> Segments;
  P.L->getStringLiteralSegments(Tok, Segments);
  if (Segments.size() != 1 ||
      Segments.front().Kind == Lexer::StringSegment::Expr) {
   P.diagnose(Loc, diag::attr_interpolated_string, DiagText);
   return None;
  }

  return P.SourceMgr.extractText(CharSourceRange(Segments.front().Loc,
                                                 Segments.front().Length));
}

bool Parser::parseSpecializeAttribute(swift::tok ClosingBrace, SourceLoc AtLoc,
                                      SourceLoc Loc, SpecializeAttr *&Attr) {
  assert(ClosingBrace == tok::r_paren || ClosingBrace == tok::r_square);
  SourceLoc lParenLoc = consumeToken();
  bool DiscardAttribute = false;
  StringRef AttrName = "_specialize";

  Optional<bool> exported;
  Optional<SpecializeAttr::SpecializationKind> kind;

  // Parse optional "exported" and "kind" labeled parameters.
  while (!Tok.is(tok::kw_where)) {
    if (Tok.is(tok::identifier)) {
      auto ParamLabel = Tok.getText();
      if (ParamLabel != "exported" && ParamLabel != "kind") {
        diagnose(Tok.getLoc(), diag::attr_specialize_unknown_parameter_name,
                 ParamLabel);
      }
      consumeToken();
      if (!consumeIf(tok::colon)) {
        diagnose(Tok.getLoc(), diag::attr_specialize_missing_colon, ParamLabel);
        skipUntil(tok::comma, tok::kw_where);
        if (Tok.is(ClosingBrace))
          break;
        if (Tok.is(tok::kw_where)) {
          continue;
        }
        if (Tok.is(tok::comma)) {
          consumeToken();
          continue;
        }
        DiscardAttribute = true;
        return false;
      }
      if ((ParamLabel == "exported" && exported.hasValue()) ||
          (ParamLabel == "kind" && kind.hasValue())) {
        diagnose(Tok.getLoc(), diag::attr_specialize_parameter_already_defined,
                 ParamLabel);
      }
      if (ParamLabel == "exported") {
        bool isTrue = consumeIf(tok::kw_true);
        bool isFalse = consumeIf(tok::kw_false);
        if (!isTrue && !isFalse) {
          diagnose(Tok.getLoc(), diag::attr_specialize_expected_bool_value);
          skipUntil(tok::comma, tok::kw_where);
          if (Tok.is(ClosingBrace))
            break;
          if (Tok.is(tok::kw_where)) {
            continue;
          }
          if (Tok.is(tok::comma)) {
            consumeToken();
            continue;
          }
          DiscardAttribute = true;
          return false;
        }
        if (ParamLabel == "exported") {
          exported = isTrue ? true : false;
        }
      }
      if (ParamLabel == "kind") {
        SourceLoc paramValueLoc;
        if (Tok.is(tok::identifier)) {
          if (Tok.getText() == "partial") {
            kind = SpecializeAttr::SpecializationKind::Partial;
          } else if (Tok.getText() == "full") {
            kind = SpecializeAttr::SpecializationKind::Full;
          } else {
            diagnose(Tok.getLoc(),
                     diag::attr_specialize_expected_partial_or_full);
          }
          consumeToken();
        } else if (consumeIf(tok::kw_true, paramValueLoc) ||
                   consumeIf(tok::kw_false, paramValueLoc)) {
          diagnose(paramValueLoc,
                   diag::attr_specialize_expected_partial_or_full);
        }
      }
      if (!consumeIf(tok::comma)) {
        diagnose(Tok.getLoc(), diag::attr_specialize_missing_comma);
        skipUntil(tok::comma, tok::kw_where);
        if (Tok.is(ClosingBrace))
          break;
        if (Tok.is(tok::kw_where)) {
          continue;
        }
        if (Tok.is(tok::comma)) {
          consumeToken();
          continue;
        }
        DiscardAttribute = true;
        return false;
      }
      continue;
    }
    diagnose(Tok.getLoc(),
             diag::attr_specialize_missing_parameter_label_or_where_clause);
    DiscardAttribute = true;
    return false;
  };

  // Parse the where clause.
  TrailingWhereClause *trailingWhereClause = nullptr;
  if (Tok.is(tok::kw_where)) {
    SourceLoc whereLoc;
    SmallVector<RequirementRepr, 4> requirements;
    bool firstTypeInComplete;
    parseGenericWhereClause(whereLoc, requirements, firstTypeInComplete,
                            /* AllowLayoutConstraints */ true);
    trailingWhereClause =
      TrailingWhereClause::create(Context, whereLoc, requirements);
  }

  // Parse the closing ')' or ']'.
  SourceLoc rParenLoc;
  if (!consumeIf(ClosingBrace, rParenLoc)) {
    if (ClosingBrace == tok::r_paren)
      diagnose(lParenLoc, diag::attr_expected_rparen, AttrName,
             /*DeclModifier=*/false);
    else if (ClosingBrace == tok::r_square)
      diagnose(lParenLoc, diag::attr_expected_rparen, AttrName,
             /*DeclModifier=*/false);
    return false;
  }
  // Not exported by default.
  if (!exported.hasValue())
    exported = false;
  // Full specialization by default.
  if (!kind.hasValue())
    kind = SpecializeAttr::SpecializationKind::Full;

  if (DiscardAttribute) {
    Attr = nullptr;
    return false;
  }
  // Store the attribute.
  Attr = SpecializeAttr::create(Context, AtLoc, SourceRange(Loc, rParenLoc),
                                trailingWhereClause, exported.getValue(),
                                kind.getValue());
  return true;
}

bool Parser::parseNewDeclAttribute(DeclAttributes &Attributes, SourceLoc AtLoc,
                                   DeclAttrKind DK) {
  // Ok, it is a valid attribute, eat it, and then process it.
  StringRef AttrName = Tok.getText();
  SourceLoc Loc = consumeToken();
  bool DiscardAttribute = false;

  // Diagnose duplicated attributes.
  const DeclAttribute *DuplicateAttribute = nullptr;
  if (!DeclAttribute::allowMultipleAttributes(DK))
    if ((DuplicateAttribute = Attributes.getAttribute(DK))) {
      // Delay issuing the diagnostic until we parse the attribute.
      DiscardAttribute = true;
    }
 
  // If this is a SIL-only attribute, reject it.
  if ((DeclAttribute::getOptions(DK) & DeclAttribute::SILOnly) != 0 &&
      !isInSILMode()) {
    diagnose(Loc, diag::only_allowed_in_sil, AttrName);
    DiscardAttribute = true;
  }  

  // Filled in during parsing.  If there is a duplicate
  // diagnostic this can be used for better error presentation.
  SourceRange AttrRange;


  // Check 'Tok', return false if ':' or '=' cannot be found.
  // Complain if '=' is found and suggest replacing it with ": ".
  auto findAttrValueDelimiter = [&]() -> bool {
    if (!Tok.is(tok::colon)) {
      if (!Tok.is(tok::equal))
        return false;

      diagnose(Tok.getLoc(), diag::replace_equal_with_colon_for_value)
        .fixItReplace(Tok.getLoc(), ": ");
    }
    return true;
  };

  switch (DK) {
  case DAK_Count:
    llvm_unreachable("DAK_Count should not appear in parsing switch");

  case DAK_AutoClosure: {
    // If we don't have "(escaping", it's just a bare @autoclosure.
    if (Tok.isNot(tok::l_paren) || peekToken().getText() != "escaping") {
      if (!DiscardAttribute)
        Attributes.add(new (Context) AutoClosureAttr(AtLoc, Loc,
                                                     /*escaping=*/false));
      break;
    }

    // Consume the '('.
    SourceLoc lParenLoc = consumeToken(tok::l_paren);

    // Consume the 'escaping'.
    (void)consumeToken();

    // Parse the closing ')'.
    SourceLoc rParenLoc;
    parseMatchingToken(tok::r_paren, rParenLoc,
                       diag::attr_autoclosure_expected_r_paren,
                       lParenLoc);

    // Add the attribute.
    if (!DiscardAttribute)
      Attributes.add(new (Context) AutoClosureAttr(AtLoc,
                                                   SourceRange(Loc, rParenLoc),
                                                   /*escaping=*/true));

    break;
  }

  case DAK_RawDocComment:
  case DAK_ObjCBridged:
  case DAK_SynthesizedProtocol:
    llvm_unreachable("virtual attributes should not be parsed "
                     "by attribute parsing code");
  case DAK_SetterAccessibility:
    llvm_unreachable("handled by DAK_Accessibility");

#define SIMPLE_DECL_ATTR(_, CLASS, ...) \
  case DAK_##CLASS: \
    if (!DiscardAttribute) \
      Attributes.add(new (Context) CLASS##Attr(AtLoc, Loc)); \
    break;
#include "swift/AST/Attr.def"

  case DAK_Effects: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));      return false;
    }

    if (Tok.isNot(tok::identifier)) {
      diagnose(Loc, diag::effects_attribute_expect_option, AttrName);
      return false;
    }

    EffectsKind kind;
    if (Tok.getText() == "readonly")
      kind = EffectsKind::ReadOnly;
    else if (Tok.getText() == "readnone")
      kind = EffectsKind::ReadNone;
    else if (Tok.getText() == "readwrite")
      kind = EffectsKind::ReadWrite;
    else {
      diagnose(Loc, diag::effects_attribute_unknown_option,
               Tok.getText(), AttrName);
      return false;
    }
    AttrRange = SourceRange(Loc, Tok.getRange().getStart());
    consumeToken(tok::identifier);

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    if (!DiscardAttribute)
      Attributes.add(new (Context) EffectsAttr(AtLoc, AttrRange, kind));
    break;
  }

  case DAK_Inline: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    if (Tok.isNot(tok::identifier)) {
      diagnose(Loc, diag::inline_attribute_expect_option, AttrName);
      return false;
    }

    InlineKind kind;
    if (Tok.getText() == "never")
      kind = InlineKind::Never;
    else if (Tok.getText() == "__always")
      kind = InlineKind::Always;
    else {
      diagnose(Loc, diag::inline_attribute_unknown_option,
               Tok.getText(), AttrName);
      return false;
    }
    consumeToken(tok::identifier);
    AttrRange = SourceRange(Loc, Tok.getRange().getStart());
    
    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    if (!DiscardAttribute)
      Attributes.add(new (Context) InlineAttr(AtLoc, AttrRange, kind));

    break;
  }

  case DAK_Ownership: {
    // Handle weak/unowned/unowned(unsafe).
    Ownership Kind = AttrName == "weak" ? Ownership::Weak : Ownership::Unowned;
    SourceLoc EndLoc = Loc;

    if (Kind == Ownership::Unowned && Tok.is(tok::l_paren)) {
      // Parse an optional specifier after unowned.
      SourceLoc lp = consumeToken(tok::l_paren);
      if (Tok.is(tok::identifier) && Tok.getText() == "safe") {
        consumeToken();
      } else if (Tok.is(tok::identifier) && Tok.getText() == "unsafe") {
        consumeToken();
        Kind = Ownership::Unmanaged;
      } else {
        diagnose(Tok, diag::attr_unowned_invalid_specifier);
        consumeIf(tok::identifier);
      }

      SourceLoc rp;
      parseMatchingToken(tok::r_paren, rp, diag::attr_unowned_expected_rparen,
                         lp);
      EndLoc = rp;
    }

    if (!DiscardAttribute)
      Attributes.add(new (Context) OwnershipAttr(SourceRange(Loc, EndLoc),
                                                 Kind));
    break;
  }

  case DAK_Accessibility: {

    // Diagnose using accessibility in a local scope, which isn't meaningful.
    if (CurDeclContext->isLocalContext()) {
      diagnose(Loc, diag::attr_only_at_non_local_scope, AttrName);
    }

    Accessibility access = llvm::StringSwitch<Accessibility>(AttrName)
      .Case("private", Accessibility::Private)
      .Case("fileprivate", Accessibility::FilePrivate)
      .Case("internal", Accessibility::Internal)
      .Case("public", Accessibility::Public)
      .Case("open", Accessibility::Open);

    if (!consumeIf(tok::l_paren)) {
      // Normal accessibility attribute.
      AttrRange = Loc;
      DuplicateAttribute = Attributes.getAttribute<AccessibilityAttr>();
      if (!DuplicateAttribute)
        Attributes.add(new (Context) AccessibilityAttr(AtLoc, Loc, access));
      break;
    }

    // Parse the subject.
    if (Tok.isContextualKeyword("set")) {
      consumeToken();
    } else {
      diagnose(Loc, diag::attr_accessibility_expected_set, AttrName);
      // Minimal recovery: if there's a single token and then an r_paren,
      // consume them both. If there's just an r_paren, consume that.
      if (!consumeIf(tok::r_paren)) {
        if (Tok.isNot(tok::l_paren) && peekToken().is(tok::r_paren)) {
          consumeToken();
          consumeToken(tok::r_paren);
        }
      }
      return false;
    }

    AttrRange = SourceRange(Loc, Tok.getLoc());

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    DuplicateAttribute = Attributes.getAttribute<SetterAccessibilityAttr>();
    if (!DuplicateAttribute) {
      Attributes.add(new (Context) SetterAccessibilityAttr(AtLoc, AttrRange,
                                                           access));
    }

    break;
  }

  case DAK_CDecl:
  case DAK_SILGenName: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    if (Tok.isNot(tok::string_literal)) {
      diagnose(Loc, diag::attr_expected_string_literal, AttrName);
      return false;
    }

    Optional<StringRef> AsmName =
      getStringLiteralIfNotInterpolated(*this, Loc, Tok, AttrName);

    consumeToken(tok::string_literal);

    if (AsmName.hasValue())
      AttrRange = SourceRange(Loc, Tok.getRange().getStart());
    else
      DiscardAttribute = true;

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    // Diagnose using @_silgen_name in a local scope.  These don't
    // actually work.
    if (CurDeclContext->isLocalContext()) {
      // Emit an error, but do not discard the attribute.  This enables
      // better recovery in the parser.
      diagnose(Loc, diag::attr_only_at_non_local_scope, AttrName);
    }

    if (!DiscardAttribute) {
      if (DK == DAK_SILGenName)
        Attributes.add(new (Context) SILGenNameAttr(AsmName.getValue(), AtLoc,
                                                AttrRange, /*Implicit=*/false));
      else if (DK == DAK_CDecl)
        Attributes.add(new (Context) CDeclAttr(AsmName.getValue(), AtLoc,
                                               AttrRange, /*Implicit=*/false));
      else
        llvm_unreachable("out of sync with switch");
    }

    break;
  }
  
  case DAK_Alignment: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }
    
    if (Tok.isNot(tok::integer_literal)) {
      diagnose(Loc, diag::alignment_must_be_positive_integer);
      return false;
    }
    
    StringRef alignmentText = Tok.getText();
    unsigned alignmentValue;
    if (alignmentText.getAsInteger(0, alignmentValue)) {
      diagnose(Loc, diag::alignment_must_be_positive_integer);
      return false;
    }
    
    consumeToken(tok::integer_literal);
    
    auto range = SourceRange(Loc, Tok.getRange().getStart());
    
    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    Attributes.add(new (Context) AlignmentAttr(alignmentValue, AtLoc, range,
                                               /*implicit*/ false));
    
    break;
  }
  
  case DAK_SwiftNativeObjCRuntimeBase: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    if (Tok.isNot(tok::identifier)) {
      diagnose(Loc, diag::swift_native_objc_runtime_base_must_be_identifier);
      return false;
    }
    
    Identifier name = Context.getIdentifier(Tok.getText());
    
    consumeToken(tok::identifier);
    
    auto range = SourceRange(Loc, Tok.getRange().getStart());

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }
    
    Attributes.add(new (Context) SwiftNativeObjCRuntimeBaseAttr(name,
                                            AtLoc, range, /*implicit*/ false));
    break;
  }
  
  case DAK_Semantics: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    if (Tok.isNot(tok::string_literal)) {
      diagnose(Loc, diag::attr_expected_string_literal, AttrName);
      return false;
    }

    auto Value = getStringLiteralIfNotInterpolated(*this, Loc, Tok, AttrName);

    consumeToken(tok::string_literal);

    if (Value.hasValue())
      AttrRange = SourceRange(Loc, Tok.getRange().getStart());
    else
      DiscardAttribute = true;

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    // Diagnose using @_semantics in a local scope.  These don't
    // actually work.
    if (CurDeclContext->isLocalContext()) {
      // Emit an error, but do not discard the attribute.  This enables
      // better recovery in the parser.
      diagnose(Loc, diag::attr_only_at_non_local_scope, AttrName);
    }

    if (!DiscardAttribute)
      Attributes.add(new (Context) SemanticsAttr(Value.getValue(), AtLoc,
                                                 AttrRange,
                                                 /*Implicit=*/false));
    break;
  }

  case DAK_Available: {
    if (!consumeIf(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }

    // platform:
    //   *
    //   identifier
    if (!Tok.is(tok::identifier) &&
        !(Tok.isAnyOperator() && Tok.getText() == "*")) {
      if (Tok.is(tok::code_complete) && CodeCompletion) {
        CodeCompletion->completeDeclAttrParam(DAK_Available, 0);
        consumeToken(tok::code_complete);
      }
      diagnose(Tok.getLoc(), diag::attr_availability_platform, AttrName)
        .highlight(SourceRange(Tok.getLoc()));
      consumeIf(tok::r_paren);
      return false;
    }

    // Delay processing of platform until later, after we have
    // parsed more of the attribute.
    StringRef Platform = Tok.getText();

    if (Platform != "*" &&
        peekToken().isAny(tok::integer_literal, tok::floating_literal)) {
      // We have the short form of available: @available(iOS 8.0.1, *)
      SmallVector<AvailabilitySpec *, 5> Specs;
      ParserStatus Status = parseAvailabilitySpecList(Specs);

      if (Status.isError())
        return false;

      AttrRange = SourceRange(Loc, Tok.getLoc());
      // For each platform version spec in the spec list, create an
      // implicit AvailableAttr for the platform with the introduced
      // version from the spec. For example, if we have
      //   @available(iOS 8.0, OSX 10.10, *):
      // we will synthesize:
      //  @available(iOS, introduced: 8.0)
      //  @available(OSX, introduced: 10.10)
      //
      // Similarly if we have a language version spec in the spec
      // list, create an implicit AvailableAttr with the specified
      // version as the introduced argument. For example, if we have
      //   @available(swift 3.1)
      // we will synthesize
      //   @available(swift, introduced: 3.1)

      for (auto *Spec : Specs) {
        PlatformKind Platform;
        clang::VersionTuple Version;
        SourceRange VersionRange;
        PlatformAgnosticAvailabilityKind PlatformAgnostic;

        if (auto *PlatformVersionSpec =
            dyn_cast<PlatformVersionConstraintAvailabilitySpec>(Spec)) {
          Platform = PlatformVersionSpec->getPlatform();
          Version = PlatformVersionSpec->getVersion();
          VersionRange = PlatformVersionSpec->getVersionSrcRange();
          PlatformAgnostic = PlatformAgnosticAvailabilityKind::None;

        } else if (auto *LanguageVersionSpec =
                   dyn_cast<LanguageVersionConstraintAvailabilitySpec>(Spec)) {
          Platform = PlatformKind::none;
          Version = LanguageVersionSpec->getVersion();
          VersionRange = LanguageVersionSpec->getVersionSrcRange();
          PlatformAgnostic =
            PlatformAgnosticAvailabilityKind::SwiftVersionSpecific;

        } else {
          continue;
        }

        Attributes.add(new (Context)
                       AvailableAttr(AtLoc, AttrRange,
                                     Platform,
                                     /*Message=*/StringRef(),
                                     /*Rename=*/StringRef(),
                                     /*Introduced=*/Version,
                                     /*IntroducedRange=*/VersionRange,
                                     /*Deprecated=*/clang::VersionTuple(),
                                     /*DeprecatedRange=*/SourceRange(),
                                     /*Obsoleted=*/clang::VersionTuple(),
                                     /*ObsoletedRange=*/SourceRange(),
                                     PlatformAgnostic,
                                     /*Implicit=*/true));
      }

      if (!consumeIf(tok::r_paren)) {
        diagnose(Tok.getLoc(), diag::attr_expected_rparen, AttrName,
                 DeclAttribute::isDeclModifier(DK));
        return false;
      }

      break;
    }

    consumeToken();

    StringRef Message, Renamed;
    clang::VersionTuple Introduced, Deprecated, Obsoleted;
    SourceRange IntroducedRange, DeprecatedRange, ObsoletedRange;
    auto PlatformAgnostic = PlatformAgnosticAvailabilityKind::None;
    bool AnyAnnotations = false;
    int ParamIndex = 0;

    while (consumeIf(tok::comma)) {
      AnyAnnotations = true;
      StringRef ArgumentKindStr = Tok.getText();
      ParamIndex ++;

      enum {
        IsMessage, IsRenamed,
        IsIntroduced, IsDeprecated, IsObsoleted,
        IsUnavailable,
        IsInvalid
      } ArgumentKind = IsInvalid;

      if (Tok.is(tok::identifier)) {
        ArgumentKind =
          llvm::StringSwitch<decltype(ArgumentKind)>(ArgumentKindStr)
            .Case("message", IsMessage)
            .Case("renamed", IsRenamed)
            .Case("introduced", IsIntroduced)
            .Case("deprecated", IsDeprecated)
            .Case("obsoleted", IsObsoleted)
            .Case("unavailable", IsUnavailable)
            .Default(IsInvalid);
      }

      if (ArgumentKind == IsInvalid) {
        DiscardAttribute = true;
        diagnose(Tok.getLoc(), diag::attr_availability_expected_option,
                 AttrName)
          .highlight(SourceRange(Tok.getLoc()));
        if (Tok.is(tok::code_complete) && CodeCompletion) {
          CodeCompletion->completeDeclAttrParam(DAK_Available, ParamIndex);
          consumeToken(tok::code_complete);
        } else {
          consumeIf(tok::identifier);
        }
        break;
      }

      consumeToken();

      switch (ArgumentKind) {
      case IsMessage:
      case IsRenamed: {
        // Items with string arguments.
        if (findAttrValueDelimiter()) {
          consumeToken();
        } else {
          diagnose(Tok, diag::attr_availability_expected_equal,
                   AttrName, ArgumentKindStr);
          DiscardAttribute = true;
          if (peekToken().isAny(tok::r_paren, tok::comma))
            consumeToken();
          continue;
        }

        if (!Tok.is(tok::string_literal)) {
          diagnose(Loc, diag::attr_expected_string_literal, AttrName);
          DiscardAttribute = true;
          if (peekToken().isAny(tok::r_paren, tok::comma))
            consumeToken();
          continue;
        }

        auto Value =
          getStringLiteralIfNotInterpolated(*this, Loc, Tok, ArgumentKindStr);
        consumeToken();
        if (!Value) {
          DiscardAttribute = true;
          continue;
        }

        if (ArgumentKind == IsMessage) {
          Message = Value.getValue();
        } else {
          ParsedDeclName parsedName = parseDeclName(Value.getValue());
          if (!parsedName) {
            diagnose(Loc, diag::attr_availability_invalid_renamed, AttrName);
            DiscardAttribute = true;
            continue;
          }
          Renamed = Value.getValue();
        }
        break;
      }

      case IsDeprecated:
        if (!findAttrValueDelimiter()) {
          if (PlatformAgnostic != PlatformAgnosticAvailabilityKind::None) {
            diagnose(Tok, diag::attr_availability_unavailable_deprecated,
                     AttrName);
          }

          PlatformAgnostic = PlatformAgnosticAvailabilityKind::Deprecated;
          break;
        }
        LLVM_FALLTHROUGH;

      case IsIntroduced:
      case IsObsoleted: {
        // Items with version arguments.
        if (findAttrValueDelimiter()) {
          consumeToken();
        } else {
          diagnose(Tok, diag::attr_availability_expected_equal,
                   AttrName, ArgumentKindStr);
          DiscardAttribute = true;
          if (peekToken().isAny(tok::r_paren, tok::comma))
            consumeToken();
          continue;
        }

        auto &VersionArg = (ArgumentKind == IsIntroduced) ? Introduced :
                           (ArgumentKind == IsDeprecated) ? Deprecated :
                                                            Obsoleted;

        auto &VersionRange = (ArgumentKind == IsIntroduced) ? IntroducedRange :
                             (ArgumentKind == IsDeprecated) ? DeprecatedRange :
                                                              ObsoletedRange;

        if (parseVersionTuple(
                VersionArg, VersionRange,
                Diagnostic(diag::attr_availability_expected_version,
                           AttrName))) {
          DiscardAttribute = true;
          if (peekToken().isAny(tok::r_paren, tok::comma))
            consumeToken();
        }

        break;
      }

      case IsUnavailable:
        if (PlatformAgnostic != PlatformAgnosticAvailabilityKind::None) {
          diagnose(Tok, diag::attr_availability_unavailable_deprecated,
                   AttrName);
        }

        PlatformAgnostic = PlatformAgnosticAvailabilityKind::Unavailable;
        break;

      case IsInvalid:
        llvm_unreachable("handled above");
      }
    }

    if (!AnyAnnotations) {
      diagnose(Tok.getLoc(), diag::attr_expected_comma, AttrName,
               DeclAttribute::isDeclModifier(DK));
      DiscardAttribute = true;
    }

    AttrRange = SourceRange(Loc, Tok.getLoc());

    if (!consumeIf(tok::r_paren)) {
      if (!DiscardAttribute) {
        diagnose(Tok.getLoc(), diag::attr_expected_rparen, AttrName,
                 DeclAttribute::isDeclModifier(DK));
      }
      return false;
    }

    if (!DiscardAttribute) {
      auto PlatformKind = platformFromString(Platform);

      // Treat 'swift' as a valid version-qualifying token, when
      // at least some versions were mentioned and no other
      // platform-agnostic availability spec has been provided.
      bool SomeVersion = (!Introduced.empty() ||
                          !Deprecated.empty() ||
                          !Obsoleted.empty());
      if (!PlatformKind.hasValue() &&
          Platform == "swift" &&
          SomeVersion &&
          PlatformAgnostic == PlatformAgnosticAvailabilityKind::None) {
        PlatformKind = PlatformKind::none;
        PlatformAgnostic =
          PlatformAgnosticAvailabilityKind::SwiftVersionSpecific;
      }

      if (PlatformKind.hasValue()) {
        Attributes.add(new (Context)
                       AvailableAttr(AtLoc, AttrRange,
                                        PlatformKind.getValue(),
                                        Message, Renamed,
                                        Introduced, IntroducedRange,
                                        Deprecated, DeprecatedRange,
                                        Obsoleted, ObsoletedRange,
                                        PlatformAgnostic,
                                        /*Implicit=*/false));
      } else {
        // Not a known platform. Just drop the attribute.
        diagnose(Loc, diag::attr_availability_unknown_platform,
                 Platform, AttrName);
        return false;
      }
    }
    break;
  }

  case DAK_ObjC: {
    // Unnamed @objc attribute.
    if (Tok.isNot(tok::l_paren)) {
      auto attr = ObjCAttr::createUnnamed(Context, AtLoc, Loc);
      Attributes.add(attr);
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
            diagnose(Tok, diag::attr_objc_missing_colon)
              .fixItInsertAfter(NameLocs.back(), ":");
            sawColon = true;
          }

          break;
        }

        // If we see another identifier or keyword, complain about
        // the missing colon and keep going.
        if (Tok.is(tok::identifier) || Tok.isKeyword()) {
          diagnose(Tok, diag::attr_objc_missing_colon)
            .fixItInsertAfter(NameLocs.back(), ":");
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
    bool Invalid = parseMatchingToken(tok::r_paren, RParenLoc,
                                      diag::attr_objc_expected_rparen,
                                      LParenLoc);

    ObjCAttr *attr;
    if (Names.empty()) {
      // When there are no names, recover as if there were no parentheses.
      if (!Invalid)
        diagnose(LParenLoc, diag::attr_objc_empty_name);
      attr = ObjCAttr::createUnnamed(Context, AtLoc, Loc);
    } else if (!sawColon) {
      // When we didn't see a colon, this is a nullary name.
      assert(Names.size() == 1 && "Forgot to set sawColon?");
      attr = ObjCAttr::createNullary(Context, AtLoc, Loc, LParenLoc,
                                     NameLocs.front(), Names.front(),
                                     RParenLoc);
    } else {
      // When we did see a colon, this is a selector.
      attr = ObjCAttr::createSelector(Context, AtLoc, Loc, LParenLoc,
                                      NameLocs, Names, RParenLoc);
    }
    Attributes.add(attr);
    break;
  }

  case DAK_Specialize: {
    if (Tok.isNot(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return false;
    }
    SpecializeAttr *Attr;
    if (!parseSpecializeAttribute(tok::r_paren, AtLoc, Loc, Attr))
      return false;

    Attributes.add(Attr);
    break;
    }
  }

  if (DuplicateAttribute) {
    diagnose(Loc, diag::duplicate_attribute, DeclAttribute::isDeclModifier(DK))
      .highlight(AttrRange);
    diagnose(DuplicateAttribute->getLocation(), diag::previous_attribute, DeclAttribute::isDeclModifier(DK))
      .highlight(DuplicateAttribute->getRange());
  }

    // If this is a decl modifier spelled with an @, emit an error and remove it
  // with a fixit.
  if (AtLoc.isValid() && DeclAttribute::isDeclModifier(DK))
    diagnose(AtLoc, diag::cskeyword_not_attribute, AttrName).fixItRemove(AtLoc);
  
  return false;
}

bool Parser::parseVersionTuple(clang::VersionTuple &Version,
                               SourceRange &Range,
                               const Diagnostic &D) {
  // A version number is either an integer (8), a float (8.1), or a
  // float followed by a dot and an integer (8.1.0).
  if (!Tok.isAny(tok::integer_literal, tok::floating_literal)) {
    diagnose(Tok, D);
    return true;
  }

  SourceLoc StartLoc = Tok.getLoc();
  
  if (Tok.is(tok::integer_literal)) {
    unsigned major = 0;
    if (Tok.getText().getAsInteger(10, major)) {
      // Maybe the literal was in hex. Reject that.
      diagnose(Tok, D);
      consumeToken();
      return true;
    }
    Version = clang::VersionTuple(major);
    Range = SourceRange(StartLoc, Tok.getLoc());
    consumeToken();
    return false;
  }

  unsigned major = 0, minor = 0;
  StringRef majorPart, minorPart;
  std::tie(majorPart, minorPart) = Tok.getText().split('.');
  if (majorPart.getAsInteger(10, major) || minorPart.getAsInteger(10, minor)) {
    // Reject things like 0.1e5 and hex literals.
    diagnose(Tok, D);
    consumeToken();
    return true;
  }

  Range = SourceRange(StartLoc, Tok.getLoc());
  consumeToken();
  
  if (consumeIf(tok::period)) {
    unsigned micro = 0;
    if (!Tok.is(tok::integer_literal) ||
        Tok.getText().getAsInteger(10, micro)) {
      // Reject things like 0.1e5 and hex literals.
      diagnose(Tok, D);
      if (Tok.is(tok::integer_literal) ||
          peekToken().isAny(tok::r_paren, tok::comma))
        consumeToken();
      return true;
    }
    
    Range = SourceRange(StartLoc, Tok.getLoc());
    consumeToken();
    
    Version = clang::VersionTuple(major, minor, micro);
  } else {
    Version = clang::VersionTuple(major, minor);
  }

  return false;
}

/// \verbatim
///   attribute:
///     '_silgen_name' '(' identifier ')'
///     'semantics' '(' identifier ')'
///     'infix' '=' numeric_constant
///     'unary'
///     'stdlib'
///     'weak'
///     'inout'
///     'unowned'
///     'unowned' '(' 'safe' ')'
///     'unowned' '(' 'unsafe' ')'
///     'noreturn'
///     'optional'
///     'mutating'
///     ( 'private' | 'internal' | 'public' )
///     ( 'private' | 'internal' | 'public' ) '(' 'set' ')'
///     'requires_stored_property_inits'
/// \endverbatim
///
/// Note that various attributes (like mutating, weak, and unowned) are parsed
/// but rejected since they have context-sensitive keywords.
///
bool Parser::parseDeclAttribute(DeclAttributes &Attributes, SourceLoc AtLoc) {
  // If this not an identifier, the attribute is malformed.
  if (Tok.isNot(tok::identifier) &&
      Tok.isNot(tok::kw_in) &&
      Tok.isNot(tok::kw_inout)) {
    diagnose(Tok, diag::expected_attribute_name);
    return true;
  }

  // If the attribute follows the new representation, switch
  // over to the alternate parsing path.
  DeclAttrKind DK = DeclAttribute::getAttrKindFromString(Tok.getText());

  if (DK == DAK_Count && Tok.getText() == "availability") {
    // We renamed @availability to @available, so if we see the former,
    // treat it as the latter and emit a Fix-It.
    DK = DAK_Available;
    diagnose(Tok, diag::attr_availability_renamed)
        .fixItReplace(Tok.getLoc(), "available");
  }


  if (DK == DAK_Count && Tok.getText() == "warn_unused_result") {
    // The behavior created by @warn_unused_result is now the default. Emit a
    // Fix-It to remove.
    SourceLoc attrLoc = consumeToken();

    // @warn_unused_result with no arguments.
    if (Tok.isNot(tok::l_paren)) {
      diagnose(AtLoc, diag::attr_warn_unused_result_removed)
        .fixItRemove(SourceRange(AtLoc, attrLoc));

      return false;
    }

    // @warn_unused_result with arguments.
    SourceLoc lParenLoc = consumeToken();
    skipUntil(tok::r_paren);

    // Parse the closing ')'.
    SourceLoc rParenLoc;
    if (Tok.isNot(tok::r_paren)) {
      parseMatchingToken(tok::r_paren, rParenLoc,
                         diag::attr_warn_unused_result_expected_rparen,
                         lParenLoc);
    }
    if (Tok.is(tok::r_paren)) {
      rParenLoc = consumeToken();
    }

    diagnose(AtLoc, diag::attr_warn_unused_result_removed)
      .fixItRemove(SourceRange(AtLoc, rParenLoc));

    return false;
  }

  if (DK != DAK_Count && !DeclAttribute::shouldBeRejectedByParser(DK))
    return parseNewDeclAttribute(Attributes, AtLoc, DK);

  if (TypeAttributes::getAttrKindFromString(Tok.getText()) != TAK_Count)
    diagnose(Tok, diag::type_attribute_applied_to_decl);
  else
    diagnose(Tok, diag::unknown_attribute, Tok.getText());

  // Recover by eating @foo(...) when foo is not known.
  consumeToken();
  if (Tok.is(tok::l_paren))
    skipSingle();

  return true;
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
  if (Tok.isNot(tok::identifier) &&
      // These are keywords that we accept as attribute names.
      Tok.isNot(tok::kw_in) && Tok.isNot(tok::kw_inout)) {
    if (!justChecking)
      diagnose(Tok, diag::expected_attribute_name);
    return true;
  }
  
  // Determine which attribute it is, and diagnose it if unknown.
  TypeAttrKind attr = TypeAttributes::getAttrKindFromString(Tok.getText());

  if (attr == TAK_Count) {
    if (justChecking) return true;

    auto declAttrID = DeclAttribute::getAttrKindFromString(Tok.getText());
    if (declAttrID == DAK_Count) {
      // Not a decl or type attribute.
      diagnose(Tok, diag::unknown_attribute, Tok.getText());
    } else {
      // Otherwise this is a valid decl attribute so they should have put it on
      // the decl instead of the type.

      // If this is the first attribute, and if we are on a simple decl, emit a
      // fixit to move the attribute.  Otherwise, we don't have the location of
      // the @ sign, or we don't have confidence that the fixit will be right.
      if (!Attributes.empty() || StructureMarkers.empty() ||
          StructureMarkers.back().Kind != StructureMarkerKind::Declaration ||
          StructureMarkers.back().Loc.isInvalid() ||
          peekToken().is(tok::equal)) {
        diagnose(Tok, diag::decl_attribute_applied_to_type);
      } else {
        // Otherwise, this is the first type attribute and we know where the
        // declaration is.  Emit the same diagnostic, but include a fixit to
        // move the attribute.  Unfortunately, we don't have enough info to add
        // the attribute to DeclAttributes.
        diagnose(Tok, diag::decl_attribute_applied_to_type)
          .fixItRemove(SourceRange(Attributes.AtLoc, Tok.getLoc()))
          .fixItInsert(StructureMarkers.back().Loc,
                       "@" + Tok.getText().str()+" ");
      }
    }
    
    // Recover by eating @foo(...) when foo is not known.
    consumeToken();
    if (Tok.is(tok::l_paren) && getEndOfPreviousLoc() == Tok.getLoc()) {
      ParserPosition LParenPosition = getParserPosition();
      skipSingle();
      // If we found '->', or 'throws' after paren, it's likely a parameter
      // of function type.
      if (Tok.isAny(tok::arrow, tok::kw_throws, tok::kw_rethrows,
                    tok::kw_throw))
        backtrackToPosition(LParenPosition);
    }
    return true;
  }
  
  // Ok, it is a valid attribute, eat it, and then process it.
  StringRef Text = Tok.getText();
  SourceLoc Loc = consumeToken();

  bool isAutoclosureEscaping = false;
  SourceRange autoclosureEscapingParenRange;
  StringRef conventionName;

  // Handle @autoclosure(escaping)
  if (attr == TAK_autoclosure) {
    // We need to do a bit of lookahead here to make sure we parse a (weird)
    // type like: "@autoclosure (escaping) -> Int" correctly (escaping is the
    // name of a type here).  We also want to support the case where the
    // function type coming up is a typealias, e.g. "@autoclosure (escaping) T".
    if (Tok.is(tok::l_paren) && peekToken().getText() == "escaping") {
      Parser::BacktrackingScope Backtrack(*this);
      consumeToken(tok::l_paren);
      consumeToken(tok::identifier);
      isAutoclosureEscaping =
        Tok.is(tok::r_paren) && peekToken().isNot(tok::arrow);
    }

    if (isAutoclosureEscaping) {
      autoclosureEscapingParenRange.Start = consumeToken(tok::l_paren);
      consumeToken(tok::identifier);
      autoclosureEscapingParenRange.End = consumeToken(tok::r_paren);
    }
  } else if (attr == TAK_convention) {
    SourceLoc LPLoc;
    if (!consumeIfNotAtStartOfLine(tok::l_paren)) {
      if (!justChecking)
        diagnose(Tok, diag::convention_attribute_expected_lparen);
      return true;
    }

    if (Tok.isNot(tok::identifier)) {
      if (!justChecking)
        diagnose(Tok, diag::convention_attribute_expected_name);
      return true;
    }

    conventionName = Tok.getText();
    consumeToken(tok::identifier);

    // Parse the ')'.  We can't use parseMatchingToken if we're in
    // just-checking mode.
    if (justChecking && Tok.isNot(tok::r_paren))
      return true;

    SourceLoc RPLoc;
    parseMatchingToken(tok::r_paren, RPLoc,
                       diag::convention_attribute_expected_rparen,
                       LPLoc);
  }


  // In just-checking mode, we only need to consume the tokens, and we don't
  // want to do any other analysis.
  if (justChecking)
    return false;

  // Diagnose duplicated attributes.
  if (Attributes.has(attr)) {
    diagnose(Loc, diag::duplicate_attribute, /*isModifier=*/false);
    return false;
  }

  // Handle any attribute-specific processing logic.
  switch (attr) {
  default: break;
  case TAK_autoclosure:
    // Handle @autoclosure(escaping)
    if (isAutoclosureEscaping) {
      // @noescape @autoclosure(escaping) makes no sense.
      if (Attributes.has(TAK_noescape)) {
        diagnose(Loc, diag::attr_noescape_conflicts_escaping_autoclosure);
      } else {
        diagnose(Loc, diag::attr_autoclosure_escaping_deprecated)
            .fixItReplace(autoclosureEscapingParenRange, " @escaping ");
      }
      Attributes.setAttr(TAK_escaping, Loc);
    } else if (Attributes.has(TAK_noescape)) {
      diagnose(Loc, diag::attr_noescape_implied_by_autoclosure);
    }
    break;

  case TAK_noescape:
    // You can't specify @noescape and @escaping together.
    if (Attributes.has(TAK_escaping)) {
      diagnose(Loc, diag::attr_escaping_conflicts_noescape);
      return false;
    }

    // @noescape after @autoclosure is redundant.
    if (Attributes.has(TAK_autoclosure)) {
      diagnose(Loc, diag::attr_noescape_implied_by_autoclosure);
    }

    // @noescape is deprecated and no longer used
    diagnose(Loc, diag::attr_noescape_deprecated)
      .fixItRemove({Attributes.AtLoc,Loc});

    break;
  case TAK_escaping:
    // You can't specify @noescape and @escaping together.
    if (Attributes.has(TAK_noescape)) {
      diagnose(Loc, diag::attr_escaping_conflicts_noescape);
      return false;
    }
    break;
  case TAK_out:
  case TAK_in:
  case TAK_owned:
  case TAK_unowned_inner_pointer:
  case TAK_guaranteed:
  case TAK_autoreleased:
  case TAK_callee_owned:
  case TAK_callee_guaranteed:
  case TAK_objc_metatype:
    if (!isInSILMode()) {
      diagnose(Loc, diag::only_allowed_in_sil, Text);
      return false;
    }
    break;
    
  // Ownership attributes.
  case TAK_sil_weak:
  case TAK_sil_unowned:
    if (!isInSILMode()) {
      diagnose(Loc, diag::only_allowed_in_sil, Text);
      return false;
    }
      
    if (Attributes.hasOwnership()) {
      diagnose(Loc, diag::duplicate_attribute, /*isModifier*/false);
      return false;
    }
    break;

  // 'inout' attribute.
  case TAK_inout:
    if (!isInSILMode()) {
      diagnose(Loc, diag::inout_not_attribute);
      return false;
    }
    break;
      
  case TAK_opened: {
    if (!isInSILMode()) {
      diagnose(Loc, diag::only_allowed_in_sil, "opened");
      return false;
    }

    // Parse the opened existential ID string in parens
    SourceLoc beginLoc = Tok.getLoc(), idLoc, endLoc;
    if (consumeIfNotAtStartOfLine(tok::l_paren)) {
      if (Tok.is(tok::string_literal)) {
        UUID openedID;
        idLoc = Tok.getLoc();
        auto literalText = Tok.getText().slice(1, Tok.getText().size() - 1);
        llvm::SmallString<UUID::StringBufferSize> text(literalText);
        if (auto openedID = UUID::fromString(text.c_str())) {
          Attributes.OpenedID = openedID;
        } else {
          diagnose(Tok, diag::opened_attribute_id_value);
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

    break;
  }

  // Convention attribute.
  case TAK_convention:
    Attributes.convention = conventionName;
    break;
  }

  Attributes.setAttr(attr, Loc);
  return false;
}

/// \verbatim
///   attribute-list:
///     /*empty*/
///     attribute-list-clause attribute-list
///   attribute-list-clause:
///     '@' attribute
/// \endverbatim
bool Parser::parseDeclAttributeList(DeclAttributes &Attributes,
                                    bool &FoundCCToken) {
  FoundCCToken = false;
  while (Tok.is(tok::at_sign)) {
    if (peekToken().is(tok::code_complete)) {
      consumeToken(tok::at_sign);
      consumeToken(tok::code_complete);
      FoundCCToken = true;
      continue;
    }
    SourceLoc AtLoc = consumeToken();
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
///     'inout' attribute-list-clause attribute-list
///   attribute-list-clause:
///     '@' attribute
///     '@' attribute attribute-list-clause
/// \endverbatim
bool Parser::parseTypeAttributeListPresent(SourceLoc &InOutLoc,
                                           TypeAttributes &Attributes) {
  while (Tok.is(tok::kw_inout)) {
    if (InOutLoc.isValid())
      diagnose(Tok, diag::parameter_inout_var_let_repeated)
        .fixItRemove(InOutLoc);
    InOutLoc = consumeToken();
  }

  while (Tok.is(tok::at_sign)) {
    if (Attributes.AtLoc.isInvalid())
      Attributes.AtLoc = Tok.getLoc();
    consumeToken();
    if (parseTypeAttribute(Attributes))
      return true;
  }
  
  return false;
}

static bool isStartOfOperatorDecl(const Token &Tok, const Token &Tok2) {
  return Tok.isContextualKeyword("operator") &&
         (Tok2.isContextualKeyword("prefix") ||
          Tok2.isContextualKeyword("postfix") ||
          Tok2.isContextualKeyword("infix"));
}

/// \brief Diagnose issues with fixity attributes, if any.
static void diagnoseOperatorFixityAttributes(Parser &P,
                                             DeclAttributes &Attrs,
                                             const Decl *D) {
  auto isFixityAttr = [](DeclAttribute *attr){
    DeclAttrKind kind = attr->getKind();
    return attr->isValid() && (kind == DAK_Prefix ||
                               kind == DAK_Infix ||
                               kind == DAK_Postfix);
  };
  
  SmallVector<DeclAttribute *, 3> fixityAttrs;
  std::copy_if(Attrs.begin(), Attrs.end(),
               std::back_inserter(fixityAttrs), isFixityAttr);
  std::reverse(fixityAttrs.begin(), fixityAttrs.end());
  
  for (auto it = fixityAttrs.begin(); it != fixityAttrs.end(); ++it) {
    if (it != fixityAttrs.begin()) {
      auto *attr = *it;
      P.diagnose(attr->getLocation(), diag::mutually_exclusive_attrs,
                 attr->getAttrName(), fixityAttrs.front()->getAttrName(),
                 attr->isDeclModifier())
      .fixItRemove(attr->getRange());
      attr->setInvalid();
    }
  }
  
  // Operator declarations must specify a fixity.
  if (auto *OD = dyn_cast<OperatorDecl>(D)) {
    if (fixityAttrs.empty()) {
      P.diagnose(OD->getOperatorLoc(), diag::operator_decl_no_fixity);
    }
  }
  // Infix operator is only allowed on operator declarations, not on func.
  else if (isa<FuncDecl>(D)) {
    if (auto *attr = Attrs.getAttribute<InfixAttr>()) {
      P.diagnose(attr->getLocation(), diag::invalid_infix_on_func)
        .fixItRemove(attr->getLocation());
      attr->setInvalid();
    }
  } else {
    llvm_unreachable("unexpected decl kind?");
  }
}

bool swift::isKeywordPossibleDeclStart(const Token &Tok) {
  switch (Tok.getKind()) {
  case tok::at_sign:
  case tok::kw_associatedtype:
  case tok::kw_case:
  case tok::kw_class:
  case tok::kw_deinit:
  case tok::kw_enum:
  case tok::kw_extension:
  case tok::kw_fileprivate:
  case tok::kw_func:
  case tok::kw_import:
  case tok::kw_init:
  case tok::kw_internal:
  case tok::kw_let:
  case tok::kw_operator:
  case tok::kw_precedencegroup:
  case tok::kw_private:
  case tok::kw_protocol:
  case tok::kw_public:
  case tok::kw_static:
  case tok::kw_struct:
  case tok::kw_subscript:
  case tok::kw_typealias:
  case tok::kw_var:
  case tok::pound_if:
  case tok::identifier:
  case tok::pound_sourceLocation:
    return true;
  case tok::pound_line:
    // #line at the start of the line is a directive, but it's deprecated.
    // #line within a line is an expression.
    return Tok.isAtStartOfLine();

  case tok::kw_try:
    // 'try' is not a valid way to start a decl, but we special-case 'try let'
    // and 'try var' for better recovery.
    return true;
  default:
    return false;
  }
}

/// Given a current token of 'unowned', check to see if it is followed by a
/// "(safe)" or "(unsafe)" specifier.
static bool isParenthesizedUnowned(Parser &P) {
  assert(P.Tok.getText() == "unowned" && P.peekToken().is(tok::l_paren) &&
         "Invariant violated");
  
  // Look ahead to parse the parenthesized expression.
  Parser::BacktrackingScope Backtrack(P);
  P.consumeToken(tok::identifier);
  P.consumeToken(tok::l_paren);
  return P.Tok.is(tok::identifier) && P.peekToken().is(tok::r_paren) &&
          (P.Tok.getText() == "safe" || P.Tok.getText() == "unsafe");
}

  
bool Parser::isStartOfDecl() {
  // If this is obviously not the start of a decl, then we're done.
  if (!isKeywordPossibleDeclStart(Tok)) return false;

  // When 'init' appears inside another 'init', it's likely the user wants to
  // invoke an initializer but forgets to prefix it with 'self.' or 'super.'
  // Otherwise, expect 'init' to be the start of a declaration (and complain
  // when the expectation is not fulfilled).
  if (Tok.is(tok::kw_init)) {
    return !isa<ConstructorDecl>(CurDeclContext);
  }
  
  // The protocol keyword needs more checking to reject "protocol<Int>".
  if (Tok.is(tok::kw_protocol)) {
    const Token &Tok2 = peekToken();
    return !Tok2.isAnyOperator() || !Tok2.getText().equals("<");
  }

  // The 'try' case is only for simple local recovery, so we only bother to
  // check 'let' and 'var' right now.
  if (Tok.is(tok::kw_try))
    return peekToken().isAny(tok::kw_let, tok::kw_var);
  
  // Look through attribute list, because it may be an *type* attribute list.
  if (Tok.is(tok::at_sign)) {
    BacktrackingScope backtrack(*this);
    while (consumeIf(tok::at_sign)) {
      // If not identifier or code complete token, consider '@' as an incomplete
      // attribute.
      if (Tok.isNot(tok::identifier, tok::code_complete))
        continue;
      consumeToken();
      // Eat paren after attribute name; e.g. @foo(x)
      if (consumeIf(tok::l_paren)) {
        while (Tok.isNot(tok::r_brace, tok::eof, tok::pound_endif)) {
          if (consumeIf(tok::r_paren)) break;
          skipSingle();
        }
      }
    }
    // If this attribute is the last element in the block,
    // consider it is a start of incomplete decl.
    if (Tok.isAny(tok::r_brace, tok::eof, tok::pound_endif))
      return true;

    return isStartOfDecl();
  }

  // Otherwise, the only hard case left is the identifier case.
  if (Tok.isNot(tok::identifier)) return true;

  // If this is an operator declaration, handle it.
  const Token &Tok2 = peekToken();
  if (isStartOfOperatorDecl(Tok, Tok2))
    return true;
    
  // If this can't possibly be a contextual keyword, then this identifier is
  // not interesting.  Bail out.
  if (!Tok.isContextualDeclKeyword())
    return false;
      
  // If it might be, we do some more digging.

  // If this is 'unowned', check to see if it is valid.
  if (Tok.getText() == "unowned" && Tok2.is(tok::l_paren) &&
      isParenthesizedUnowned(*this)) {
    Parser::BacktrackingScope Backtrack(*this);
    consumeToken(tok::identifier);
    consumeToken(tok::l_paren);
    consumeToken(tok::identifier);
    consumeToken(tok::r_paren);
    return isStartOfDecl();
  }

  // If the next token is obviously not the start of a decl, bail early.
  if (!isKeywordPossibleDeclStart(Tok2))
    return false;
  
  // Otherwise, do a recursive parse.
  Parser::BacktrackingScope Backtrack(*this);
  consumeToken(tok::identifier);
  return isStartOfDecl();
}

void Parser::consumeDecl(ParserPosition BeginParserPosition,
                         ParseDeclOptions Flags,
                         bool IsTopLevel) {
  SourceLoc CurrentLoc = Tok.getLoc();
  backtrackToPosition(BeginParserPosition);
  SourceLoc BeginLoc = Tok.getLoc();
  // Consume tokens up to code completion token.
  while (Tok.isNot(tok::code_complete, tok::eof))
    consumeToken();

  // Consume the code completion token, if there is one.
  consumeIf(tok::code_complete);
  SourceLoc EndLoc = DelayedDeclEnd.isValid() &&
                     SourceMgr.isBeforeInBuffer(Tok.getLoc(), DelayedDeclEnd) ?
                       DelayedDeclEnd : Tok.getLoc();
  State->delayDecl(PersistentParserState::DelayedDeclKind::Decl, Flags.toRaw(),
                   CurDeclContext, { BeginLoc, EndLoc },
                   BeginParserPosition.PreviousLoc);

  while (SourceMgr.isBeforeInBuffer(Tok.getLoc(), CurrentLoc))
    consumeToken();

  if (IsTopLevel) {
    // Skip the rest of the file to prevent the parser from constructing the
    // AST for it.  Forward references are not allowed at the top level.
    while (Tok.isNot(tok::eof))
      consumeToken();
  }
}

void Parser::setLocalDiscriminator(ValueDecl *D) {
  // If we're not in a local context, this is unnecessary.
  if (!CurLocalContext || !D->getDeclContext()->isLocalContext())
    return;

  if (auto TD = dyn_cast<TypeDecl>(D))
    if (!getScopeInfo().isInactiveConfigBlock())
      SF.LocalTypeDecls.insert(TD);

  Identifier name = D->getName();
  unsigned discriminator = CurLocalContext->claimNextNamedDiscriminator(name);
  D->setLocalDiscriminator(discriminator);
}

void Parser::delayParseFromBeginningToHere(ParserPosition BeginParserPosition,
                                           ParseDeclOptions Flags) {
  auto CurLoc = Tok.getLoc();
  backtrackToPosition(BeginParserPosition);
  SourceLoc BeginLoc = Tok.getLoc();
  SourceLoc EndLoc = CurLoc;
  State->delayDecl(PersistentParserState::DelayedDeclKind::Decl,
                   Flags.toRaw(),
                   CurDeclContext, {BeginLoc, EndLoc},
                   BeginParserPosition.PreviousLoc);

  while (Tok.isNot(tok::eof))
    consumeToken();
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
ParserResult<Decl>
Parser::parseDecl(ParseDeclOptions Flags,
                  llvm::function_ref<void(Decl*)> Handler) {
  if (Tok.isAny(tok::pound_sourceLocation, tok::pound_line)) {
    auto LineDirectiveStatus = parseLineDirective(Tok.is(tok::pound_line));
    if (LineDirectiveStatus.isError())
      return LineDirectiveStatus;
    // If success, go on. line directive never produce decls.
  }

  if (Tok.is(tok::pound_if)) {
    auto IfConfigResult = parseDeclIfConfig(Flags);
    if (auto ICD = IfConfigResult.getPtrOrNull()) {
      // The IfConfigDecl is ahead of its members in source order.
      Handler(ICD);
      // Copy the active members into the entries list.
      for (auto activeMember : ICD->getActiveMembers()) {
        Handler(activeMember);
      }
    }
    return IfConfigResult;
  }

  ParserPosition BeginParserPosition;
  if (isCodeCompletionFirstPass())
    BeginParserPosition = getParserPosition();

  SourceLoc tryLoc;
  (void)consumeIf(tok::kw_try, tryLoc);

  // Note that we're parsing a declaration.
  StructureMarkerRAII ParsingDecl(*this, Tok.getLoc(),
                                  StructureMarkerKind::Declaration);

  DeclAttributes Attributes;
  if (Tok.hasComment())
    Attributes.add(new (Context) RawDocCommentAttr(Tok.getCommentRange()));
  bool FoundCCTokenInAttr;
  parseDeclAttributeList(Attributes, FoundCCTokenInAttr);

  // Keep track of where and whether we see a contextual keyword on the decl.
  SourceLoc StaticLoc;
  StaticSpellingKind StaticSpelling = StaticSpellingKind::None;
  ParserResult<Decl> DeclResult;

  while (1) {
    // Save the original token, in case code-completion needs it.
    auto OrigTok = Tok;
    bool MayNeedOverrideCompletion = false;

    switch (Tok.getKind()) {
    // Modifiers
    case tok::kw_static:
      if (StaticLoc.isValid()) {
        diagnose(Tok, diag::decl_already_static,
                 StaticSpellingKind::KeywordStatic)
        .highlight(StaticLoc)
        .fixItRemove(Tok.getLoc());
      } else {
        StaticLoc = Tok.getLoc();
        StaticSpelling = StaticSpellingKind::KeywordStatic;
      }
      consumeToken(tok::kw_static);
      continue;

    // 'class' is a modifier on func, but is also a top-level decl.
    case tok::kw_class: {
      SourceLoc ClassLoc = consumeToken(tok::kw_class);

      // If 'class' is a modifier on another decl kind, like var or func,
      // then treat it as a modifier.
      if (isStartOfDecl()) {
        if (StaticLoc.isValid()) {
          diagnose(Tok, diag::decl_already_static,
                   StaticSpellingKind::KeywordClass)
            .highlight(StaticLoc).fixItRemove(ClassLoc);
        } else {
          StaticLoc = ClassLoc;
          StaticSpelling = StaticSpellingKind::KeywordClass;
        }
        continue;
      }

      // Otherwise this is the start of a class declaration.
      DeclResult = parseDeclClass(ClassLoc, Flags, Attributes);
      break;
    }

    case tok::kw_private:
    case tok::kw_fileprivate:
    case tok::kw_internal:
    case tok::kw_public:
      // We still model these specifiers as attributes.
      parseNewDeclAttribute(Attributes, /*AtLoc=*/{}, DAK_Accessibility);
      continue;

    // Context sensitive keywords.
    case tok::identifier:
      // FIXME: This is ridiculous, this all needs to be sucked into the
      // declparsing goop.
      if (Tok.isContextualKeyword("open")) {
        parseNewDeclAttribute(Attributes, /*AtLoc=*/{}, DAK_Accessibility);
        continue;
      }
      if (Tok.isContextualKeyword("weak") ||
          Tok.isContextualKeyword("unowned")) {
        parseNewDeclAttribute(Attributes, /*AtLoc=*/{}, DAK_Ownership);
        continue;
      }
      if (Tok.isContextualKeyword("optional")) {
        parseNewDeclAttribute(Attributes, /*AtLoc=*/{}, DAK_Optional);
        continue;
      }
      if (Tok.isContextualKeyword("required")) {
        parseNewDeclAttribute(Attributes, /*AtLoc=*/{}, DAK_Required);
        continue;
      }
      if (Tok.isContextualKeyword("lazy")) {
        parseNewDeclAttribute(Attributes, /*AtLoc=*/{}, DAK_Lazy);
        continue;
      }
      if (Tok.isContextualKeyword("final")) {
        parseNewDeclAttribute(Attributes, /*AtLoc=*/{}, DAK_Final);
        continue;
      }
      if (Tok.isContextualKeyword("dynamic")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Dynamic);
        continue;
      }
      if (Tok.isContextualKeyword("prefix")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Prefix);
        continue;
      }
      if (Tok.isContextualKeyword("postfix")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Postfix);
        continue;
      }
      if (Tok.isContextualKeyword("indirect")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Indirect);
        continue;
      }
      if (Tok.isContextualKeyword("infix")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Infix);
        continue;
      }
      if (Tok.isContextualKeyword("override")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Override);
        continue;
      }
      if (Tok.isContextualKeyword("mutating")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Mutating);
        continue;
      }
      if (Tok.isContextualKeyword("nonmutating")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_NonMutating);
        continue;
      }
      if (Tok.isContextualKeyword("convenience")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Convenience);
        continue;
      }

      // Otherwise this is not a context-sensitive keyword.
      LLVM_FALLTHROUGH;

    case tok::pound_if:
    case tok::pound_sourceLocation:
    case tok::pound_line:
      // We see some attributes right before these pounds.
      // TODO: Emit dedicated errors for them.
      LLVM_FALLTHROUGH;

    // Obvious nonsense.
    default:
      if (FoundCCTokenInAttr) {
        if (!CodeCompletion) {
          delayParseFromBeginningToHere(BeginParserPosition, Flags);
        } else {
          CodeCompletion->completeDeclAttrKeyword(nullptr, isInSILMode(),
                                                  false);
        }
      }

      diagnose(Tok, diag::expected_decl);

      if (CurDeclContext) {
        if (auto nominal = dyn_cast<NominalTypeDecl>(CurDeclContext)) {
          diagnose(nominal->getLoc(), diag::note_in_decl_extension, false,
                   nominal->getName());
        } else if (auto extension = dyn_cast<ExtensionDecl>(CurDeclContext)) {
          if (auto repr = extension->getExtendedTypeLoc().getTypeRepr()) {
            if (auto idRepr = dyn_cast<IdentTypeRepr>(repr)) {
              diagnose(extension->getLoc(), diag::note_in_decl_extension, true,
                idRepr->getComponentRange().front()->getIdentifier());
            }
          }
        }
      }
      return makeParserErrorResult<Decl>();

    case tok::unknown:
      consumeToken(tok::unknown);
      continue;

    // Unambiguous top level decls.
    case tok::kw_import:
      DeclResult = parseDeclImport(Flags, Attributes);
      break;
    case tok::kw_extension:
      DeclResult = parseDeclExtension(Flags, Attributes);
      break;
    case tok::kw_let:
    case tok::kw_var: {
      llvm::SmallVector<Decl *, 4> Entries;
      DeclResult = parseDeclVar(Flags, Attributes, Entries, StaticLoc,
                                StaticSpelling, tryLoc);
      StaticLoc = SourceLoc();   // we handled static if present.
      MayNeedOverrideCompletion = true;
      std::for_each(Entries.begin(), Entries.end(), Handler);
      if (auto *D = DeclResult.getPtrOrNull())
        markWasHandled(D);
      break;
    }
    case tok::kw_typealias:
      DeclResult = parseDeclTypeAlias(Flags, Attributes);
      MayNeedOverrideCompletion = true;
      break;
    case tok::kw_associatedtype:
      DeclResult = parseDeclAssociatedType(Flags, Attributes);
      break;
    case tok::kw_enum:
      DeclResult = parseDeclEnum(Flags, Attributes);
      break;
    case tok::kw_case: {
      llvm::SmallVector<Decl *, 4> Entries;
      DeclResult = parseDeclEnumCase(Flags, Attributes, Entries);
      std::for_each(Entries.begin(), Entries.end(), Handler);
      if (auto *D = DeclResult.getPtrOrNull())
        markWasHandled(D);
      break;
    }
    case tok::kw_struct:
      DeclResult = parseDeclStruct(Flags, Attributes);
      break;
    case tok::kw_init:
      DeclResult = parseDeclInit(Flags, Attributes);
      break;
    case tok::kw_deinit:
      DeclResult = parseDeclDeinit(Flags, Attributes);
      break;
    case tok::kw_operator:
      DeclResult = parseDeclOperator(Flags, Attributes);
      break;
    case tok::kw_precedencegroup:
      DeclResult = parseDeclPrecedenceGroup(Flags, Attributes);
      break;
    case tok::kw_protocol:
      DeclResult = parseDeclProtocol(Flags, Attributes);
      break;

    case tok::kw_func:
      DeclResult = parseDeclFunc(StaticLoc, StaticSpelling, Flags, Attributes);
      StaticLoc = SourceLoc();   // we handled static if present.
      MayNeedOverrideCompletion = true;
      break;

    case tok::kw_subscript: {
      if (StaticLoc.isValid()) {
        diagnose(Tok, diag::subscript_static, StaticSpelling)
          .fixItRemove(SourceRange(StaticLoc));
        StaticLoc = SourceLoc();
      }
      llvm::SmallVector<Decl *, 4> Entries;
      DeclResult = parseDeclSubscript(Flags, Attributes, Entries);
      std::for_each(Entries.begin(), Entries.end(), Handler);
      MayNeedOverrideCompletion = true;
      if (auto *D = DeclResult.getPtrOrNull())
        markWasHandled(D);
      break;
    }

    case tok::code_complete:
      MayNeedOverrideCompletion = true;
      DeclResult = makeParserError();
      // Handled below.
      break;
    }

    if (DeclResult.isParseError() && MayNeedOverrideCompletion &&
        Tok.is(tok::code_complete)) {
      DeclResult = makeParserCodeCompletionStatus();
      if (CodeCompletion) {
        // If we need to complete an override, collect the keywords already
        // specified so that we do not duplicate them in code completion
        // strings.
        SmallVector<StringRef, 3> Keywords;
        switch (OrigTok.getKind()) {
        case tok::kw_func:
        case tok::kw_subscript:
        case tok::kw_var:
        case tok::kw_typealias:
          Keywords.push_back(OrigTok.getText());
          break;
        default:
          // Other tokens are already accounted for.
          break;
        }
        for (auto attr : Attributes) {
          Keywords.push_back(attr->getAttrName());
        }
        CodeCompletion->completeNominalMemberBeginning(Keywords);
      }
    }

    // If we 'break' out of the switch, break out of the loop too.
    break;
  }

  if (auto SF = CurDeclContext->getParentSourceFile()) {
    if (!getScopeInfo().isInactiveConfigBlock()) {
      for (auto Attr : Attributes) {
        if (isa<ObjCAttr>(Attr) || isa<DynamicAttr>(Attr))
          SF->AttrsRequiringFoundation.insert(Attr);
      }
    }
  }

  if (FoundCCTokenInAttr) {
    if (CodeCompletion) {
      CodeCompletion->completeDeclAttrKeyword(DeclResult.getPtrOrNull(),
                                              isInSILMode(),
                                              false);
    } else {
      delayParseFromBeginningToHere(BeginParserPosition, Flags);
      return makeParserError();
    }
  }

  if (DeclResult.hasCodeCompletion() && isCodeCompletionFirstPass() &&
      !CurDeclContext->isModuleScopeContext()) {
    // Only consume non-toplevel decls.
    consumeDecl(BeginParserPosition, Flags, /*IsTopLevel=*/false);

    return makeParserError();
  }

  if (DeclResult.isNonNull()) {
    Decl *D = DeclResult.get();
    if (!declWasHandledAlready(D))
      Handler(DeclResult.get());
  }

  if (!DeclResult.isParseError()) {
    // If we parsed 'class' or 'static', but didn't handle it above, complain
    // about it.
    if (StaticLoc.isValid())
      diagnose(DeclResult.get()->getLoc(), diag::decl_not_static,
               StaticSpelling)
          .fixItRemove(SourceRange(StaticLoc));
  }

  return DeclResult;
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

  // Create a lexer that cannot go past the end state.
  Lexer LocalLex(*L, BeginParserPosition.LS, EndLexerState);

  // Temporarily swap out the parser's current lexer with our new one.
  llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

  // Rewind to the beginning of the decl.
  restoreParserPosition(BeginParserPosition);

  // Re-enter the lexical scope.
  Scope S(this, DelayedState->takeScope());
  ContextChange CC(*this, DelayedState->ParentContext);

  parseDecl(ParseDeclOptions(DelayedState->Flags), [](Decl *D) { });
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
  DebuggerContextChange DCC (*this);

  if (!CodeCompletion && !DCC.movedToTopLevel() && !(Flags & PD_AllowTopLevel)) {
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
      diagnose(Tok, diag::keyword_cant_be_identifier, Tok.getText());
      diagnose(Tok, diag::backticks_to_escape);
      return nullptr;
    }
    KindLoc = consumeToken();
  }

  std::vector<std::pair<Identifier, SourceLoc>> ImportPath;
  do {
    if (Tok.is(tok::code_complete)) {
      consumeToken();
      if (CodeCompletion) {
        CodeCompletion->completeImportDecl(ImportPath);
      }
      return makeParserCodeCompletionStatus();
    }
    ImportPath.push_back(std::make_pair(Identifier(), Tok.getLoc()));
    if (parseAnyIdentifier(ImportPath.back().first,
                           diag::expected_identifier_in_decl, "import"))
      return nullptr;
  } while (consumeIf(tok::period));

  if (Tok.is(tok::code_complete)) {
    // We omit the code completion token if it immediately follows the module
    // identifiers.
    auto BufferId = SourceMgr.getCodeCompletionBufferID();
    auto IdEndOffset = SourceMgr.getLocOffsetInBuffer(ImportPath.back().second,
      BufferId) + ImportPath.back().first.str().size();
    auto CCTokenOffset = SourceMgr.getLocOffsetInBuffer(SourceMgr.
      getCodeCompletionLoc(), BufferId);
    if (IdEndOffset == CCTokenOffset) {
      consumeToken();
    }
  }

  if (Kind != ImportKind::Module && ImportPath.size() == 1) {
    diagnose(ImportPath.front().second, diag::decl_expected_module_name);
    return nullptr;
  }

  auto *ID = ImportDecl::create(Context, CurDeclContext, ImportLoc, Kind,
                                KindLoc, ImportPath);
  ID->getAttrs() = Attributes;
  return DCC.fixupParserResult(ID);
}

/// \brief Parse an inheritance clause.
///
/// \verbatim
///   inheritance:
///      ':' inherited (',' inherited)*
///
///   inherited:
///     'class'
///     type-identifier
/// \endverbatim
ParserStatus Parser::parseInheritance(SmallVectorImpl<TypeLoc> &Inherited,
                                      SourceLoc *classRequirementLoc) {
  Scope S(this, ScopeKind::InheritanceClause);
  consumeToken(tok::colon);

  // Clear out the class requirement location.
  if (classRequirementLoc)
    *classRequirementLoc = SourceLoc();

  ParserStatus Status;
  SourceLoc prevComma;
  do {
    // Parse the 'class' keyword for a class requirement.
    if (Tok.is(tok::kw_class)) {
      // If we aren't allowed to have a class requirement here, complain.
      auto classLoc = consumeToken();
      if (!classRequirementLoc) {
        SourceLoc endLoc = Tok.is(tok::comma) ? Tok.getLoc() : classLoc;
        diagnose(classLoc, diag::invalid_class_requirement)
          .fixItRemove(SourceRange(classLoc, endLoc));
        continue;
      }

      // If we already saw a class requirement, complain.
      if (classRequirementLoc->isValid()) {
        diagnose(classLoc, diag::redundant_class_requirement)
          .highlight(*classRequirementLoc)
          .fixItRemove(SourceRange(prevComma, classLoc));
        continue;
      }

      // If the class requirement was not the first requirement, complain.
      if (!Inherited.empty()) {
        SourceLoc properLoc = Inherited[0].getSourceRange().Start;
        diagnose(classLoc, diag::late_class_requirement)
          .fixItInsert(properLoc, "class, ")
          .fixItRemove(SourceRange(prevComma, classLoc));
      }

      // Record the location of the 'class' keyword.
      *classRequirementLoc = classLoc;
      continue;
    }

    bool usesDeprecatedCompositionSyntax =
      Tok.is(tok::kw_protocol) && startsWithLess(peekToken());
    bool isAny = Tok.is(tok::kw_Any); // We allow (redundant) inheritance from Any

    auto ParsedTypeResult = parseTypeForInheritance(
        diag::expected_identifier_for_type,
        diag::expected_ident_type_in_inheritance);
    Status |= ParsedTypeResult;

    // Recover and emit nice diagnostic for composition.
    if (auto Composition = dyn_cast_or_null<CompositionTypeRepr>(
          ParsedTypeResult.getPtrOrNull())) {
      // Record the protocols inside the composition.
      Inherited.append(Composition->getTypes().begin(),
                       Composition->getTypes().end());
      // We can inherit from Any
      if (!isAny) {
        if (usesDeprecatedCompositionSyntax) {
          // Provide fixits to remove the composition, leaving the types intact.
          auto compositionRange = Composition->getCompositionRange();
          auto token = Lexer::getTokenAtLocation(SourceMgr, compositionRange.End);
          diagnose(Composition->getSourceLoc(),
                   diag::disallowed_protocol_composition)
            .highlight({Composition->getStartLoc(), compositionRange.End})
            .fixItRemove({Composition->getSourceLoc(), compositionRange.Start})
            .fixItRemove(startsWithGreater(token)
                         ? compositionRange.End
                         : SourceLoc());
        } else {
          diagnose(Composition->getStartLoc(),
                   diag::disallowed_protocol_composition)
            .highlight(Composition->getSourceRange());
          // TODO: Decompose 'A & B & C' list to 'A, B, C'
        }
      }
      continue;
    }

    // Record the type if its a single type.
    if (ParsedTypeResult.isNonNull())
      Inherited.push_back(ParsedTypeResult.get());

    // Check for a ',', which indicates that there are more protocols coming.
  } while (consumeIf(tok::comma, prevComma));

  return Status;
}

enum class TokenProperty {
  None,
  StartsWithLess,
};

static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &Loc,
                        StringRef DeclKindName, tok ResyncT1, tok ResyncT2,
                        tok ResyncT3, tok ResyncT4,
                        TokenProperty ResyncP1) {
  if (P.Tok.is(tok::identifier)) {
    Loc = P.consumeIdentifier(&Result);

    // We parsed an identifier for the declaration. If we see another
    // identifier, it might've been a single identifier that got broken by a
    // space or newline accidentally.
    if (P.Tok.isIdentifierOrUnderscore() && !P.Tok.isContextualDeclKeyword())
      P.diagnoseConsecutiveIDs(Result.str(), Loc, DeclKindName);

    // Return success anyway
    return makeParserSuccess();
  }

  P.checkForInputIncomplete();

  if (P.Tok.isKeyword()) {
    P.diagnose(P.Tok, diag::keyword_cant_be_identifier, P.Tok.getText());
    P.diagnose(P.Tok, diag::backticks_to_escape)
      .fixItReplace(P.Tok.getLoc(), "`" + P.Tok.getText().str() + "`");

    // Recover if the next token is one of the expected tokens.
    auto Next = P.peekToken();
    if (Next.isAny(ResyncT1, ResyncT2, ResyncT3, ResyncT4) ||
        (ResyncP1 != TokenProperty::None && P.startsWithLess(Next))) {
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

  P.diagnose(P.Tok, diag::expected_identifier_in_decl, DeclKindName);
  return makeParserError();
}

static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        StringRef DeclKindName, tok ResyncT1, tok ResyncT2) {
  return parseIdentifierDeclName(P, Result, L, DeclKindName, ResyncT1, ResyncT2,
                                 tok::NUM_TOKENS, tok::NUM_TOKENS,
                                 TokenProperty::None);
}

static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        StringRef DeclKindName, tok ResyncT1, tok ResyncT2,
                        tok ResyncT3, tok ResyncT4) {
  return parseIdentifierDeclName(P, Result, L, DeclKindName, ResyncT1, ResyncT2,
                                 ResyncT3, ResyncT4, TokenProperty::None);
}

static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        StringRef DeclKindName, tok ResyncT1, tok ResyncT2,
                        TokenProperty ResyncP1) {
  return parseIdentifierDeclName(P, Result, L, DeclKindName, ResyncT1, ResyncT2,
                                 tok::NUM_TOKENS, tok::NUM_TOKENS, ResyncP1);
}

static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &L,
                        StringRef DeclKindName, tok ResyncT1, tok ResyncT2,
                        tok ResyncT3, TokenProperty ResyncP1) {
  return parseIdentifierDeclName(P, Result, L, DeclKindName, ResyncT1, ResyncT2,
                                 ResyncT3, tok::NUM_TOKENS, ResyncP1);
}

/// Add a fix-it to remove the space in consecutive identifiers.
/// Add a camel-cased option if it is different than the first option.
void Parser::diagnoseConsecutiveIDs(StringRef First, SourceLoc FirstLoc,
                                    StringRef DeclKindName) {
  assert(Tok.isAny(tok::identifier, tok::kw__));

  diagnose(Tok, diag::repeated_identifier, DeclKindName);
  auto Second = Tok.getText();
  auto SecondLoc = consumeToken();

  SourceRange FixRange(FirstLoc, SecondLoc);
  // Provide two fix-its: a direct concatenation of the two identifiers
  // and a camel-cased version.
  //
  auto DirectConcatenation = First.str() + Second.str();
  diagnose(SecondLoc, diag::join_identifiers)
    .fixItReplace(FixRange, DirectConcatenation);

  SmallString<8> CapitalizedScratch;
  auto Capitalized = camel_case::toSentencecase(Second,
                                                CapitalizedScratch);
  if (Capitalized != Second) {
    auto CamelCaseConcatenation = First.str() + Capitalized.str();
    diagnose(SecondLoc, diag::join_identifiers_camel_case)
      .fixItReplace(FixRange, CamelCaseConcatenation);
  }
}

/// Parse a Decl item in decl list.
static ParserStatus parseDeclItem(Parser &P,
                                  bool &PreviousHadSemi,
                                  Parser::ParseDeclOptions Options,
                                  llvm::function_ref<void(Decl*)> handler) {
  if (P.Tok.is(tok::semi)) {
    // Consume ';' without preceding decl.
    P.diagnose(P.Tok, diag::unexpected_separator, ";")
      .fixItRemove(P.Tok.getLoc());
    P.consumeToken();
    // Return success because we already recovered.
    return makeParserSuccess();
  }

  // If the previous declaration didn't have a semicolon and this new
  // declaration doesn't start a line, complain.
  if (!PreviousHadSemi && !P.Tok.isAtStartOfLine() && !P.Tok.is(tok::unknown)) {
    auto endOfPrevious = P.getEndOfPreviousLoc();
    P.diagnose(endOfPrevious, diag::declaration_same_line_without_semi)
      .fixItInsert(endOfPrevious, ";");
  }

  auto Result = P.parseDecl(Options, handler);
  if (Result.isParseError())
    P.skipUntilDeclRBrace(tok::semi, tok::pound_endif);
  SourceLoc SemiLoc;
  PreviousHadSemi = P.consumeIf(tok::semi, SemiLoc);
  if (PreviousHadSemi && Result.isNonNull())
    Result.get()->TrailingSemiLoc = SemiLoc;
  return Result;
}

/// \brief Parse the members in a struct/class/enum/protocol/extension.
///
/// \verbatim
///    decl* '}'
/// \endverbatim
bool Parser::parseDeclList(SourceLoc LBLoc, SourceLoc &RBLoc,
                           Diag<> ErrorDiag, ParseDeclOptions Options,
                           llvm::function_ref<void(Decl*)> handler) {
  ParserStatus Status;
  bool PreviousHadSemi = true;
  while (Tok.isNot(tok::r_brace)) {
    Status |= parseDeclItem(*this, PreviousHadSemi, Options, handler);
    if (Tok.isAny(tok::eof, tok::pound_endif, tok::pound_else,
                  tok::pound_elseif)) {
      IsInputIncomplete = true;
      break;
    }
  }

  parseMatchingToken(tok::r_brace, RBLoc, ErrorDiag, LBLoc);

  // If we found the closing brace, then the caller should not care if there
  // were errors while parsing inner decls, because we recovered.
  return !RBLoc.isValid();
}

/// \brief Parse an 'extension' declaration.
///
/// \verbatim
///   extension:
///    'extension' attribute-list type inheritance? where-clause?
///        '{' decl* '}'
/// \endverbatim
ParserResult<ExtensionDecl>
Parser::parseDeclExtension(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  SourceLoc ExtensionLoc = consumeToken(tok::kw_extension);
  
  DebuggerContextChange DCC (*this);

  // Parse the type being extended.
  ParserStatus status;
  ParserResult<TypeRepr> extendedType = parseType(diag::extension_type_expected);
  status |= extendedType;

  // Parse optional inheritance clause.
  SmallVector<TypeLoc, 2> Inherited;
  if (Tok.is(tok::colon))
    status |= parseInheritance(Inherited, /*classRequirementLoc=*/nullptr);

  // Parse the optional where-clause.
  TrailingWhereClause *trailingWhereClause = nullptr;
  if (Tok.is(tok::kw_where)) {
    SourceLoc whereLoc;
    SmallVector<RequirementRepr, 4> requirements;
    bool firstTypeInComplete;
    auto whereStatus = parseGenericWhereClause(whereLoc, requirements,
                                               firstTypeInComplete);
    if (whereStatus.isSuccess()) {
      trailingWhereClause = TrailingWhereClause::create(Context, whereLoc,
                                                        requirements);
    } else if (whereStatus.hasCodeCompletion()) {
      if (CodeCompletion && firstTypeInComplete) {
        CodeCompletion->completeGenericParams(extendedType.getPtrOrNull());
      } else
        return makeParserCodeCompletionResult<ExtensionDecl>();
    }
  }

  ExtensionDecl *ext = ExtensionDecl::create(Context, ExtensionLoc,
                                             extendedType.getPtrOrNull(),
                                             Context.AllocateCopy(Inherited),
                                             CurDeclContext,
                                             trailingWhereClause);
  ext->getAttrs() = Attributes;

  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_extension)) {
    LBLoc = PreviousLoc;
    RBLoc = LBLoc;
    status.setIsParseError();
  } else {
    // Parse the body.
    ContextChange CC(*this, ext);
    Scope S(this, ScopeKind::Extension);

    ParseDeclOptions Options(PD_HasContainerType | PD_InExtension);

    if (parseDeclList(LBLoc, RBLoc, diag::expected_rbrace_extension,
                      Options, [&] (Decl *D) {ext->addMember(D);}))
      status.setIsParseError();

    // Don't propagate the code completion bit from members: we cannot help
    // code completion inside a member decl, and our callers cannot do
    // anything about it either.  But propagate the error bit.
  }

  ext->setBraces({LBLoc, RBLoc});

  if (!DCC.movedToTopLevel() && !(Flags & PD_AllowTopLevel)) {
    diagnose(ExtensionLoc, diag::decl_inner_scope);
    status.setIsParseError();

    // Tell the type checker not to touch this extension.
    ext->setInvalid();
  }

  return DCC.fixupParserResult(status, ext);
}

ParserStatus Parser::parseLineDirective(bool isLine) {
  SourceLoc Loc = consumeToken();
  if (isLine) {
    diagnose(Loc, diag::line_directive_style_deprecated)
        .fixItReplace(Loc, "#sourceLocation");
  }
  bool WasInPoundLineEnvironment = InPoundLineEnvironment;
  if (WasInPoundLineEnvironment) {
    SourceMgr.closeVirtualFile(Loc);
    InPoundLineEnvironment = false;
  }

  
  unsigned StartLine = 0;
  Optional<StringRef> Filename;
  const char *LastTokTextEnd;
  if (!isLine) {
    // #sourceLocation()
    // #sourceLocation(file: "foo", line: 42)
    if (parseToken(tok::l_paren, diag::sourceLocation_expected, "("))
      return makeParserError();

    // Handle the "reset" form.
    if (consumeIf(tok::r_paren)) {
      if (!WasInPoundLineEnvironment) {
        diagnose(Tok, diag::unexpected_line_directive);
        return makeParserError();
      }
      return makeParserSuccess();
    }
    
    if (parseSpecificIdentifier("file", diag::sourceLocation_expected,"file:")||
        parseToken(tok::colon, diag::sourceLocation_expected, ":"))
      return makeParserError();

    if (Tok.isNot(tok::string_literal)) {
      diagnose(Tok, diag::expected_line_directive_name);
      return makeParserError();
    }
    
    Filename = getStringLiteralIfNotInterpolated(*this, Loc, Tok,
                                                 "#sourceLocation");
    if (!Filename.hasValue())
      return makeParserError();
    consumeToken(tok::string_literal);
    
    if (parseToken(tok::comma, diag::sourceLocation_expected, ",") ||
        parseSpecificIdentifier("line", diag::sourceLocation_expected,"line:")||
        parseToken(tok::colon, diag::sourceLocation_expected, ":"))
      return makeParserError();
  
    if (Tok.isNot(tok::integer_literal)) {
      diagnose(Tok, diag::expected_line_directive_number);
      return makeParserError();
    }
    if (Tok.getText().getAsInteger(0, StartLine)) {
      diagnose(Tok, diag::expected_line_directive_number);
      return makeParserError();
    }
    if (StartLine == 0) {
      diagnose(Tok, diag::line_directive_line_zero);
      return makeParserError();
    }
    consumeToken(tok::integer_literal);

    LastTokTextEnd = Tok.getText().end();
    if (parseToken(tok::r_paren, diag::sourceLocation_expected, ")"))
      return makeParserError();
    
  } else {  // Legacy #line syntax.
  
    // #line\n returns to the main buffer.
    if (Tok.isAtStartOfLine()) {
      if (!WasInPoundLineEnvironment) {
        diagnose(Tok, diag::unexpected_line_directive);
        return makeParserError();
      }
      return makeParserSuccess();
    }

    // #line 42 "file.swift"\n
    if (Tok.isNot(tok::integer_literal)) {
      diagnose(Tok, diag::expected_line_directive_number);
      return makeParserError();
    }
    if (Tok.getText().getAsInteger(0, StartLine)) {
      diagnose(Tok, diag::expected_line_directive_number);
      return makeParserError();
    }
    if (StartLine == 0) {
      diagnose(Tok, diag::line_directive_line_zero);
      return makeParserError();
    }
    consumeToken(tok::integer_literal);

    if (Tok.isNot(tok::string_literal)) {
      diagnose(Tok, diag::expected_line_directive_name);
      return makeParserError();
    }
    
    Filename = getStringLiteralIfNotInterpolated(*this, Loc, Tok,
                                                 "#line");
    if (!Filename.hasValue())
      return makeParserError();
    LastTokTextEnd = Tok.getText().end();
    consumeToken(tok::string_literal);
  }
  
  // Skip over trailing whitespace and a single \n to the start of the next
  // line.
  while (*LastTokTextEnd == ' ' || *LastTokTextEnd == '\t')
    ++LastTokTextEnd;
  SourceLoc nextLineStartLoc = Lexer::getSourceLoc(LastTokTextEnd);
  
  if (*LastTokTextEnd == '\n')
    nextLineStartLoc = nextLineStartLoc.getAdvancedLoc(1);
  else {
    diagnose(Tok.getLoc(), diag::extra_tokens_line_directive);
    return makeParserError();
  }
  
  int LineOffset = StartLine - SourceMgr.getLineNumber(nextLineStartLoc);
 
  // Create a new virtual file for the region started by the #line marker.
  bool isNewFile = SourceMgr.openVirtualFile(nextLineStartLoc,
                                             Filename.getValue(), LineOffset);
  assert(isNewFile);(void)isNewFile;

  InPoundLineEnvironment = true;
  return makeParserSuccess();
}

ParserResult<IfConfigDecl> Parser::parseDeclIfConfig(ParseDeclOptions Flags) {
  StructureMarkerRAII ParsingDecl(*this, Tok.getLoc(),
                                  StructureMarkerKind::IfConfig);

  SmallVector<IfConfigDeclClause, 4> Clauses;

  bool foundActive = false;
  ConditionalCompilationExprState ConfigState;
  while (1) {
    bool isElse = Tok.is(tok::pound_else);
    SourceLoc ClauseLoc = consumeToken();
    Expr *Condition = nullptr;

    if (isElse) {
      ConfigState.setConditionActive(!foundActive);
    } else {
      // Evaluate the condition.
      llvm::SaveAndRestore<bool> S(InPoundIfEnvironment, true);
      ParserResult<Expr> Result = parseExprSequence(diag::expected_expr,
                                                    /*isBasic*/true,
                                                    /*isForDirective*/true);
      if (Result.isNull())
        return makeParserError();

      Condition = Result.get();

      // Evaluate the condition, to validate it.
      ConfigState = classifyConditionalCompilationExpr(Condition, Context,
                                                       Diags);
      if (foundActive)
        ConfigState.setConditionActive(false);
    }

    foundActive |= ConfigState.isConditionActive();

    if (!Tok.isAtStartOfLine() && Tok.isNot(tok::eof)) {
      diagnose(Tok.getLoc(),
               diag::extra_tokens_conditional_compilation_directive);
    }

    Optional<Scope> scope;
    if (!ConfigState.isConditionActive())
      scope.emplace(this, getScopeInfo().getCurrentScope()->getKind(),
                    /*inactiveConfigBlock=*/true);

    SmallVector<Decl*, 8> Decls;
    if (ConfigState.shouldParse()) {
      ParserStatus Status;
      bool PreviousHadSemi = true;
      while (Tok.isNot(tok::pound_else, tok::pound_endif, tok::pound_elseif,
                       tok::eof)) {
        if (Tok.is(tok::r_brace)) {
          diagnose(Tok.getLoc(),
                   diag::unexpected_rbrace_in_conditional_compilation_block);
          // If we see '}', following declarations don't look like belong to
          // the current decl context; skip them.
          skipUntilConditionalBlockClose();
          break;
        }
        Status |= parseDeclItem(*this, PreviousHadSemi, Flags,
                                [&](Decl *D) {Decls.push_back(D);});
      }
    } else {
      DiagnosticTransaction DT(Diags);
      skipUntilConditionalBlockClose();
      DT.abort();
    }

    Clauses.push_back(IfConfigDeclClause(ClauseLoc, Condition,
                                         Context.AllocateCopy(Decls),
                                         ConfigState.isConditionActive()));

    if (Tok.isNot(tok::pound_elseif) && Tok.isNot(tok::pound_else))
      break;

    if (isElse)
      diagnose(Tok, diag::expected_close_after_else_directive);
  }

  SourceLoc EndLoc;
  bool HadMissingEnd = parseEndIfDirective(EndLoc);

  IfConfigDecl *ICD = new (Context) IfConfigDecl(CurDeclContext,
                                                 Context.AllocateCopy(Clauses),
                                                 EndLoc, HadMissingEnd);
  return makeParserResult(ICD);
}

/// \brief Parse a typealias decl.
///
/// \verbatim
///   decl-typealias:
///     'typealias' identifier generic-params? '=' type requirement-clause?
/// \endverbatim
ParserResult<TypeDecl> Parser::
parseDeclTypeAlias(Parser::ParseDeclOptions Flags, DeclAttributes &Attributes) {
  ParserPosition startPosition = getParserPosition();
  SourceLoc TypeAliasLoc = consumeToken(tok::kw_typealias);
  SourceLoc EqualLoc;
  Identifier Id;
  SourceLoc IdLoc;
  ParserStatus Status;

  Status |= parseIdentifierDeclName(*this, Id, IdLoc, "typealias",
                                    tok::colon, tok::equal);
  if (Status.isError())
    return nullptr;
    
  DebuggerContextChange DCC(*this, Id, DeclKind::TypeAlias);

  Optional<Scope> GenericsScope;
  GenericsScope.emplace(this, ScopeKind::Generics);

  // Parse a generic parameter list if it is present.
  GenericParamList *genericParams = nullptr;
  if (startsWithLess(Tok)) {
    auto Result = parseGenericParameters();
    if (Result.hasCodeCompletion() && !CodeCompletion)
      return makeParserCodeCompletionStatus();
    genericParams = Result.getPtrOrNull();

    if (!genericParams) {
      // If the parser returned null, it is an already diagnosed parse error.
    } else if (!genericParams->getRequirements().empty()) {
      // Reject a where clause.
      diagnose(genericParams->getWhereLoc(),
               diag::associated_type_generic_parameter_list)
          .highlight(genericParams->getWhereClauseSourceRange());
    }
  }

  if (Flags.contains(PD_InProtocol) && !genericParams && !Tok.is(tok::equal)) {
    // If we're in a protocol and don't see an '=' this looks like leftover Swift 2
    // code intending to be an associatedtype.
    backtrackToPosition(startPosition);
    return parseDeclAssociatedType(Flags, Attributes);
  }
  
  ParserResult<TypeRepr> UnderlyingTy;

  if (Tok.is(tok::colon) || Tok.is(tok::equal)) {
    if (Tok.is(tok::colon)) {
      // It is a common mistake to write "typealias A : Int" instead of = Int.
      // Recognize this and produce a fixit.
      diagnose(Tok, diag::expected_equal_in_typealias)
          .fixItReplace(Tok.getLoc(), " = ");
      consumeToken(tok::colon);
    } else {
      EqualLoc = consumeToken(tok::equal);
    }

    UnderlyingTy = parseType(diag::expected_type_in_typealias);
    Status |= UnderlyingTy;
    if (UnderlyingTy.isNull())
      return Status;
  }

  // Parse a 'where' clause if present, adding it to our GenericParamList.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(genericParams);
    if (whereStatus.shouldStopParsing())
      return whereStatus;
  }

  if (UnderlyingTy.isNull()) {
    diagnose(Tok, diag::expected_equal_in_typealias);
    Status.setIsParseError();
    return Status;
  }

  auto *TAD = new (Context) TypeAliasDecl(TypeAliasLoc, EqualLoc, Id, IdLoc,
                                          genericParams, CurDeclContext);
  TAD->getUnderlyingTypeLoc() = UnderlyingTy.getPtrOrNull();
  TAD->getAttrs() = Attributes;

  // Exit the scope introduced for the generic parameters.
  GenericsScope.reset();

  addToScope(TAD);
  return DCC.fixupParserResult(Status, TAD);
}

/// \brief Parse an associatedtype decl.
///
/// \verbatim
///   decl-associatedtype:
///     'associatedtype' identifier inheritance? ('=' type)? where-clause?
/// \endverbatim

ParserResult<TypeDecl> Parser::parseDeclAssociatedType(Parser::ParseDeclOptions Flags,
                                                       DeclAttributes &Attributes) {
  SourceLoc AssociatedTypeLoc;
  ParserStatus Status;
  Identifier Id;
  SourceLoc IdLoc;
  
  // Look for 'typealias' here and diagnose a fixit because parseDeclTypeAlias can
  // ask us to fix up leftover Swift 2 code intending to be an associatedtype.
  if (Tok.is(tok::kw_typealias)) {
    AssociatedTypeLoc = consumeToken(tok::kw_typealias);
    diagnose(AssociatedTypeLoc, diag::typealias_inside_protocol_without_type)
        .fixItReplace(AssociatedTypeLoc, "associatedtype");
  } else {
    AssociatedTypeLoc = consumeToken(tok::kw_associatedtype);
  }
  
  Status = parseIdentifierDeclName(*this, Id, IdLoc, "associatedtype",
                                   tok::colon, tok::equal);
  if (Status.isError())
    return nullptr;
  
  DebuggerContextChange DCC(*this, Id, DeclKind::AssociatedType);
  
  // Reject generic parameters with a specific error.
  if (startsWithLess(Tok)) {
    // Introduce a throwaway scope to capture the generic parameters. We
    // don't want them visible anywhere!
    Scope S(this, ScopeKind::Generics);

    if (auto genericParams = parseGenericParameters().getPtrOrNull()) {
      diagnose(genericParams->getLAngleLoc(),
               diag::associated_type_generic_parameter_list)
      .fixItRemove(genericParams->getSourceRange());
    }
  }
  
  // Parse optional inheritance clause.
  // FIXME: Allow class requirements here.
  SmallVector<TypeLoc, 2> Inherited;
  if (Tok.is(tok::colon))
    Status |= parseInheritance(Inherited, /*classRequirementLoc=*/nullptr);
  
  ParserResult<TypeRepr> UnderlyingTy;
  if (Tok.is(tok::equal)) {
    consumeToken(tok::equal);
    UnderlyingTy = parseType(diag::expected_type_in_associatedtype);
    Status |= UnderlyingTy;
    if (UnderlyingTy.isNull())
      return Status;
  }

  TrailingWhereClause *TrailingWhere = nullptr;
  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    SourceLoc whereLoc;
    SmallVector<RequirementRepr, 4> requirements;
    bool firstTypeInComplete;
    auto whereStatus =
        parseGenericWhereClause(whereLoc, requirements, firstTypeInComplete);
    if (whereStatus.isSuccess()) {
      TrailingWhere =
          TrailingWhereClause::create(Context, whereLoc, requirements);
    } else if (whereStatus.hasCodeCompletion()) {
      // FIXME: this is completely (hah) cargo culted.
      if (CodeCompletion && firstTypeInComplete) {
        CodeCompletion->completeGenericParams(nullptr);
      } else {
        return makeParserCodeCompletionResult<AssociatedTypeDecl>();
      }
    }

    if (Context.isSwiftVersion3()) {
      diagnose(whereLoc, diag::associatedtype_where_swift_3);
      // There's nothing to see here, move along.
      TrailingWhere = nullptr;
    }
  }

  if (!Flags.contains(PD_InProtocol)) {
    diagnose(AssociatedTypeLoc, diag::associatedtype_outside_protocol)
        .fixItReplace(AssociatedTypeLoc, "typealias");
    Status.setIsParseError();
    return Status;
  }

  auto assocType = new (Context)
      AssociatedTypeDecl(CurDeclContext, AssociatedTypeLoc, Id, IdLoc,
                         UnderlyingTy.getPtrOrNull(), TrailingWhere);
  assocType->getAttrs() = Attributes;
  if (!Inherited.empty())
    assocType->setInherited(Context.AllocateCopy(Inherited));
  addToScope(assocType);
  return makeParserResult(Status, assocType);
}

/// This function creates an accessor function (with no body) for a computed
/// property or subscript.
static FuncDecl *createAccessorFunc(SourceLoc DeclLoc, ParameterList *param,
                                    GenericParamList *GenericParams,
                                    ParameterList *Indices,
                                    TypeLoc ElementTy,
                                    SourceLoc StaticLoc,
                                    Parser::ParseDeclOptions Flags,
                                    AccessorKind Kind,
                                    AddressorKind addressorKind,
                                    Parser *P, SourceLoc AccessorKeywordLoc) {
  // First task, set up the value argument list.  This is the "newValue" name
  // (for setters) followed by the index list (for subscripts).  For
  // non-subscript getters, this degenerates down to "()".
  //
  // We put the 'newValue' argument before the subscript index list as a
  // micro-optimization for Objective-C thunk generation.
  ParameterList *ValueArg;
  {
    SmallVector<ParamDecl*, 2> ValueArgElements;
    SourceLoc StartLoc, EndLoc;
    if (param) {
      assert(param->size() == 1 &&
             "Should only have a single parameter in the list");
      ValueArgElements.push_back(param->get(0));
      StartLoc = param->getStartLoc();
      EndLoc = param->getEndLoc();
    }

    if (Indices) {
      Indices = Indices->clone(P->Context, ParameterList::Implicit);
      ValueArgElements.append(Indices->begin(), Indices->end());
      if (StartLoc.isInvalid()) {
        StartLoc = Indices->getStartLoc();
        EndLoc = Indices->getEndLoc();
      }
    }

    ValueArg = ParameterList::create(P->Context, StartLoc, ValueArgElements,
                                     EndLoc);
  }


  // Create the parameter list(s) for the getter.
  SmallVector<ParameterList*, 4> Params;
  
  // Add the implicit 'self' to Params, if needed.
  if (Flags & Parser::PD_HasContainerType)
    Params.push_back(ParameterList::createUnboundSelf(DeclLoc, P->CurDeclContext));
  
  // Add the "(value)" and subscript indices parameter clause.
  Params.push_back(ValueArg);

  TypeLoc ReturnType;

  // Getters return the value type.
  if (Kind == AccessorKind::IsGetter) {
    ReturnType = ElementTy.clone(P->Context);

  // Addressors return Unsafe{,Mutable}Pointer<T>, plus sometimes an
  // owner or pinned owner.
  } else if (Kind == AccessorKind::IsAddressor ||
             Kind == AccessorKind::IsMutableAddressor) {

    // If we don't have a declared type, we will diagnose later,
    // so skip this to avoid crashing.
    if (ElementTy.getTypeRepr()) {
      // Construct "Unsafe{,Mutable}Pointer<T>".
      
      TypeRepr *args[] = { ElementTy.clone(P->Context).getTypeRepr() };

      // FIXME: the fact that this could resolve in the local scope is dumb.
      bool isMutable = (Kind == AccessorKind::IsMutableAddressor);
      Identifier name = P->Context.getIdentifier(
                      isMutable ? "UnsafeMutablePointer" : "UnsafePointer");

      TypeRepr *resultType =
        new (P->Context) GenericIdentTypeRepr(SourceLoc(), name,
                                              P->Context.AllocateCopy(args),
                                              SourceRange());

      auto makeKnownType = [&](Type type) -> TypeRepr* {
        return new (P->Context) FixedTypeRepr(type, SourceLoc());
      };
      auto makePairType = [&](TypeRepr *fst, TypeRepr *snd) -> TypeRepr* {
        return TupleTypeRepr::create(P->Context, {fst, snd}, SourceRange());
      };

      switch (addressorKind) {
      case AddressorKind::NotAddressor:
        llvm_unreachable("not an addressor!");

      // For unsafe addressors, that's all we've got.
      case AddressorKind::Unsafe:
        break;

      // For non-native owning addressors, the return type is actually
      //   (Unsafe{,Mutable}Pointer<T>, Builtin.UnknownObject)
      case AddressorKind::Owning:
        resultType = makePairType(resultType,
                                  makeKnownType(P->Context.TheUnknownObjectType));
        break;

      // For native owning addressors, the return type is actually
      //   (Unsafe{,Mutable}Pointer<T>, Builtin.NativeObject)
      case AddressorKind::NativeOwning:
        resultType = makePairType(resultType,
                                  makeKnownType(P->Context.TheNativeObjectType));
        break;

      // For native pinning addressors, the return type is actually
      //   (Unsafe{,Mutable}Pointer<T>, Builtin.NativeObject?)
      case AddressorKind::NativePinning: {
        auto optNativePtr = new (P->Context) OptionalTypeRepr(
            makeKnownType(P->Context.TheNativeObjectType),
            SourceLoc());
        resultType = makePairType(resultType, optNativePtr);
        break;
      }
      }

      ReturnType = resultType;
    }

  // Everything else returns ().
  } else {
    ReturnType = TypeLoc::withoutLoc(TupleType::getEmpty(P->Context));
  }

  // Start the function.
  auto *D = FuncDecl::create(P->Context, StaticLoc, StaticSpellingKind::None,
                             /*FIXME FuncLoc=*/DeclLoc, Identifier(),
                             /*NameLoc=*/DeclLoc, /*Throws=*/false,
                             /*ThrowsLoc=*/SourceLoc(), AccessorKeywordLoc,
                             (GenericParams
                              ? GenericParams->clone(P->CurDeclContext)
                              : nullptr),
                             Params, ReturnType,
                             P->CurDeclContext);

  // Non-static set/willSet/didSet/materializeForSet/mutableAddress
  // default to mutating.  get/address default to
  // non-mutating.
  if (!D->isStatic()) {
    switch (Kind) {
    case AccessorKind::IsAddressor:
      D->setAddressorKind(addressorKind);
      break;

    case AccessorKind::IsGetter:
      break;

    case AccessorKind::IsMutableAddressor:
      D->setAddressorKind(addressorKind);
      LLVM_FALLTHROUGH;

    case AccessorKind::IsSetter:
    case AccessorKind::IsWillSet:
    case AccessorKind::IsDidSet:
      D->setMutating();
      break;

    case AccessorKind::IsMaterializeForSet:
    case AccessorKind::NotAccessor:
      llvm_unreachable("not parseable accessors");
    }
  }

  return D;
}

static ParamDecl *
createSetterAccessorArgument(SourceLoc nameLoc, Identifier name,
                             TypeLoc elementTy, AccessorKind accessorKind,
                             Parser &P) {
  // Add the parameter. If no name was specified, the name defaults to
  // 'value'.
  bool isNameImplicit = name.empty();
  if (isNameImplicit) {
    const char *implName =
      accessorKind == AccessorKind::IsDidSet ? "oldValue" : "newValue";
    name = P.Context.getIdentifier(implName);
  }

  auto result = new (P.Context) ParamDecl(/*IsLet*/true,SourceLoc(),SourceLoc(),
                                          Identifier(), nameLoc, name,
                                          Type(), P.CurDeclContext);
  if (isNameImplicit)
    result->setImplicit();

  result->getTypeLoc() = elementTy.clone(P.Context);
  
  // AST Walker shouldn't go into the type recursively.
  result->setIsTypeLocImplicit(true);
  return result;
}

/// Parse a "(value)" specifier for "set" or "willSet" if present.  Create a
/// parameter list to represent the spelled argument or return null if none is
/// present.
static ParameterList *
parseOptionalAccessorArgument(SourceLoc SpecifierLoc, TypeLoc ElementTy,
                              Parser &P, AccessorKind Kind) {
  // 'set' and 'willSet' have a (value) parameter, 'didSet' takes an (oldValue)
  // parameter and 'get' and always takes a () parameter.
  if (Kind != AccessorKind::IsSetter && Kind != AccessorKind::IsWillSet &&
      Kind != AccessorKind::IsDidSet)
    return nullptr;

  SourceLoc StartLoc, NameLoc, EndLoc;
  Identifier Name;

  // If the SpecifierLoc is invalid, then the caller just wants us to synthesize
  // the default, not actually try to parse something.
  if (SpecifierLoc.isValid() && P.Tok.is(tok::l_paren)) {
    StartLoc = P.consumeToken(tok::l_paren);
    if (P.Tok.isNot(tok::identifier)) {
      P.diagnose(P.Tok, diag::expected_accessor_name, (unsigned)Kind);
      P.skipUntil(tok::r_paren, tok::l_brace);
      if (P.Tok.is(tok::r_paren))
        EndLoc = P.consumeToken();
      else
        EndLoc = StartLoc;
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

  if (Name.empty()) NameLoc = SpecifierLoc;
  auto param = createSetterAccessorArgument(NameLoc, Name, ElementTy, Kind, P);
  return ParameterList::create(P.Context, StartLoc, param, EndLoc);
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

static AddressorKind getImmutableAddressorKind(Token &tok) {
  if (tok.isContextualKeyword("unsafeAddress")) {
    return AddressorKind::Unsafe;
  } else if (tok.isContextualKeyword("addressWithOwner")) {
    return AddressorKind::Owning;
  } else if (tok.isContextualKeyword("addressWithNativeOwner")) {
    return AddressorKind::NativeOwning;
  } else if (tok.isContextualKeyword("addressWithPinnedNativeOwner")) {
    return AddressorKind::NativePinning;
  } else {
    return AddressorKind::NotAddressor;
  }
}
static AddressorKind getMutableAddressorKind(Token &tok) {
  if (tok.isContextualKeyword("unsafeMutableAddress")) {
    return AddressorKind::Unsafe;
  } else if (tok.isContextualKeyword("mutableAddressWithOwner")) {
    return AddressorKind::Owning;
  } else if (tok.isContextualKeyword("mutableAddressWithNativeOwner")) {
    return AddressorKind::NativeOwning;
  } else if (tok.isContextualKeyword("mutableAddressWithPinnedNativeOwner")) {
    return AddressorKind::NativePinning;
  } else {
    return AddressorKind::NotAddressor;
  }
}

/// Returns an accessor kind that 
static StringRef getAccessorNameForDiagnostic(AccessorKind accessorKind,
                                              AddressorKind addressorKind) {
  switch (accessorKind) {
  case AccessorKind::NotAccessor: llvm_unreachable("invalid");
  case AccessorKind::IsGetter: return "getter";
  case AccessorKind::IsSetter: return "setter";
  case AccessorKind::IsDidSet: return "didSet";
  case AccessorKind::IsWillSet: return "willSet";
  case AccessorKind::IsMaterializeForSet: return "materializeForSet";
  case AccessorKind::IsAddressor:
    switch (addressorKind) {
    case AddressorKind::NotAddressor: llvm_unreachable("invalid");
    case AddressorKind::Unsafe: return "unsafeAddress";
    case AddressorKind::Owning: return "addressWithOwner";
    case AddressorKind::NativeOwning: return "addressWithNativeOwner";
    case AddressorKind::NativePinning: return "addressWithPinnedNativeOwner";
    }
    llvm_unreachable("bad addressor kind");
  case AccessorKind::IsMutableAddressor:
    switch (addressorKind) {
    case AddressorKind::NotAddressor: llvm_unreachable("invalid");
    case AddressorKind::Unsafe: return "unsafeMutableAddress";
    case AddressorKind::Owning: return "mutableAddressWithOwner";
    case AddressorKind::NativeOwning: return "mutableAddressWithNativeOwner";
    case AddressorKind::NativePinning: return "mutableAddressWithPinnedNativeOwner";
    }
    llvm_unreachable("bad addressor kind");
  }
  llvm_unreachable("bad accessor kind");  
}

static void diagnoseRedundantAccessors(Parser &P, SourceLoc loc,
                                       AccessorKind accessorKind,
                                       AddressorKind addressorKind,
                                       bool isSubscript,
                                       FuncDecl *previousDecl) {
  // Different addressor safety kinds still count as the same addressor.
  if (previousDecl->getAddressorKind() != addressorKind) {
    assert(accessorKind == AccessorKind::IsAddressor ||
           accessorKind == AccessorKind::IsMutableAddressor);
    P.diagnose(loc, diag::conflicting_property_addressor,
               unsigned(isSubscript),
               unsigned(accessorKind == AccessorKind::IsMutableAddressor));

    // Be less specific about the previous definition.
    P.diagnose(previousDecl->getLoc(), diag::previous_accessor,
               accessorKind == AccessorKind::IsMutableAddressor
                 ? "mutable addressor" : "addressor");
    return;
  }

  P.diagnose(loc, diag::duplicate_property_accessor,
             getAccessorNameForDiagnostic(accessorKind, addressorKind));
  P.diagnose(previousDecl->getLoc(), diag::previous_accessor,
             getAccessorNameForDiagnostic(accessorKind, addressorKind));
}

/// \brief Parse a get-set clause, optionally containing a getter, setter,
/// willSet, and/or didSet clauses.  'Indices' is a paren or tuple pattern,
/// specifying the index list for a subscript.
bool Parser::parseGetSetImpl(ParseDeclOptions Flags,
                             GenericParamList *GenericParams,
                             ParameterList *Indices,
                             TypeLoc ElementTy, ParsedAccessors &accessors,
                             SourceLoc &LastValidLoc, SourceLoc StaticLoc,
                             SourceLoc VarLBLoc,
                             SmallVectorImpl<Decl *> &Decls) {

  // Properties in protocols use sufficiently limited syntax that we have a
  // special parsing loop for them.  SIL mode uses the same syntax.
  if (Flags.contains(PD_InProtocol) || isInSILMode()) {
    while (Tok.isNot(tok::r_brace)) {
      if (Tok.is(tok::eof))
        return true;

      // Parse any leading attributes.
      DeclAttributes Attributes;
      bool FoundCCToken;
      parseDeclAttributeList(Attributes, FoundCCToken);

      // Parse the contextual keywords for 'mutating' and 'nonmutating' before
      // get and set.
      if (Tok.isContextualKeyword("mutating")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Mutating);
      } else if (Tok.isContextualKeyword("nonmutating")) {
        parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_NonMutating);
      }

      AccessorKind Kind;
      AddressorKind addressorKind = AddressorKind::NotAddressor;
      FuncDecl **TheDeclPtr;
      SourceLoc AccessorKeywordLoc = Tok.getLoc();
      if (Tok.isContextualKeyword("get")) {
        Kind = AccessorKind::IsGetter;
        TheDeclPtr = &accessors.Get;
      } else if (Tok.isContextualKeyword("set")) {
        Kind = AccessorKind::IsSetter;
        TheDeclPtr = &accessors.Set;
      } else if (!Flags.contains(PD_InProtocol) &&
                 (addressorKind = getImmutableAddressorKind(Tok))
                   != AddressorKind::NotAddressor) {
        Kind = AccessorKind::IsAddressor;
        TheDeclPtr = &accessors.Addressor;
      } else if (!Flags.contains(PD_InProtocol) &&
                 (addressorKind = getMutableAddressorKind(Tok))
                   != AddressorKind::NotAddressor) {
        Kind = AccessorKind::IsMutableAddressor;
        TheDeclPtr = &accessors.MutableAddressor;
      } else {
        AccessorKeywordLoc = SourceLoc();
        diagnose(Tok, diag::expected_getset_in_protocol);
        return true;
      }

      FuncDecl *&TheDecl = *TheDeclPtr;
      SourceLoc Loc = consumeToken();

      // Have we already parsed this kind of clause?
      if (TheDecl) {
        diagnoseRedundantAccessors(*this, Loc, Kind, addressorKind,
                                   /*subscript*/Indices != nullptr, TheDecl);

        // Forget the previous decl.
        Decls.erase(std::find(Decls.begin(), Decls.end(), TheDecl));
        TheDecl = nullptr;  // Forget the previous decl.
      }

      // "set" could have a name associated with it.  This isn't valid in a
      // protocol, but we parse and then reject it, for better QoI.
      if (Tok.is(tok::l_paren))
        diagnose(Loc, diag::protocol_setter_name);

      auto *ValueNameParams
        = parseOptionalAccessorArgument(Loc, ElementTy, *this, Kind);

      // Set up a function declaration.
      TheDecl = createAccessorFunc(Loc, ValueNameParams,
                                   GenericParams, Indices, ElementTy,
                                   StaticLoc, Flags, Kind, addressorKind, this,
                                   AccessorKeywordLoc);
      TheDecl->getAttrs() = Attributes;
      
      Decls.push_back(TheDecl);
    }

    return false;
  }


  // Otherwise, we have a normal var or subscript declaration, parse the full
  // complement of specifiers, along with their bodies.

  // If the body is completely empty, preserve it.  This is at best a getter with
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
    bool FoundCCToken;
    parseDeclAttributeList(Attributes, FoundCCToken);

    // Parse the contextual keywords for 'mutating' and 'nonmutating' before
    // get and set.
    if (Tok.isContextualKeyword("mutating")) {
      parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_Mutating);
    } else if (Tok.isContextualKeyword("nonmutating")) {
      parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DAK_NonMutating);
    }
    
    bool isImplicitGet = false;
    AccessorKind Kind;
    AddressorKind addressorKind = AddressorKind::NotAddressor;
    FuncDecl **TheDeclPtr;
    SourceLoc AccessorKeywordLoc =  Tok.getLoc();
    if (Tok.isContextualKeyword("get")) {
      Kind = AccessorKind::IsGetter;
      TheDeclPtr = &accessors.Get;
    } else if (Tok.isContextualKeyword("set")) {
      Kind = AccessorKind::IsSetter;
      TheDeclPtr = &accessors.Set;
    } else if (Tok.isContextualKeyword("willSet")) {
      Kind = AccessorKind::IsWillSet;
      TheDeclPtr = &accessors.WillSet;
    } else if (Tok.isContextualKeyword("didSet")) {
      Kind = AccessorKind::IsDidSet;
      TheDeclPtr = &accessors.DidSet;
    } else if ((addressorKind = getImmutableAddressorKind(Tok))
                 != AddressorKind::NotAddressor) {
      Kind = AccessorKind::IsAddressor;
      TheDeclPtr = &accessors.Addressor;
    } else if ((addressorKind = getMutableAddressorKind(Tok))
                 != AddressorKind::NotAddressor) {
      Kind = AccessorKind::IsMutableAddressor;
      TheDeclPtr = &accessors.MutableAddressor;
    } else {
      AccessorKeywordLoc = SourceLoc();
      // This is an implicit getter.  Might be not valid in this position,
      // though.  Anyway, go back to the beginning of the getter code to ensure
      // that the diagnostics point to correct tokens.
      if (BeginParserPosition.isValid()) {
        backtrackToPosition(BeginParserPosition);
        Attributes = DeclAttributes();
      }
      if (!IsFirstAccessor) {
        // Cannot have an implicit getter after other accessor.
        diagnose(Tok, diag::expected_accessor_kw);
        skipUntil(tok::r_brace);
        // Don't signal an error since we recovered.
        return false;
      }
      Kind = AccessorKind::IsGetter;
      TheDeclPtr = &accessors.Get;
      isImplicitGet = true;
    }

    IsFirstAccessor = false;

    // Consume the contextual keyword, if present.
    SourceLoc Loc = isImplicitGet ? VarLBLoc : consumeToken();

    FuncDecl *&TheDecl = *TheDeclPtr;

    // Have we already parsed this kind of clause?
    if (TheDecl) {
      diagnoseRedundantAccessors(*this, Loc, Kind, addressorKind,
                                 /*subscript*/Indices != nullptr, TheDecl);

      // Forget the previous decl.
      Decls.erase(std::find(Decls.begin(), Decls.end(), TheDecl));
      TheDecl = nullptr;
    }

    // 'set' and 'willSet' can have an optional name.
    //
    //     set-name    ::= '(' identifier ')'
    auto *ValueNamePattern =
      parseOptionalAccessorArgument(Loc, ElementTy, *this, Kind);

    SourceLoc LBLoc = isImplicitGet ? VarLBLoc : Tok.getLoc();
    // FIXME: Use outer '{' loc if isImplicitGet.
    bool ExternalAsmName = false;
    if (!isImplicitGet && !consumeIf(tok::l_brace)) {
      // _silgen_name'd accessors don't need bodies.
      if (!Attributes.hasAttribute<SILGenNameAttr>()) {
        diagnose(Tok, diag::expected_lbrace_accessor,
                 getAccessorNameForDiagnostic(Kind, addressorKind));
        return true;
      }
      ExternalAsmName = true;
    }

    // Set up a function declaration.
    TheDecl = createAccessorFunc(Loc, ValueNamePattern,
                                 GenericParams, Indices, ElementTy,
                                 StaticLoc, Flags, Kind, addressorKind, this,
                                 AccessorKeywordLoc);
    TheDecl->getAttrs() = Attributes;

    // Parse the body, if any.
    if (ExternalAsmName) {
      LastValidLoc = Loc;
    } else {
      Scope S(this, ScopeKind::FunctionBody);
      for (auto PL : TheDecl->getParameterLists())
        addParametersToScope(PL);

      if (TheDecl->isGeneric())
        for (auto *GP : TheDecl->getGenericParams()->getParams())
          addToScope(GP);

      // Establish the new context.
      ParseFunctionBody CC(*this, TheDecl);

      // Parse the body.
      SmallVector<ASTNode, 16> Entries;
      {
        llvm::SaveAndRestore<bool> T(IsParsingInterfaceTokens, false);
        if (!isDelayedParsingEnabled())
          parseBraceItems(Entries);
        else
          consumeGetSetBody(TheDecl, LBLoc);
      }

      SourceLoc RBLoc;
      if (!isImplicitGet) {
        parseMatchingToken(tok::r_brace, RBLoc, diag::expected_rbrace_in_getset,
                           LBLoc);
      } else {
        RBLoc = Tok.is(tok::r_brace) ? Tok.getLoc() : PreviousLoc;
      }

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

bool Parser::parseGetSet(ParseDeclOptions Flags,
                         GenericParamList *GenericParams,
                         ParameterList *Indices,
                         TypeLoc ElementTy, ParsedAccessors &accessors,
                         SourceLoc StaticLoc,
                         SmallVectorImpl<Decl *> &Decls) {
  accessors.LBLoc = consumeToken(tok::l_brace);
  SourceLoc LastValidLoc = accessors.LBLoc;
  bool Invalid = parseGetSetImpl(Flags, GenericParams, Indices, ElementTy,
                                 accessors, LastValidLoc, StaticLoc,
                                 accessors.LBLoc, Decls);

  // Parse the final '}'.
  if (Invalid)
    skipUntil(tok::r_brace);

  parseMatchingToken(tok::r_brace, accessors.RBLoc,
                     diag::expected_rbrace_in_getset, accessors.LBLoc);
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

  // Create a lexer that cannot go past the end state.
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
VarDecl *Parser::parseDeclVarGetSet(Pattern *pattern,
                                    ParseDeclOptions Flags,
                                    SourceLoc StaticLoc, bool hasInitializer,
                                    const DeclAttributes &Attributes,
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
  }

  TypeLoc TyLoc;
  if (TypedPattern *TP = dyn_cast<TypedPattern>(pattern)) {
    TyLoc = TP->getTypeLoc();
  } else if (!PrimaryVar) {
    TyLoc = TypeLoc::withoutLoc(ErrorType::get(Context));
  }

  // Parse getter and setter.
  ParsedAccessors accessors;
  if (parseGetSet(Flags, /*GenericParams=*/nullptr,
                  /*Indices=*/nullptr, TyLoc, accessors, StaticLoc,
                  Decls))
    Invalid = true;

  // If we have an invalid case, bail out now.
  if (!PrimaryVar)
    return nullptr;

  if (!TyLoc.hasLocation()) {
    if (accessors.Get || accessors.Set || accessors.Addressor ||
        accessors.MutableAddressor) {
      diagnose(pattern->getLoc(), diag::computed_property_missing_type);
      Invalid = true;
    }
  }

  // Reject accessors on 'let's after parsing them (for better recovery).
  if (PrimaryVar->isLet() && !Attributes.hasAttribute<SILStoredAttr>()) {
    if (accessors.WillSet || accessors.DidSet)
      diagnose(accessors.LBLoc, diag::let_cannot_be_observing_property);
    else if (accessors.Addressor || accessors.MutableAddressor)
      diagnose(accessors.LBLoc, diag::let_cannot_be_addressed_property);
    else
      diagnose(accessors.LBLoc, diag::let_cannot_be_computed_property);
    PrimaryVar->setLet(false);
    Invalid = true;
  }

  // Lazy var should not have explicit getter/setter.
  // For error-recovery, we mark them as invalid.
  if (Attributes.hasAttribute<LazyAttr>()){
    if (accessors.Get)
      accessors.Get->setInvalid();
    if (accessors.Set)
      accessors.Set->setInvalid();
  }

  accessors.record(*this, PrimaryVar, Invalid, Flags, StaticLoc,
                   Attributes, TyLoc, /*indices*/ nullptr, Decls);

  return PrimaryVar;
}

/// Record a bunch of parsed accessors into the given abstract storage decl.
void Parser::ParsedAccessors::record(Parser &P, AbstractStorageDecl *storage,
                                     bool invalid, ParseDeclOptions flags,
                                     SourceLoc staticLoc,
                                     const DeclAttributes &attrs,
                                     TypeLoc elementTy, ParameterList *indices,
                                     SmallVectorImpl<Decl *> &decls) {
  auto flagInvalidAccessor = [&](FuncDecl *&func) {
    if (func) {
      func->setInvalid();
    }
  };
  auto ignoreInvalidAccessor = [&](FuncDecl *&func) {
    if (func) {
      flagInvalidAccessor(func);

      // Forget the decl being invalidated
      auto PositionInDecls = std::find(decls.begin(), decls.end(), func);
      assert(PositionInDecls != decls.end());
      decls.erase(PositionInDecls);

      func = nullptr;
    }
  };

  GenericParamList *genericParams = nullptr;
  if (auto *subscript = dyn_cast<SubscriptDecl>(storage))
    genericParams = subscript->getGenericParams();

  // Create an implicit accessor declaration.
  auto createImplicitAccessor =
  [&](AccessorKind kind, AddressorKind addressorKind,
      ParameterList *argList) -> FuncDecl* {
    auto accessor = createAccessorFunc(SourceLoc(), argList,
                                       genericParams, indices, elementTy,
                                       staticLoc, flags, kind, addressorKind,
                                       &P, SourceLoc());
    accessor->setImplicit();
    decls.push_back(accessor);
    return accessor;
  };

  // 'address' is exclusive with 'get', and 'mutableAddress' is
  // exclusive with 'set'.
  if (Addressor || MutableAddressor) {
    // Require either a 'get' or an 'address' accessor if there's
    // a 'mutableAddress' accessor.  In principle, we could synthesize
    // 'address' from 'mutableAddress', but for now we'll enforce this.
    if (!Addressor && !Get) {
      P.diagnose(MutableAddressor->getLoc(),
                 diag::mutableaddressor_without_address,
                 isa<SubscriptDecl>(storage));

      Addressor = createImplicitAccessor(AccessorKind::IsAddressor,
                                         AddressorKind::Unsafe,
                                         nullptr);

    // Don't allow both.
    } else if (Addressor && Get) {
      P.diagnose(Addressor->getLoc(), diag::addressor_with_getter,
                 isa<SubscriptDecl>(storage));      
      ignoreInvalidAccessor(Get);
    }

    // Disallow mutableAddress+set.
    //
    // Currently we don't allow the address+set combination either.
    // In principle, this is an unnecessary restriction, and you can
    // imagine caches that might want to vend this combination of
    // accessors.  But we assume that in-place gets aren't all that
    // important.  (For example, we don't make any effort to optimize
    // them for polymorphic accesses.)
    if (Set) {
      if (MutableAddressor) {
        P.diagnose(MutableAddressor->getLoc(), diag::mutableaddressor_with_setter,
                   isa<SubscriptDecl>(storage));
      } else {
        P.diagnose(Set->getLoc(), diag::addressor_with_setter,
                   isa<SubscriptDecl>(storage));
      }
      ignoreInvalidAccessor(Set);
    }
  }

  // For now, we don't support the observing accessors on subscripts.
  if (isa<SubscriptDecl>(storage) && (WillSet || DidSet)) {
    P.diagnose(DidSet ? DidSet->getLoc() : WillSet->getLoc(),
               diag::observingproperty_in_subscript, bool(DidSet));
    ignoreInvalidAccessor(WillSet);
    ignoreInvalidAccessor(DidSet);
  }
  
  // If this decl is invalid, mark any parsed accessors as invalid to avoid
  // tripping up later invariants.
  if (invalid) {
    flagInvalidAccessor(Get);
    flagInvalidAccessor(Set);
    flagInvalidAccessor(Addressor);
    flagInvalidAccessor(MutableAddressor);
    flagInvalidAccessor(WillSet);
    flagInvalidAccessor(DidSet);
  }

  // If this is a willSet/didSet observing property, record this and we're done.
  if (WillSet || DidSet) {
    if (Get || Set) {
      P.diagnose(Get ? Get->getLoc() : Set->getLoc(),
                 diag::observingproperty_with_getset, bool(DidSet), bool(Set));
      ignoreInvalidAccessor(Get);
      ignoreInvalidAccessor(Set);
    }

    if (Addressor) {
      if (!MutableAddressor) {
        P.diagnose(WillSet ? WillSet->getLoc() : DidSet->getLoc(),
                   diag::observingproperty_without_mutableaddress,
                   bool(DidSet));
        MutableAddressor =
          createImplicitAccessor(AccessorKind::IsMutableAddressor,
                                 AddressorKind::Unsafe, nullptr);
      }

      storage->makeAddressedWithObservers(LBLoc, Addressor, MutableAddressor,
                                          WillSet, DidSet, RBLoc);
    } else if (attrs.hasAttribute<OverrideAttr>()) {
      storage->makeInheritedWithObservers(LBLoc, WillSet, DidSet, RBLoc);
    } else {
      storage->makeStoredWithObservers(LBLoc, WillSet, DidSet, RBLoc);
    }

    // Observing properties will have getters and setters synthesized by sema.
    // Create their prototypes now.
    Get = createImplicitAccessor(AccessorKind::IsGetter,
                                 AddressorKind::NotAddressor, nullptr);

    auto argFunc = (WillSet ? WillSet : DidSet);
    auto argLoc = argFunc->getParameterLists().back()->getStartLoc();

    auto argument = createSetterAccessorArgument(argLoc, Identifier(),elementTy,
                                                 AccessorKind::IsSetter, P);
    auto argList = ParameterList::create(P.Context, argument);
    Set = createImplicitAccessor(AccessorKind::IsSetter,
                                 AddressorKind::NotAddressor, argList);

    storage->setObservingAccessors(Get, Set, nullptr);
    return;
  }

  // If we have addressors, at this point mark it as addressed.
  if (Addressor) {
    assert(!Get && !Set);
    storage->makeAddressed(LBLoc, Addressor, MutableAddressor, RBLoc);
    return;
  }

  // If this is a get+mutableAddress property, synthesize an implicit
  // setter and record what we've got.
  if (MutableAddressor) {
    assert(Get && !Set);
    auto argument =
      createSetterAccessorArgument(MutableAddressor->getLoc(), Identifier(),
                                   elementTy, AccessorKind::IsSetter, P);
    auto argList = ParameterList::create(P.Context, argument);
    Set = createImplicitAccessor(AccessorKind::IsSetter,
                                 AddressorKind::NotAddressor, argList);

    storage->makeComputedWithMutableAddress(LBLoc, Get, Set, nullptr,
                                            MutableAddressor, RBLoc);
    return;
  } 

  // Otherwise, this must be a get/set property.  The set is optional,
  // but get is not.
  if (!Get) {
    // Subscripts always have to have *something*; they can't be
    // purely stored.
    if (isa<SubscriptDecl>(storage)) {
      if (!invalid) P.diagnose(LBLoc, diag::subscript_without_get);
      // Create a getter so we don't break downstream invariants by having a
      // setter without a getter.
      Get = createImplicitAccessor(AccessorKind::IsGetter,
                                   AddressorKind::NotAddressor, nullptr);
    } else if (Set) {
      if (!invalid) P.diagnose(Set->getLoc(), diag::var_set_without_get);
      // Create a getter so we don't break downstream invariants by having a
      // setter without a getter.
      Get = createImplicitAccessor(AccessorKind::IsGetter,
                                   AddressorKind::NotAddressor, nullptr);
    }
  }

  if (Set || Get) {
    if (attrs.hasAttribute<SILStoredAttr>())
      // Turn this into a stored property with trivial accessors.
      storage->addTrivialAccessors(Get, Set, nullptr);
    else
      // Turn this into a computed variable.
      storage->makeComputed(LBLoc, Get, Set, nullptr, RBLoc);
  } else {
    // Otherwise this decl is invalid and the accessors have been rejected above.
    // Make sure to at least record the braces range in the AST.
    storage->setInvalidBracesRange(SourceRange(LBLoc, RBLoc));
  }
}


/// \brief Parse a 'var' or 'let' declaration, doing no token skipping on error.
ParserResult<PatternBindingDecl>
Parser::parseDeclVar(ParseDeclOptions Flags,
                     DeclAttributes &Attributes,
                     SmallVectorImpl<Decl *> &Decls,
                     SourceLoc StaticLoc,
                     StaticSpellingKind StaticSpelling,
                     SourceLoc TryLoc) {
  assert(StaticLoc.isInvalid() || StaticSpelling != StaticSpellingKind::None);

  if (StaticLoc.isValid()) {
    if (!Flags.contains(PD_HasContainerType)) {
      diagnose(Tok, diag::static_var_decl_global_scope, StaticSpelling)
          .fixItRemove(StaticLoc);
      StaticLoc = SourceLoc();
      StaticSpelling = StaticSpellingKind::None;
    } else if (Flags.contains(PD_InStruct) || Flags.contains(PD_InEnum) ||
               Flags.contains(PD_InProtocol)) {
      if (StaticSpelling == StaticSpellingKind::KeywordClass)
        diagnose(Tok, diag::class_var_not_in_class)
            .fixItReplace(StaticLoc, "static");
    }
  }

  bool isLet = Tok.is(tok::kw_let);
  assert(Tok.getKind() == tok::kw_let || Tok.getKind() == tok::kw_var);
  SourceLoc VarLoc = consumeToken();

  // If this is a var in the top-level of script/repl source file, wrap the
  // PatternBindingDecl in a TopLevelCodeDecl, since it represents executable
  // code.  The VarDecl and any accessor decls (for computed properties) go in
  // CurDeclContext.
  //
  TopLevelCodeDecl *topLevelDecl = nullptr;
  if (allowTopLevelCode() && CurDeclContext->isModuleScopeContext()) {
    // The body of topLevelDecl will get set later.
    topLevelDecl = new (Context) TopLevelCodeDecl(CurDeclContext);
  }

  bool HasAccessors = false;  // Syntactically has accessor {}'s.
  ParserStatus Status;

  unsigned NumDeclsInResult = Decls.size();
  
  // In var/let decl with multiple patterns, accumulate them all in this list
  // so we can build our singular PatternBindingDecl at the end.
  SmallVector<PatternBindingEntry, 4> PBDEntries;
  auto BaseContext = CurDeclContext;

  // No matter what error path we take, make sure the
  // PatternBindingDecl/TopLevel code block are added.
  auto makeResult =
    [&](ParserStatus Status) -> ParserResult<PatternBindingDecl> {

    // If we didn't parse any patterns, don't create the pattern binding decl.
    if (PBDEntries.empty())
      return Status;
    
    // Now that we've parsed all of our patterns, initializers and accessors, we
    // can finally create our PatternBindingDecl to represent the
    // pattern/initializer pairs.
    auto PBD = PatternBindingDecl::create(Context, StaticLoc, StaticSpelling,
                                          VarLoc, PBDEntries, BaseContext);

    // Wire up any initializer contexts we needed.
    for (unsigned i : indices(PBDEntries)) {
      if (auto initContext = PBDEntries[i].getInitContext())
        cast<PatternBindingInitializer>(initContext)->setBinding(PBD, i);
    }

    // If we're setting up a TopLevelCodeDecl, configure it by setting up the
    // body that holds PBD and we're done.  The TopLevelCodeDecl is already set
    // up in Decls to be returned to caller.
    if (topLevelDecl) {
      PBD->setDeclContext(topLevelDecl);
      auto range = PBD->getSourceRange();
      topLevelDecl->setBody(BraceStmt::create(Context, range.Start,
                                              ASTNode(PBD), range.End, true));
      Decls.insert(Decls.begin()+NumDeclsInResult, topLevelDecl);
      return makeParserResult(Status, PBD);
    }

    // Otherwise return the PBD in "Decls" to the caller.  We add it at a
    // specific spot to get it in before any accessors, which SILGen seems to
    // want.
    Decls.insert(Decls.begin()+NumDeclsInResult, PBD);

    // Always return the result for PBD.
    return makeParserResult(Status, PBD);
  };
  
  do {
    Pattern *pattern;
    {
      // In our recursive parse, remember that we're in a var/let pattern.
      llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
      T(InVarOrLetPattern, isLet ? IVOLP_InLet : IVOLP_InVar);

      auto patternRes = parseTypedPattern();
      if (patternRes.hasCodeCompletion())
        return makeResult(makeParserCodeCompletionStatus());
      if (patternRes.isNull())
        return makeResult(makeParserError());

      pattern = patternRes.get();
    }
    
    // Configure all vars with attributes, 'static' and parent pattern.
    pattern->forEachVariable([&](VarDecl *VD) {
      VD->setStatic(StaticLoc.isValid());
      VD->getAttrs() = Attributes;
      Decls.push_back(VD);
    });

    // Remember this pattern/init pair for our ultimate PatternBindingDecl. The
    // Initializer will be added later when/if it is parsed.
    PBDEntries.push_back({pattern, nullptr, nullptr});

    Expr *PatternInit = nullptr;
    
    // Parse an initializer if present.
    if (Tok.is(tok::equal)) {
      // If we're not in a local context, we'll need a context to parse initializers
      // into (should we have one).  This happens for properties and global
      // variables in libraries.
      PatternBindingInitializer *initContext = nullptr;

      // Record the variables that we're trying to initialize.  This allows us
      // to cleanly reject "var x = x" when "x" isn't bound to an enclosing
      // decl (even though names aren't injected into scope when the initializer
      // is parsed).
      SmallVector<VarDecl *, 4> Vars;
      Vars.append(DisabledVars.begin(), DisabledVars.end());
      pattern->collectVariables(Vars);
      
      llvm::SaveAndRestore<decltype(DisabledVars)>
      RestoreCurVars(DisabledVars, Vars);

      llvm::SaveAndRestore<decltype(DisabledVarReason)>
      RestoreReason(DisabledVarReason, diag::var_init_self_referential);
      
      // If we have no local context to parse the initial value into, create one
      // for the PBD we'll eventually create.  This allows us to have reasonable
      // DeclContexts for any closures that may live inside of initializers.
      if (!CurDeclContext->isLocalContext() && !topLevelDecl && !initContext)
        initContext = new (Context) PatternBindingInitializer(CurDeclContext);

      // If we're using a local context (either a TopLevelCodeDecl or a
      // PatternBindingContext) install it now so that CurDeclContext is set
      // right when parsing the initializer.
      Optional<ParseFunctionBody> initParser;
      Optional<ContextChange> topLevelParser;
      if (topLevelDecl)
        topLevelParser.emplace(*this, topLevelDecl,
                               &State->getTopLevelContext());
      if (initContext)
        initParser.emplace(*this, initContext);

      
      SourceLoc EqualLoc = consumeToken(tok::equal);
      ParserResult<Expr> init = parseExpr(diag::expected_init_value);
      
      // If this Pattern binding was not supposed to have an initializer, but it
      // did, diagnose this and remove it.
      if (Flags & PD_DisallowInit && init.isNonNull()) {
        diagnose(EqualLoc, diag::disallowed_init);
        init = nullptr;
      }
      
      // Otherwise, if this pattern binding *was* supposed (or allowed) to have
      // an initializer, but it was a parse error, replace it with ErrorExpr so
      // that downstream clients know that it was present (well, at least the =
      // was present).  This silences downstream diagnostics checking to make
      // sure that some PBD's that require initializers actually had them.
      if (!(Flags & PD_DisallowInit) && init.isNull())
        init = makeParserResult(init, new (Context) ErrorExpr(EqualLoc));
      
      
      // Remember this init for the PatternBindingDecl.
      PatternInit = init.getPtrOrNull();
      PBDEntries.back().setInit(PatternInit);

      // If we set up an initialization context for a property or module-level
      // global, record it.
      PBDEntries.back().setInitContext(initContext);

      // If we are doing second pass of code completion, we don't want to
      // suddenly cut off parsing and throw away the declaration.
      if (init.hasCodeCompletion() && isCodeCompletionFirstPass()) {

        // Register the end of the init as the end of the delayed parsing.
        DelayedDeclEnd
          = init.getPtrOrNull() ? init.get()->getEndLoc() : SourceLoc();
        return makeResult(makeParserCodeCompletionStatus());
      }

      if (init.isNull())
        return makeResult(makeParserError());
    }
    
    // Parse a behavior block if present.
    if (Context.LangOpts.EnableExperimentalPropertyBehaviors
        && Tok.is(tok::identifier)
        && Tok.getRawText().equals("__behavior")) {
      consumeToken(tok::identifier);
      auto type = parseType(diag::expected_behavior_name,
                            /*handle completion*/ true);
      if (type.isParseError())
        return makeResult(makeParserError());
      if (type.hasCodeCompletion())
        return makeResult(makeParserCodeCompletionStatus());
      
      // Parse a following trailing closure argument.
      // FIXME: Handle generalized parameters.
      Expr *paramExpr = nullptr;
      if (Tok.is(tok::l_brace)) {
        // If we're not in a local context, we'll need a context to parse initializers
        // into (should we have one).  This happens for properties and global
        // variables in libraries.
        PatternBindingInitializer *initContext = nullptr;

        // Record the variables that we're trying to set up.  This allows us
        // to cleanly reject "var x = x" when "x" isn't bound to an enclosing
        // decl (even though names aren't injected into scope when the parameter
        // is parsed).
        SmallVector<VarDecl *, 4> Vars;
        Vars.append(DisabledVars.begin(), DisabledVars.end());
        pattern->collectVariables(Vars);
        
        llvm::SaveAndRestore<decltype(DisabledVars)>
        RestoreCurVars(DisabledVars, Vars);
        
        llvm::SaveAndRestore<decltype(DisabledVarReason)>
        RestoreReason(DisabledVarReason, diag::var_init_self_referential);

        // Set up a decl context for the closure.
        // This will be recontextualized to a method we synthesize during
        // type checking.
        if (!CurDeclContext->isLocalContext() && !topLevelDecl && !initContext)
          initContext = new (Context) PatternBindingInitializer(CurDeclContext);
        Optional<ParseFunctionBody> initParser;
        Optional<ContextChange> topLevelParser;
        if (topLevelDecl)
          topLevelParser.emplace(*this, topLevelDecl,
                                 &State->getTopLevelContext());
        if (initContext)
          initParser.emplace(*this, initContext);

        auto closure = parseExprClosure();
        PBDEntries.back().setInitContext(initContext);

        if (closure.isParseError())
          return makeResult(makeParserError());
        if (closure.hasCodeCompletion())
          return makeResult(makeParserCodeCompletionStatus());
        paramExpr = closure.get();
      }

      unsigned numVars = 0;
      pattern->forEachVariable([&](VarDecl *VD) {
        ++numVars;
        // TODO: Support parameter closure with multiple vars. This is tricky
        // since the behavior's parameter type may be dependent on the
        // property type, so we'd need to clone the closure expr for each var
        // to re-type-check it.
        if (numVars > 1 && paramExpr) {
          diagnose(paramExpr->getLoc(), diag::behavior_multiple_vars);
          paramExpr = nullptr;
        }
        
        VD->addBehavior(type.get(), paramExpr);
      });
    // If we syntactically match the second decl-var production, with a
    // var-get-set clause, parse the var-get-set clause.
    } else if (Tok.is(tok::l_brace) && !Flags.contains(PD_InLoop)) {
      HasAccessors = true;
      
      if (auto *boundVar = parseDeclVarGetSet(pattern, Flags, StaticLoc,
                                              PatternInit != nullptr,
                                              Attributes, Decls)) {
        if (PatternInit && !boundVar->hasStorage()) {
          diagnose(pattern->getLoc(), diag::getset_init)
            .highlight(PatternInit->getSourceRange());
          PatternInit = nullptr;
        }
      }
    }
    
    // Add all parsed vardecls to this scope.
    addPatternVariablesToScope(pattern);
    
    // Propagate back types for simple patterns, like "var A, B : T".
    if (TypedPattern *TP = dyn_cast<TypedPattern>(pattern)) {
      if (isa<NamedPattern>(TP->getSubPattern()) && PatternInit == nullptr) {
        for (unsigned i = PBDEntries.size() - 1; i != 0; --i) {
          Pattern *PrevPat = PBDEntries[i-1].getPattern();
          if (!isa<NamedPattern>(PrevPat) || PBDEntries[i-1].getInit())
            break;
          if (HasAccessors) {
            // FIXME -- offer a fixit to explicitly specify the type
            diagnose(PrevPat->getLoc(), diag::getset_cannot_be_implied);
            Status.setIsParseError();
          }

          TypedPattern *NewTP = new (Context) TypedPattern(PrevPat,
                                                           TP->getTypeLoc());
          NewTP->setPropagatedType();
          PBDEntries[i-1].setPattern(NewTP);
        }
      }
    }
  } while (consumeIf(tok::comma));
  
  if (HasAccessors && PBDEntries.size() > 1) {
    diagnose(VarLoc, diag::disallowed_var_multiple_getset);
    Status.setIsParseError();
  }

  if (TryLoc.isValid()) {
    auto inFlightDiag = diagnose(TryLoc, diag::try_on_var_let);

    if (PBDEntries.size() == 1 && PBDEntries.front().getInit() &&
        !isa<ErrorExpr>(PBDEntries.front().getInit())) {
      auto *init = PBDEntries.front().getInit();
      inFlightDiag.fixItRemoveChars(TryLoc, VarLoc);
      inFlightDiag.fixItInsert(init->getStartLoc(), "try ");

      // Note: We can't use TryLoc here because it's outside the PBD source
      // range.
      PBDEntries.front().setInit(new (Context) TryExpr(init->getStartLoc(),
                                                       init));
    }
  }

  return makeResult(Status);
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
           (Tok.isNot(tok::eof) && !isStartOfDecl())) {
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
///     attribute-list? ('static' | 'class')? 'mutating'? 'func' 
///               any-identifier generic-params? func-signature where-clause?
///               stmt-brace?
/// \endverbatim
///
/// \note The caller of this method must ensure that the next token is 'func'.
ParserResult<FuncDecl>
Parser::parseDeclFunc(SourceLoc StaticLoc, StaticSpellingKind StaticSpelling,
                      ParseDeclOptions Flags, DeclAttributes &Attributes) {
  assert(StaticLoc.isInvalid() || StaticSpelling != StaticSpellingKind::None);

  bool HasContainerType = Flags.contains(PD_HasContainerType);

  if (StaticLoc.isValid()) {
    if (!HasContainerType) {
      // Reject static functions at global scope.
      diagnose(Tok, diag::static_func_decl_global_scope, StaticSpelling)
          .fixItRemove(StaticLoc);
      StaticLoc = SourceLoc();
      StaticSpelling = StaticSpellingKind::None;
    } else if (Flags.contains(PD_InStruct) || Flags.contains(PD_InEnum) ||
               Flags.contains(PD_InProtocol)) {
      if (StaticSpelling == StaticSpellingKind::KeywordClass) {
        diagnose(Tok, diag::class_func_not_in_class)
            .fixItReplace(StaticLoc, "static");

        StaticSpelling = StaticSpellingKind::KeywordStatic;
      }
    }
  }

  SourceLoc FuncLoc = consumeToken(tok::kw_func);

  // Forgive the lexer
  if (Tok.is(tok::amp_prefix)) {
    Tok.setKind(tok::oper_prefix);
  }
  Identifier SimpleName;
  Token NameTok = Tok;
  SourceLoc NameLoc;

  if (Tok.is(tok::identifier) || Tok.isKeyword()) {
    ParserStatus NameStatus =
        parseIdentifierDeclName(*this, SimpleName, NameLoc, "function",
                                tok::l_paren, tok::arrow, tok::l_brace,
                                TokenProperty::StartsWithLess);
    if (NameStatus.isError())
      return nullptr;
  } else {
    // May be operator.
    if (parseAnyIdentifier(SimpleName, NameLoc,
                           diag::expected_identifier_in_decl, "function")) {
      return nullptr;
    }
    assert(SimpleName.isOperator());
    // Within a protocol, recover from a missing 'static'.
    if (Flags & PD_InProtocol) {
      switch (StaticSpelling) {
      case StaticSpellingKind::None:
        diagnose(NameLoc, diag::operator_static_in_protocol, SimpleName.str())
          .fixItInsert(FuncLoc, "static ");
        StaticSpelling = StaticSpellingKind::KeywordStatic;
        break;

      case StaticSpellingKind::KeywordStatic:
        // Okay, this is correct.
        break;

      case StaticSpellingKind::KeywordClass:
        llvm_unreachable("should have been fixed above");
      }
    }
  }

  DebuggerContextChange DCC(*this, SimpleName, DeclKind::Func);
  
  // Parse the generic-params, if present.
  Optional<Scope> GenericsScope;
  GenericsScope.emplace(this, ScopeKind::Generics);
  GenericParamList *GenericParams;
  bool GPHasCodeCompletion = false;
  // If the name is an operator token that ends in '<' and the following token
  // is an identifier, split the '<' off as a separate token. This allows things
  // like 'func ==<T>(x:T, y:T) {}' to parse as '==' with generic type variable
  // '<T>' as expected.
  if (SimpleName.str().size() > 1 && SimpleName.str().back() == '<'
      && Tok.is(tok::identifier)) {
    SimpleName = Context.getIdentifier(SimpleName.str().
                                       slice(0, SimpleName.str().size() - 1));
    SourceLoc LAngleLoc = NameLoc.getAdvancedLoc(SimpleName.str().size());
    auto Result = parseGenericParameters(LAngleLoc);
    GenericParams = Result.getPtrOrNull();
    GPHasCodeCompletion |= Result.hasCodeCompletion();

    auto NameTokText = NameTok.getRawText();
    markSplitToken(tok::identifier,
                   NameTokText.substr(0, NameTokText.size() - 1));
    markSplitToken(tok::oper_binary_unspaced,
                   NameTokText.substr(NameTokText.size() - 1));

  } else {
    auto Result = maybeParseGenericParams();
    GenericParams = Result.getPtrOrNull();
    GPHasCodeCompletion |= Result.hasCodeCompletion();
  }
  if (GPHasCodeCompletion && !CodeCompletion)
    return makeParserCodeCompletionStatus();

  SmallVector<ParameterList*, 8> BodyParams;
  
  // If we're within a container, add an implicit first pattern to match the
  // container type as an element named 'self'.
  //
  // This turns an instance function "(int)->int" on FooTy into
  // "(inout self: FooTy)->(int)->int", and a static function
  // "(int)->int" on FooTy into "(self: FooTy.Type)->(int)->int".
  // Note that we can't actually compute the type here until Sema.
  if (HasContainerType)
    BodyParams.push_back(ParameterList::createUnboundSelf(NameLoc, CurDeclContext));

  DefaultArgumentInfo DefaultArgs(HasContainerType);
  TypeRepr *FuncRetTy = nullptr;
  DeclName FullName;
  SourceLoc throwsLoc;
  bool rethrows;
  ParserStatus SignatureStatus =
      parseFunctionSignature(SimpleName, FullName, BodyParams, DefaultArgs,
                             throwsLoc, rethrows, FuncRetTy);

  if (SignatureStatus.hasCodeCompletion() && !CodeCompletion) {
    // Trigger delayed parsing, no need to continue.
    return SignatureStatus;
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // Parse a 'where' clause if present, adding it to our GenericParamList.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(GenericParams);
    if (whereStatus.shouldStopParsing())
      return whereStatus;
  }
  
  // Protocol method arguments may not have default values.
  if (Flags.contains(PD_InProtocol) && DefaultArgs.HasDefaultArgument) {
    diagnose(FuncLoc, diag::protocol_method_argument_init);
    return nullptr;
  }

  // Add the 'rethrows' attribute.
  if (rethrows) {
    Attributes.add(new (Context) RethrowsAttr(throwsLoc));
  }
  
  // Enter the arguments for the function into a new function-body scope.  We
  // need this even if there is no function body to detect argument name
  // duplication.
  FuncDecl *FD;
  {
    Scope S(this, ScopeKind::FunctionBody);

    // Create the decl for the func and add it to the parent scope.
    FD = FuncDecl::create(Context, StaticLoc, StaticSpelling,
                          FuncLoc, FullName, NameLoc,
                          /*Throws=*/throwsLoc.isValid(), throwsLoc,
                          /*AccessorKeywordLoc=*/SourceLoc(),
                          GenericParams, BodyParams, FuncRetTy,
                          CurDeclContext);
    
    diagnoseOperatorFixityAttributes(*this, Attributes, FD);
    
    // Add the attributes here so if we need them while parsing the body
    // they are available.
    FD->getAttrs() = Attributes;

    // Code completion for the generic type params.
    if (GPHasCodeCompletion)
      CodeCompletion->setDelayedParsedDecl(FD);

    // Pass the function signature to code completion.
    if (SignatureStatus.hasCodeCompletion())
      CodeCompletion->setDelayedParsedDecl(FD);

    DefaultArgs.setFunctionContext(FD);
    for (auto PL : FD->getParameterLists())
      addParametersToScope(PL);
    setLocalDiscriminator(FD);
    
    // Establish the new context.
    ParseFunctionBody CC(*this, FD);

    // Check to see if we have a "{" to start a brace statement.
    if (Tok.is(tok::l_brace)) {
      // Record the curly braces but nothing inside.
      SF.recordInterfaceToken("{");
      SF.recordInterfaceToken("}");
      llvm::SaveAndRestore<bool> T(IsParsingInterfaceTokens, false);

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

  addToScope(FD);
  return DCC.fixupParserResult(FD);
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

  // Create a lexer that cannot go past the end state.
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
///          where-clause? '{' decl-enum-body '}'
///   decl-enum-body:
///      decl*
/// \endverbatim
ParserResult<EnumDecl> Parser::parseDeclEnum(ParseDeclOptions Flags,
                                             DeclAttributes &Attributes) {
  SourceLoc EnumLoc = consumeToken(tok::kw_enum);

  Identifier EnumName;
  SourceLoc EnumNameLoc;
  ParserStatus Status;

  Status |= parseIdentifierDeclName(*this, EnumName, EnumNameLoc, "enum",
                                    tok::colon, tok::l_brace,
                                    TokenProperty::StartsWithLess);
  if (Status.isError())
    return nullptr;

  DebuggerContextChange DCC(*this, EnumName, DeclKind::Enum);
  
  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    Scope S(this, ScopeKind::Generics);
    auto Result = maybeParseGenericParams();
    GenericParams = Result.getPtrOrNull();
    if (Result.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
  }

  EnumDecl *ED = new (Context) EnumDecl(EnumLoc, EnumName, EnumNameLoc,
                                        { }, GenericParams, CurDeclContext);
  setLocalDiscriminator(ED);
  ED->getAttrs() = Attributes;

  // Parse optional inheritance clause within the context of the enum.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, ED);
    SmallVector<TypeLoc, 2> Inherited;
    Status |= parseInheritance(Inherited, /*classRequirementLoc=*/nullptr);
    ED->setInherited(Context.AllocateCopy(Inherited));
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);
  
  // Parse a 'where' clause if present, adding it to our GenericParamList.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(GenericParams);
    if (whereStatus.shouldStopParsing())
      return whereStatus;
    ED->setGenericParams(GenericParams);
  }

  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_enum)) {
    LBLoc = PreviousLoc;
    RBLoc = LBLoc;
    Status.setIsParseError();
  } else {
    ContextChange CC(*this, ED);
    Scope S(this, ScopeKind::ClassBody);
    ParseDeclOptions Options(PD_HasContainerType | PD_AllowEnumElement | PD_InEnum);
    if (parseDeclList(LBLoc, RBLoc, diag::expected_rbrace_enum,
                      Options, [&] (Decl *D) { ED->addMember(D); }))
      Status.setIsParseError();
  }

  ED->setBraces({LBLoc, RBLoc});

  addToScope(ED);

  return DCC.fixupParserResult(Status, ED);
}

/// \brief Parse a 'case' of an enum.
///
/// \verbatim
///   enum-case:
///      identifier type-tuple?
///   decl-enum-element:
///      'case' attribute-list enum-case (',' enum-case)*
/// \endverbatim
ParserResult<EnumCaseDecl>
Parser::parseDeclEnumCase(ParseDeclOptions Flags,
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

    // Consume an extraneous '.' so we can recover the case name.
    SourceLoc DotLoc;
    consumeIf(tok::period_prefix, DotLoc);

    // Handle the likely case someone typed 'case X, case Y'.
    if (Tok.is(tok::kw_case) && CommaLoc.isValid()) {
      diagnose(Tok, diag::expected_identifier_after_case_comma);
      Status.setIsParseError();
      return Status;
    }

    if (Tok.is(tok::identifier)) {
      Status |= parseIdentifierDeclName(*this, Name, NameLoc, "enum 'case'",
                                        tok::l_paren, tok::kw_case, tok::colon,
                                        tok::r_brace);
      assert(Status.isSuccess());
      if (DotLoc.isValid())
        diagnose(DotLoc, diag::enum_case_dot_prefix)
          .fixItRemove(DotLoc);
    } else {
      NameLoc = CaseLoc;
      bool NameIsKeyword = Tok.isKeyword();
      SourceLoc TokLoc = Tok.getLoc();
      StringRef TokText = Tok.getText();

      // For recovery, see if the user typed something resembling a switch
      // "case" label.
      llvm::SaveAndRestore<decltype(InVarOrLetPattern)>
      T(InVarOrLetPattern, Parser::IVOLP_InMatchingPattern);
      parseMatchingPattern(/*isExprBasic*/false);

      if (consumeIf(tok::colon)) {
        diagnose(CaseLoc, diag::case_outside_of_switch, "case");
        Status.setIsParseError();
        return Status;
      }
      if (CommaLoc.isValid()) {
        diagnose(Tok, diag::expected_identifier_after_case_comma);
        Status.setIsParseError();
        return Status;
      }
      if (NameIsKeyword) {
        diagnose(TokLoc, diag::keyword_cant_be_identifier, TokText);
        diagnose(TokLoc, diag::backticks_to_escape)
          .fixItReplace(TokLoc, "`" + TokText.str() + "`");
      } else {
        diagnose(CaseLoc, diag::expected_identifier_in_decl, "enum 'case'");
      }
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
        if (!CurLocalContext) {
          // A local context is needed for parsing closures. We want to parse
          // them anyways for proper diagnosis.
          LocalContext tempContext{};
          CurLocalContext = &tempContext;
          RawValueExpr = parseExpr(diag::expected_expr_enum_case_raw_value);
          CurLocalContext = nullptr;
        } else {
          RawValueExpr = parseExpr(diag::expected_expr_enum_case_raw_value);
        }
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
    
    // For recovery, again make sure the user didn't try to spell a switch
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
    TypeRepr *ArgTR = ArgType.getPtrOrNull();
    auto *result = new (Context) EnumElementDecl(NameLoc, Name,
                                                 ArgTR,
                                                 ArgTR != nullptr,
                                                 EqualsLoc,
                                                 LiteralRawValueExpr,
                                                 CurDeclContext);
    if (NameLoc == CaseLoc) {
      result->setImplicit(); // Parse error
    }

    result->getAttrs() = Attributes;
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
  return makeParserResult(Status, TheCase);
}

/// \brief Parse a 'struct' declaration, returning true (and doing no token
/// skipping) on error.
///
/// \verbatim
///   decl-struct:
///      'struct' attribute-list identifier generic-params? inheritance?
///          where-clause? '{' decl-struct-body '}
///   decl-struct-body:
///      decl*
/// \endverbatim
ParserResult<StructDecl> Parser::parseDeclStruct(ParseDeclOptions Flags,
                                                 DeclAttributes &Attributes) {
  SourceLoc StructLoc = consumeToken(tok::kw_struct);
  
  Identifier StructName;
  SourceLoc StructNameLoc;
  ParserStatus Status;

  Status |= parseIdentifierDeclName(*this, StructName, StructNameLoc, "struct",
                                    tok::colon, tok::l_brace,
                                    TokenProperty::StartsWithLess);
  if (Status.isError())
    return nullptr;

  DebuggerContextChange DCC (*this, StructName, DeclKind::Struct);
  
  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    Scope S(this, ScopeKind::Generics);
    auto Result = maybeParseGenericParams();
    GenericParams = Result.getPtrOrNull();
    if (Result.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
  }

  StructDecl *SD = new (Context) StructDecl(StructLoc, StructName,
                                            StructNameLoc,
                                            { },
                                            GenericParams,
                                            CurDeclContext);
  setLocalDiscriminator(SD);
  SD->getAttrs() = Attributes;

  // Parse optional inheritance clause within the context of the struct.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, SD);
    SmallVector<TypeLoc, 2> Inherited;
    Status |= parseInheritance(Inherited, /*classRequirementLoc=*/nullptr);
    SD->setInherited(Context.AllocateCopy(Inherited));
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // Parse a 'where' clause if present, adding it to our GenericParamList.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(GenericParams);
    if (whereStatus.shouldStopParsing())
      return whereStatus;
    SD->setGenericParams(GenericParams);
  }

  
  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_struct)) {
    LBLoc = PreviousLoc;
    RBLoc = LBLoc;
    Status.setIsParseError();
  } else {
    // Parse the body.
    ContextChange CC(*this, SD);
    Scope S(this, ScopeKind::StructBody);
    ParseDeclOptions Options(PD_HasContainerType | PD_InStruct);
    if (parseDeclList(LBLoc, RBLoc, diag::expected_rbrace_struct,
                      Options, [&](Decl *D) {SD->addMember(D);}))
      Status.setIsParseError();
  }

  SD->setBraces({LBLoc, RBLoc});

  addToScope(SD);

  return DCC.fixupParserResult(Status, SD);
}

/// \brief Parse a 'class' declaration, doing no token skipping on error.
///
/// \verbatim
///   decl-class:
///      'class' attribute-list identifier generic-params? inheritance?
///          where-clause? '{' decl-class-body '}
///   decl-class-body:
///      decl*
/// \endverbatim
ParserResult<ClassDecl> Parser::parseDeclClass(SourceLoc ClassLoc,
                                               ParseDeclOptions Flags,
                                               DeclAttributes &Attributes) {
  Identifier ClassName;
  SourceLoc ClassNameLoc;
  ParserStatus Status;

  Status |= parseIdentifierDeclName(*this, ClassName, ClassNameLoc, "class",
                                    tok::colon, tok::l_brace,
                                    TokenProperty::StartsWithLess);
  if (Status.isError())
    return nullptr;

  DebuggerContextChange DCC (*this, ClassName, DeclKind::Class);
  
  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    Scope S(this, ScopeKind::Generics);
    auto Result = maybeParseGenericParams();
    GenericParams = Result.getPtrOrNull();
    if (Result.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
  }

  // Create the class.
  ClassDecl *CD = new (Context) ClassDecl(ClassLoc, ClassName, ClassNameLoc,
                                          { }, GenericParams, CurDeclContext);
  setLocalDiscriminator(CD);

  // Attach attributes.
  CD->getAttrs() = Attributes;

  // Parse optional inheritance clause within the context of the class.
  if (Tok.is(tok::colon)) {
    ContextChange CC(*this, CD);
    SmallVector<TypeLoc, 2> Inherited;
    Status |= parseInheritance(Inherited, /*classRequirementLoc=*/nullptr);
    CD->setInherited(Context.AllocateCopy(Inherited));
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // Parse a 'where' clause if present, adding it to our GenericParamList.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(GenericParams);
    if (whereStatus.shouldStopParsing())
      return whereStatus;
    CD->setGenericParams(GenericParams);
  }

  SourceLoc LBLoc, RBLoc;
  if (parseToken(tok::l_brace, LBLoc, diag::expected_lbrace_class)) {
    LBLoc = PreviousLoc;
    RBLoc = LBLoc;
    Status.setIsParseError();
  } else {
    // Parse the body.
    ContextChange CC(*this, CD);
    Scope S(this, ScopeKind::ClassBody);
    ParseDeclOptions Options(PD_HasContainerType | PD_AllowDestructor |
                             PD_InClass);
    auto Handler = [&] (Decl *D) {
      CD->addMember(D);
      if (isa<DestructorDecl>(D))
        CD->setHasDestructor();
    };
    if (parseDeclList(LBLoc, RBLoc, diag::expected_rbrace_class,
                      Options, Handler))
      Status.setIsParseError();
  }

  CD->setBraces({LBLoc, RBLoc});

  addToScope(CD);

  return DCC.fixupParserResult(Status, CD);
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

  Status |= parseIdentifierDeclName(*this, ProtocolName, NameLoc, "protocol",
                                    tok::colon, tok::l_brace);
  if (Status.isError())
    return nullptr;

  // Protocols don't support generic parameters, but people often want them and
  // we want to have good error recovery if they try them out.  Parse them and
  // produce a specific diagnostic if present.
  if (startsWithLess(Tok)) {
    diagnose(Tok, diag::generic_arguments_protocol);
    Scope S(this, ScopeKind::Generics);
    maybeParseGenericParams();
  }

  DebuggerContextChange DCC (*this);
  
  // Parse optional inheritance clause.
  SmallVector<TypeLoc, 4> InheritedProtocols;
  SourceLoc classRequirementLoc;
  SourceLoc colonLoc;
  if (Tok.is(tok::colon)) {
    colonLoc = Tok.getLoc();
    Status |= parseInheritance(InheritedProtocols, &classRequirementLoc);
  }

  // Parse a 'where' clause if present. These are not supported, but we will
  // get better QoI this way.
  if (Tok.is(tok::kw_where)) {
    GenericParamList *unused = nullptr;
    auto whereStatus = parseFreestandingGenericWhereClause(
        unused, WhereClauseKind::Protocol);
    if (whereStatus.shouldStopParsing())
      return whereStatus;
  }

  ProtocolDecl *Proto
    = new (Context) ProtocolDecl(CurDeclContext, ProtocolLoc, NameLoc,
                                 ProtocolName,
                                 Context.AllocateCopy(InheritedProtocols));
  // No need to setLocalDiscriminator: protocols can't appear in local contexts.

  // If there was a 'class' requirement, mark this as a class-bounded protocol.
  if (classRequirementLoc.isValid())
    Proto->setRequiresClass();

  Proto->getAttrs() = Attributes;

  ContextChange CC(*this, Proto);
  Scope ProtocolBodyScope(this, ScopeKind::ProtocolBody);

  // Parse the body.
  {
    SourceLoc LBraceLoc;
    SourceLoc RBraceLoc;
    if (parseToken(tok::l_brace, LBraceLoc, diag::expected_lbrace_protocol)) {
      LBraceLoc = PreviousLoc;
      RBraceLoc = LBraceLoc;
      Status.setIsParseError();
    } else {
      // Parse the members.
      ParseDeclOptions Options(PD_HasContainerType |
                               PD_DisallowInit |
                               PD_InProtocol);
      if (parseDeclList(LBraceLoc, RBraceLoc, diag::expected_rbrace_protocol,
                        Options, [&](Decl *D) {Proto->addMember(D);}))
        Status.setIsParseError();
    }

    // Install the protocol elements.
    Proto->setBraces({LBraceLoc, RBraceLoc});
  }
  
  return DCC.fixupParserResult(Status, Proto);
}

/// \brief Parse a 'subscript' declaration.
///
/// \verbatim
///   decl-subscript:
///     subscript-head get-set
///   subscript-head
///     attribute-list? 'subscript' parameter-clause '->' type
/// \endverbatim
ParserResult<SubscriptDecl>
Parser::parseDeclSubscript(ParseDeclOptions Flags,
                           DeclAttributes &Attributes,
                           SmallVectorImpl<Decl *> &Decls) {
  ParserStatus Status;
  SourceLoc SubscriptLoc = consumeToken(tok::kw_subscript);

  // Parse the generic-params, if present.
  Optional<Scope> GenericsScope;
  GenericsScope.emplace(this, ScopeKind::Generics);
  GenericParamList *GenericParams;
  bool GPHasCodeCompletion = false;

  auto Result = maybeParseGenericParams();
  GenericParams = Result.getPtrOrNull();
  GPHasCodeCompletion |= Result.hasCodeCompletion();

  if (GPHasCodeCompletion && !CodeCompletion)
    return makeParserCodeCompletionStatus();

  // Parse the parameter list.
  SmallVector<Identifier, 4> argumentNames;
  ParserResult<ParameterList> Indices
    = parseSingleParameterClause(ParameterContextKind::Subscript,
                                 &argumentNames);
  if (Indices.isNull() || Indices.hasCodeCompletion())
    return ParserStatus(Indices);
  
  // '->'
  if (!Tok.is(tok::arrow)) {
    if (!Indices.isParseError())
      diagnose(Tok, diag::expected_arrow_subscript);
    return makeParserError();
  }
  SourceLoc ArrowLoc = consumeToken();
  
  // type
  ParserResult<TypeRepr> ElementTy = parseType(diag::expected_type_subscript);
  if (ElementTy.isNull() || ElementTy.hasCodeCompletion())
    return ParserStatus(ElementTy);

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // Parse a 'where' clause if present, adding it to our GenericParamList.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(GenericParams);
    if (whereStatus.shouldStopParsing())
      return whereStatus;
  }

  // Build an AST for the subscript declaration.
  DeclName name = DeclName(Context, Context.Id_subscript, argumentNames);
  auto *Subscript = new (Context) SubscriptDecl(name,
                                                SubscriptLoc, Indices.get(),
                                                ArrowLoc, ElementTy.get(),
                                                CurDeclContext,
                                                GenericParams);
  Subscript->getAttrs() = Attributes;

  // Code completion for the generic type params.
  //
  // FIXME: What is this?
  if (GPHasCodeCompletion)
    CodeCompletion->setDelayedParsedDecl(Subscript);

  Decls.push_back(Subscript);

  // '{'
  // Parse getter and setter.
  ParsedAccessors accessors;
  if (Tok.isNot(tok::l_brace)) {
    // Subscript declarations must always have at least a getter, so they need
    // to be followed by a {.
    if (Flags.contains(PD_InProtocol))
      diagnose(Tok, diag::expected_lbrace_subscript_protocol)
        .fixItInsertAfter(ElementTy.get()->getEndLoc(), " { get set }");
    else
      diagnose(Tok, diag::expected_lbrace_subscript);
    Status.setIsParseError();
  } else {
    if (parseGetSet(Flags, GenericParams,
                    Indices.get(), ElementTy.get(),
                    accessors, /*StaticLoc=*/SourceLoc(), Decls))
      Status.setIsParseError();
  }

  bool Invalid = false;
  // Reject 'subscript' functions outside of type decls
  if (!(Flags & PD_HasContainerType)) {
    diagnose(SubscriptLoc, diag::subscript_decl_wrong_scope);
    Invalid = true;
  }

  accessors.record(*this, Subscript, (Invalid || !Status.isSuccess()),
                   Flags, /*static*/ SourceLoc(), Attributes,
                   ElementTy.get(), Indices.get(), Decls);

  if (Invalid) {
    Subscript->setInterfaceType(ErrorType::get(Context));
    Subscript->setInvalid();
  }

  // No need to setLocalDiscriminator because subscripts cannot
  // validly appear outside of type decls.
  return makeParserResult(Status, Subscript);
}

ParserResult<ConstructorDecl>
Parser::parseDeclInit(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  assert(Tok.is(tok::kw_init));
  SourceLoc ConstructorLoc = consumeToken();
  OptionalTypeKind Failability = OTK_None;
  SourceLoc FailabilityLoc;

  const bool ConstructorsNotAllowed = !(Flags & PD_HasContainerType);

  // Reject constructors outside of types.
  if (ConstructorsNotAllowed) {
    diagnose(Tok, diag::initializer_decl_wrong_scope);
  }

  // Parse the '!' or '?' for a failable initializer.
  if (Tok.isAny(tok::exclaim_postfix, tok::sil_exclamation) ||
      (Tok.isAnyOperator() && Tok.getText() == "!")) {
    Failability = OTK_ImplicitlyUnwrappedOptional;
    FailabilityLoc = consumeToken();
  } else if (Tok.isAny(tok::question_postfix, tok::question_infix)) {
    Failability = OTK_Optional;
    FailabilityLoc = consumeToken();
  }

  // Parse the generic-params, if present.
  Scope S(this, ScopeKind::Generics);
  auto GPResult = maybeParseGenericParams();
  GenericParamList *GenericParams = GPResult.getPtrOrNull();
  if (GPResult.hasCodeCompletion())
    return makeParserCodeCompletionStatus();

  // Parse the parameters.
  DefaultArgumentInfo DefaultArgs(/*inTypeContext*/true);
  llvm::SmallVector<Identifier, 4> namePieces;
  ParserResult<ParameterList> Params
    = parseSingleParameterClause(ParameterContextKind::Initializer,
                                 &namePieces, &DefaultArgs);

  if (Params.hasCodeCompletion() && !CodeCompletion) {
    // Trigger delayed parsing, no need to continue.
    return makeParserCodeCompletionStatus();
  }

  // Protocol initializer arguments may not have default values.
  if (Flags.contains(PD_InProtocol) && DefaultArgs.HasDefaultArgument) {
    diagnose(ConstructorLoc, diag::protocol_init_argument_init);
    return nullptr;
  }

  // Parse 'throws' or 'rethrows'.
  SourceLoc throwsLoc;
  if (consumeIf(tok::kw_throws, throwsLoc)) {
    // okay
  } else if (consumeIf(tok::kw_rethrows, throwsLoc)) {
    Attributes.add(new (Context) RethrowsAttr(throwsLoc));
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // Parse a 'where' clause if present, adding it to our GenericParamList.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(GenericParams);
    if (whereStatus.shouldStopParsing())
      return whereStatus;
  }
  
  auto *SelfDecl = ParamDecl::createUnboundSelf(ConstructorLoc, CurDeclContext);
  DeclName FullName(Context, Context.Id_init, namePieces);

  Scope S2(this, ScopeKind::ConstructorBody);
  auto *CD = new (Context) ConstructorDecl(FullName, ConstructorLoc,
                                           Failability, FailabilityLoc,
                                           throwsLoc.isValid(), throwsLoc,
                                           SelfDecl, Params.get(),
                                           GenericParams,
                                           CurDeclContext);
  
  CtorInitializerKind initKind = CtorInitializerKind::Designated;
  if (Attributes.hasAttribute<ConvenienceAttr>())
    initKind = CtorInitializerKind::Convenience;
  CD->setInitKind(initKind);

  // No need to setLocalDiscriminator.

  DefaultArgs.setFunctionContext(CD);

  // Pass the function signature to code completion.
  if (Params.hasCodeCompletion())
    CodeCompletion->setDelayedParsedDecl(CD);

  if (ConstructorsNotAllowed || Params.isParseError()) {
    // Tell the type checker not to touch this constructor.
    CD->setInvalid();
  }
  
  addToScope(SelfDecl);
  addParametersToScope(Params.get());

  // '{'
  if (Tok.is(tok::l_brace)) {
    // Record the curly braces but nothing inside.
    SF.recordInterfaceToken("{");
    SF.recordInterfaceToken("}");
    llvm::SaveAndRestore<bool> T(IsParsingInterfaceTokens, false);

    if (Flags.contains(PD_InProtocol)) {
      diagnose(Tok, diag::protocol_init_with_body);
      skipUntilDeclRBrace();
    } else {
      // Parse the body.
      ParseFunctionBody CC(*this, CD);

      if (!isDelayedParsingEnabled()) {
        ParserResult<BraceStmt> Body =
          parseBraceItemList(diag::invalid_diagnostic);

        if (!Body.isNull())
          CD->setBody(Body.get());
      } else {
        consumeAbstractFunctionBody(CD, Attributes);
      }
    }
  }

  CD->getAttrs() = Attributes;

  return makeParserResult(CD);
}

ParserResult<DestructorDecl> Parser::
parseDeclDeinit(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  SourceLoc DestructorLoc = consumeToken(tok::kw_deinit);

  // Parse extraneous parentheses and remove them with a fixit.
  if (Tok.is(tok::l_paren)) {
    SourceRange ParenRange;
    SourceLoc LParenLoc = consumeToken();
    SourceLoc RParenLoc;
    skipUntil(tok::r_paren);

    if (Tok.is(tok::r_paren)) {
      SourceLoc RParenLoc = consumeToken();
      ParenRange = SourceRange(LParenLoc, RParenLoc);
      
      diagnose(ParenRange.Start, diag::destructor_params)
      .fixItRemoveChars(Lexer::getLocForEndOfToken(Context.SourceMgr,
                                                   DestructorLoc),
                        Lexer::getLocForEndOfToken(Context.SourceMgr,
                                                   ParenRange.End));
    } else {
      diagnose(Tok, diag::opened_destructor_expected_rparen);
      diagnose(LParenLoc, diag::opening_paren);
    }
  }

  // '{'
  if (!Tok.is(tok::l_brace)) {
    if (!Tok.is(tok::l_brace) && !isInSILMode()) {
      if (Tok.is(tok::identifier)) {
        diagnose(Tok, diag::destructor_has_name).fixItRemove(Tok.getLoc());
      } else
        diagnose(Tok, diag::expected_lbrace_destructor);
      return nullptr;
    }
  }

  auto *SelfDecl = ParamDecl::createUnboundSelf(DestructorLoc, CurDeclContext);

  Scope S(this, ScopeKind::DestructorBody);
  auto *DD = new (Context) DestructorDecl(Context.Id_deinit, DestructorLoc,
                                          SelfDecl, CurDeclContext);

  // Parse the body.
  if (Tok.is(tok::l_brace)) {
    // Record the curly braces but nothing inside.
    SF.recordInterfaceToken("{");
    SF.recordInterfaceToken("}");
    llvm::SaveAndRestore<bool> T(IsParsingInterfaceTokens, false);

    ParseFunctionBody CC(*this, DD);
    if (!isDelayedParsingEnabled()) {
      ParserResult<BraceStmt> Body=parseBraceItemList(diag::invalid_diagnostic);

      if (!Body.isNull())
        DD->setBody(Body.get());
    } else {
      consumeAbstractFunctionBody(DD, Attributes);
    }
  }

  DD->getAttrs() = Attributes;

  // Reject 'destructor' functions outside of classes
  if (!(Flags & PD_AllowDestructor)) {
    diagnose(DestructorLoc, diag::destructor_decl_outside_class);

    // Tell the type checker not to touch this destructor.
    DD->setInvalid();
  }

  return makeParserResult(DD);
}

ParserResult<OperatorDecl> 
Parser::parseDeclOperator(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  SourceLoc OperatorLoc = consumeToken(tok::kw_operator);
  bool AllowTopLevel = Flags.contains(PD_AllowTopLevel);

  if (!Tok.isAnyOperator() && !Tok.is(tok::exclaim_postfix)) {
    // A common error is to try to define an operator with something in the
    // unicode plane considered to be an operator, or to try to define an
    // operator like "not".  Diagnose this specifically.
    if (Tok.is(tok::identifier))
      diagnose(Tok, diag::identifier_when_expecting_operator,
               Context.getIdentifier(Tok.getText()));
    else
      diagnose(Tok, diag::expected_operator_name_after_operator);

    // To improve recovery, check to see if we have a { right after this token.
    // If so, swallow until the end } to avoid tripping over the body of the
    // malformed operator decl.
    if (peekToken().is(tok::l_brace)) {
      consumeToken();
      skipSingle();
    }

    return nullptr;
  }

  DebuggerContextChange DCC (*this);
  
  Identifier Name = Context.getIdentifier(Tok.getText());
  SourceLoc NameLoc = consumeToken();
    
  if (Attributes.hasAttribute<PostfixAttr>()) {
    if (!Name.empty() && (Name.get()[0] == '?' || Name.get()[0] == '!'))
      diagnose(NameLoc, diag::expected_operator_name_after_operator);      
  }
  
  auto Result = parseDeclOperatorImpl(OperatorLoc, Name, NameLoc, Attributes);

  if (!DCC.movedToTopLevel() && !AllowTopLevel) {
    diagnose(OperatorLoc, diag::operator_decl_inner_scope);
    return nullptr;
  }
  
  return DCC.fixupParserResult(Result);
}

ParserResult<OperatorDecl>
Parser::parseDeclOperatorImpl(SourceLoc OperatorLoc, Identifier Name,
                                SourceLoc NameLoc, DeclAttributes &Attributes) {
  bool isPrefix = Attributes.hasAttribute<PrefixAttr>();
  bool isInfix = Attributes.hasAttribute<InfixAttr>();
  bool isPostfix = Attributes.hasAttribute<PostfixAttr>();
  
  // Parse (or diagnose) a specified precedence group.
  SourceLoc colonLoc;
  Identifier precedenceGroupName;
  SourceLoc precedenceGroupNameLoc;
  if (consumeIf(tok::colon, colonLoc)) {
    if (Tok.is(tok::identifier)) {
      precedenceGroupName = Context.getIdentifier(Tok.getText());
      precedenceGroupNameLoc = consumeToken(tok::identifier);
      
      if (isPrefix || isPostfix)
        diagnose(colonLoc, diag::precedencegroup_not_infix)
          .fixItRemove({colonLoc, precedenceGroupNameLoc});
    }
  }
  
  // Diagnose deprecated operator body syntax `operator + { ... }`.
  SourceLoc lBraceLoc;
  if (consumeIf(tok::l_brace, lBraceLoc)) {
    if (isInfix && !Tok.is(tok::r_brace)) {
      diagnose(lBraceLoc, diag::deprecated_operator_body_use_group);
    } else {
      auto Diag = diagnose(lBraceLoc, diag::deprecated_operator_body);
      if (Tok.is(tok::r_brace)) {
        SourceLoc lastGoodLoc = precedenceGroupNameLoc;
        if (lastGoodLoc.isInvalid())
          lastGoodLoc = NameLoc;
        SourceLoc lastGoodLocEnd = Lexer::getLocForEndOfToken(SourceMgr,
                                                              lastGoodLoc);
        SourceLoc rBraceEnd = Lexer::getLocForEndOfToken(SourceMgr, Tok.getLoc());
        Diag.fixItRemoveChars(lastGoodLocEnd, rBraceEnd);
      }
    }

    skipUntilDeclRBrace();
    (void) consumeIf(tok::r_brace);
  }
  
  OperatorDecl *res;
  if (Attributes.hasAttribute<PrefixAttr>())
    res = new (Context) PrefixOperatorDecl(CurDeclContext, OperatorLoc,
                                           Name, NameLoc);
  else if (Attributes.hasAttribute<PostfixAttr>())
    res = new (Context) PostfixOperatorDecl(CurDeclContext, OperatorLoc,
                                            Name, NameLoc);
  else
    res = new (Context) InfixOperatorDecl(CurDeclContext, OperatorLoc,
                                          Name, NameLoc, colonLoc,
                                          precedenceGroupName,
                                          precedenceGroupNameLoc);
  
  diagnoseOperatorFixityAttributes(*this, Attributes, res);
  
  res->getAttrs() = Attributes;
  return makeParserResult(res);
}

ParserResult<PrecedenceGroupDecl>
Parser::parseDeclPrecedenceGroup(ParseDeclOptions flags,
                                 DeclAttributes &attributes) {
  SourceLoc precedenceGroupLoc = consumeToken(tok::kw_precedencegroup);
  DebuggerContextChange DCC (*this);

  if (!CodeCompletion && !DCC.movedToTopLevel() && !(flags & PD_AllowTopLevel))
  {
    diagnose(precedenceGroupLoc, diag::decl_inner_scope);
    return nullptr;
  }

  Identifier name;
  SourceLoc nameLoc;
  if (parseIdentifier(name, nameLoc, diag::expected_precedencegroup_name)) {
    // If the identifier is missing or a keyword or something, try to skip
    // skip the entire body.
    if (consumeIf(tok::l_brace)) {
      skipUntilDeclRBrace();
      (void) consumeIf(tok::r_brace);
    } else if (Tok.isNot(tok::eof) && peekToken().is(tok::l_brace)) {
      consumeToken();
      skipBracedBlock(*this);
    }
    return nullptr;
  }

  SourceLoc lbraceLoc, rbraceLoc;
  SourceLoc associativityKeywordLoc, associativityValueLoc;
  SourceLoc assignmentKeywordLoc, assignmentValueLoc;
  SourceLoc higherThanKeywordLoc, lowerThanKeywordLoc;
  SmallVector<PrecedenceGroupDecl::Relation, 4> higherThan, lowerThan;
  Associativity associativity = Associativity::None;
  bool assignment = false;
  bool invalid = false;

  // Helper functions.
  auto create = [&] {
    auto result = PrecedenceGroupDecl::create(CurDeclContext,
                                              precedenceGroupLoc,
                                              nameLoc, name, lbraceLoc,
                                              associativityKeywordLoc,
                                              associativityValueLoc,
                                              associativity,
                                              assignmentKeywordLoc,
                                              assignmentValueLoc,
                                              assignment,
                                              higherThanKeywordLoc, higherThan,
                                              lowerThanKeywordLoc, lowerThan,
                                              rbraceLoc);
    result->getAttrs() = attributes;
    return result;
  };
  auto createInvalid = [&] {
    // Use the last consumed token location as the rbrace to satisfy
    // the AST invariant about a decl's source range including all of
    // its components.
    if (!rbraceLoc.isValid()) rbraceLoc = PreviousLoc;

    auto result = create();
    result->setInvalid();
    return makeParserErrorResult(result);
  };

  // Expect the body to start here.
  if (!consumeIf(tok::l_brace, lbraceLoc)) {
    diagnose(Tok, diag::expected_precedencegroup_lbrace);
    return createInvalid();
  }

  auto abortBody = [&] {
    skipUntilDeclRBrace();
    (void) consumeIf(tok::r_brace, rbraceLoc);
    return createInvalid();
  };

  auto parseAttributePrefix = [&](SourceLoc &attrKeywordLoc) {
    auto attrName = Tok.getText(); 
    if (attrKeywordLoc.isValid()) {
      diagnose(Tok, diag::precedencegroup_attribute_redeclared, attrName);
      // We want to continue parsing after this.
      invalid = true;
    }
    attrKeywordLoc = consumeToken(tok::identifier);
    if (!consumeIf(tok::colon)) {
      diagnose(Tok, diag::expected_precedencegroup_attribute_colon, attrName);
      // Try to recover by allowing the colon to be missing.
    }
  };

  // Parse the attributes in the body.
  while (!consumeIf(tok::r_brace, rbraceLoc)) {
    if (!Tok.is(tok::identifier)) {
      diagnose(Tok, diag::expected_precedencegroup_attribute);
      return abortBody();
    }

    auto attrName = Tok.getText();

    if (attrName == "associativity") {
      parseAttributePrefix(associativityKeywordLoc);

      if (!Tok.is(tok::identifier)) {
        diagnose(Tok, diag::expected_precedencegroup_associativity);
        return abortBody();
      }
      auto parsedAssociativity
        = llvm::StringSwitch<Optional<Associativity>>(Tok.getText())
          .Case("none", Associativity::None)
          .Case("left", Associativity::Left)
          .Case("right", Associativity::Right)
          .Default(None);
      if (!parsedAssociativity) {
        diagnose(Tok, diag::expected_precedencegroup_associativity);
        parsedAssociativity = Associativity::None;
        invalid = true;
      }
      associativity = *parsedAssociativity;
      associativityValueLoc = consumeToken();
      continue;
    }
    
    if (attrName == "assignment") {
      parseAttributePrefix(assignmentKeywordLoc);

      if (consumeIf(tok::kw_true, assignmentValueLoc)) {
        assignment = true;
      } else if (consumeIf(tok::kw_false, assignmentValueLoc)) {
        assignment = false;
      } else {
        diagnose(Tok, diag::expected_precedencegroup_assignment);
        return abortBody();
      }
      continue;
    }

    bool isLowerThan = false;
    if (attrName == "higherThan" ||
        (isLowerThan = (attrName == "lowerThan"))) {
      parseAttributePrefix(isLowerThan ? lowerThanKeywordLoc
                                       : higherThanKeywordLoc);
      auto &relations = (isLowerThan ? lowerThan : higherThan);

      do {
        if (!Tok.is(tok::identifier)) {
          diagnose(Tok, diag::expected_precedencegroup_relation, attrName);
          return abortBody();
        }
        auto name = Context.getIdentifier(Tok.getText());
        auto loc = consumeToken();
        relations.push_back({loc, name, nullptr});
      } while (consumeIf(tok::comma));
      continue;
    }
    
    diagnose(Tok, diag::unknown_precedencegroup_attribute, attrName);
    return abortBody();
  }

  auto result = create();
  if (invalid) result->setInvalid();
  return makeParserResult(result);
}

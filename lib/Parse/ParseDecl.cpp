//===--- ParseDecl.cpp - Swift Language Parser for Declarations -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
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

#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/LifetimeDependence.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ParseRequests.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Bridging/ASTGen.h"
#include "swift/Parse/IDEInspectionCallbacks.h"
#include "swift/Parse/ParseDeclName.h"
#include "swift/Parse/ParseSILSupport.h"
#include "swift/Parse/Parser.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>
#include <initializer_list>

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
  /// whether we are in the debugger function, and whether it needs to swap
  /// the Decl that is currently being parsed.
  ///
  /// If you are making one of these objects to address issue 1, call
  /// the constructor that only takes a DeclKind, and it will be moved
  /// unconditionally.  Otherwise pass in the Name and DeclKind and the
  /// DebuggerClient will be asked whether to move it or not.
  class DebuggerContextChange {
  protected:
    Parser &P;
    std::optional<Parser::ContextChange> CC;
    SourceFile *SF;
  public:
    DebuggerContextChange(Parser &P) : P(P), SF(nullptr) {
      if (!inDebuggerContext())
        return;

      switchContext();
    }
    
    DebuggerContextChange(Parser &P, Identifier Name, DeclKind Kind)
        : P(P), SF(nullptr) {
      if (!inDebuggerContext())
        return;

      if (auto *client = getDebuggerClient())
        if (client->shouldGlobalize(Name, Kind))
          switchContext();
    }
    
    bool movedToTopLevel() {
      return CC.has_value();
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
      if (movedToTopLevel())
        hoistDecl(D);
      return ParserResult<T>(D);
    }
    
    template <typename T>
    ParserResult<T>
    fixupParserResult(ParserStatus Status, T *D) {
      if (movedToTopLevel())
        hoistDecl(D);
      return makeParserResult(Status, D);
    }

    // The destructor doesn't need to do anything, the CC's destructor will
    // pop the context if we set it.
    ~DebuggerContextChange () {}

  private:
    DebuggerClient *getDebuggerClient() {
      ModuleDecl *M = P.CurDeclContext->getParentModule();
      return M->getDebugClient();
    }
    
    bool inDebuggerContext() {
      if (!P.Context.LangOpts.DebuggerSupport)
        return false;
      if (!P.CurDeclContext)
        return false;
      auto *func = dyn_cast<FuncDecl>(P.CurDeclContext);
      if (!func)
        return false;

      if (!func->getAttrs().hasAttribute<LLDBDebuggerFunctionAttr>())
        return false;

      return true;
    }
    
    void switchContext() {
      SF = P.CurDeclContext->getParentSourceFile();
      CC.emplace(P, SF);
    }

    template<typename T>
    void hoistDecl(T *D) {
      D->setHoisted();
      SF->addHoistedDecl(D);
      getDebuggerClient()->didGlobalize(D);
    }
  };
} // end anonymous namespace

/// Main entrypoint for the parser.
///
/// \verbatim
///   top-level:
///     stmt-brace-item*
///     decl-sil       [[only in SIL mode]
///     decl-sil-stage [[only in SIL mode]
/// \endverbatim
void Parser::parseTopLevelItems(SmallVectorImpl<ASTNode> &items) {
  // Prime the lexer.
  if (Tok.is(tok::NUM_TOKENS))
    consumeTokenWithoutFeedingReceiver();

  // Parse the body of the file.
  while (!Tok.is(tok::eof)) {
    // If we run into a SIL decl, skip over until the next Swift decl. We need
    // to delay parsing these, as SIL parsing currently requires type checking
    // Swift decls.
    if (isStartOfSILDecl()) {
      assert(!isStartOfSwiftDecl() && "Start of both a Swift and SIL decl?");
      skipSILUntilSwiftDecl();
      continue;
    }

    // Figure out how to parse the items in this source file.
    BraceItemListKind braceItemListKind;
    switch (SF.Kind) {
    case SourceFileKind::Main:
      braceItemListKind = BraceItemListKind::TopLevelCode;
      break;

    case SourceFileKind::Library:
    case SourceFileKind::Interface:
    case SourceFileKind::SIL:
      braceItemListKind = BraceItemListKind::TopLevelLibrary;
      break;

    case SourceFileKind::MacroExpansion:
    case SourceFileKind::DefaultArgument:
      braceItemListKind = BraceItemListKind::MacroExpansion;
      break;
    }

    parseBraceItems(items, braceItemListKind);

    // In the case of a catastrophic parse error, consume any trailing
    // #else, #elseif, or #endif and move on to the next statement or
    // declaration block.
    if (Tok.is(tok::pound_else) || Tok.is(tok::pound_elseif) ||
        Tok.is(tok::pound_endif)) {
      diagnose(Tok.getLoc(),
               diag::unexpected_conditional_compilation_block_terminator);
      consumeToken();
    }
  }

#if SWIFT_BUILD_SWIFT_SYNTAX
  using ParsingFlags = SourceFile::ParsingFlags;
  const auto parsingOpts = SF.getParsingOptions();

  // Perform validation checking.
  if (parsingOpts.contains(ParsingFlags::ValidateNewParserDiagnostics) &&
      !Context.Diags.hadAnyError()) {
    auto *exportedSourceFile = SF.getExportedSourceFile();
    if (!exportedSourceFile)
      return;

    auto hadSyntaxError = swift_ASTGen_emitParserDiagnostics(
        Context, &Context.Diags, exportedSourceFile,
        /*emitOnlyErrors=*/true,
        /*downgradePlaceholderErrorsToWarnings=*/
        Context.LangOpts.Playground ||
            Context.LangOpts.WarnOnEditorPlaceholder);
    if (hadSyntaxError) {
      // We might have emitted warnings in the C++ parser but no errors, in
      // which case we still have `hadAnyError() == false`. To avoid
      // emitting the same warnings from SwiftParser, only emit errors from
      // SwiftParser
      SourceLoc loc = Context.SourceMgr.getLocForBufferStart(SF.getBufferID());
      diagnose(loc, diag::parser_new_parser_errors);
    }
  }
#endif
}

bool Parser::parseTopLevelSIL() {
  assert(SIL && isInSILMode());

  // Prime the lexer.
  if (Tok.is(tok::NUM_TOKENS))
    consumeTokenWithoutFeedingReceiver();

  auto skipToNextSILDecl = [&]() {
    while (!Tok.is(tok::eof) && !isStartOfSILDecl())
      skipSingle();
  };

  auto hadError = false;
  while (!Tok.is(tok::eof)) {
    // If we run into a Swift decl, skip over until we find the next SIL decl.
    if (isStartOfSwiftDecl()) {
      assert(!isStartOfSILDecl() && "Start of both a Swift and SIL decl?");
      skipToNextSILDecl();
      continue;
    }

    switch (Tok.getKind()) {
#define CASE_SIL(KW, NAME)                                                     \
    case tok::kw_##KW: {                                                       \
      /* If we failed to parse a SIL decl, move onto the next SIL decl to      \
         better help recovery. */                                              \
      if (SIL->parse##NAME(*this)) {                                           \
        Lexer::SILBodyRAII sbr(*L);                                            \
        skipToNextSILDecl();                                                   \
        hadError = true;                                                       \
      }                                                                        \
      break;                                                                   \
    }
    CASE_SIL(sil, DeclSIL)
    CASE_SIL(sil_stage, DeclSILStage)
    CASE_SIL(sil_vtable, SILVTable)
    CASE_SIL(sil_moveonlydeinit, SILMoveOnlyDeinit)
    CASE_SIL(sil_global, SILGlobal)
    CASE_SIL(sil_witness_table, SILWitnessTable)
    CASE_SIL(sil_default_witness_table, SILDefaultWitnessTable)
    CASE_SIL(sil_default_override_table, SILDefaultOverrideTable)
    CASE_SIL(sil_differentiability_witness, SILDifferentiabilityWitness)
    CASE_SIL(sil_coverage_map, SILCoverageMap)
    CASE_SIL(sil_property, SILProperty)
    CASE_SIL(sil_scope, SILScope)
#undef CASE_SIL
    default:
      // If we reached here, we have something malformed that isn't a Swift decl
      // or a SIL decl. Emit an error and skip ahead to the next SIL decl.
      diagnose(Tok, diag::expected_sil_keyword);
      skipToNextSILDecl();
      hadError = true;
      break;
    }
  }
  return hadError;
}

ParserResult<AvailableAttr> Parser::parseExtendedAvailabilitySpecList(
    SourceLoc AtLoc, SourceLoc AttrLoc, StringRef AttrName) {
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

  struct VersionArg {
    llvm::VersionTuple Version;
    SourceRange Range;
    SourceLoc DelimiterLoc;
    bool empty() const {
      return Version.empty();
    }
  };

  StringRef Platform = Tok.getText();
  SourceLoc PlatformLoc = Tok.getLoc();

  StringRef Message, Renamed;
  VersionArg Introduced, Deprecated, Obsoleted;
  auto AttrKind = AvailableAttr::Kind::Default;
  bool HasUpcomingEntry = false;

  {
    consumeToken();
    if (consumeIf(tok::comma)) {
      HasUpcomingEntry = true;
    }
  }

  bool AnyAnnotations = false;
  bool AnyArgumentInvalid = false;
  int ParamIndex = 0;

  while (HasUpcomingEntry) {
    auto ArgumentLoc = Tok.getLoc();
    AnyAnnotations = true;
    StringRef ArgumentKindStr = Tok.getText();
    ++ParamIndex;

    enum {
      IsMessage,
      IsRenamed,
      IsIntroduced,
      IsDeprecated,
      IsObsoleted,
      IsUnavailable,
      IsNoAsync,
      IsInvalid
    } ArgumentKind = IsInvalid;

    if (Tok.is(tok::identifier)) {
      ArgumentKind = llvm::StringSwitch<decltype(ArgumentKind)>(ArgumentKindStr)
                         .Case("message", IsMessage)
                         .Case("renamed", IsRenamed)
                         .Case("introduced", IsIntroduced)
                         .Case("deprecated", IsDeprecated)
                         .Case("obsoleted", IsObsoleted)
                         .Case("unavailable", IsUnavailable)
                         .Case("noasync", IsNoAsync)
                         .Default(IsInvalid);
    }

    auto attrKindToStr = [](AvailableAttr::Kind kind) {
      switch (kind) {
      case AvailableAttr::Kind::Default:
        return "default";
      case AvailableAttr::Kind::Deprecated:
        return "deprecated";
      case AvailableAttr::Kind::Unavailable:
        return "unavailable";
      case AvailableAttr::Kind::NoAsync:
        return "noasync";
      }
    };

    if (ArgumentKind == IsInvalid) {
      diagnose(ArgumentLoc, diag::attr_availability_expected_option, AttrName)
          .highlight(SourceRange(ArgumentLoc));
      if (Tok.is(tok::code_complete) && CodeCompletionCallbacks) {
        CodeCompletionCallbacks->completeDeclAttrParam(
            ParameterizedDeclAttributeKind::Available, ParamIndex,
            /*HasLabel=*/false);
        consumeToken(tok::code_complete);
      } else {
        consumeIf(tok::identifier);
      }
      return nullptr;
    }

    consumeToken();

    auto diagnoseDuplicate = [&](bool WasEmpty) {
      if (!WasEmpty) {
        diagnose(ArgumentLoc, diag::attr_availability_invalid_duplicate,
                 ArgumentKindStr);
      }
    };

    switch (ArgumentKind) {
    case IsMessage:
    case IsRenamed: {
      // Items with string arguments.
      if (findAttrValueDelimiter()) {
        consumeToken();
      } else {
        diagnose(Tok, diag::attr_availability_expected_equal, AttrName,
                 ArgumentKindStr);
        AnyArgumentInvalid = true;
        if (peekToken().isAny(tok::r_paren, tok::comma))
          consumeToken();
        break;
      }

      if (!Tok.is(tok::string_literal)) {
        diagnose(AttrLoc, diag::attr_expected_string_literal, AttrName);
        AnyArgumentInvalid = true;
        if (peekToken().isAny(tok::r_paren, tok::comma))
          consumeToken();
        break;
      }

      auto Value = getStringLiteralIfNotInterpolated(
          AttrLoc, ("'" + ArgumentKindStr + "'").str());
      consumeToken();
      if (!Value) {
        AnyArgumentInvalid = true;
        break;
      }

      if (ArgumentKind == IsMessage) {
        diagnoseDuplicate(Message.empty());
        Message = Value.value();
      } else {
        ParsedDeclName parsedName = parseDeclName(Value.value());
        if (!parsedName) {
          diagnose(AttrLoc, diag::attr_availability_invalid_renamed, AttrName);
          AnyArgumentInvalid = true;
          break;
        }
        diagnoseDuplicate(Renamed.empty());
        Renamed = Value.value();
      }

      break;
    }

    case IsDeprecated:
      if (!findAttrValueDelimiter()) {
        if (AttrKind != AvailableAttr::Kind::Default) {
          diagnose(Tok, diag::attr_availability_multiple_kinds, AttrName,
                   "deprecated", attrKindToStr(AttrKind));
        }

        AttrKind = AvailableAttr::Kind::Deprecated;
        break;
      }
      LLVM_FALLTHROUGH;

    case IsIntroduced:
    case IsObsoleted: {
      // Items with version arguments.
      SourceLoc DelimiterLoc;
      if (findAttrValueDelimiter()) {
        DelimiterLoc = Tok.getLoc();
        consumeToken();
      } else {
        diagnose(Tok, diag::attr_availability_expected_equal, AttrName,
                 ArgumentKindStr);
        AnyArgumentInvalid = true;
        if (peekToken().isAny(tok::r_paren, tok::comma))
          consumeToken();
        break;
      }

      auto &VerArg =
          (ArgumentKind == IsIntroduced)
              ? Introduced
              : (ArgumentKind == IsDeprecated) ? Deprecated : Obsoleted;

      bool VerArgWasEmpty = VerArg.empty();
      if (parseVersionTuple(
              VerArg.Version, VerArg.Range,
              {diag::attr_availability_expected_version, {AttrName}})) {
        AnyArgumentInvalid = true;
        if (peekToken().isAny(tok::r_paren, tok::comma))
          consumeToken();
      }
      VerArg.DelimiterLoc = DelimiterLoc;
      diagnoseDuplicate(VerArgWasEmpty);

      break;
    }

    case IsUnavailable:
      if (AttrKind != AvailableAttr::Kind::Default) {
        diagnose(Tok, diag::attr_availability_multiple_kinds, AttrName,
                 "unavailable", attrKindToStr(AttrKind));
      }

      AttrKind = AvailableAttr::Kind::Unavailable;
      break;

    case IsNoAsync:
      if (AttrKind != AvailableAttr::Kind::Default) {
        diagnose(Tok, diag::attr_availability_multiple_kinds, AttrName,
                 "noasync", attrKindToStr(AttrKind));
      }

      AttrKind = AvailableAttr::Kind::NoAsync;
      break;

    case IsInvalid:
      llvm_unreachable("handled above");
    }

    // Parse the trailing comma
    if (consumeIf(tok::comma)) {
      HasUpcomingEntry = true;
    } else {
      HasUpcomingEntry = false;
    }
  }

  if (!AnyAnnotations &&
      !Context.LangOpts.hasFeature(Feature::CustomAvailability)) {
    diagnose(Tok.getLoc(), diag::attr_expected_comma, AttrName,
             /*isDeclModifier*/ false);
  }

  if (AnyArgumentInvalid)
    return nullptr;

  auto Attr = new (Context) AvailableAttr(
      AtLoc, SourceRange(AttrLoc, Tok.getLoc()),
      Context.getIdentifier(Platform), PlatformLoc, AttrKind, Message, Renamed,
      Introduced.Version, Introduced.Range, Deprecated.Version,
      Deprecated.Range, Obsoleted.Version, Obsoleted.Range,
      /*Implicit=*/false, AttrName == SPI_AVAILABLE_ATTRNAME);
  return makeParserResult(Attr);

}

bool Parser::parseSpecializeAttributeArguments(
    swift::tok ClosingBrace, bool &DiscardAttribute,
    std::optional<bool> &Exported,
    std::optional<SpecializeAttr::SpecializationKind> &Kind,
    swift::TrailingWhereClause *&TrailingWhereClause,
    DeclNameRef &targetFunction, AvailabilityRange *SILAvailability,
    SmallVectorImpl<Identifier> &spiGroups,
    SmallVectorImpl<AvailableAttr *> &availableAttrs,
    llvm::function_ref<bool(Parser &)> parseSILTargetName,
    llvm::function_ref<bool(Parser &)> parseSILSIPModule) {
  bool isSIL = SILAvailability != nullptr;
  // Parse optional "exported" and "kind" labeled parameters.
  while (!Tok.is(tok::kw_where)) {
    bool isAvailability = false;
    if (Tok.is(tok::identifier)) {
      auto ParamLabel = Tok.getText();
      if (ParamLabel != "exported" && ParamLabel != "kind" &&
          ParamLabel != "target" && ParamLabel != "spi" &&
          ParamLabel != "spiModule" && ParamLabel != "availability" &&
          (!isSIL || ParamLabel != "available")) {
        diagnose(Tok.getLoc(), diag::attr_specialize_unknown_parameter_name,
                 ParamLabel);
      }
      auto AtLoc = consumeToken();
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
      if ((ParamLabel == "exported" && Exported.has_value()) ||
          (ParamLabel == "kind" && Kind.has_value()) ||
          (ParamLabel == "spi" && !spiGroups.empty())) {
        diagnose(Tok.getLoc(), diag::attr_specialize_parameter_already_defined,
                 ParamLabel);
      }
      if (ParamLabel == "available") {
        SourceRange range;
        llvm::VersionTuple version;
        if (parseVersionTuple(version, range,
                                 diag::sil_availability_expected_version))
          return false;

        *SILAvailability = AvailabilityRange(version);
      }
      if (ParamLabel == "availability") {
        SourceRange attrRange;
        auto Loc = Tok.getLoc();
        bool shouldDiscardAvailabilityAttr = false;
        isAvailability = true;
        if (!parseAvailability(true, ParamLabel, shouldDiscardAvailabilityAttr,
                               attrRange, AtLoc, Loc, [&](AvailableAttr *attr) {
                                 availableAttrs.push_back(attr);
                               }))
          return false;
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
          Exported = isTrue;
        }
      }
      if (ParamLabel == "kind") {
        SourceLoc paramValueLoc;
        if (Tok.is(tok::identifier)) {
          if (Tok.getText() == "partial") {
            Kind = SpecializeAttr::SpecializationKind::Partial;
          } else if (Tok.getText() == "full") {
            Kind = SpecializeAttr::SpecializationKind::Full;
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
      if (ParamLabel == "target") {
        if (!parseSILTargetName(*this)) {
          DeclNameLoc loc;
          targetFunction = parseDeclNameRef(
              loc, diag::attr_specialize_expected_function,
              DeclNameFlag::AllowZeroArgCompoundNames |
                  DeclNameFlag::AllowKeywordsUsingSpecialNames |
                  DeclNameFlag::AllowOperators |
                  DeclNameFlag::AllowLowercaseAndUppercaseSelf);
        }
      }
      if (ParamLabel == "spiModule") {
        if (!parseSILSIPModule(*this)) {
          diagnose(Tok.getLoc(), diag::attr_specialize_unknown_parameter_name,
                   ParamLabel);
          return false;
        }
      }
      if (ParamLabel == "spi") {
        if (!Tok.isIdentifierOrUnderscore()) {
          diagnose(Tok.getLoc(), diag::attr_specialize_expected_spi_name);
          consumeToken();
          return false;
        }
        auto text = Tok.getText();
        spiGroups.push_back(Context.getIdentifier(text));
        consumeToken();
      }
      if (!isAvailability && !consumeIf(tok::comma)) {
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
  if (Tok.is(tok::kw_where)) {
    SourceLoc whereLoc, endLoc;
    SmallVector<RequirementRepr, 4> requirements;
    parseGenericWhereClause(whereLoc, endLoc, requirements,
                            /* AllowLayoutConstraints */ true);
    TrailingWhereClause =
        TrailingWhereClause::create(Context, whereLoc, endLoc, requirements);
  }
  return true;
}

bool Parser::parseAvailability(
    bool parseAsPartOfSpecializeAttr, StringRef AttrName,
    bool &DiscardAttribute, SourceRange &attrRange, SourceLoc AtLoc,
    SourceLoc Loc, llvm::function_ref<void(AvailableAttr *)> addAttribute) {
  // platform:
  //   *
  //   identifier
  if (!Tok.is(tok::identifier) &&
      !(Tok.isAnyOperator() && Tok.getText() == "*")) {
    if (Tok.is(tok::code_complete) && CodeCompletionCallbacks) {
      CodeCompletionCallbacks->completeDeclAttrParam(
          ParameterizedDeclAttributeKind::Available, 0, /*HasLabel=*/false);
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
      (peekToken().isAny(tok::integer_literal, tok::floating_literal) ||
       peekAvailabilityMacroName())) {
    // We have the short form of available: @available(iOS 8.0.1, *)
    SmallVector<AvailabilitySpec *, 5> Specs;
    ParserStatus Status =
        parseAvailabilitySpecList(Specs, AvailabilitySpecSource::Available);

    if (Status.isErrorOrHasCompletion())
      return false;

    auto availTerminator =
        parseAsPartOfSpecializeAttr ? tok::semi : tok::r_paren;
    if (!consumeIf(availTerminator)) {
      auto diagnostic = parseAsPartOfSpecializeAttr
                            ? diag::attr_expected_semi
                            : diag::attr_expected_rparen;
      diagnose(Tok.getLoc(), diagnostic, AttrName, parseAsPartOfSpecializeAttr);
      return false;
    }

    attrRange = SourceRange(Loc, PreviousLoc);
    // For each platform version spec in the spec list, create an
    // implicit AvailableAttr for the platform with the introduced
    // version from the spec. For example, if we have
    //   @available(iOS 8.0, OSX 10.10, *):
    // we will synthesize:
    //  @available(iOS, introduced: 8.0)
    //  @available(OSX, introduced: 10.10)
    //
    // Similarly if we have a language version spec or PackageDescription
    // version in the spec list, create an implicit AvailableAttr
    // with the specified version as the introduced argument.
    // For example, if we have
    //   @available(swift 3.1)
    // we will synthesize
    //   @available(swift, introduced: 3.1)
    // or, if we have
    //   @available(_PackageDescription 4.2)
    // we will synthesize
    //   @available(_PackageDescription, introduced: 4.2)

    bool groupContainsWildcard = llvm::any_of(
        Specs, [](AvailabilitySpec *Spec) { return Spec->isWildcard(); });

    AvailableAttr *PrevAttr = nullptr;
    for (auto *Spec : Specs) {
      if (Spec->isWildcard())
        continue;

      auto DomainOrIdentifier = Spec->getDomainOrIdentifier();

      // The domain should not be resolved during parsing.
      DEBUG_ASSERT(!DomainOrIdentifier.isDomain());

      auto DomainIdentifier = DomainOrIdentifier.getAsIdentifier();
      if (!DomainIdentifier)
        continue;

      auto Attr = new (Context) AvailableAttr(
          AtLoc, attrRange, *DomainIdentifier, Spec->getSourceRange().Start,
          AvailableAttr::Kind::Default,
          /*Message=*/StringRef(),
          /*Rename=*/StringRef(),
          /*Introduced=*/Spec->getRawVersion(),
          /*IntroducedRange=*/Spec->getVersionSrcRange(),
          /*Deprecated=*/llvm::VersionTuple(),
          /*DeprecatedRange=*/SourceRange(),
          /*Obsoleted=*/llvm::VersionTuple(),
          /*ObsoletedRange=*/SourceRange(),
          /*Implicit=*/false, AttrName == SPI_AVAILABLE_ATTRNAME);
      addAttribute(Attr);

      Attr->setIsGroupMember();
      if (groupContainsWildcard)
        Attr->setIsGroupedWithWildcard();
      if (!PrevAttr)
        Attr->setIsGroupTerminator();

      PrevAttr = Attr;
    }

    return true;
  }

  auto AvailabilityAttr =
      parseExtendedAvailabilitySpecList(AtLoc, Loc, AttrName);
  DiscardAttribute |= AvailabilityAttr.isParseErrorOrHasCompletion();

  auto availTerminator = parseAsPartOfSpecializeAttr ? tok::semi : tok::r_paren;
  if (!consumeIf(availTerminator)) {
    if (!DiscardAttribute) {
      auto diagnostic = parseAsPartOfSpecializeAttr ? diag::attr_expected_semi
                                                    : diag::attr_expected_rparen;
      diagnose(Tok.getLoc(), diagnostic, AttrName, parseAsPartOfSpecializeAttr);
    }
    return false;
  }

  if (!DiscardAttribute) {
    addAttribute(AvailabilityAttr.get());
  } else {
    return false;
  }
  return true;
}

bool Parser::parseSpecializeAttribute(
    swift::tok ClosingBrace, SourceLoc AtLoc, SourceLoc Loc,
    SpecializeAttr *&Attr, AvailabilityRange *SILAvailability,
    llvm::function_ref<bool(Parser &)> parseSILTargetName,
    llvm::function_ref<bool(Parser &)> parseSILSIPModule) {
  assert(ClosingBrace == tok::r_paren || ClosingBrace == tok::r_square);

  SourceLoc lParenLoc;
  if (Tok.is(tok::l_paren)) {
    lParenLoc = consumeAttributeLParen();
  } else {
    // SIL parsing is positioned at _specialize when entering this and parses
    // the location of the _specialize keyword as the lParenLoc.
    lParenLoc = consumeToken();
  }
  bool DiscardAttribute = false;
  StringRef AttrName = "_specialize";

  std::optional<bool> exported;
  std::optional<SpecializeAttr::SpecializationKind> kind;

  TrailingWhereClause *trailingWhereClause = nullptr;

  DeclNameRef targetFunction;
  SmallVector<Identifier, 4> spiGroups;
  SmallVector<AvailableAttr *, 4> availableAttrs;
  if (!parseSpecializeAttributeArguments(
          ClosingBrace, DiscardAttribute, exported, kind, trailingWhereClause,
          targetFunction, SILAvailability, spiGroups, availableAttrs,
          parseSILTargetName, parseSILSIPModule)) {
    return false;
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
  if (!exported.has_value())
    exported = false;
  // Full specialization by default.
  if (!kind.has_value())
    kind = SpecializeAttr::SpecializationKind::Full;

  if (DiscardAttribute) {
    Attr = nullptr;
    return false;
  }  

  // Store the attribute.
  Attr = SpecializeAttr::create(Context, AtLoc, SourceRange(Loc, rParenLoc),
                                trailingWhereClause, exported.value(),
                                kind.value(), targetFunction, spiGroups,
                                availableAttrs);
  return true;
}

ParserResult<StorageRestrictionsAttr>
Parser::parseStorageRestrictionsAttribute(SourceLoc AtLoc, SourceLoc Loc) {
  StringRef AttrName = "storageRestrictions";
  ParserStatus Status;

  if (Tok.isNot(tok::l_paren)) {
    diagnose(Loc, diag::attr_expected_lparen, AttrName,
             /*DeclModifier=*/false);
    Status.setIsParseError();
    return Status;
  }

  // Consume '('
  SourceLoc lParenLoc = consumeAttributeLParen();

  SmallVector<Identifier> initializesProperties;
  SmallVector<Identifier> accessesProperties;

  enum class AccessKind { Initialization, Access, Invalid };

  auto parseProperties =
      [&](SourceLoc loc, Identifier label, AccessKind kind,
          SmallVectorImpl<Identifier> &properties) -> ParserStatus {
    ParserStatus status;
    if (!properties.empty()) {
      diagnose(loc, diag::duplicate_storage_restrictions_attr_label, label);
      status.setIsParseError();
      return status;
    }

    bool hasNextProperty = false;
    do {
      // Next is not a property name but a label followed by ':'
      if (peekToken().is(tok::colon))
        break;

      Identifier propertyName;
      SourceLoc propertyNameLoc;
      if (consumeIf(tok::code_complete)) {
        status.setHasCodeCompletion();
        if (this->CodeCompletionCallbacks) {
          StorageRestrictionsCompletionKind completionKind;
          if (properties.empty()) {
            // We are parsing the first argument after a label. The argument is
            // required.
            completionKind = StorageRestrictionsCompletionKind::Argument;
          } else if (!initializesProperties.empty() &&
                     !accessesProperties.empty()) {
            // We have specified both an 'initializes' and an 'accesses' label
            // already. All we can accept are arguments now.
            completionKind = StorageRestrictionsCompletionKind::Argument;
          } else {
            // We can accept either an argument or the label that hasn't been
            // specified yet.
            switch (kind) {
            case AccessKind::Access:
              completionKind =
                  StorageRestrictionsCompletionKind::ArgumentOrInitializesLabel;
              break;
            case AccessKind::Initialization:
              completionKind =
                  StorageRestrictionsCompletionKind::ArgumentOrAccessesLabel;
              break;
            case AccessKind::Invalid:
              // We should never end up in this case. 'parseProperties' does not
              // get called with 'AccessKind::Invalid'.
              // If we do end up here, suggest arguments instead of crashing
              // since it's the most sensible fallback.
              completionKind = StorageRestrictionsCompletionKind::Argument;
              break;
            }
          }
          this->CodeCompletionCallbacks->completeDeclAttrParam(
              ParameterizedDeclAttributeKind::StorageRestrictions,
              static_cast<int>(completionKind), /*HasLabel=*/false);
        }
      } else if (parseIdentifier(propertyName, propertyNameLoc,
                                 diag::storage_restrictions_attr_expected_name,
                                 /*diagnoseDollarPrefix=*/true)) {
        status.setIsParseError();
        return status;
      }

      properties.push_back(propertyName);

      // Parse the comma, if the list continues.
      hasNextProperty = consumeIf(tok::comma);
    } while (hasNextProperty);

    return status;
  };

  auto parseArgument = [&](bool isOptional = false) -> ParserStatus {
    ParserStatus status;

    if (Tok.is(tok::r_paren) && isOptional)
      return status;

    Identifier accessLabel;
    SourceLoc loc;
    parseOptionalArgumentLabel(accessLabel, loc);

    if (accessLabel.empty()) {
      if (consumeIf(tok::code_complete)) {
        if (this->CodeCompletionCallbacks) {
          this->CodeCompletionCallbacks->completeDeclAttrParam(
              ParameterizedDeclAttributeKind::StorageRestrictions,
              static_cast<int>(StorageRestrictionsCompletionKind::Label),
              /*HasLabel=*/false);
        }
        status.setHasCodeCompletion();
        return status;
      } else {
        diagnose(Loc, diag::missing_storage_restrictions_attr_label);
        status.setIsParseError();
        return status;
      }
    }

    auto access = llvm::StringSwitch<AccessKind>(accessLabel.str())
                      .Case("initializes", AccessKind::Initialization)
                      .Case("accesses", AccessKind::Access)
                      .Default(AccessKind::Invalid);

    switch (access) {
    case AccessKind::Initialization:
      return parseProperties(loc, accessLabel, access, initializesProperties);

    case AccessKind::Access:
      return parseProperties(loc, accessLabel, access, accessesProperties);

    case AccessKind::Invalid:
      diagnose(loc, diag::invalid_storage_restrictions_attr_label, accessLabel);
      status.setIsParseError();
      return status;
    }
  };

  ParserStatus argumentStatus = parseArgument();
  // Attribute should have at least one argument.
  if (!argumentStatus.isErrorOrHasCompletion()) {
    argumentStatus |= parseArgument(/*isOptional=*/true);
  }
  Status |= argumentStatus;
  if (argumentStatus.isErrorOrHasCompletion()) {
    // Let's skip ahead to `)` to recover.
    skipUntil(tok::r_paren);
  }

  // Consume ')'
  SourceLoc rParenLoc;
  if (!consumeIf(tok::r_paren, rParenLoc)) {
    diagnose(lParenLoc, diag::attr_expected_rparen, AttrName,
             /*DeclModifier=*/false);
    Status.setIsParseError();
  }

  auto attribute = StorageRestrictionsAttr::create(
      Context, AtLoc, SourceRange(Loc, rParenLoc), initializesProperties,
      accessesProperties);
  return makeParserResult(Status, attribute);
}

ParserResult<ImplementsAttr>
Parser::parseImplementsAttribute(SourceLoc AtLoc, SourceLoc Loc) {
  StringRef AttrName = "_implements";
  ParserStatus Status;

  if (Tok.isNot(tok::l_paren)) {
    diagnose(Loc, diag::attr_expected_lparen, AttrName,
             /*DeclModifier=*/false);
    Status.setIsParseError();
    return Status;
  }

  SourceLoc lParenLoc = consumeAttributeLParen();

  DeclNameLoc MemberNameLoc;
  DeclNameRef MemberName;
  ParserResult<TypeRepr> ProtocolType;
  {
    ProtocolType = parseType();
    Status |= ProtocolType;

    if (!(Status.isErrorOrHasCompletion() || consumeIf(tok::comma))) {
      diagnose(Tok.getLoc(), diag::attr_expected_comma, AttrName,
               /*DeclModifier=*/false);
      Status.setIsParseError();
    }

    if (!Status.isErrorOrHasCompletion()) {
      MemberName = parseDeclNameRef(
          MemberNameLoc, diag::attr_implements_expected_member_name,
          DeclNameFlag::AllowZeroArgCompoundNames |
              DeclNameFlag::AllowOperators |
              DeclNameFlag::AllowLowercaseAndUppercaseSelf);
      if (!MemberName) {
        Status.setIsParseError();
      }
    }
  }

  if (Status.isErrorOrHasCompletion()) {
    skipUntil(tok::r_paren);
  }

  SourceLoc rParenLoc;
  if (!consumeIf(tok::r_paren, rParenLoc)) {
    diagnose(lParenLoc, diag::attr_expected_rparen, AttrName,
             /*DeclModifier=*/false);
    Status.setIsParseError();
  }

  if (Status.isErrorOrHasCompletion()) {
    return Status;
  }

  // FIXME(ModQual): Reject module qualification on MemberName.
  return ParserResult<ImplementsAttr>(
    ImplementsAttr::create(Context, AtLoc, SourceRange(Loc, rParenLoc),
                           ProtocolType.get(), MemberName.getFullName(),
                           MemberNameLoc));
}

/// Parse a `@differentiable` attribute, returning true on error.
///
/// \verbatim
///   differentiable-attribute-arguments:
///     '(' (differentiability-params-clause ',')?
///         where-clause?
///     ')'
/// \endverbatim
ParserResult<DifferentiableAttr>
Parser::parseDifferentiableAttribute(SourceLoc atLoc, SourceLoc loc) {
  StringRef AttrName = "differentiable";
  SourceLoc rParenLoc = loc;
  DifferentiabilityKind diffKind = DifferentiabilityKind::Normal;
  SmallVector<ParsedAutoDiffParameter, 8> parameters;
  TrailingWhereClause *whereClause = nullptr;

  // Parse '('.
  if (consumeIfAttributeLParen()) {
    // Parse @differentiable attribute arguments.
    if (parseDifferentiableAttributeArguments(
            diffKind, parameters, whereClause))
      return makeParserError();
    // Parse ')'.
    if (!consumeIf(tok::r_paren, rParenLoc)) {
      diagnose(getEndOfPreviousLoc(), diag::attr_expected_rparen, AttrName,
               /*DeclModifier=*/false);
      return makeParserError();
    }
  } else {
    // TODO: Change this to an error once clients have migrated to 'reverse'.
    diagnose(
        getEndOfPreviousLoc(), diag::attr_differentiable_expected_reverse)
        .fixItInsert(getEndOfPreviousLoc(), "(reverse)");
    diffKind = DifferentiabilityKind::Reverse;
  }

  return ParserResult<DifferentiableAttr>(DifferentiableAttr::create(
      Context, /*implicit*/ false, atLoc, SourceRange(loc, rParenLoc), diffKind,
      parameters, whereClause));
}

bool Parser::parseExternAttribute(DeclAttributes &Attributes,
                                  bool &DiscardAttribute, StringRef AttrName,
                                  SourceLoc AtLoc, SourceLoc Loc) {
  SourceLoc lParenLoc = Tok.getLoc(), rParenLoc;

  // Parse @_extern(<language>, ...)
  if (!consumeIfAttributeLParen()) {
    diagnose(Loc, diag::attr_expected_lparen, AttrName,
             DeclAttribute::isDeclModifier(DeclAttrKind::Extern));
    return false;
  }
  auto diagnoseExpectLanguage = [&]() {
    diagnose(Tok.getLoc(), diag::attr_expected_option_such_as, AttrName, "c");
  };
  if (Tok.isNot(tok::identifier)) {
    diagnoseExpectLanguage();
    return false;
  }

  auto parseStringLiteralArgument =
      [&](std::optional<StringRef> fieldName) -> std::optional<StringRef> {
    if (!consumeIf(tok::comma)) {
      diagnose(Loc, diag::attr_expected_comma, AttrName,
               DeclAttribute::isDeclModifier(DeclAttrKind::Extern));
      return std::nullopt;
    }
    if (fieldName) {
      if (Tok.isNot(tok::identifier) || Tok.getText() != fieldName) {
        diagnose(Loc, diag::attr_extern_expected_label, *fieldName);
        return std::nullopt;
      }
      consumeToken(tok::identifier);

      if (!consumeIf(tok::colon)) {
        diagnose(Tok.getLoc(), diag::attr_expected_colon_after_label, *fieldName);
        return std::nullopt;
      }
    }

    if (Tok.isNot(tok::string_literal)) {
      diagnose(Loc, diag::attr_expected_string_literal, AttrName);
      return std::nullopt;
    }
    std::optional<StringRef> importModuleName =
        getStringLiteralIfNotInterpolated(Loc, ("'" + AttrName + "'").str());
    consumeToken(tok::string_literal);

    if (!importModuleName.has_value()) {
      DiscardAttribute = true;
      return std::nullopt;
    }
    return importModuleName.value();
  };

  auto kindTok = Tok;
  consumeToken(tok::identifier);

  // Parse @_extern(wasm, module: "x", name: "y") or @_extern(c[, "x"])
  ExternKind kind;
  std::optional<StringRef> importModuleName, importName;

  if (kindTok.getText() == "wasm") {
    kind = ExternKind::Wasm;
    if (!(importModuleName = parseStringLiteralArgument("module")))
      DiscardAttribute = true;
    if (!(importName = parseStringLiteralArgument("name")))
      DiscardAttribute = true;
  } else if (kindTok.getText() == "c") {
    kind = ExternKind::C;
    if (Tok.is(tok::comma)) {
      if (!(importName = parseStringLiteralArgument(std::nullopt))) {
        DiscardAttribute = true;
      }
    }
  } else {
    diagnoseExpectLanguage();
    DiscardAttribute = true;
    while (Tok.isNot(tok::r_paren)) {
      consumeToken();
    }
  }

  rParenLoc = Tok.getLoc();
  if (!consumeIf(tok::r_paren)) {
    diagnose(Loc, diag::attr_expected_rparen, AttrName,
             DeclAttribute::isDeclModifier(DeclAttrKind::Extern));
    return false;
  }

  auto AttrRange = SourceRange(Loc, rParenLoc);

  // Reject duplicate attributes with the same kind.
  if (ExternAttr::find(Attributes, kind)) {
    diagnose(Loc, diag::duplicate_attribute, false);
    DiscardAttribute = true;
  }

  if (!DiscardAttribute) {
    Attributes.add(new (Context)
                       ExternAttr(importModuleName, importName, AtLoc,
                                  lParenLoc, rParenLoc, AttrRange, kind,
                                  /*Implicit=*/false));
  }
  return false;
}

// Attribute parsing error helper.
// For the given parentheses depth, skip until ')' and consume it if possible.
// If no ')' is found, produce error.
// Always returns true to indicate a parsing error has occurred.
static bool errorAndSkipUntilConsumeRightParen(Parser &P, StringRef attrName,
                                               int parenDepth = 1) {
  for (int i = 0; i < parenDepth; ++i) {
    P.skipUntil(tok::r_paren);
    if (!P.consumeIf(tok::r_paren)) {
      P.diagnose(P.Tok, diag::attr_expected_rparen, attrName,
                 /*DeclModifier=*/false);
      return true;
    }
  }
  return true;
}

/// Parse a differentiability parameters 'wrt:' clause, returning true on error.
/// If `allowNamedParameters` is false, allow only index parameters and 'self'.
///
/// \verbatim
///   differentiability-params-clause:
///     'wrt' ':' (differentiability-param | differentiability-params)
///   differentiability-params:
///     '(' differentiability-param (',' differentiability-param)* ')'
///   differentiability-param:
///     'self' | identifier | [0-9]+
/// \endverbatim
bool Parser::parseDifferentiabilityParametersClause(
    SmallVectorImpl<ParsedAutoDiffParameter> &parameters, StringRef attrName,
    bool allowNamedParameters) {
  consumeToken(tok::identifier);
  if (!consumeIf(tok::colon)) {
    diagnose(Tok, diag::expected_colon_after_label, "wrt");
    return errorAndSkipUntilConsumeRightParen(*this, attrName);
  }

  // Function that parses a parameter into `parameters`. Returns true if error
  // occurred.
  auto parseParam = [&](bool parseTrailingComma = true) -> bool {
    SourceLoc paramLoc;
    switch (Tok.getKind()) {
    case tok::identifier: {
      // If named parameters are not allowed, diagnose.
      if (!allowNamedParameters) {
        diagnose(Tok, diag::diff_params_clause_expected_parameter_unnamed);
        return true;
      }
      Identifier paramName;
      paramLoc = consumeIdentifier(paramName, /*diagnoseDollarPrefix=*/false);
      parameters.push_back(
          ParsedAutoDiffParameter::getNamedParameter(paramLoc, paramName));
      break;
    }
    case tok::integer_literal: {
      unsigned paramNum;
      if (parseUnsignedInteger(
              paramNum, paramLoc,
              diag::diff_params_clause_expected_parameter))
        return true;
      parameters.push_back(
          ParsedAutoDiffParameter::getOrderedParameter(paramLoc, paramNum));
      break;
    }
    case tok::kw_self: {
      paramLoc = consumeToken(tok::kw_self);
      parameters.push_back(ParsedAutoDiffParameter::getSelfParameter(paramLoc));
      break;
    }
    default:
      diagnose(Tok, diag::diff_params_clause_expected_parameter);
      return true;
    }
    if (parseTrailingComma && Tok.isNot(tok::r_paren))
      return parseToken(tok::comma, diag::attr_expected_comma, attrName,
                        /*isDeclModifier=*/false);
    return false;
  };

  // Parse opening '(' of the parameter list.
  if (Tok.is(tok::l_paren)) {
    consumeToken(tok::l_paren);
    // Parse first parameter. At least one is required.
    if (parseParam())
      return errorAndSkipUntilConsumeRightParen(*this, attrName, 2);
    // Parse remaining parameters until ')'.
    while (Tok.isNot(tok::r_paren))
      if (parseParam())
        return errorAndSkipUntilConsumeRightParen(*this, attrName, 2);
    // Parse closing ')' of the parameter list.
    consumeToken(tok::r_paren);
  }
  // If no opening '(' for parameter list, parse a single parameter.
  else {
    if (parseParam(/*parseTrailingComma*/ false))
      return errorAndSkipUntilConsumeRightParen(*this, attrName);
  }
  return false;
}

bool Parser::parseDifferentiableAttributeArguments(
    DifferentiabilityKind &diffKind,
    SmallVectorImpl<ParsedAutoDiffParameter> &parameters,
    TrailingWhereClause *&whereClause) {
  StringRef AttrName = "differentiable";

  // Parse trailing comma, if it exists, and check for errors.
  auto consumeIfTrailingComma = [&]() -> bool {
    if (!consumeIf(tok::comma)) return false;
    // Diagnose trailing comma before 'where' or ')'.
    if (Tok.is(tok::kw_where) || Tok.is(tok::r_paren)) {
      diagnose(Tok, diag::unexpected_separator, ",");
      return true;
    }
    // Check that token after comma is 'wrt'.
    if (isIdentifier(Tok, "wrt")) {
      return false;
    }
    diagnose(Tok, diag::attr_differentiable_expected_label);
    return true;
  };

  // Store starting parser position.
  auto startingLoc = Tok.getLoc();

  // Parse optional differentiability parameters.
  // Parse differentiability kind (optional).
  if (Tok.is(tok::identifier)) {
    diffKind = llvm::StringSwitch<DifferentiabilityKind>(Tok.getText())
        .Case("reverse", DifferentiabilityKind::Reverse)
        .Cases("wrt", "withRespectTo", DifferentiabilityKind::Normal)
        .Case("_linear", DifferentiabilityKind::Linear)
        .Case("_forward", DifferentiabilityKind::Forward)
        .Default(DifferentiabilityKind::NonDifferentiable);

    switch (diffKind) {
    // Reject unsupported differentiability kinds.
    case DifferentiabilityKind::Forward:
      diagnose(Tok, diag::attr_differentiable_kind_not_supported,
               Tok.getText())
          .fixItReplaceChars(Tok.getRange().getStart(),
                             Tok.getRange().getEnd(), "reverse");
      return errorAndSkipUntilConsumeRightParen(*this, AttrName);
    case DifferentiabilityKind::NonDifferentiable:
      diagnose(Tok, diag::attr_differentiable_unknown_kind,
               Tok.getText())
          .fixItReplaceChars(Tok.getRange().getStart(),
                             Tok.getRange().getEnd(), "reverse");
      return errorAndSkipUntilConsumeRightParen(*this, AttrName);
    // Accepted kinds.
    case DifferentiabilityKind::Linear:
    case DifferentiabilityKind::Reverse:
      consumeToken(tok::identifier);
      // If no trailing comma or 'where' clause, terminate parsing arguments.
      if (Tok.isNot(tok::comma, tok::kw_where))
        return false;
      if (consumeIfTrailingComma())
        return errorAndSkipUntilConsumeRightParen(*this, AttrName);
      break;
    default:
      break;
    }
  }

  if (diffKind == DifferentiabilityKind::Normal) {
    // TODO: Change this to an error when clients have migrated to 'reverse'.
    diagnose(Tok, diag::attr_differentiable_expected_reverse)
        .fixItInsert(
            startingLoc,
            peekToken().is(tok::r_paren) ? "reverse" : "reverse, ");
    diffKind = DifferentiabilityKind::Reverse;
  }

  // If 'withRespectTo' is used, make the user change it to 'wrt'.
  if (isIdentifier(Tok, "withRespectTo")) {
    SourceRange withRespectToRange(Tok.getLoc(), peekToken().getLoc());
    diagnose(Tok, diag::attr_differentiable_use_wrt_not_withrespectto)
        .highlight(withRespectToRange)
        .fixItReplace(withRespectToRange, "wrt:");
    return errorAndSkipUntilConsumeRightParen(*this, AttrName);
  }
  // Parse the optional 'wrt' differentiability parameters clause.
  if (isIdentifier(Tok, "wrt")) {
    if (parseDifferentiabilityParametersClause(parameters, AttrName))
      return true;
    // If no trailing comma or 'where' clause, terminate parsing arguments.
    if (Tok.isNot(tok::comma, tok::kw_where))
      return false;
    if (consumeIfTrailingComma())
      return errorAndSkipUntilConsumeRightParen(*this, AttrName);
  }

  // If parser has not advanced and token is not 'where' or ')', emit error.
  if (Tok.getLoc() == startingLoc && Tok.isNot(tok::kw_where, tok::r_paren)) {
    diagnose(Tok, diag::attr_differentiable_expected_label);
    return errorAndSkipUntilConsumeRightParen(*this, AttrName);
  }

  // Parse a trailing 'where' clause if any.
  if (Tok.is(tok::kw_where)) {
    SourceLoc whereLoc, endLoc;
    SmallVector<RequirementRepr, 4> requirements;
    parseGenericWhereClause(whereLoc, endLoc, requirements,
                            /*AllowLayoutConstraints*/ true);
    whereClause =
        TrailingWhereClause::create(Context, whereLoc, endLoc, requirements);
  }
  return false;
}

// Helper function that returns the accessor kind if a token is an accessor
// label.
static std::optional<AccessorKind> isAccessorLabel(const Token &token) {
  if (token.is(tok::identifier)) {
    StringRef tokText = token.getText();
    for (auto accessor : allAccessorKinds())
      if (tokText == getAccessorLabel(accessor))
        return accessor;
  }
  return std::nullopt;
}

/// Helper function that parses a base type for `parseQualifiedDeclName`.
/// Returns true on error. Sets `baseType` to the parsed base type if present,
/// or to `nullptr` if not. A missing base type is not considered an error.
static bool parseBaseTypeForQualifiedDeclName(Parser &P, TypeRepr *&baseType) {
  baseType = nullptr;
  Parser::CancellableBacktrackingScope backtrack(P);

  // If base type cannot be parsed, return false (no error).
  if (!P.canParseBaseTypeForQualifiedDeclName())
    return false;

  auto result = P.parseQualifiedDeclNameBaseType();
  // If base type should be parseable but the actual base type result is null,
  // return true (error).
  if (result.isNull())
    return true;

  // Consume the leading period before the final declaration name component.
  // `parseQualifiedDeclNameBaseType` leaves the leading period unparsed to
  // avoid syntax verification errors.
  assert(P.startsWithSymbol(P.Tok, '.') && "false");

  // Check if this is a reference to a property or subscript accessor.
  //
  // Note: There is an parsing ambiguity here. An accessor label identifier
  // (e.g. "set") may refer to the final declaration name component instead of
  // an accessor kind.
  //
  // FIXME: It is wrong to backtrack parsing the entire base type if an accessor
  // label is found. Instead, only the final component of the base type should
  // be backtracked. It may be best to implement this in
  // `Parser::parseTypeIdentifier`.
  //
  // Example: consider parsing `A.B.property.set`.
  // Current behavior: base type is entirely backtracked.
  // Ideal behavior: base type is parsed as `A.B`.
  if (P.Tok.is(tok::period)) {
    const Token &nextToken = P.peekToken();
    if (isAccessorLabel(nextToken).has_value())
      return false;
  }

  backtrack.cancelBacktrack();
  P.consumeStartingCharacterOfCurrentToken(tok::period);

  // Set base type and return false (no error).
  baseType = result.getPtrOrNull();
  return false;
}

/// Parses an optional base type, followed by a declaration name.
/// Returns true on error (if declaration name could not be parsed).
///
/// \verbatim
///   qualified-decl-name:
///     qualified-decl-name-base-type? unqualified-decl-name
/// \endverbatim
///
// TODO(TF-1066): Use module qualified name syntax/parsing instead of custom
// qualified name syntax/parsing.
static bool parseQualifiedDeclName(Parser &P, Diag<> nameParseError,
                                   TypeRepr *&baseType,
                                   DeclNameRefWithLoc &original) {
  {
    // Parse base type.
    if (parseBaseTypeForQualifiedDeclName(P, baseType))
      return true;
    // Parse final declaration name.
    original.Name = P.parseDeclNameRef(
        original.Loc, nameParseError,
        Parser::DeclNameFlag::AllowZeroArgCompoundNames |
            Parser::DeclNameFlag::AllowKeywordsUsingSpecialNames |
            Parser::DeclNameFlag::AllowOperators |
            Parser::DeclNameFlag::AllowLowercaseAndUppercaseSelf);
    // The base type is optional, but the final unqualified declaration name is
    // not. If name could not be parsed, return true for error.
    if (!original.Name)
      return true;
  }

  // Parse an optional accessor kind.
  //
  // Note: there is an parsing ambiguity here.
  //
  // Example: `A.B.property.set` may be parsed as one of the following:
  //
  // 1. No accessor kind.
  // - Base type: `A.B.property`
  // - Declaration name: `set`
  // - Accessor kind: <none>
  //
  // 2. Accessor kind exists.
  // - Base type: `A.B`
  // - Declaration name: `property`
  // - Accessor kind: `set`
  //
  // Currently, we follow (2) because it's more useful in practice.
  if (P.Tok.is(tok::period)) {
    const Token &nextToken = P.peekToken();
    std::optional<AccessorKind> kind = isAccessorLabel(nextToken);
    if (kind.has_value()) {
      original.AccessorKind = kind;
      P.consumeIf(tok::period);
      P.consumeIf(tok::identifier);
    }
  }

  return false;
}

/// Parse a `@derivative(of:)` attribute, returning true on error.
///
/// \verbatim
///   derivative-attribute-arguments:
///     '(' 'of' ':' qualified-decl-name (',' differentiability-params-clause)?
///     ')'
/// \endverbatim
ParserResult<DerivativeAttr> Parser::parseDerivativeAttribute(SourceLoc atLoc,
                                                              SourceLoc loc) {
  StringRef AttrName = "derivative";
  SourceLoc rParenLoc = loc;
  TypeRepr *baseType = nullptr;
  DeclNameRefWithLoc original;
  SmallVector<ParsedAutoDiffParameter, 8> parameters;

  // Parse trailing comma, if it exists, and check for errors.
  auto consumeIfTrailingComma = [&](bool requireComma = false) -> bool {
    if (!consumeIf(tok::comma)) {
      // If comma is required but does not exist and ')' has not been reached,
      // diagnose missing comma.
      if (requireComma && !Tok.is(tok::r_paren)) {
        diagnose(getEndOfPreviousLoc(), diag::expected_separator, ",");
        return true;
      }
      return false;
    }
    // Diagnose trailing comma before ')'.
    if (Tok.is(tok::r_paren)) {
      diagnose(Tok, diag::unexpected_separator, ",");
      return errorAndSkipUntilConsumeRightParen(*this, AttrName);
    }
    // Check that token after comma is 'wrt:'.
    if (isIdentifier(Tok, "wrt"))
      return false;
    diagnose(Tok, diag::attr_expected_label, "wrt", AttrName);
    return errorAndSkipUntilConsumeRightParen(*this, AttrName);
  };
  // Parse '('.
  if (!consumeIfAttributeLParen()) {
    diagnose(getEndOfPreviousLoc(), diag::attr_expected_lparen, AttrName,
             /*DeclModifier*/ false);
    return makeParserError();
  }
  {
    // Parse the 'of:' label and colon.
    if (parseSpecificIdentifier("of", diag::attr_missing_label, "of",
                                AttrName) ||
        parseToken(tok::colon, diag::expected_colon_after_label, "of")) {
      return makeParserError();
    }
    {
      // Parse the optionally qualified function name.
      if (parseQualifiedDeclName(
              *this, diag::autodiff_attr_expected_original_decl_name,
              baseType, original))
        return makeParserError();
    }
    if (consumeIfTrailingComma(/*requireComma*/ true))
      return makeParserError();
    // Parse the optional 'wrt' differentiability parameters clause.
    if (isIdentifier(Tok, "wrt") &&
        parseDifferentiabilityParametersClause(parameters, AttrName))
      return makeParserError();
  }
  // Parse ')'.
  if (!consumeIf(tok::r_paren, rParenLoc)) {
    diagnose(getEndOfPreviousLoc(), diag::attr_expected_rparen, AttrName,
             /*DeclModifier*/ false);
    return makeParserError();
  }
  return ParserResult<DerivativeAttr>(DerivativeAttr::create(
      Context, /*implicit*/ false, atLoc, SourceRange(loc, rParenLoc), baseType,
      original, parameters));
}

/// Parse a `@transpose(of:)` attribute, returning true on error.
///
/// \verbatim
///   transpose-attribute-arguments:
///     '(' 'of' ':' qualified-decl-name (',' linearity-params-clause)? ')'
///   linearity-params-clause:
///     'wrt' ':' (linearity-param | linearity-params)
///   linearity-params:
///     '(' linearity-param (',' linearity-param)* ')'
///   linearity-param:
///     'self' | [0-9]+
/// \endverbatim
ParserResult<TransposeAttr> Parser::parseTransposeAttribute(SourceLoc atLoc,
                                                            SourceLoc loc) {
  StringRef AttrName = "transpose";
  SourceLoc rParenLoc = loc;
  TypeRepr *baseType = nullptr;
  DeclNameRefWithLoc original;
  SmallVector<ParsedAutoDiffParameter, 8> parameters;

  // Parse trailing comma, if it exists, and check for errors.
  auto consumeIfTrailingComma = [&](bool requireComma = false) -> bool {
    if (!consumeIf(tok::comma)) {
      // If comma is required but does not exist and ')' has not been reached,
      // diagnose missing comma.
      if (requireComma && !Tok.is(tok::r_paren)) {
        diagnose(Tok, diag::expected_separator, ",");
        return true;
      }
      return false;
    }
    // Diagnose trailing comma before ')'.
    if (Tok.is(tok::r_paren)) {
      diagnose(Tok, diag::unexpected_separator, ",");
      return errorAndSkipUntilConsumeRightParen(*this, AttrName);
    }
    // Check that token after comma is 'wrt:'.
    if (isIdentifier(Tok, "wrt"))
      return false;
    diagnose(Tok, diag::attr_expected_label, "wrt", AttrName);
    return errorAndSkipUntilConsumeRightParen(*this, AttrName);
  };

  // Parse '('.
  if (!consumeIfAttributeLParen()) {
    diagnose(getEndOfPreviousLoc(), diag::attr_expected_lparen, AttrName,
             /*DeclModifier*/ false);
    return makeParserError();
  }
  {
    // Parse the 'of:' label and colon.
    if (parseSpecificIdentifier("of", diag::attr_missing_label, "of",
                                AttrName) ||
        parseToken(tok::colon, diag::expected_colon_after_label, "of")) {
      return makeParserError();
    }
    {
      // Parse the optionally qualified function name.
      if (parseQualifiedDeclName(
              *this, diag::autodiff_attr_expected_original_decl_name,
              baseType, original))
        return makeParserError();
    }
    if (consumeIfTrailingComma(/*requireComma*/ true))
      return makeParserError();
    // Parse the optional 'wrt' linearity parameters clause.
    if (Tok.is(tok::identifier) && Tok.getText() == "wrt" &&
        parseDifferentiabilityParametersClause(parameters, AttrName,
                                               /*allowNamedParameters*/ false))
      return makeParserError();
  }
  // Parse ')'.
  if (!consumeIf(tok::r_paren, rParenLoc)) {
    diagnose(getEndOfPreviousLoc(), diag::attr_expected_rparen, AttrName,
             /*DeclModifier*/ false);
    return makeParserError();
  }
  return ParserResult<TransposeAttr>(TransposeAttr::create(
      Context, /*implicit*/ false, atLoc, SourceRange(loc, rParenLoc), baseType,
      original, parameters));
}

void Parser::parseObjCSelector(SmallVector<Identifier, 4> &Names,
                               SmallVector<SourceLoc, 4> &NameLocs,
                               bool &IsNullarySelector) {
  IsNullarySelector = true;
  while (true) {
    // Empty selector piece.
    if (Tok.is(tok::colon)) {
      Names.push_back(Identifier());
      NameLocs.push_back(Tok.getLoc());
      IsNullarySelector = false;
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
        IsNullarySelector = false;
        continue;
      }

      // If we see a closing parentheses, we're done.
      if (Tok.is(tok::r_paren)) {
        // If we saw more than one identifier, there's a ':'
        // missing here. Complain and pretend we saw it.
        if (Names.size() > 1) {
          diagnose(Tok, diag::attr_objc_missing_colon)
          .fixItInsertAfter(NameLocs.back(), ":");
          IsNullarySelector = false;
        }

        break;
      }

      // If we see another identifier or keyword, complain about
      // the missing colon and keep going.
      if (Tok.is(tok::identifier) || Tok.isKeyword()) {
        diagnose(Tok, diag::attr_objc_missing_colon)
        .fixItInsertAfter(NameLocs.back(), ":");
        IsNullarySelector = false;
        continue;
      }

      // We don't know what happened. Break out.
      break;
    }

    // We didn't parse anything, don't create a selector piece.
    break;
  }
}

bool Parser::peekAvailabilityMacroName() {
  return Context.getAvailabilityMacroMap().hasMacroName(Tok.getText());
}

ParserStatus
Parser::parseAvailabilityMacro(SmallVectorImpl<AvailabilitySpec *> &Specs) {
  // Get the macros from the compiler arguments.
  const AvailabilityMacroMap &Map = Context.getAvailabilityMacroMap();

  StringRef MacroName = Tok.getText();
  auto NameMatch = Map.Impl.find(MacroName);
  if (NameMatch == Map.Impl.end())
    return makeParserSuccess(); // No match, it could be a standard platform.

  consumeToken();

  llvm::VersionTuple Version;
  SourceRange VersionRange;
  if (Tok.isAny(tok::integer_literal, tok::floating_literal)) {
    if (parseVersionTuple(Version, VersionRange,
                          diag::avail_query_expected_version_number))
      return makeParserError();
  }

  auto VersionMatch = NameMatch->second.find(Version);
  if (VersionMatch == NameMatch->second.end()) {
    diagnose(PreviousLoc, diag::attr_availability_unknown_version,
        Version.getAsString(), MacroName);
    return makeParserError(); // Failed to match the version, that's an error.
  }

  // Make a copy of the specs to add the macro source location
  // for the diagnostic about the use of macros in inlinable code.
  SourceLoc MacroLoc = Tok.getLoc();
  for (auto *Spec : VersionMatch->getSecond()) {
    auto SpecCopy = Spec->clone(Context);
    SpecCopy->setMacroLoc(MacroLoc);
    Specs.push_back(SpecCopy);
  }

  return makeParserSuccess();
}

static PlatformKind getPlatformFromDomainOrIdentifier(
    const AvailabilityDomainOrIdentifier &domainOrIdentifier) {
  if (auto domain = domainOrIdentifier.getAsDomain())
    return domain->getPlatformKind();

  if (auto platform =
          platformFromString(domainOrIdentifier.getAsIdentifier()->str()))
    return *platform;

  return PlatformKind::none;
}

ParserStatus Parser::parsePlatformVersionInList(StringRef AttrName,
    llvm::SmallVector<PlatformAndVersion, 4> &PlatformAndVersions,
    bool &ParsedUnrecognizedPlatformName) {
  // Check for availability macros first.
  if (peekAvailabilityMacroName()) {
    SmallVector<AvailabilitySpec *, 4> Specs;
    ParserStatus MacroStatus = parseAvailabilityMacro(Specs);
    if (MacroStatus.isError())
      return MacroStatus;

    for (auto *Spec : Specs) {
      auto Platform =
          getPlatformFromDomainOrIdentifier(Spec->getDomainOrIdentifier());
      if (Platform == PlatformKind::none)
        continue;

      auto Version = Spec->getRawVersion();
      if (Version.getSubminor().has_value() || Version.getBuild().has_value()) {
        diagnose(Spec->getVersionSrcRange().Start,
                 diag::attr_availability_platform_version_major_minor_only,
                 AttrName);
      }
      PlatformAndVersions.emplace_back(Platform, Version);
    }

    return makeParserSuccess();
  }

  // Expect a possible platform name (e.g. 'macOS' or '*').
  if (!Tok.isAny(tok::identifier, tok::oper_binary_spaced)) {
    diagnose(Tok, diag::attr_availability_expected_platform, AttrName);
    return makeParserError();
  }

  // Parse the platform name.
  StringRef platformText = Tok.getText();
  auto MaybePlatform = platformFromString(platformText);
  ParsedUnrecognizedPlatformName = ParsedUnrecognizedPlatformName || !MaybePlatform.has_value();
  SourceLoc PlatformLoc = Tok.getLoc();
  consumeToken();

  if (!MaybePlatform.has_value()) {
    if (auto correctedPlatform = closestCorrectedPlatformString(platformText)) {
      diagnose(PlatformLoc, diag::attr_availability_suggest_platform,
               platformText, AttrName, *correctedPlatform)
          .fixItReplace(SourceRange(PlatformLoc), *correctedPlatform);
    } else {
      diagnose(PlatformLoc, diag::attr_availability_unknown_platform,
               platformText, AttrName);
    }
  } else if (*MaybePlatform == PlatformKind::none) {
    // Wildcards ('*') aren't supported in this kind of list.
    diagnose(PlatformLoc, diag::attr_availability_wildcard_ignored,
             AttrName);

    // If this list entry is just a wildcard, skip it.
    if (Tok.isAny(tok::comma, tok::r_paren))
      return makeParserSuccess();
  }

  // Parse version number.
  llvm::VersionTuple VerTuple;
  SourceRange VersionRange;
  if (parseVersionTuple(VerTuple, VersionRange,
        {diag::attr_availability_expected_version, {AttrName}})) {
    return makeParserError();
  }

  // Diagnose specification of patch versions (e.g. '13.0.1').
  if (VerTuple.getSubminor().has_value() ||
      VerTuple.getBuild().has_value()) {
    diagnose(VersionRange.Start,
             diag::attr_availability_platform_version_major_minor_only,
             AttrName);
  }

  if (MaybePlatform.has_value()) {
    auto Platform = *MaybePlatform;
    if (Platform != PlatformKind::none) {
      PlatformAndVersions.emplace_back(Platform, VerTuple);
    }
  }

  return makeParserSuccess();
}

bool Parser::parseBackDeployedAttribute(DeclAttributes &Attributes,
                                        StringRef AttrName, SourceLoc AtLoc,
                                        SourceLoc Loc) {
  std::string AtAttrName = (llvm::Twine("@") + AttrName).str();
  auto LeftLoc = Tok.getLoc();
  if (!consumeIfAttributeLParen()) {
    diagnose(Loc, diag::attr_expected_lparen, AtAttrName,
             DeclAttribute::isDeclModifier(DeclAttrKind::BackDeployed));
    return false;
  }

  SourceLoc RightLoc;
  ParserStatus Status;
  bool SuppressLaterDiags = false;
  bool ParsedUnrecognizedPlatformName = false;
  llvm::SmallVector<PlatformAndVersion, 4> PlatformAndVersions;

  {
    // Parse 'before' ':'.
    if (Tok.is(tok::identifier) && Tok.getText() == "before") {
      consumeToken();
      if (!consumeIf(tok::colon))
        diagnose(Tok, diag::attr_back_deploy_expected_colon_after_before)
            .fixItInsertAfter(PreviousLoc, ":");
    } else {
      diagnose(Tok, diag::attr_back_deploy_expected_before_label)
          .fixItInsertAfter(PreviousLoc, "before:");
    }

    // Parse the version list.
    if (!Tok.is(tok::r_paren)) {
      ParseListItemResult Result;
      do {
        Result = parseListItem(
            Status, tok::r_paren, LeftLoc, RightLoc,
            /*AllowSepAfterLast=*/false, [&]() -> ParserStatus {
              return parsePlatformVersionInList(AtAttrName, PlatformAndVersions,
                                                ParsedUnrecognizedPlatformName);
            });
      } while (Result == ParseListItemResult::Continue);
    }
  }

  if (parseMatchingToken(tok::r_paren, RightLoc,
                         diag::attr_back_deploy_missing_rparen, LeftLoc))
    return false;

  if (Status.isErrorOrHasCompletion() || SuppressLaterDiags) {
    return false;
  }

  if (PlatformAndVersions.empty() && !ParsedUnrecognizedPlatformName) {
    diagnose(Loc, diag::attr_availability_need_platform_version, AtAttrName);
    return false;
  }

  auto AttrRange = SourceRange(Loc, RightLoc);
  for (auto &Item : PlatformAndVersions) {
    Attributes.add(new (Context) BackDeployedAttr(AtLoc, AttrRange, Item.first,
                                                  Item.second,
                                                  /*IsImplicit*/ false));
  }
  return true;
}

static bool isKnownDocumentationAttributeArgument(StringRef ArgumentName) {
  return llvm::StringSwitch<bool>(ArgumentName)
    .Case("visibility", true)
    .Case("metadata", true)
    .Default(false);
}

bool Parser::parseDocumentationAttributeArgument(
    std::optional<StringRef> &Metadata,
    std::optional<AccessLevel> &Visibility) {
  if (Tok.isNot(tok::identifier)) {
    diagnose(Tok.getLoc(), diag::documentation_attr_expected_argument);
    return false;
  }

  auto ArgumentName = Tok.getText();

  if (!isKnownDocumentationAttributeArgument(ArgumentName)) {
    diagnose(Tok.getLoc(), diag::documentation_attr_unknown_argument, ArgumentName);
    return false;
  }

  consumeToken(tok::identifier);
  if (!consumeIf(tok::colon)) {
    diagnose(Tok.getLoc(), diag::expected_colon_after_label, ArgumentName);
    return false;
  }

  if (ArgumentName == "visibility") {
    if (!Tok.isAny(tok::kw_public, tok::kw_internal, tok::kw_private, tok::kw_fileprivate, tok::identifier)) {
      diagnose(Tok.getLoc(), diag::documentation_attr_expected_access_level);
      return false;
    }
    auto ArgumentValue = Tok.getText();
    std::optional<AccessLevel> ParsedVisibility =
        llvm::StringSwitch<std::optional<AccessLevel>>(ArgumentValue)
            .Case("open", AccessLevel::Open)
            .Case("public", AccessLevel::Public)
            .Case("package", AccessLevel::Package)
            .Case("internal", AccessLevel::Internal)
            .Case("private", AccessLevel::Private)
            .Case("fileprivate", AccessLevel::FilePrivate)
            .Default(std::nullopt);

    if (!ParsedVisibility) {
      diagnose(Tok.getLoc(), diag::documentation_attr_unknown_access_level, ArgumentValue);
      return false;
    }

    if (Visibility) {
      diagnose(Tok.getLoc(), diag::documentation_attr_duplicate_visibility);
      return false;
    }

    consumeToken();
    Visibility = ParsedVisibility;
  } else if (ArgumentName == "metadata") {
    if (!Tok.isAny(tok::identifier, tok::string_literal)) {
      diagnose(Tok.getLoc(), diag::documentation_attr_metadata_expected_text);
      return false;
    }
    auto ArgumentValue = Tok.getText();
    if (ArgumentValue.front() == '\"' && ArgumentValue.back() == '\"') {
      // String literals get saved with surrounding quotes. Trim them off if they're present.
      ArgumentValue = ArgumentValue.slice(1, ArgumentValue.size() - 1);
    }

    if (Metadata) {
      diagnose(Tok.getLoc(), diag::documentation_attr_duplicate_metadata);
      return false;
    }

    consumeToken();
    Metadata = ArgumentValue;
  } else {
    llvm_unreachable("unimplemented @_documentation attr argument");
  }

  return true;
}

ParserStatus
Parser::parseAttributeArguments(SourceLoc attrLoc, StringRef attrName,
                                bool isModifier, SourceRange &parensRange,
                                llvm::function_ref<ParserStatus()> parseArg) {
  parensRange.Start = Tok.getLoc();
  if (!consumeIfAttributeLParen()) {
    diagnose(attrLoc, diag::attr_expected_lparen, attrName, isModifier);
    return makeParserError();
  }

  return parseList(tok::r_paren, parensRange.Start, parensRange.End,
                   /*AllowSepAfterLast=*/true,
                   {diag::attr_expected_rparen, {attrName, isModifier}},
                   parseArg);
}

ParserResult<DocumentationAttr>
Parser::parseDocumentationAttribute(SourceLoc atLoc, SourceLoc loc) {
  StringRef attrName = "_documentation";

  std::optional<AccessLevel> visibility = std::nullopt;
  std::optional<StringRef> metadata = std::nullopt;

  SourceRange parensRange;
  auto status = parseAttributeArguments(loc, attrName, false, parensRange,
                                        [&] {
    if (!parseDocumentationAttributeArgument(metadata, visibility))
      return makeParserError();
    return makeParserSuccess();
  });
  if (!status.isSuccess())
    return status;

  auto range = SourceRange(loc, parensRange.End);
  StringRef finalMetadata = metadata.value_or("");

  return makeParserResult(
    new (Context) DocumentationAttr(atLoc, range, finalMetadata,
                                    visibility, false));
}

ParserResult<AllowFeatureSuppressionAttr>
Parser::parseAllowFeatureSuppressionAttribute(bool inverted, SourceLoc atLoc,
                                              SourceLoc loc) {
  StringRef attrName =
      inverted ? "_disallowFeatureSuppression" : "_allowFeatureSuppression";

  SmallVector<Identifier, 4> features;
  SourceRange parensRange;
  auto status = parseAttributeArguments(loc, attrName, /*modifier*/ false,
                                        parensRange, [&] {
    Identifier feature;
    if (parseAnyIdentifier(feature, /*diagnose dollars*/ true,
                           diag::attr_expected_feature_name, attrName))
      return makeParserError();
    features.push_back(feature);
    return makeParserSuccess();
  });
  if (!status.isSuccess())
    return status;

  auto range = SourceRange(loc, parensRange.End);
  return makeParserResult(AllowFeatureSuppressionAttr::create(
      Context, atLoc, range, /*implicit*/ false, /*inverted*/ inverted,
      features));
}

static std::optional<MacroIntroducedDeclNameKind>
getMacroIntroducedDeclNameKind(Identifier name) {
  return llvm::StringSwitch<std::optional<MacroIntroducedDeclNameKind>>(
             name.str())
      .Case("named", MacroIntroducedDeclNameKind::Named)
      .Case("overloaded", MacroIntroducedDeclNameKind::Overloaded)
      .Case("prefixed", MacroIntroducedDeclNameKind::Prefixed)
      .Case("suffixed", MacroIntroducedDeclNameKind::Suffixed)
      .Case("arbitrary", MacroIntroducedDeclNameKind::Arbitrary)
      .Default(std::nullopt);
}

/// Determine the macro role based on its name.
std::optional<MacroRole> getMacroRole(StringRef roleName) {
  // Match the role string to the known set of roles.
  return llvm::StringSwitch<std::optional<MacroRole>>(roleName)
#define MACRO_ROLE(Name, Description)  \
      .Case(Description, MacroRole::Name)
#include "swift/Basic/MacroRoles.def"
      .Default(std::nullopt);
}

static ParameterizedDeclAttributeKind getParameterizedDeclAttributeKind(bool isAttached) {
  if (isAttached) {
    return ParameterizedDeclAttributeKind::AttachedMacro;
  } else {
    return ParameterizedDeclAttributeKind::FreestandingMacro;
  }
}

ParserResult<MacroRoleAttr>
Parser::parseMacroRoleAttribute(
    MacroSyntax syntax, SourceLoc AtLoc, SourceLoc Loc)
{
  StringRef attrName;
  bool isAttached;
  switch (syntax) {
  case MacroSyntax::Freestanding:
    attrName = "freestanding";
    isAttached = false;
    break;

  case MacroSyntax::Attached:
    attrName = "attached";
    isAttached = true;
    break;
  }

  if (!Tok.isFollowingLParen()) {
    diagnose(Tok, diag::attr_expected_lparen, attrName, false);
    return makeParserError();
  }

  // Parse the argments.
  SourceLoc lParenLoc = consumeAttributeLParen();
  SourceLoc rParenLoc;
  std::optional<MacroRole> role;
  bool sawRole = false;
  bool sawConformances = false;
  bool sawNames = false;
  SmallVector<MacroIntroducedDeclName, 2> names;
  SmallVector<Expr *, 2> conformances;
  auto argumentsStatus = parseList(
      tok::r_paren, lParenLoc, rParenLoc,
      /*AllowSepAfterLast=*/false, diag::expected_rparen_expr_list, [&] {
        ParserStatus status;

        if (consumeIf(tok::code_complete)) {
          status.setHasCodeCompletionAndIsError();
          if (!sawRole) {
            sawRole = true;
            if (this->CodeCompletionCallbacks) {
              this->CodeCompletionCallbacks->completeDeclAttrParam(
                  getParameterizedDeclAttributeKind(isAttached), 0,
                  /*HasLabel=*/false);
            }
          } else if (!sawNames) {
            if (this->CodeCompletionCallbacks) {
              this->CodeCompletionCallbacks->completeDeclAttrParam(
                  getParameterizedDeclAttributeKind(isAttached), 1,
                  /*HasLabel=*/false);
            }
          }
        }

        // Parse the argment label, if there is one.
        Identifier fieldName;
        SourceLoc fieldNameLoc;
        parseOptionalArgumentLabel(fieldName, fieldNameLoc);

        // If there is a field name, it better be 'names'.
        if (!(fieldName.empty() || fieldName.is("names") ||
              fieldName.is("conformances"))) {
          diagnose(fieldNameLoc, diag::macro_attribute_unknown_label,
                   isAttached, fieldName);
          status.setIsParseError();
          return status;
        }

        // If there is no field name and we haven't seen either names or the
        // role, this is the role.
        if (fieldName.empty() && !sawConformances && !sawNames && !sawRole) {
          // Whether we saw anything we tried to treat as a role.
          sawRole = true;

          auto diagKind =
              isAttached ? diag::macro_role_attr_expected_attached_kind
                         : diag::macro_role_attr_expected_freestanding_kind;
          Identifier roleName;
          SourceLoc roleNameLoc;
          if (Tok.is(tok::kw_extension)) {
            roleNameLoc = consumeToken();
            role = MacroRole::Extension;
          } else if (parseIdentifier(roleName, roleNameLoc, diagKind,
                                     /*diagnoseDollarPrefix=*/true)) {
            status.setIsParseError();
            return status;
          }

          if (!role)
            role = getMacroRole(roleName.str());

          if (!role) {
            diagnose(roleNameLoc, diag::macro_role_attr_expected_kind,
                     isAttached);
            status.setIsParseError();
            return status;
          }
          if (!isMacroSupported(*role, Context)) {
            diagnose(roleNameLoc, diag::macro_experimental, roleName.str(), "");
            status.setIsParseError();
            return status;
          }

          // Check that the role makes sense.
          if (isAttached == !isAttachedMacro(*role)) {
            diagnose(roleNameLoc, diag::macro_role_syntax_mismatch, isAttached,
                     roleName);

            status.setIsParseError();
            return status;
          }

          return status;
        }

        if (fieldName.is("conformances") ||
            (fieldName.empty() && sawConformances && !sawNames)) {
          if (fieldName.is("conformances") && sawConformances) {
            diagnose(fieldNameLoc.isValid() ? fieldNameLoc : Tok.getLoc(),
                     diag::macro_attribute_duplicate_label, isAttached,
                     "conformances");
          }

          sawConformances = true;

          // Parse the introduced conformances
          auto expr = parseExpr(diag::expected_type);
          if (expr.isNonNull())
            conformances.push_back(expr.get());

          return status;
        }

        // If the field name is empty and we haved seen "names", or the field
        // name is "names" but we've already seen the argument label, complain.
        if (fieldName.empty() != sawNames) {
          diagnose(fieldNameLoc.isValid() ? fieldNameLoc : Tok.getLoc(),
                   sawNames ? diag::macro_attribute_duplicate_label
                            : diag::macro_attribute_missing_label,
                   isAttached, "names");
        }
        sawNames = true;

        // Parse the introduced name kind.
        Identifier introducedNameKind;
        SourceLoc introducedNameKindLoc;
        if (consumeIf(tok::code_complete)) {
          status.setHasCodeCompletionAndIsError();
          if (this->CodeCompletionCallbacks) {
            this->CodeCompletionCallbacks->completeDeclAttrParam(
                getParameterizedDeclAttributeKind(isAttached), 1, /*HasLabel=*/true);
          }
        } else if (parseIdentifier(introducedNameKind, introducedNameKindLoc,
                                   diag::macro_attribute_unknown_argument_form,
                                   /*diagnoseDollarPrefix=*/true)) {
          status.setIsParseError();
          return status;
        }

        auto introducedKind =
            getMacroIntroducedDeclNameKind(introducedNameKind);
        if (!introducedKind) {
          diagnose(introducedNameKindLoc,
                   diag::macro_attribute_unknown_name_kind, introducedNameKind);
          status.setIsParseError();
          return status;
        }

        // If we don't need an argument, we're done.
        if (!macroIntroducedNameRequiresArgument(*introducedKind)) {
          // If there is an argument, complain about it.
          if (Tok.is(tok::l_paren)) {
            diagnose(Tok,
                     diag::macro_attribute_introduced_name_requires_no_argument,
                     introducedNameKind);
            skipSingle();
          }

          names.push_back(MacroIntroducedDeclName(*introducedKind));
          return status;
        }

        if (!Tok.is(tok::l_paren)) {
          diagnose(Tok, diag::macro_attribute_introduced_name_requires_argument,
                   introducedNameKind);
          status.setIsParseError();
          return status;
        }

        // Parse the name.
        (void)consumeToken(tok::l_paren);
        DeclNameLoc nameLoc;
        DeclNameRef name = parseDeclNameRef(
            nameLoc, diag::macro_attribute_unknown_argument_form,
            (DeclNameFlag::AllowOperators | DeclNameFlag::AllowKeywords |
             DeclNameFlag::AllowKeywordsUsingSpecialNames |
             DeclNameFlag::AllowCompoundNames |
             DeclNameFlag::AllowZeroArgCompoundNames));
        if (!name) {
          status.setIsParseError();
          return status;
        }

        SourceLoc rParenLoc;
        if (!consumeIf(tok::r_paren, rParenLoc)) {
          diagnose(Tok, diag::attr_expected_rparen, attrName, false);
          rParenLoc = Tok.getLoc();
        }

        // Add the name we introduced.
        names.push_back(
            MacroIntroducedDeclName(*introducedKind, name.getFullName()));

        return status;
      });

  if (argumentsStatus.isErrorOrHasCompletion())
    return argumentsStatus;

  // Diagnose a missing role.
  if (!role) {
    // If we saw a role of any kind, we already diagnosed this.
    if (!sawRole) {
      diagnose(lParenLoc, diag::macro_role_attr_expected_kind, isAttached);
    }

    return makeParserError();
  }

  SourceRange range(Loc, rParenLoc);
  return makeParserResult(MacroRoleAttr::create(
      Context, AtLoc, range, syntax, lParenLoc, *role, names,
      conformances, rParenLoc, /*isImplicit*/ false));
}

/// Guts of \c parseSingleAttrOption and \c parseSingleAttrOptionIdentifier.
///
/// \param P The parser object.
/// \param Loc The location of the attribute name (before the \c tok::l_paren, if any).
/// \param AttrRange Will be set to the range of the entire attribute, including its option if any.
/// \param AttrName The spelling of the attribute in the source code. Used in diagnostics.
/// \param DK The kind of the attribute being parsed.
/// \param allowOmitted If true, treat a missing argument list as permitted and return
///        \c Identifier() ; if false, diagnose a missing argument list as an error.
/// \param nonIdentifierDiagnostic The diagnostic to emit if something other than a
///        \c tok::identifier is used as an argument.
/// \param PDK Specific kind of the parameterized attribute being passed. Used when invoking code completion callbacks.
///
/// \returns \c None if an error was diagnosed; \c Identifier() if the argument list was permissibly
///          omitted; the identifier written by the user otherwise.
static std::optional<Identifier> parseSingleAttrOptionImpl(
    Parser &P, SourceLoc Loc, SourceRange &AttrRange, StringRef AttrName,
    DeclAttrKind DK, bool allowOmitted, DiagRef nonIdentifierDiagnostic,
    std::optional<ParameterizedDeclAttributeKind> PDK = std::nullopt) {
  SWIFT_DEFER {
    AttrRange = SourceRange(Loc, P.PreviousLoc);
  };
  bool isDeclModifier = DeclAttribute::isDeclModifier(DK);

  if (!P.Tok.is(tok::l_paren)) {
    if (allowOmitted)
      return Identifier();

    P.diagnose(Loc, diag::attr_expected_lparen, AttrName, isDeclModifier);
    return std::nullopt;
  }

  P.consumeAttributeLParen();
  
  if (P.Tok.is(tok::code_complete) && P.CodeCompletionCallbacks && PDK) {
    P.CodeCompletionCallbacks->completeDeclAttrParam(*PDK, 0, /*HasLabel=*/false);
    P.consumeToken(tok::code_complete);
  }

  StringRef parsedName = P.Tok.getText();
  if (!P.consumeIf(tok::identifier)) {
    P.diagnose(Loc, nonIdentifierDiagnostic);
    return std::nullopt;
  }
  
  if (!P.consumeIf(tok::r_paren)) {
    P.diagnose(Loc, diag::attr_expected_rparen, AttrName, isDeclModifier);
    return std::nullopt;
  }

  return P.Context.getIdentifier(parsedName);
}

static std::optional<LifetimeDescriptor>
parseLifetimeDescriptor(Parser &P,
                        ParsedLifetimeDependenceKind lifetimeDependenceKind =
                            ParsedLifetimeDependenceKind::Default) {
  auto token = P.Tok;
  switch (token.getKind()) {
  case tok::identifier: {
    Identifier name;
    auto loc = P.consumeIdentifier(name, /*diagnoseDollarPrefix=*/false);
    
    // In SIL, lifetimes explicitly state whether they are dependent on a
    // memory location in addition to the value stored at that location.
    if (P.isInSILMode()
        && (name.str() == "address" || name.str() == "address_for_deps")
        && P.Tok.is(tok::integer_literal)) {
      SourceLoc orderedLoc;
      unsigned index;
      
      if (P.parseUnsignedInteger(
              index, loc, diag::expected_param_index_lifetime_dependence)) {
        return std::nullopt;
      }
      return LifetimeDescriptor::forOrdered(index, lifetimeDependenceKind, loc,
        name.str() == "address_for_deps"
          ? LifetimeDescriptor::IsConditionallyAddressable
          : LifetimeDescriptor::IsAddressable);
    }
    
    return LifetimeDescriptor::forNamed(name, lifetimeDependenceKind, loc);
  }
  case tok::integer_literal: {
    SourceLoc loc;
    unsigned index;
    if (P.parseUnsignedInteger(
            index, loc, diag::expected_param_index_lifetime_dependence)) {
      return std::nullopt;
    }
    return LifetimeDescriptor::forOrdered(index, lifetimeDependenceKind, loc);
  }
  case tok::kw_self: {
    auto loc = P.consumeToken(tok::kw_self);
    return LifetimeDescriptor::forSelf(lifetimeDependenceKind, loc);
  }
  default: {
    P.diagnose(
        token,
        diag::expected_identifier_or_index_or_self_after_lifetime_dependence);
    return std::nullopt;
  }
  }
}

ParserResult<LifetimeAttr> Parser::parseLifetimeAttribute(StringRef attrName,
                                                          SourceLoc atLoc,
                                                          SourceLoc loc) {
  ParserStatus status;

  auto lifetimeEntry = parseLifetimeEntry(loc);
  if (lifetimeEntry.isNull()) {
    status.setIsParseError();
    return status;
  }

  return ParserResult<LifetimeAttr>(LifetimeAttr::create(
      Context, atLoc, SourceRange(loc, lifetimeEntry.get()->getEndLoc()),
      /* implicit */ false, lifetimeEntry.get(),
      attrName.compare("_lifetime") == 0));
}

/// Parses a (possibly optional) argument for an attribute containing a single, arbitrary identifier.
///
/// \param P The parser object.
/// \param Loc The location of the attribute name (before the \c tok::l_paren, if any).
/// \param AttrRange Will be set to the range of the entire attribute, including its option if any.
/// \param AttrName The spelling of the attribute in the source code. Used in diagnostics.
/// \param DK The kind of the attribute being parsed.
/// \param allowOmitted If true, treat a missing argument list as permitted and return
///        \c Identifier() ; if false, diagnose a missing argument list as an error.
///
/// \returns \c None if an error was diagnosed; \c Identifier() if the argument list was permissibly
///          omitted; the identifier written by the user otherwise.
static std::optional<Identifier>
parseSingleAttrOptionIdentifier(Parser &P, SourceLoc Loc, SourceRange &AttrRange,
                                StringRef AttrName, DeclAttrKind DK,
                                bool allowOmitted = false) {
  return parseSingleAttrOptionImpl(
             P, Loc, AttrRange, AttrName, DK, allowOmitted,
             {diag::attr_expected_option_identifier, {AttrName}});
}

/// Parses a (possibly optional) argument for an attribute containing a single identifier from a known set of
/// supported values, mapping it to a domain-specific type.
///
/// \param P The parser object.
/// \param Loc The location of the attribute name (before the \c tok::l_paren, if any).
/// \param AttrRange Will be set to the range of the entire attribute, including its option if any.
/// \param AttrName The spelling of the attribute in the source code. Used in diagnostics.
/// \param DK The kind of the attribute being parsed.
/// \param options The set of permitted keywords and their corresponding values.
/// \param valueIfOmitted If present, treat a missing argument list as permitted and return
///        the provided value; if absent, diagnose a missing argument list as an error.
/// \param PDK Specific kind of the parameterized attribute being passed. Used when invoking code completion callbacks.
///
/// \returns \c None if an error was diagnosed; the value corresponding to the identifier written by the
///          user otherwise.
template <typename R>
static std::optional<R>
parseSingleAttrOption(Parser &P, SourceLoc Loc, SourceRange &AttrRange,
                      StringRef AttrName, DeclAttrKind DK,
                      ArrayRef<std::pair<Identifier, R>> options,
                      std::optional<R> valueIfOmitted = std::nullopt,
                      std::optional<ParameterizedDeclAttributeKind> PDK = std::nullopt) {
  auto parsedIdentifier = parseSingleAttrOptionImpl(
             P, Loc, AttrRange, AttrName, DK,
             /*allowOmitted=*/valueIfOmitted.has_value(),
             {diag::attr_expected_option_such_as,
             {AttrName, options.front().first.str()}}, PDK);

  if (!parsedIdentifier)
    return std::nullopt;

  // If omitted (and omission is permitted), return valueIfOmitted.
  if (parsedIdentifier == Identifier())
    return *valueIfOmitted;

  for (auto &option : options)
    if (option.first == *parsedIdentifier)
      return option.second;

  P.diagnose(Loc, diag::attr_unknown_option, parsedIdentifier->str(), AttrName);
  return std::nullopt;
}

ParserStatus Parser::parseNewDeclAttribute(DeclAttributes &Attributes,
                                           SourceLoc AtLoc, DeclAttrKind DK,
                                           bool isFromClangAttribute) {
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
  if ((DeclAttribute::getBehaviors(DK) & DeclAttribute::SILOnly) != 0 &&
      !isInSILMode()) {
    diagnose(Loc, diag::only_allowed_in_sil, AttrName);
    DiscardAttribute = true;
  }

  // If this attribute is only permitted when concurrency is enabled, reject it.
  if (DeclAttribute::isConcurrencyOnly(DK) &&
      !shouldParseExperimentalConcurrency()) {
    // Ignore concurrency-only attributes that come from Clang.
    if (!isFromClangAttribute) {
      diagnose(
          Loc, diag::attr_requires_concurrency, AttrName,
          DeclAttribute::isDeclModifier(DK));
    }

    DiscardAttribute = true;
  }

  // Filled in during parsing.  If there is a duplicate
  // diagnostic this can be used for better error presentation.
  SourceRange AttrRange;

  ParserStatus Status;

  switch (DK) {
  case DeclAttrKind::RawDocComment:
  case DeclAttrKind::ObjCBridged:
  case DeclAttrKind::RestatedObjCConformance:
  case DeclAttrKind::SynthesizedProtocol:
  case DeclAttrKind::ClangImporterSynthesizedType:
  case DeclAttrKind::Custom:
    llvm_unreachable("virtual attributes should not be parsed "
                     "by attribute parsing code");
  case DeclAttrKind::SetterAccess:
    llvm_unreachable("handled by DeclAttrKind::AccessControl");

#define SIMPLE_DECL_ATTR(_, CLASS, ...) case DeclAttrKind::CLASS:
#include "swift/AST/DeclAttr.def"
    if (!DiscardAttribute)
      Attributes.add(DeclAttribute::createSimple(Context, DK, AtLoc, Loc));
    break;

  case DeclAttrKind::Effects: {
    if (!consumeIfAttributeLParen()) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }
    EffectsKind kind = EffectsKind::Unspecified;
    SourceLoc customStart, customEnd;
    {
      if (Tok.isNot(tok::identifier)) {
        diagnose(Loc, diag::error_in_effects_attribute, "expected identifier");
        return makeParserSuccess();
      }

      if (Tok.getText() == "readonly")
        kind = EffectsKind::ReadOnly;
      else if (Tok.getText() == "readnone")
        kind = EffectsKind::ReadNone;
      else if (Tok.getText() == "readwrite")
        kind = EffectsKind::ReadWrite;
      else if (Tok.getText() == "releasenone")
        kind = EffectsKind::ReleaseNone;
      else {
        customStart = customEnd = Tok.getLoc();
        while (Tok.isNot(tok::r_paren) && Tok.isNot(tok::eof)) {
          consumeToken();
        }
        customEnd = Tok.getLoc();
        kind = EffectsKind::Custom;
      }
      if (kind != EffectsKind::Custom) {
        consumeToken(tok::identifier);
      }
    }
    SourceLoc rParenLoc;
    if (!consumeIf(tok::r_paren, rParenLoc)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }
    AttrRange = SourceRange(Loc, rParenLoc);

    if (!DiscardAttribute) {
      if (kind == EffectsKind::Custom) {
        StringRef customStr = SourceMgr.extractText(
                          CharSourceRange(SourceMgr, customStart, customEnd));
        Attributes.add(new (Context) EffectsAttr(AtLoc, AttrRange,
                                                 customStr, customStart));
      } else {
        Attributes.add(new (Context) EffectsAttr(AtLoc, AttrRange, kind));
      }
    }
    break;
  }

  case DeclAttrKind::Inline: {
    auto kind = parseSingleAttrOption<InlineKind>(
        *this, Loc, AttrRange, AttrName, DK, {
          { Context.Id_never,   InlineKind::Never },
          { Context.Id__always, InlineKind::Always }
        });
    if (!kind)
      return makeParserSuccess();

    if (!DiscardAttribute)
      Attributes.add(new (Context) InlineAttr(AtLoc, AttrRange, *kind));

    break;
  }

  case DeclAttrKind::Optimize: {
    auto optMode = parseSingleAttrOption<OptimizationMode>(
        *this, Loc, AttrRange, AttrName, DK, {
          { Context.Id_speed, OptimizationMode::ForSpeed },
          { Context.Id_size,  OptimizationMode::ForSize },
          { Context.Id_none,  OptimizationMode::NoOptimization }
        });
    if (!optMode)
      return makeParserSuccess();

    if (!DiscardAttribute)
      Attributes.add(new (Context) OptimizeAttr(AtLoc, AttrRange, *optMode));

    break;
  }

  case DeclAttrKind::Exclusivity: {
    auto mode = parseSingleAttrOption<ExclusivityAttr::Mode>(
           *this, Loc, AttrRange, AttrName, DK, {
             { Context.Id_checked, ExclusivityAttr::Mode::Checked },
             { Context.Id_unchecked, ExclusivityAttr::Mode::Unchecked }
           });
    if (!mode)
      return makeParserSuccess();

    if (!DiscardAttribute)
      Attributes.add(new (Context) ExclusivityAttr(AtLoc, AttrRange, *mode));

    break;
  }

  case DeclAttrKind::ReferenceOwnership: {
    // Handle weak/unowned/unowned(unsafe).
    auto Kind = AttrName == "weak" ? ReferenceOwnership::Weak
                                   : ReferenceOwnership::Unowned;

    if (Kind == ReferenceOwnership::Unowned) {
      // Parse an optional specifier after unowned.
      Kind = parseSingleAttrOption<ReferenceOwnership>(
          *this, Loc, AttrRange, AttrName, DK, {
            { Context.Id_unsafe, ReferenceOwnership::Unmanaged },
            { Context.Id_safe,   ReferenceOwnership::Unowned }
          }, ReferenceOwnership::Unowned,
          ParameterizedDeclAttributeKind::Unowned)
            // Recover from errors by going back to Unowned.
            .value_or(ReferenceOwnership::Unowned);
    }
    else {
      AttrRange = SourceRange(Loc);
    }

    if (!DiscardAttribute)
      Attributes.add(
          new (Context) ReferenceOwnershipAttr(AttrRange, Kind));

    break;
  }

  case DeclAttrKind::NonSendable: {
    auto kind = parseSingleAttrOption<NonSendableKind>(
        *this, Loc, AttrRange, AttrName, DK, {
          { Context.Id_assumed, NonSendableKind::Assumed }
        }, NonSendableKind::Specific);
    if (!kind)
      return makeParserSuccess();

    if (!DiscardAttribute)
      Attributes.add(new (Context) NonSendableAttr(AtLoc, AttrRange, *kind));

    break;
  }

  case DeclAttrKind::AccessControl: {

    // Diagnose using access control in a local scope, which isn't meaningful.
    if (CurDeclContext->isLocalContext()) {
      diagnose(Loc, diag::attr_name_only_at_non_local_scope, AttrName);
    }

    AccessLevel access = llvm::StringSwitch<AccessLevel>(AttrName)
      .Case("private", AccessLevel::Private)
      .Case("fileprivate", AccessLevel::FilePrivate)
      .Case("internal", AccessLevel::Internal)
      .Case("package", AccessLevel::Package)
      .Case("public", AccessLevel::Public)
      .Case("open", AccessLevel::Open);

    if (!Tok.is(tok::l_paren)) {
      // Normal access control attribute.
      AttrRange = Loc;

      if (!DiscardAttribute) {
        Attributes.add(new (Context) AccessControlAttr(AtLoc, Loc, access));
        break;
      }

      // Diagnose if there's already an access control attribute on
      // this declaration with a different access level.
      if (access != cast<AccessControlAttr>(DuplicateAttribute)->getAccess()) {
        diagnose(Loc, diag::multiple_access_level_modifiers)
            .highlight(AttrRange);
        diagnose(DuplicateAttribute->getLocation(),
                 diag::previous_access_level_modifier)
            .highlight(DuplicateAttribute->getRange());

        // Remove the reference to the duplicate attribute
        // to avoid the extra diagnostic.
        DuplicateAttribute = nullptr;
      }
      break;
    }

    consumeAttributeLParen();

    if (Tok.is(tok::code_complete)) {
      if (CodeCompletionCallbacks) {
        CodeCompletionCallbacks->completeDeclAttrParam(
            ParameterizedDeclAttributeKind::AccessControl, 0, /*HasLabel=*/false);
        consumeToken(tok::code_complete);
      }
      return makeParserSuccess();
    }
    
    // Parse the subject.
    if (Tok.isContextualKeyword("set")) {
      consumeToken();
    } else {
      diagnose(Loc, diag::attr_access_expected_set, AttrName);

      // Minimal recovery: if there's a single token and then an r_paren,
      // consume them both. If there's just an r_paren, consume that.
      if (!consumeIf(tok::r_paren)) {
        if (Tok.isNot(tok::l_paren) && peekToken().is(tok::r_paren)) {
          consumeToken();
          consumeToken(tok::r_paren);
        }
      }
      return makeParserSuccess();
    }

    AttrRange = SourceRange(Loc, Tok.getLoc());

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    // Check for duplicate setter access control attributes.
    DuplicateAttribute = Attributes.getAttribute<SetterAccessAttr>();
    DiscardAttribute = DuplicateAttribute != nullptr;

    if (!DiscardAttribute) {
      Attributes.add(new (Context) SetterAccessAttr(AtLoc, AttrRange, access));
      break;
    }

    // Diagnose if there's already a setter access control attribute on
    // this declaration with a different access level.
    if (access != cast<SetterAccessAttr>(DuplicateAttribute)->getAccess()) {
      diagnose(Loc, diag::multiple_access_level_modifiers)
        .highlight(AttrRange);
      diagnose(DuplicateAttribute->getLocation(),
               diag::previous_access_level_modifier)
          .highlight(DuplicateAttribute->getRange());

      // Remove the reference to the duplicate attribute
      // to avoid the extra diagnostic.
      DuplicateAttribute = nullptr;
    }
    break;
  }

  case DeclAttrKind::SPIAccessControl: {
    if (!consumeIfAttributeLParen()) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    SmallVector<Identifier, 4> spiGroups;

    if (!Tok.isIdentifierOrUnderscore() ||
        Tok.isContextualKeyword("set")) {
      diagnose(getEndOfPreviousLoc(), diag::attr_access_expected_spi_name);
      consumeToken();
      consumeIf(tok::r_paren);
      return makeParserSuccess();
    }

    auto text = Tok.getText();
    // An spi group name can be '_' as in @_spi(_), a specifier for implicit import of the SPI.
    // '_' in source code is represented as an empty identifier in AST so match the behavior
    // here for consistency
    if (Tok.is(tok::kw__))
      text = StringRef();
    spiGroups.push_back(Context.getIdentifier(text));
    consumeToken();

    AttrRange = SourceRange(Loc, Tok.getLoc());

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    Attributes.add(SPIAccessControlAttr::create(Context, AtLoc, AttrRange,
                                                spiGroups));
    break;
  }

  case DeclAttrKind::CDecl: {
    if (!AttrName.starts_with("_") &&

        // Backwards support for @cdecl("stringId"). Remove before enabling in
        // production so we accept only the identifier format.
        lookahead<bool>(1, [&](CancellableBacktrackingScope &) {
           return Tok.isNot(tok::string_literal);
        })) {

      std::optional<StringRef> CName;
      if (consumeIfAttributeLParen()) {
        // Custom C name.
        if (Tok.isNot(tok::identifier)) {
          diagnose(Loc, diag::attr_expected_cname, AttrName);
          return makeParserSuccess();
        }

        CName = Tok.getText();
        consumeToken(tok::identifier);
        AttrRange = SourceRange(Loc, Tok.getRange().getStart());

        if (!consumeIf(tok::r_paren)) {
          diagnose(Loc, diag::attr_expected_rparen, AttrName,
                   DeclAttribute::isDeclModifier(DK));
          return makeParserSuccess();
        }
      } else {
        AttrRange = SourceRange(Loc);
      }

      Attributes.add(new (Context) CDeclAttr(CName.value_or(StringRef()), AtLoc,
                                             AttrRange, /*Implicit=*/false,
                                             /*isUnderscored*/false));
      break;
    }

    // Leave legacy @_cdecls to the logic expecting a string.
    LLVM_FALLTHROUGH;
  }
  case DeclAttrKind::Expose:
  case DeclAttrKind::SILGenName: {
    if (!consumeIfAttributeLParen()) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    bool ParseSymbolName = true;

    ExposureKind ExpKind;
    if (DK == DeclAttrKind::Expose) {
      auto diagnoseExpectOption = [&]() {
        diagnose(Tok.getLoc(), diag::attr_expected_option_such_as, AttrName,
                 "Cxx");
        ParseSymbolName = false;
      };
      if (Tok.isNot(tok::identifier)) {
        diagnoseExpectOption();
        return makeParserSuccess();
      }
      if (Tok.getText() == "Cxx") {
        ExpKind = ExposureKind::Cxx;
      } else if (Tok.getText() == "wasm") {
        ExpKind = ExposureKind::Wasm;
      } else {
        diagnoseExpectOption();
        DiscardAttribute = true;
      }
      consumeToken(tok::identifier);
      ParseSymbolName = consumeIf(tok::comma);
    }

    bool Raw = false;
    if (DK == DeclAttrKind::SILGenName) {
      if (Tok.is(tok::identifier) && Tok.getText() == "raw") {
        consumeToken(tok::identifier);
        consumeToken(tok::colon);
        Raw = true;
      }
    }

    std::optional<StringRef> AsmName;
    if (ParseSymbolName) {
      if (Tok.isNot(tok::string_literal)) {
        diagnose(Loc, diag::attr_expected_string_literal, AttrName);
        return makeParserSuccess();
      }

      AsmName =
          getStringLiteralIfNotInterpolated(Loc, ("'" + AttrName + "'").str());

      consumeToken(tok::string_literal);

      if (AsmName.has_value())
        AttrRange = SourceRange(Loc, Tok.getRange().getStart());
      else
        DiscardAttribute = true;
    } else
      AttrRange = SourceRange(Loc, Tok.getRange().getStart());

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    // Diagnose using @_silgen_name in a local scope.  These don't
    // actually work.
    if (CurDeclContext->isLocalContext()) {
      // Emit an error, but do not discard the attribute.  This enables
      // better recovery in the parser.
      diagnose(Loc, diag::attr_name_only_at_non_local_scope, AttrName);
    }

    if (!DiscardAttribute) {
      if (DK == DeclAttrKind::SILGenName)
        Attributes.add(new (Context) SILGenNameAttr(AsmName.value(), Raw, AtLoc,
                                                AttrRange, /*Implicit=*/false));
      else if (DK == DeclAttrKind::CDecl) {
        bool underscored = AttrName.starts_with("_");
        Attributes.add(new (Context) CDeclAttr(AsmName.value(), AtLoc,
                                               AttrRange, /*Implicit=*/false,
                                               /*isUnderscored*/underscored));
      } else if (DK == DeclAttrKind::Expose) {
        for (auto *EA : Attributes.getAttributes<ExposeAttr>()) {
          // A single declaration cannot have two @_exported attributes with
          // the same exposure kind.
          if (EA->getExposureKind() == ExpKind) {
            diagnose(Loc, diag::duplicate_attribute, false);
            break;
          }
        }
        Attributes.add(new (Context) ExposeAttr(
            AsmName ? AsmName.value() : StringRef(""), AtLoc, AttrRange,
            ExpKind, /*Implicit=*/false));
      } else
        llvm_unreachable("out of sync with switch");
    }

    break;
  }

  case DeclAttrKind::Extern: {
    if (!parseExternAttribute(Attributes, DiscardAttribute, AttrName, AtLoc, Loc))
      return makeParserSuccess();
    break;
  }

  case DeclAttrKind::Section: {
    if (!consumeIfAttributeLParen()) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    if (Tok.isNot(tok::string_literal)) {
      diagnose(Loc, diag::attr_expected_string_literal, AttrName);
      return makeParserSuccess();
    }

    auto Name = getStringLiteralIfNotInterpolated(
        Loc, ("'" + AttrName + "'").str());

    consumeToken(tok::string_literal);

    if (Name.has_value())
      AttrRange = SourceRange(Loc, Tok.getRange().getStart());
    else
      DiscardAttribute = true;

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    // @_section in a local scope is not allowed.
    if (CurDeclContext->isLocalContext()) {
      diagnose(Loc, diag::attr_name_only_at_non_local_scope, AttrName);
    }

    if (!DiscardAttribute)
      Attributes.add(new (Context) SectionAttr(Name.value(), AtLoc,
                                               AttrRange, /*Implicit=*/false));

    break;
  }

  case DeclAttrKind::Alignment: {
    if (!consumeIfAttributeLParen()) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    if (Tok.isNot(tok::integer_literal)) {
      diagnose(Loc, diag::alignment_must_be_positive_integer);
      return makeParserSuccess();
    }
    
    StringRef alignmentText = Tok.getText();
    unsigned alignmentValue;
    if (alignmentText.getAsInteger(0, alignmentValue)) {
      diagnose(Loc, diag::alignment_must_be_positive_integer);
      return makeParserSuccess();
    }
    
    consumeToken(tok::integer_literal);
    
    auto range = SourceRange(Loc, Tok.getRange().getStart());
    
    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    Attributes.add(new (Context) AlignmentAttr(alignmentValue, AtLoc, range,
                                               /*implicit*/ false));
    
    break;
  }

  case DeclAttrKind::SwiftNativeObjCRuntimeBase: {
    SourceRange range;
    auto name = parseSingleAttrOptionIdentifier(*this, Loc, range,
                                                AttrName, DK);
    if (!name)
      return makeParserSuccess();

    Attributes.add(new (Context) SwiftNativeObjCRuntimeBaseAttr(*name,
                                            AtLoc, range, /*implicit*/ false));
    break;
  }

  case DeclAttrKind::Semantics: {
    if (!consumeIfAttributeLParen()) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    if (Tok.isNot(tok::string_literal)) {
      diagnose(Loc, diag::attr_expected_string_literal, AttrName);
      return makeParserSuccess();
    }

    auto Value = getStringLiteralIfNotInterpolated(
        Loc, ("'" + AttrName + "'").str());

    consumeToken(tok::string_literal);

    if (Value.has_value())
      AttrRange = SourceRange(Loc, Tok.getRange().getStart());
    else
      DiscardAttribute = true;

    if (!consumeIf(tok::r_paren)) {
      diagnose(Loc, diag::attr_expected_rparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    if (!DiscardAttribute)
      Attributes.add(new (Context) SemanticsAttr(Value.value(), AtLoc,
                                                 AttrRange,
                                                 /*Implicit=*/false));
    break;
  }
  case DeclAttrKind::OriginallyDefinedIn: {
    auto LeftLoc = Tok.getLoc();
    if (!consumeIfAttributeLParen()) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }
    SourceLoc RightLoc;
    enum class NextSegmentKind: uint8_t {
      ModuleName = 0,
      PlatformVersion,
    };
    NextSegmentKind NK = NextSegmentKind::ModuleName;
    StringRef OriginalModuleName;
    llvm::SmallVector<std::pair<PlatformKind, llvm::VersionTuple>, 4>
      PlatformAndVersions;

    StringRef AttrName = "@_originallyDefinedIn";
    bool SuppressLaterDiags = false;
    bool ParsedUnrecognizedPlatformName = false;
    if (parseList(tok::r_paren, LeftLoc, RightLoc,
                  /*AllowSepAfterLast=*/false,
                  diag::originally_defined_in_missing_rparen,
                  [&]() -> ParserStatus {
      SWIFT_DEFER {
        if (NK != NextSegmentKind::PlatformVersion) {
          NK = (NextSegmentKind)((uint8_t)NK + (uint8_t)1);
        }
      };
      switch (NK) {
      // Parse 'module: "original_module_name"'.
      case NextSegmentKind::ModuleName: {
        // Parse 'module' ':'.
        if (!Tok.is(tok::identifier) || Tok.getText() != "module" ||
            !peekToken().is(tok::colon)) {
          diagnose(Tok, diag::originally_defined_in_need_original_module_name);
          SuppressLaterDiags = true;
          return makeParserError();
        }
        consumeToken(tok::identifier);
        consumeToken(tok::colon);
        // Parse the next string literal as the original module name.
        auto ModuleNameLoc = Tok.getLoc();
        if (Tok.is(tok::string_literal)) {
          auto NameOp = getStringLiteralIfNotInterpolated(Tok.getLoc(),
                                                          "original module name");
          if (NameOp.has_value())
            OriginalModuleName = *NameOp;
          consumeToken();
        }
        if (OriginalModuleName.empty()) {
          diagnose(ModuleNameLoc,
                   diag::originally_defined_in_need_nonempty_module_name);
          SuppressLaterDiags = true;
          return makeParserError();
        }
        return makeParserSuccess();
      }
      // Parse 'OSX 13.13'.
      case NextSegmentKind::PlatformVersion: {
        ParserStatus ListItemStatus = parsePlatformVersionInList(
            AttrName, PlatformAndVersions, ParsedUnrecognizedPlatformName);
        if (ListItemStatus.isErrorOrHasCompletion())
          SuppressLaterDiags = true;
        return ListItemStatus;
      }
      }
      llvm_unreachable("invalid next segment kind");
    }).isErrorOrHasCompletion() || SuppressLaterDiags) {
      return makeParserSuccess();
    }
    if (OriginalModuleName.empty()) {
      diagnose(AtLoc, diag::originally_defined_in_need_nonempty_module_name);
      return makeParserSuccess();
    }

    if (PlatformAndVersions.empty() && !ParsedUnrecognizedPlatformName) {
      diagnose(AtLoc, diag::attr_availability_need_platform_version, AttrName);
      return makeParserSuccess();
    }

    assert(!OriginalModuleName.empty());
    assert(NK == NextSegmentKind::PlatformVersion);
    AttrRange = SourceRange(Loc, RightLoc);
    for (auto &Item: PlatformAndVersions) {
      Attributes.add(new (Context) OriginallyDefinedInAttr(AtLoc, AttrRange,
                                                           OriginalModuleName,
                                                           Item.first,
                                                           Item.second,
                                                           /*IsImplicit*/false));
    }
    break;
  }

  case DeclAttrKind::ABI: {
    SourceLoc lParenLoc = Tok.getLoc();
    if (!consumeIfAttributeLParen()) {
      diagnose(lParenLoc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    Decl *abiDecl = nullptr;
    Status |= parseDecl(/*atStart=*/true, /*ifConfigInAttr=*/true, [&](Decl *D){
      // Look through a TopLevelCodeDecl.
      // FIXME: Do we need to reparent these decls to match their counterparts?
      //        Feels like there might be more to do here.
      if (auto tlcd = dyn_cast<TopLevelCodeDecl>(D)) {
        ASSERT(tlcd->getBody()->getNumElements() == 1
                  && "TopLevelCodeDecl with != 1 element?");
        D = tlcd->getBody()->getElements().front().dyn_cast<Decl *>();
        if (!D)
          return;
      }
      // TODO: MacroExpansionDecl?

      // When parsing a compound decl, like a PBD, we'll get called for both the
      // enclosing decl and the ones inside it. We only want the enclosing decl,
      // which will always come first.
      if (!abiDecl)
        abiDecl = D;
    }, /*stubOnly=*/true);
    ASSERT(!isa_and_nonnull<VarDecl>(abiDecl)
              && "should have gotten PBD, not VarDecl");

    SourceLoc rParenLoc;
    if (parseMatchingToken(tok::r_paren, rParenLoc,
                           DiagRef(diag::attr_expected_rparen,
                                   { AttrName,
                                     DeclAttribute::isDeclModifier(DK) }),
                           lParenLoc)) {
      return makeParserSuccess();
    }

    if (abiDecl) {
      auto attr = new (Context) ABIAttr(abiDecl, AtLoc, { Loc, rParenLoc },
                                        /*implicit=*/false);

      // Diagnose syntactically invalid abiDecl kind here to match behavior of
      // Swift parser.
      if (!attr->canAppearOnDecl(abiDecl) && !isa<PatternBindingDecl>(abiDecl)){
        diagnose(abiDecl->getLoc(), diag::attr_abi_incompatible_kind,
                 abiDecl->getDescriptiveKind());
        attr->setInvalid();
      }

      Attributes.add(attr);
    }

    break;
  }

  case DeclAttrKind::Available: {
    if (!consumeIfAttributeLParen()) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }
    if (!parseAvailability(false, AttrName, DiscardAttribute, AttrRange, AtLoc,
                           Loc,
                           [&](AvailableAttr *attr) { Attributes.add(attr); }))
      return makeParserSuccess();
    break;
  }
  case DeclAttrKind::PrivateImport: {
    // Parse the leading '('.
    if (Tok.isNot(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }
    SourceLoc LParenLoc = consumeAttributeLParen();
    std::optional<StringRef> filename;
    {
      // Parse 'sourceFile'.
      if (Tok.getText() != "sourceFile") {
        diagnose(LParenLoc, diag::attr_private_import_expected_sourcefile);
        return makeParserSuccess();
      }
      auto ForLoc = consumeToken();

      // Parse ':'.
      if (Tok.getKind() != tok::colon) {
        diagnose(ForLoc, diag::attr_private_import_expected_colon);
        return makeParserSuccess();
      }
      auto ColonLoc = consumeToken(tok::colon);

      // Parse '"'function-name'"'
      if (Tok.isNot(tok::string_literal)) {
        diagnose(ColonLoc, diag::attr_private_import_expected_sourcefile_name);
        return makeParserSuccess();
      }
      filename = getStringLiteralIfNotInterpolated(Loc, "_private");
      if (!filename.has_value()) {
        diagnose(ColonLoc, diag::attr_private_import_expected_sourcefile_name);
        return makeParserSuccess();
      }
      consumeToken(tok::string_literal);
    }
    // Parse the matching ')'.
    SourceLoc RParenLoc;
    bool Invalid = parseMatchingToken(tok::r_paren, RParenLoc,
                                      diag::attr_private_import_expected_rparen,
                                      LParenLoc);
    if (Invalid)
      return makeParserSuccess();
    auto *attr = PrivateImportAttr::create(Context, AtLoc, Loc, LParenLoc,
                                           *filename, RParenLoc);
    Attributes.add(attr);

    break;
  }
  case DeclAttrKind::ObjC: {
    // Unnamed @objc attribute.
    if (Tok.isNot(tok::l_paren)) {
      auto attr = ObjCAttr::createUnnamed(Context, AtLoc, Loc);
      Attributes.add(attr);
      break;
    }

    // Parse the leading '('.
    SourceLoc LParenLoc = consumeAttributeLParen();

    // Parse the names, with trailing colons (if there are present) and populate
    // the inout parameters
    SmallVector<Identifier, 4> Names;
    SmallVector<SourceLoc, 4> NameLocs;
    bool NullarySelector = true;
    parseObjCSelector(Names, NameLocs, NullarySelector);

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
    } else if (NullarySelector) {
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
  case DeclAttrKind::ObjCImplementation: {
    SourceRange range;
    auto name = parseSingleAttrOptionIdentifier(*this, Loc, range, AttrName, DK,
                                                /*allowOmitted=*/true);
    if (!name)
      return makeParserSuccess();

    bool isEarlyAdopter = (AttrName != "implementation");
    Attributes.add(new (Context) ObjCImplementationAttr(*name, AtLoc, range,
                                                        isEarlyAdopter));
    break;
  }
  case DeclAttrKind::ObjCRuntimeName: {
    SourceRange range;
    auto name =
        parseSingleAttrOptionIdentifier(*this, Loc, range, AttrName, DK);
    if (!name)
      return makeParserSuccess();

    Attributes.add(new (Context) ObjCRuntimeNameAttr(name->str(), AtLoc, range,
                                                     /*implicit*/ false));
    break;
  }

  case DeclAttrKind::DynamicReplacement: {
    // Parse the leading '('.
    if (Tok.isNot(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    SourceLoc LParenLoc = consumeAttributeLParen();
    DeclNameRef replacedFunction;
    {
      // Parse 'for'.
      if (Tok.getText() != "for") {
        diagnose(Loc, diag::attr_dynamic_replacement_expected_for);
        return makeParserSuccess();
      }
      auto ForLoc = consumeToken();

      // Parse ':'.
      if (Tok.getText() != ":") {
        diagnose(ForLoc, diag::attr_dynamic_replacement_expected_colon);
        return makeParserSuccess();
      }
      consumeToken(tok::colon);
      {
        DeclNameLoc loc;
        replacedFunction = parseDeclNameRef(
            loc, diag::attr_dynamic_replacement_expected_function,
            DeclNameFlag::AllowZeroArgCompoundNames |
                DeclNameFlag::AllowKeywordsUsingSpecialNames |
                DeclNameFlag::AllowOperators |
                DeclNameFlag::AllowLowercaseAndUppercaseSelf);
      }
    }

    // Parse the matching ')'.
    SourceLoc RParenLoc;
    bool Invalid = parseMatchingToken(
        tok::r_paren, RParenLoc, diag::attr_dynamic_replacement_expected_rparen,
        LParenLoc);
    if (Invalid) {
      return makeParserSuccess();
    }


    DynamicReplacementAttr *attr = DynamicReplacementAttr::create(
        Context, AtLoc, Loc, LParenLoc, replacedFunction, RParenLoc);
    Attributes.add(attr);
    break;
  }

  case DeclAttrKind::TypeEraser: {
    // Parse leading '('
    if (Tok.isNot(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    SourceLoc LParenLoc = consumeAttributeLParen();
    ParserResult<TypeRepr> ErasedType;
    bool invalid = false;
    {
      // Parse type-eraser type
      ErasedType = parseType(diag::attr_type_eraser_expected_type_name);
      invalid = ErasedType.hasCodeCompletion() || ErasedType.isNull();
    }

    // Parse matching ')'
    SourceLoc RParenLoc;
    invalid |= parseMatchingToken(tok::r_paren, RParenLoc,
                                  diag::attr_type_eraser_expected_rparen,
                                  LParenLoc);
    if (invalid)
      return makeParserSuccess();

    auto *TE = new (Context) TypeExpr(ErasedType.get());
    Attributes.add(TypeEraserAttr::create(Context, AtLoc, {Loc, RParenLoc}, TE));
    break;
  }

  case DeclAttrKind::Specialize: {
    if (Tok.isNot(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }
    SpecializeAttr *Attr;
    if (!parseSpecializeAttribute(tok::r_paren, AtLoc, Loc, Attr, nullptr))
      return makeParserSuccess();

    Attributes.add(Attr);
    break;
  }

  case DeclAttrKind::StorageRestrictions: {
    ParserResult<StorageRestrictionsAttr> Attr =
        parseStorageRestrictionsAttribute(AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull()) {
      Attributes.add(Attr.get());
    }
    break;
  }

  case DeclAttrKind::Implements: {
    ParserResult<ImplementsAttr> Attr = parseImplementsAttribute(AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull()) {
      Attributes.add(Attr.get());
    }
    break;
  }

  case DeclAttrKind::Differentiable: {
    auto Attr = parseDifferentiableAttribute(AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull())
      Attributes.add(Attr.get());
    break;
  }

  case DeclAttrKind::Derivative: {
    // `@derivative` in a local scope is not allowed.
    if (CurDeclContext->isLocalContext())
      diagnose(Loc, diag::attr_name_only_at_non_local_scope,
               '@' + AttrName.str());

    auto Attr = parseDerivativeAttribute(AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull())
      Attributes.add(Attr.get());
    break;
  }

  case DeclAttrKind::Transpose: {
    // `@transpose` in a local scope is not allowed.
    if (CurDeclContext->isLocalContext())
      diagnose(Loc, diag::attr_name_only_at_non_local_scope,
               '@' + AttrName.str());

    auto Attr = parseTransposeAttribute(AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull())
      Attributes.add(Attr.get());
    break;
  }

  case DeclAttrKind::ProjectedValueProperty: {
    SourceRange range;
    auto name =
        parseSingleAttrOptionIdentifier(*this, Loc, range, AttrName, DK);
    if (!name)
      return makeParserSuccess();

    Attributes.add(new (Context) ProjectedValuePropertyAttr(
        *name, AtLoc, range, /*implicit*/ false));
    break;
  }

  case DeclAttrKind::UnavailableFromAsync: {
    StringRef message;
    if (consumeIfAttributeLParen()) {
      auto tokMayBeArgument = [&]() -> bool {
        return Tok.isNot(tok::r_paren, tok::comma) &&
                 !isKeywordPossibleDeclStart(Context.LangOpts, Tok);
      };

      Identifier label;
      SourceLoc labelLoc;
      parseOptionalArgumentLabel(label, labelLoc, /*isAttr=*/true);

      if (label.empty()) {
        // If we have the identifier 'message', assume the user forgot the
        // colon.
        if (Tok.isContextualKeyword("message")) {
          labelLoc = consumeToken();
          auto diag = diagnose(Tok, diag::attr_expected_colon_after_label,
                               "message");
          if (Tok.is(tok::string_literal))
            diag.fixItInsertAfter(labelLoc, ":");
          else
            return makeParserError();
        }
        // If the argument list just abruptly cuts off, handle that as a
        // missing argument (below). Otherwise, diagnose the missing label.
        else if (tokMayBeArgument()) {
          if (labelLoc.isValid())
            // The user wrote an explicitly omitted label (`_:`).
            diagnose(labelLoc, diag::attr_expected_label, "message", AttrName)
              .fixItReplace(labelLoc, "message");
          else
            diagnose(Tok, diag::attr_expected_label, "message", AttrName)
              .fixItInsert(Tok.getLoc(), "message: ");
        }
        // Fall through to parse the argument.
      } else if (label != Context.Id_message) {
        diagnose(labelLoc, diag::attr_unknown_option, label.str(), AttrName)
          .fixItReplace(labelLoc, "message");
        return makeParserError();
      }

      if (!Tok.is(tok::string_literal)) {
        // If this token looks like an argument, replace it; otherwise insert
        // before it.
        auto endLoc = tokMayBeArgument() ? peekToken().getLoc() : Tok.getLoc();

        diagnose(Tok, diag::attr_expected_string_literal, AttrName)
          .fixItReplaceChars(Tok.getLoc(), endLoc, "\"<#error message#>\"");

        return makeParserError();
      }

      std::optional<StringRef> value =
          getStringLiteralIfNotInterpolated(Tok.getLoc(), "message");
      if (!value)
        return makeParserError();
      Token stringTok = Tok;
      consumeToken();
      message = *value;

      if (!consumeIf(tok::r_paren))
        diagnose(stringTok.getRange().getEnd(), diag::attr_expected_rparen,
            AttrName, /*isModifiler*/false)
          .fixItInsertAfter(stringTok.getLoc(), ")");
      AttrRange = SourceRange(Loc, PreviousLoc);
    } else {
      AttrRange = SourceRange(Loc);
    }

    Attributes.add(new (Context) UnavailableFromAsyncAttr(message, AtLoc,
                                                          AttrRange, false));
    break;
  }
  case DeclAttrKind::BackDeployed: {
    if (!parseBackDeployedAttribute(Attributes, AttrName, AtLoc, Loc))
      return makeParserSuccess();
    break;
  }
  case DeclAttrKind::Documentation: {
    auto Attr = parseDocumentationAttribute(AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull())
      Attributes.add(Attr.get());
    else
      return makeParserSuccess();
    break;
  }
  case DeclAttrKind::Nonisolated: {
    AttrRange = Loc;
    std::optional<NonIsolatedModifier> Modifier(NonIsolatedModifier::None);
    if (EnableParameterizedNonisolated) {
      Modifier = parseSingleAttrOption<NonIsolatedModifier>(
          *this, Loc, AttrRange, AttrName, DK,
          {{Context.Id_unsafe, NonIsolatedModifier::Unsafe},
           {Context.Id_nonsending, NonIsolatedModifier::NonSending}},
          *Modifier, ParameterizedDeclAttributeKind::Nonisolated);
      if (!Modifier) {
        return makeParserSuccess();
      }
    }

    if (!DiscardAttribute) {
      Attributes.add(new (Context) NonisolatedAttr(AtLoc, AttrRange, *Modifier,
                                                   /*implicit*/ false));
    }
    break;
  }
  case DeclAttrKind::InheritActorContext: {
    AttrRange = Loc;
    std::optional<InheritActorContextModifier> Modifier(
        InheritActorContextModifier::None);
    Modifier = parseSingleAttrOption<InheritActorContextModifier>(
        *this, Loc, AttrRange, AttrName, DK,
        {{Context.Id_always, InheritActorContextModifier::Always}}, *Modifier,
        ParameterizedDeclAttributeKind::InheritActorContext);
    if (!Modifier) {
      return makeParserSuccess();
    }

    if (!DiscardAttribute) {
      Attributes.add(new (Context) InheritActorContextAttr(
          AtLoc, AttrRange, *Modifier, /*implicit=*/false));
    }

    break;
  }
  case DeclAttrKind::MacroRole: {
    auto syntax = (AttrName == "freestanding" ? MacroSyntax::Freestanding
                                              : MacroSyntax::Attached);
    auto Attr = parseMacroRoleAttribute(syntax, AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull())
      Attributes.add(Attr.get());
    else
      return Attr;
    break;
  }
  case DeclAttrKind::AllowFeatureSuppression: {
    auto inverted = (AttrName == "_disallowFeatureSuppression");
    auto Attr = parseAllowFeatureSuppressionAttribute(inverted, AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull())
      Attributes.add(Attr.get());
    else
      return makeParserSuccess();
    break;
  }
  case DeclAttrKind::RawLayout: {
    if (Tok.isNot(tok::l_paren)) {
      diagnose(Loc, diag::attr_expected_lparen, AttrName,
               DeclAttribute::isDeclModifier(DK));
      return makeParserSuccess();
    }

    consumeAttributeLParen();

    if (!Tok.canBeArgumentLabel()) {
      diagnose(Loc, diag::attr_rawlayout_expected_label, "'size', 'like', or 'likeArrayOf'");
      consumeIf(tok::r_paren);
      return makeParserSuccess();
    }
    
    Identifier firstLabel;
    consumeArgumentLabel(firstLabel, true);
    if (!consumeIf(tok::colon)) {
      diagnose(Loc, diag::attr_expected_colon_after_label, firstLabel.str());
      return makeParserSuccess();
    }
    
    RawLayoutAttr *attr;
    if (firstLabel.is("size")) {
      // @_rawLayout(size: N, alignment: N)
      unsigned size;
      SourceLoc sizeLoc;
      if (parseUnsignedInteger(size, sizeLoc,
                               diag::attr_rawlayout_expected_integer_size)) {
        return makeParserSuccess();
      }
      
      if (!consumeIf(tok::comma)) {
        diagnose(Loc, diag::attr_rawlayout_expected_params, "size", "alignment");
        consumeIf(tok::r_paren);
        return makeParserSuccess();
      }

      if (!Tok.canBeArgumentLabel()) {
        diagnose(Loc, diag::attr_rawlayout_expected_label, "'alignment'");
        return makeParserSuccess();
      }

      Identifier alignLabel;
      consumeArgumentLabel(alignLabel, true);
      if (!consumeIf(tok::colon)) {
        diagnose(Loc, diag::attr_expected_colon_after_label, "alignment");
        return makeParserSuccess();
      }
      if (!alignLabel.is("alignment")) {
        diagnose(Loc, diag::attr_rawlayout_expected_label, "'alignment'");
        return makeParserSuccess();
      }
      
      unsigned align;
      SourceLoc alignLoc;
      if (parseUnsignedInteger(align, alignLoc,
                             diag::attr_rawlayout_expected_integer_alignment)) {
        return makeParserSuccess();
      }
      
      SourceLoc rParenLoc;
      if (!consumeIf(tok::r_paren, rParenLoc)) {
        diagnose(Tok.getLoc(), diag::attr_expected_rparen,
                 AttrName, /*isModifier*/false);
        return makeParserSuccess();
      }
      
      attr = new (Context) RawLayoutAttr(size, align,
                                         AtLoc, SourceRange(Loc, rParenLoc));
    } else if (firstLabel.is("like")) {
      // @_rawLayout(like: T)
      auto likeType = parseType(diag::expected_type);
      if (likeType.isNull()) {
        return makeParserSuccess();
      }

      bool movesAsLike = false;

      // @_rawLayout(like: T, movesAsLike)
      if (consumeIf(tok::comma) && Tok.isAny(tok::identifier) &&
          !parseSpecificIdentifier("movesAsLike",
            diag::attr_rawlayout_expected_label, "movesAsLike")) {
        movesAsLike = true;
      }

      SourceLoc rParenLoc;
      if (!consumeIf(tok::r_paren, rParenLoc)) {
        diagnose(Tok.getLoc(), diag::attr_expected_rparen,
                 AttrName, /*isModifier*/false);
        return makeParserSuccess();
      }

      attr = new (Context) RawLayoutAttr(likeType.get(), movesAsLike,
                                         AtLoc, SourceRange(Loc, rParenLoc));
    } else if (firstLabel.is("likeArrayOf")) {
      // @_rawLayout(likeArrayOf: T, count: N)
      auto likeType = parseType(diag::expected_type);
      if (likeType.isNull()) {
        return makeParserSuccess();
      }

      if (!consumeIf(tok::comma)) {
        diagnose(Loc, diag::attr_rawlayout_expected_params, "likeArrayOf", "count");
        consumeIf(tok::r_paren);
        return makeParserSuccess();
      }

      if (!Tok.canBeArgumentLabel()) {
        diagnose(Loc, diag::attr_rawlayout_expected_label, "'count'");
        consumeIf(tok::r_paren);
        return makeParserSuccess();
      }

      Identifier countLabel;
      consumeArgumentLabel(countLabel, true);
      if (!consumeIf(tok::colon)) {
        diagnose(Loc, diag::attr_expected_colon_after_label, "count");
        return makeParserSuccess();
      }
       if (!countLabel.is("count")) {
        diagnose(Loc, diag::attr_rawlayout_expected_label, "'count'");
        return makeParserSuccess();
      }
     
      auto countType = parseTypeOrValue(diag::expected_type);
      if (countType.isNull()) {
        return makeParserSuccess();
      }
      
      bool movesAsLike = false;

      // @_rawLayout(likeArrayOf: T, count: N, movesAsLike)
      if (consumeIf(tok::comma) && Tok.isAny(tok::identifier) &&
          !parseSpecificIdentifier("movesAsLike",
            diag::attr_rawlayout_expected_label, "movesAsLike")) {
        movesAsLike = true;
      }

      SourceLoc rParenLoc;
      if (!consumeIf(tok::r_paren, rParenLoc)) {
        diagnose(Tok.getLoc(), diag::attr_expected_rparen,
                 AttrName, /*isModifier*/false);
        return makeParserSuccess();
      }
      
      attr = new (Context) RawLayoutAttr(likeType.get(), countType.get(),
                                         movesAsLike, AtLoc,
                                         SourceRange(Loc, rParenLoc));
    } else {
      diagnose(Loc, diag::attr_rawlayout_expected_label,
               "'size', 'like', or 'likeArrayOf'");
      return makeParserSuccess();
    }
    Attributes.add(attr);
    break;
  }
  case DeclAttrKind::Lifetime: {
    auto Attr = parseLifetimeAttribute(AttrName, AtLoc, Loc);
    Status |= Attr;
    if (Attr.isNonNull())
      Attributes.add(Attr.get());
    break;
  }
  }

  if (DuplicateAttribute) {
    diagnose(Loc, diag::duplicate_attribute, DeclAttribute::isDeclModifier(DK))
      .highlight(AttrRange);
    diagnose(DuplicateAttribute->getLocation(),
             diag::previous_attribute,
             DeclAttribute::isDeclModifier(DK))
      .highlight(DuplicateAttribute->getRange());
  }

  // If this is a decl modifier spelled with an @, emit an error and remove it
  // with a fixit.
  if (AtLoc.isValid() && DeclAttribute::isDeclModifier(DK))
    diagnose(AtLoc, diag::cskeyword_not_attribute, AttrName).fixItRemove(AtLoc);

  return Status;
}

bool Parser::parseVersionTuple(llvm::VersionTuple &Version,
                               SourceRange &Range,
                               DiagRef D) {
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
    Version = llvm::VersionTuple(major);
    Range = SourceRange(StartLoc, Tok.getLoc());
    consumeToken();
    if (Version.empty()) {
      // Versions cannot be empty (e.g. "0").
      diagnose(Range.Start, D).warnUntilSwiftVersion(6);
      return true;
    }
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
    
    Version = llvm::VersionTuple(major, minor, micro);
  } else {
    Version = llvm::VersionTuple(major, minor);
  }

  if (Version.empty()) {
    // Versions cannot be empty (e.g. "0.0").
    diagnose(Range.Start, D).warnUntilSwiftVersion(6);
    return true;
  }

  return false;
}

bool Parser::isCustomAttributeArgument() {
  BacktrackingScope backtrack(*this);
  if (skipSingle().hasCodeCompletion())
    return true;

  // If we have any keyword, identifier, or token that follows a function
  // type's parameter list, this is a parameter list and not an attribute.
  // Alternatively, we might have a token that illustrates we're not going to
  // get anything following the attribute, which means the parentheses describe
  // what follows the attribute.
  return !Tok.isAny(
      tok::arrow, tok::kw_throw, tok::kw_throws, tok::kw_rethrows,
      tok::r_paren, tok::r_brace, tok::r_square, tok::r_angle) &&
    !Tok.isContextualKeyword("async") && !Tok.isContextualKeyword("reasync") ;
}

bool Parser::canParseCustomAttribute() {
  if (!canParseType())
    return false;

  if (Tok.isFollowingLParen() && isCustomAttributeArgument())
    skipSingle();

  return true;
}

ParserResult<CustomAttr> Parser::parseCustomAttribute(SourceLoc atLoc) {
  assert(Tok.is(tok::identifier));

  // Parse a custom attribute.
  auto type = parseType(diag::expected_type, ParseTypeReason::CustomAttribute);
  if (type.hasCodeCompletion() || type.isNull()) {
    if (Tok.is(tok::l_paren) && isCustomAttributeArgument())
      skipSingle();

    return ParserResult<CustomAttr>(ParserStatus(type));
  }

  // If we're not in a local context, we'll need a context to parse
  // initializers into (should we have one).  This happens for properties
  // and global variables in libraries.
  ParserStatus status;
  ArgumentList *argList = nullptr;
  CustomAttributeInitializer *initContext = nullptr;
  if (Tok.isFollowingLParen() && isCustomAttributeArgument()) {
    // If we have no local context to parse the initial value into, create
    // one for the attribute.
    std::optional<ParseFunctionBody> initParser;
    if (!CurDeclContext->isLocalContext()) {
      assert(!initContext);
      initContext = CustomAttributeInitializer::create(CurDeclContext);
      initParser.emplace(*this, initContext);
    }
    if (getEndOfPreviousLoc() != Tok.getLoc()) {
      diagnose(getEndOfPreviousLoc(), diag::attr_extra_whitespace_before_lparen)
          .warnUntilSwiftVersion(6);
    }
    auto result = parseArgumentList(tok::l_paren, tok::r_paren,
                                    /*isExprBasic*/ true,
                                    /*allowTrailingClosure*/ false);
    status |= result;
    argList = result.get();
    assert(!argList->hasAnyTrailingClosures() &&
           "Cannot parse a trailing closure here");
  }

  // Form the attribute.
  auto *TE = new (Context) TypeExpr(type.get());
  auto *customAttr = CustomAttr::create(Context, atLoc, TE, initContext,
                                        argList);
  if (status.hasCodeCompletion() && CodeCompletionCallbacks) {
    CodeCompletionCallbacks->setCompletingInAttribute(customAttr);
  }
  return makeParserResult(status, customAttr);
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
ParserStatus Parser::parseDeclAttribute(DeclAttributes &Attributes,
                                        SourceLoc AtLoc, SourceLoc AtEndLoc,
                                        bool isFromClangAttribute) {
  if (AtEndLoc != Tok.getLoc()) {
    diagnose(AtEndLoc, diag::attr_extra_whitespace_after_at)
        .warnUntilSwiftVersion(6);
  }

  // If this not an identifier, the attribute is malformed.
  if (Tok.isNot(tok::identifier) &&
      Tok.isNot(tok::kw_in) &&
      Tok.isNot(tok::kw_inout) &&
      Tok.isNot(tok::kw_rethrows)) {

    if (Tok.is(tok::code_complete)) {
      if (CodeCompletionCallbacks) {
        // If the next token is not on the same line, this attribute might be
        // starting new declaration instead of adding attribute to existing
        // decl.
        auto isIndependent = peekToken().isAtStartOfLine();
        CodeCompletionCallbacks->completeDeclAttrBeginning(isInSILMode(),
                                                           isIndependent);
      }
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionStatus();
    }

    diagnose(Tok, diag::expected_attribute_name);
    return makeParserError();
  }

  // If the attribute follows the new representation, switch
  // over to the alternate parsing path.
  std::optional<DeclAttrKind> DK =
      DeclAttribute::getAttrKindFromString(Tok.getText());
  if (DK == DeclAttrKind::Rethrows) {
    DK = DeclAttrKind::AtRethrows;
  }
  if (DK == DeclAttrKind::Reasync) {
    DK = DeclAttrKind::AtReasync;
  }

  auto checkInvalidAttrName =
      [&](StringRef invalidName, StringRef correctName, DeclAttrKind kind,
          std::optional<Diag<StringRef, StringRef>> diag = std::nullopt) {
        if (!DK && Tok.getText() == invalidName) {
          DK = kind;

          if (diag) {
            diagnose(Tok, *diag, invalidName, correctName)
                .fixItReplace(Tok.getLoc(), correctName);
          }
        }
      };

  // Check if attr is availability, and suggest available instead
  checkInvalidAttrName("availability", "available", DeclAttrKind::Available,
                       diag::attr_renamed);

  // Check if attr is inlineable, and suggest inlinable instead
  checkInvalidAttrName("inlineable", "inlinable", DeclAttrKind::Inlinable,
                       diag::attr_name_close_match);

  // In Swift 5 and above, these become hard errors. In Swift 4.2, emit a
  // warning for compatibility. Otherwise, don't diagnose at all.
  if (Context.isSwiftVersionAtLeast(5)) {
    checkInvalidAttrName("_versioned", "usableFromInline",
                         DeclAttrKind::UsableFromInline, diag::attr_renamed);
    checkInvalidAttrName("_inlineable", "inlinable", DeclAttrKind::Inlinable,
                         diag::attr_renamed);
  } else if (Context.isSwiftVersionAtLeast(4, 2)) {
    checkInvalidAttrName("_versioned", "usableFromInline",
                         DeclAttrKind::UsableFromInline,
                         diag::attr_renamed_warning);
    checkInvalidAttrName("_inlineable", "inlinable", DeclAttrKind::Inlinable,
                         diag::attr_renamed_warning);
  } else {
    checkInvalidAttrName("_versioned", "usableFromInline",
                         DeclAttrKind::UsableFromInline);
    checkInvalidAttrName("_inlineable", "inlinable", DeclAttrKind::Inlinable);
  }

  // Other names of property wrappers...
  checkInvalidAttrName("propertyDelegate", "propertyWrapper",
                       DeclAttrKind::PropertyWrapper,
                       diag::attr_renamed_warning);
  checkInvalidAttrName("_propertyWrapper", "propertyWrapper",
                       DeclAttrKind::PropertyWrapper,
                       diag::attr_renamed_warning);

  // Historical name for result builders.
  checkInvalidAttrName("_functionBuilder", "resultBuilder",
                       DeclAttrKind::ResultBuilder, diag::attr_renamed_warning);

  // Historical name for 'nonisolated'.
  if (!DK && Tok.getText() == "actorIndependent") {
    diagnose(
        Tok, diag::attr_renamed_to_modifier_warning, "actorIndependent", 
        "nonisolated")
      .fixItReplace(SourceRange(AtLoc, Tok.getLoc()), "nonisolated");
    DK = DeclAttrKind::Nonisolated;
    AtLoc = SourceLoc();
  }

  // Temporary name for @preconcurrency
  checkInvalidAttrName("_predatesConcurrency", "preconcurrency",
                       DeclAttrKind::Preconcurrency,
                       diag::attr_renamed_warning);

  if (!DK && Tok.getText() == "warn_unused_result") {
    // The behavior created by @warn_unused_result is now the default. Emit a
    // Fix-It to remove.
    SourceLoc attrLoc = consumeToken();

    // @warn_unused_result with no arguments.
    if (Tok.isNot(tok::l_paren)) {
      diagnose(AtLoc, diag::attr_warn_unused_result_removed)
        .fixItRemove(SourceRange(AtLoc, attrLoc));

      // Recovered.
      return makeParserSuccess();
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

    // Recovered.
    return makeParserSuccess();
  }

  // @_unsafeSendable and @_unsafeMainActor have been removed; warn about them.
  if (!DK && (Tok.getText() == "_unsafeSendable" ||
              Tok.getText() == "_unsafeMainActor")) {
    StringRef attrName = Tok.getText();
    SourceLoc attrLoc = consumeToken();
    diagnose(AtLoc, diag::warn_attr_unsafe_removed, attrName)
      .fixItRemove(SourceRange(AtLoc, attrLoc));
    return makeParserSuccess();
  }

  // Old spelling for @freestanding(expression).
  if (!DK && Tok.getText() == "expression") {
    SourceLoc attrLoc = consumeToken();
    diagnose(attrLoc, diag::macro_expression_attribute_removed)
      .fixItReplace(SourceRange(AtLoc, attrLoc), "@freestanding(expression)");
    auto attr = MacroRoleAttr::create(
        Context, AtLoc, SourceRange(AtLoc, attrLoc),
        MacroSyntax::Freestanding, SourceLoc(), MacroRole::Expression, { },
        /*conformances=*/{}, SourceLoc(), /*isImplicit*/ false);
    Attributes.add(attr);
    return makeParserSuccess();
  }

  // Rewrite @_unavailableInEmbedded into @available(*, unavailable) when in
  // embedded Swift mode, or into nothing when in regular mode.
  if (!DK && Tok.getText() == UNAVAILABLE_IN_EMBEDDED_ATTRNAME) {
    SourceLoc attrLoc = consumeToken();
    if (Context.LangOpts.hasFeature(Feature::Embedded)) {
      auto attr = AvailableAttr::createUnavailableInEmbedded(
          Context, AtLoc, SourceRange(AtLoc, attrLoc));
      Attributes.add(attr);
    }
    return makeParserSuccess();
  }

  if (DK && !DeclAttribute::shouldBeRejectedByParser(*DK)) {
    return parseNewDeclAttribute(Attributes, AtLoc, *DK, isFromClangAttribute);
  }

  if (TypeAttribute::getAttrKindFromString(Tok.getText()).has_value())
    diagnose(Tok, diag::type_attribute_applied_to_decl);
  else if (Tok.isContextualKeyword("unknown")) {
    diagnose(Tok, diag::unknown_attribute, "unknown");
  } else {
    // Change the context to create a custom attribute syntax.
    auto customAttr = parseCustomAttribute(AtLoc);
    if (auto attr = customAttr.getPtrOrNull())
      Attributes.add(attr);

    return ParserStatus(customAttr);
  }

  // Recover by eating @foo(...) when foo is not known.
  consumeToken();
  if (Tok.is(tok::l_paren))
    skipSingle();

  return makeParserError();
}

bool Parser::canParseTypeAttribute() {
  TypeOrCustomAttr result; // ignored
  return !parseTypeAttribute(result, /*atLoc=*/SourceLoc(),
                             /*atEndLoc=*/SourceLoc(),
                             ParseTypeReason::Unspecified,
                             /*justChecking*/ true)
              .isError();
}

/// Parses the '@differentiable' type attribute argument (no argument list,
/// '(_forward)', '(reverse)', or '(_linear)') and sets the
/// `differentiabilityKind` field on `Attributes`.
///
/// \param emitDiagnostics - if false, doesn't emit diagnostics
/// \returns true on error, false on success
static bool parseDifferentiableTypeAttributeArgument(
    Parser &P, SourceRange &parenRangeResult,
    DifferentiabilityKind &diffKindResult,
    SourceLoc &diffKindLocResult, bool emitDiagnostics) {
  Parser::CancellableBacktrackingScope backtrack(P);

  SourceLoc beginLoc = P.Tok.getLoc();
  SourceLoc kindLoc, endLoc;

  // Match '( <identifier> )', and store the identifier token to `argument`.
  if (!P.consumeIfAttributeLParen())
    return false;
  auto argument = P.Tok;
  if (!P.consumeIf(tok::identifier, kindLoc))
    return false;
  if (!P.consumeIf(tok::r_paren, endLoc)) {
    // Special case handling for '( <identifier> (' so that we don't produce the
    // misleading diagnostic "expected ',' separator" when the real issue is
    // that the user forgot the ')' closing the '@differentiable' argument list.
    if (P.Tok.is(tok::l_paren)) {
      backtrack.cancelBacktrack();
      if (emitDiagnostics)
        P.diagnose(P.Tok, diag::attr_expected_rparen, "@differentiable",
                   /*DeclModifier*/ false);
      return true;
    }
    return false;
  }

  // If the next token is not a `(`, `@`, or an identifier, then the
  // matched '( <identifier> )' is actually the parameter type list,
  // not an argument to '@differentiable'.
  if (P.Tok.isNot(tok::l_paren, tok::at_sign, tok::identifier))
    return false;

  backtrack.cancelBacktrack();

  auto diffKind =
      llvm::StringSwitch<DifferentiabilityKind>(argument.getText())
      .Case("reverse", DifferentiabilityKind::Reverse)
      .Case("_forward", DifferentiabilityKind::Forward)
      .Case("_linear", DifferentiabilityKind::Linear)
      .Default(DifferentiabilityKind::NonDifferentiable);

  if (diffKind == DifferentiabilityKind::NonDifferentiable) {
    P.diagnose(argument, diag::attr_differentiable_unknown_kind,
               argument.getText())
        .fixItReplaceChars(argument.getRange().getStart(),
                           argument.getRange().getEnd(), "reverse");
    return true;
  }

  // Only 'reverse' is formally supported today. '_linear' works for testing
  // purposes. '_forward' is rejected.
  if (diffKind == DifferentiabilityKind::Forward) {
    if (emitDiagnostics)
      P.diagnose(argument, diag::attr_differentiable_kind_not_supported,
                 argument.getText())
          .fixItReplaceChars(argument.getRange().getStart(),
                             argument.getRange().getEnd(), "reverse");
    return true;
  }

  // Only now should we assign to the result references.
  diffKindResult = diffKind;
  diffKindLocResult = kindLoc;
  parenRangeResult = SourceRange(beginLoc, endLoc);
  return false;
}

/// Parse the inside of a convention attribute '(...)'.
///
/// The '@convention' prefix should've been parsed by the caller.
/// See `Parser::parseTypeAttribute` for the justChecking argument.
bool Parser::parseConventionAttributeInternal(SourceLoc atLoc, SourceLoc attrLoc,
                                              ConventionTypeAttr *&result,
                                              bool justChecking) {
  SourceLoc LPLoc = Tok.getLoc();
  if (!consumeIfAttributeLParen()) {
    if (!justChecking)
      diagnose(Tok, diag::convention_attribute_expected_lparen);
    return true;
  }

  if (Tok.isNot(tok::identifier)) {
    if (!justChecking)
      diagnose(Tok, diag::convention_attribute_expected_name);
    return true;
  }

  Located<StringRef> conventionName;
  conventionName.Item = Tok.getText();
  conventionName.Loc = consumeToken(tok::identifier);

  Located<StringRef> cType;

  // Consume extra (optional) ', cType: " blah blah "'
  if (consumeIf(tok::comma)) {
    if (Tok.isNot(tok::identifier)) {
      if (!justChecking)
        diagnose(Tok, diag::convention_attribute_ctype_expected_label);
      return true;
    }
    auto cTypeLabel = Tok.getText();
    consumeToken(tok::identifier);
    if (cTypeLabel != "cType") {
      if (!justChecking)
        diagnose(Tok, diag::convention_attribute_ctype_expected_label);
      return true;
    }
    if (!consumeIf(tok::colon)) {
      if (!justChecking)
        diagnose(Tok, diag::convention_attribute_ctype_expected_colon);
      return true;
    }
    if (Tok.isNot(tok::string_literal)) {
      if (!justChecking)
        diagnose(Tok, diag::convention_attribute_ctype_expected_string);
      return true;
    }
    if (auto ty = getStringLiteralIfNotInterpolated(Tok.getLoc(), "(C type)")) {
      cType = Located<StringRef>(ty.value(), Tok.getLoc());
    }
    consumeToken(tok::string_literal);
  }

  DeclNameRef witnessMethodProtocol;
  if (conventionName.Item == "witness_method") {
    if (!consumeIf(tok::colon)) {
      if (!justChecking)
        diagnose(Tok,
                 diag::convention_attribute_witness_method_expected_colon);
      return true;
    }

    DeclNameLoc unusedLoc;
    witnessMethodProtocol = parseDeclNameRef(
        unusedLoc, diag::convention_attribute_witness_method_expected_protocol,
        DeclNameFlag::AllowLowercaseAndUppercaseSelf);
  }
  
  // Parse the ')'.  We can't use parseMatchingToken if we're in
  // just-checking mode.
  if (justChecking && Tok.isNot(tok::r_paren))
    return true;

  SourceLoc RPLoc;
  if (parseMatchingToken(tok::r_paren, RPLoc,
                         diag::convention_attribute_expected_rparen,
                         LPLoc))
    return true;

  if (!justChecking) {
    result = new (Context) ConventionTypeAttr(atLoc, attrLoc, {LPLoc, RPLoc},
                                              conventionName,
                                              witnessMethodProtocol, cType);
  }
  return false;
}

bool Parser::parseUUIDString(UUID &uuid, Diag<> diagnostic, bool justChecking) {
  if (!Tok.is(tok::string_literal)) {
    if (!justChecking)
      diagnose(Tok, diagnostic);
    return true;
  }

  bool failed = true;
  auto literalText = Tok.getText().slice(1, Tok.getText().size() - 1);
  llvm::SmallString<UUID::StringBufferSize> text(literalText);
  if (auto id = UUID::fromString(text.c_str())) {
    uuid = *id;
    failed = false;
  } else if (!justChecking) {
    diagnose(Tok, diagnostic);
  }
  consumeToken(tok::string_literal);
  return failed;
}

/// \verbatim
///   attribute-type:
///     'noreturn'
/// \endverbatim
///
/// \param justChecking - if true, we're just checking whether we
///   canParseTypeAttribute; don't emit any diagnostics, and there's
///   no need to actually record the attribute
ParserStatus Parser::parseTypeAttribute(TypeOrCustomAttr &result,
                                        SourceLoc AtLoc, SourceLoc AtEndLoc,
                                        ParseTypeReason reason,
                                        bool justChecking) {
  if (AtEndLoc != Tok.getLoc()) {
    diagnose(AtEndLoc, diag::attr_extra_whitespace_after_at)
        .warnUntilSwiftVersion(6);
  }

  // If this not an identifier, the attribute is malformed.
  if (Tok.isNot(tok::identifier) &&
      // These are keywords that we accept as attribute names.
      Tok.isNot(tok::kw_in) && Tok.isNot(tok::kw_inout)) {

    if (Tok.is(tok::code_complete)) {
      if (!justChecking) {
        if (CodeCompletionCallbacks) {
          switch (reason) {
          case ParseTypeReason::InheritanceClause:
            CodeCompletionCallbacks->completeTypeAttrInheritanceBeginning();
            break;
          default:
            CodeCompletionCallbacks->completeTypeAttrBeginning();
            break;
          }
        }
      }
      consumeToken(tok::code_complete);
      return makeParserCodeCompletionStatus();
    }

    if (!justChecking)
      diagnose(Tok, diag::expected_attribute_name);
    return makeParserError();
  }
  
  // Determine which attribute it is, and diagnose it if unknown.
  auto optAttr = TypeAttribute::getAttrKindFromString(Tok.getText());

  if (!optAttr) {
    auto declAttrID = DeclAttribute::getAttrKindFromString(Tok.getText());
    if (declAttrID) {
      // This is a valid decl attribute so they should have put it on the decl
      // instead of the type.
      if (justChecking) return makeParserError();

      // If we are on a simple decl, emit a fixit to move or just remove
      // the attribute. Otherwise, we don't have confidence that the fixit
      // will be right.
      if (StructureMarkers.empty() ||
          StructureMarkers.back().Loc.isInvalid() ||
          peekToken().is(tok::equal)) {
        diagnose(Tok, diag::decl_attribute_applied_to_type);
      } else if (InBindingPattern != PatternBindingState::NotInBinding ||
                 StructureMarkers.back().Kind !=
                     StructureMarkerKind::Declaration) {
        // In let/var/inout pattern binding declaration context or in non-decl,
        // so we can only suggest a remove fix-it.
        diagnose(Tok, diag::decl_attribute_applied_to_type)
            .fixItRemove(SourceRange(AtLoc, Tok.getLoc()));
      } else {
        // Otherwise, this is the first type attribute and we know where the
        // declaration is.  Emit the same diagnostic, but include a fixit to
        // move the attribute.  Unfortunately, we don't have enough info to add
        // the attribute to DeclAttributes.
        diagnose(Tok, diag::decl_attribute_applied_to_type)
          .fixItRemove(SourceRange(AtLoc, Tok.getLoc()))
          .fixItInsert(StructureMarkers.back().Loc,
                       "@" + Tok.getText().str()+" ");
      }

      // Recover by eating @foo(...) when foo is not known.
      consumeToken();

      if (Tok.is(tok::l_paren) && getEndOfPreviousLoc() == Tok.getLoc()) {
        CancellableBacktrackingScope backtrack(*this);
        skipSingle();
        // If we found '->', or 'throws' after paren, it's likely a parameter
        // of function type.
        if (Tok.isNot(tok::arrow, tok::kw_throws, tok::kw_rethrows,
                      tok::kw_throw))
          backtrack.cancelBacktrack();
      }

      return makeParserError();
    }

    // If we're just checking, try to parse now.
    if (justChecking)
      return canParseCustomAttribute() ? makeParserSuccess()
                                       : makeParserError();

    // Parse as a custom attribute.
    auto customAttrResult = parseCustomAttribute(AtLoc);
    if (customAttrResult.isParseErrorOrHasCompletion())
      return customAttrResult;

    if (auto attr = customAttrResult.get())
      result = attr;
    return makeParserSuccess();
  }

  auto attr = *optAttr;
  
  // Ok, it is a valid attribute, eat it, and then process it.
  StringRef Text = Tok.getText();
  auto attrLoc = consumeToken();
  
  // Handle any attribute-specific processing logic.
  switch (attr) {

  // Simple type attributes don't need any further checking.
#define SIMPLE_SIL_TYPE_ATTR(SPELLING, CLASS)
#define SIMPLE_TYPE_ATTR(SPELLING, CLASS) case TypeAttrKind::CLASS:
#include "swift/AST/TypeAttr.def"
  SimpleAttr:
    if (!justChecking) {
      result = TypeAttribute::createSimple(Context, attr, AtLoc, attrLoc);
    }
    return makeParserSuccess();

  // For simple SIL type attributes, check whether we're parsing SIL,
  // then return to the SimpleAttr case.
#define SIMPLE_SIL_TYPE_ATTR(SPELLING, CLASS) case TypeAttrKind::CLASS:
#include "swift/AST/TypeAttr.def"
    if (!isInSILMode()) {
      if (!justChecking) {
        if (attr == TypeAttrKind::Inout) {
          diagnose(AtLoc, diag::inout_not_attribute);
        } else {
          // We could use diag::only_allowed_in_sil here, but it's better
          // not to mention SIL in general diagnostics.
          diagnose(AtLoc, diag::unknown_attribute, Text);
        }
      }
      return makeParserSuccess();
    }
    goto SimpleAttr;

  // All the non-simple attributes should get explicit cases here.

  case TypeAttrKind::Isolated: {
    SourceLoc lpLoc = Tok.getLoc(), kindLoc, rpLoc;
    if (!consumeIfNotAtStartOfLine(tok::l_paren)) {
      if (!justChecking) {
        diagnose(Tok, diag::attr_isolated_expected_lparen);
        // TODO: should we suggest removing the `@`?
      }
      return makeParserError();
    }

    bool invalid = false;
    std::optional<IsolatedTypeAttr::IsolationKind> kind;
    if (isIdentifier(Tok, "any")) {
      kindLoc = consumeToken(tok::identifier);
      kind = IsolatedTypeAttr::IsolationKind::Dynamic;

    // Add new kinds of isolation here; be sure to update the text for
    // attr_isolated_expected_kind, and change the associated fix-it below.

    } else {
      if (!justChecking) {
        diagnose(Tok, diag::attr_isolated_expected_kind)
          .fixItReplace(Tok.getLoc(), "any");
      }
      invalid = true;
      consumeIf(tok::identifier);
    }

    if (justChecking && !Tok.is(tok::r_paren))
      return makeParserError();
    if (parseMatchingToken(tok::r_paren, rpLoc,
                           diag::attr_isolated_expected_rparen,
                           lpLoc))
      return makeParserError();

    if (invalid)
      return makeParserError();
    assert(kind);

    if (!justChecking) {
      result = new (Context) IsolatedTypeAttr(AtLoc, attrLoc,
                                              {lpLoc, rpLoc},
                                              {*kind, kindLoc});
    }
    return makeParserSuccess();
  }

  case TypeAttrKind::Opened: {
    // Parse the opened existential ID string in parens
    SourceLoc beginLoc = Tok.getLoc(), idLoc, endLoc;
    if (!consumeAttributeLParen()) {
      if (!justChecking)
        diagnose(Tok, diag::opened_attribute_expected_lparen);
      return makeParserError();
    }

    idLoc = Tok.getLoc();
    UUID id;
    bool invalid = false;
    if (parseUUIDString(id, diag::opened_attribute_id_value, justChecking))
      invalid = true;

    TypeRepr *constraintType = nullptr;
    if (consumeIf(tok::comma)) {
      auto constraintTypeResult = parseType(diag::expected_type);
      if (constraintTypeResult.isNonNull())
        constraintType = constraintTypeResult.getPtrOrNull();
      else
        invalid = true;
    } else {
      if (!justChecking)
        diagnose(Tok, diag::attr_expected_comma, "@opened", false);
      invalid = true;
    }

    if (justChecking && !Tok.is(tok::r_paren))
      return makeParserError();

    if (parseMatchingToken(tok::r_paren, endLoc,
                           diag::opened_attribute_expected_rparen,
                           beginLoc))
      invalid = true;

    if (invalid)
      return makeParserError();

    if (!justChecking) {
      result = new (Context) OpenedTypeAttr(AtLoc, attrLoc,
                                            {beginLoc, endLoc},
                                            {id, idLoc}, constraintType);
    }
    return makeParserSuccess();
  }

  case TypeAttrKind::PackElement: {
    if (!isInSILMode()) {
      if (!justChecking)
        diagnose(AtLoc, diag::only_allowed_in_sil, "pack_element");
      return makeParserSuccess();
    }

    // Parse the opened ID string in parens
    SourceLoc beginLoc = Tok.getLoc(), idLoc, endLoc;
    if (!consumeIfAttributeLParen()) {
      if (!justChecking)
        diagnose(Tok, diag::pack_element_attribute_expected_lparen);
      return makeParserError();
    }

    idLoc = Tok.getLoc();
    UUID id;
    bool invalid = false;
    if (parseUUIDString(id, diag::opened_attribute_id_value, justChecking))
      invalid = true;

    // TODO: allow more information so that these can be parsed
    // prior to the open instruction.

    if (justChecking && !Tok.is(tok::r_paren))
      return makeParserError();

    if (parseMatchingToken(tok::r_paren, endLoc,
                           diag::opened_attribute_expected_rparen,
                           beginLoc))
      invalid = true;

    if (invalid)
      return makeParserError();

    if (!justChecking) {
      result = new (Context) PackElementTypeAttr(AtLoc, attrLoc,
                                                 {beginLoc, endLoc},
                                                 {id, idLoc});
    }
    return makeParserSuccess();
  }

  case TypeAttrKind::Differentiable: {
    auto diffKind = DifferentiabilityKind::Normal;
    SourceLoc diffKindLoc;
    SourceRange parensRange;
    if (parseDifferentiableTypeAttributeArgument(
            *this, parensRange, diffKind, diffKindLoc,
            /*emitDiagnostics=*/!justChecking))
      return makeParserError();

    // Only 'reverse' is supported today.
    // TODO: Change this to an error once clients have migrated to 'reverse'.
    if (diffKind == DifferentiabilityKind::Normal) {
      diagnose(getEndOfPreviousLoc(),
               diag::attr_differentiable_expected_reverse)
          .fixItInsert(getEndOfPreviousLoc(), "(reverse)");
      diffKind = DifferentiabilityKind::Reverse;
    }

    if (!justChecking) {
      result = new (Context) DifferentiableTypeAttr(AtLoc, attrLoc, parensRange,
                                                    {diffKind, diffKindLoc});
    }
    return makeParserSuccess();
  }

  case TypeAttrKind::Convention: {
    ConventionTypeAttr *convention = nullptr;
    if (parseConventionAttributeInternal(AtLoc, attrLoc, convention,
                                         justChecking)) {
      if (Tok.is(tok::r_paren))
        consumeToken();
      return makeParserError();
    }

    result = convention;
    return makeParserSuccess();
  }

  case TypeAttrKind::OpaqueReturnTypeOf: {
    // Parse the mangled decl name and index.
    auto beginLoc = Tok.getLoc();
    if (!consumeIfAttributeLParen()) {
      if (!justChecking)
        diagnose(Tok, diag::attr_expected_lparen, "_opaqueReturnTypeOf", false);
      return makeParserError();
    }

    if (!Tok.is(tok::string_literal)) {
      if (!justChecking)
        diagnose(Tok, diag::opened_attribute_id_value);
      return makeParserError();
    }
    auto mangling = Tok.getText().slice(1, Tok.getText().size() - 1);
    auto manglingLoc = consumeToken(tok::string_literal);
    
    if (!Tok.is(tok::comma)) {
      if (!justChecking)
        diagnose(Tok, diag::attr_expected_comma, "_opaqueReturnTypeOf", false);
      return makeParserError();
    }
    consumeToken(tok::comma);
    
    if (!Tok.is(tok::integer_literal)) {
      if (!justChecking)
        diagnose(Tok, diag::attr_expected_string_literal, "_opaqueReturnTypeOf");
      return makeParserError();
    }
    
    unsigned index;
    if (Tok.getText().getAsInteger(10, index)) {
      if (!justChecking)
        diagnose(Tok, diag::attr_expected_string_literal, "_opaqueReturnTypeOf");
      return makeParserError();
    }
    auto indexLoc = consumeToken(tok::integer_literal);

    if (justChecking && !Tok.is(tok::r_paren))
      return makeParserError();

    SourceLoc endLoc;
    parseMatchingToken(tok::r_paren, endLoc,
                       diag::expected_rparen_expr_list,
                       beginLoc);

    if (!justChecking) {
      result = new (Context) OpaqueReturnTypeOfTypeAttr(AtLoc, attrLoc,
                                                        {beginLoc, endLoc},
                                                        {mangling, manglingLoc},
                                                        {index, indexLoc});
    }
    return makeParserSuccess();
  }
  }

  llvm_unreachable("bad attribute kind");
}

ParserResult<LifetimeEntry> Parser::parseLifetimeEntry(SourceLoc loc) {
  ParserStatus status;

  auto getLifetimeDependenceKind =
      [&](Token Tok) -> std::optional<ParsedLifetimeDependenceKind> {
    if (Tok.isContextualKeyword("copy") &&
        peekToken().isAny(tok::identifier, tok::integer_literal,
                          tok::kw_self)) {
      return ParsedLifetimeDependenceKind::Inherit;
    }
    if (Tok.isContextualKeyword("borrow") &&
        peekToken().isAny(tok::identifier, tok::integer_literal,
                          tok::kw_self)) {
      return ParsedLifetimeDependenceKind::Borrow;
    }
    if (Tok.is(tok::amp_prefix) &&
        peekToken().isAny(tok::identifier, tok::integer_literal,
                          tok::kw_self)) {
      return ParsedLifetimeDependenceKind::Inout;
    }
    return std::nullopt;
  };

  if (!Tok.isFollowingLParen()) {
    diagnose(loc, diag::expected_lparen_after_lifetime_dependence);
    status.setIsParseError();
    return status;
  }

  auto lParenLoc = consumeAttributeLParen(); // consume the l_paren

  std::optional<LifetimeDescriptor> targetDescriptor;
  if (Tok.isAny(tok::identifier, tok::integer_literal, tok::kw_self) &&
      peekToken().is(tok::colon)) {
    targetDescriptor = parseLifetimeDescriptor(*this);
    if (!targetDescriptor) {
      status.setIsParseError();
      return status;
    }
    consumeToken(); // consume ':'
  }

  SmallVector<LifetimeDescriptor> sources;
  SourceLoc rParenLoc;
  bool foundParamId = false;
  bool invalidSourceDescriptor = false;
  status = parseList(
      tok::r_paren, lParenLoc, rParenLoc, /*AllowSepAfterLast=*/false,
      diag::expected_rparen_after_lifetime_dependence, [&]() -> ParserStatus {
        ParserStatus listStatus;
        foundParamId = true;

        auto lifetimeDependenceKind = ParsedLifetimeDependenceKind::Default;
        auto result = getLifetimeDependenceKind(Tok);
        if (result.has_value()) {
          lifetimeDependenceKind = *result;
          consumeToken();
        }

        auto sourceDescriptor =
            parseLifetimeDescriptor(*this, lifetimeDependenceKind);
        if (!sourceDescriptor) {
          invalidSourceDescriptor = true;
          listStatus.setIsParseError();
          return listStatus;
        }
        sources.push_back(*sourceDescriptor);
        return listStatus;
      });

  if (invalidSourceDescriptor) {
    status.setIsParseError();
    return status;
  }
  if (!foundParamId) {
    diagnose(
        Tok,
        diag::expected_identifier_or_index_or_self_after_lifetime_dependence);
    status.setIsParseError();
    return status;
  }

  auto *lifetimeEntry = LifetimeEntry::create(
      Context, loc, rParenLoc, sources, targetDescriptor);
  return ParserResult<LifetimeEntry>(lifetimeEntry);
}

/// \verbatim
///   attribute-list:
///     /*empty*/
///     attribute-list-clause attribute-list
///   attribute-list-clause:
///     '@' attribute
/// \endverbatim
ParserStatus Parser::parseDeclAttributeList(
    DeclAttributes &Attributes, bool ifConfigsAreDeclAttrs) {
  ParserStatus Status;
  while (Tok.isAny(tok::at_sign, tok::pound_if)) {
    if (Tok.is(tok::at_sign)) {
        SourceLoc AtEndLoc = Tok.getRange().getEnd();
        SourceLoc AtLoc = consumeToken();
        Status |= parseDeclAttribute(Attributes, AtLoc, AtEndLoc);
    } else {
      if (!ifConfigsAreDeclAttrs && !ifConfigContainsOnlyAttributes())
        break;

      Status |= parseIfConfigAttributes(Attributes, ifConfigsAreDeclAttrs);
    }
  }
  return Status;
}

// effectively parseDeclAttributeList but with selective modifier handling
ParserStatus Parser::parseClosureDeclAttributeList(DeclAttributes &Attributes) {
  auto parsingNonisolated = [this] {
    return Context.LangOpts.hasFeature(Feature::ClosureIsolation) &&
           Tok.isContextualKeyword("nonisolated");
  };

  if (Tok.isNot(tok::at_sign, tok::pound_if) && !parsingNonisolated())
    return makeParserSuccess();

  constexpr bool ifConfigsAreDeclAttrs = false;
  ParserStatus Status;
  while (Tok.isAny(tok::at_sign, tok::pound_if) || parsingNonisolated()) {
    if (Tok.is(tok::at_sign)) {
      SourceLoc AtEndLoc = Tok.getRange().getEnd();
      SourceLoc AtLoc = consumeToken();
      Status |= parseDeclAttribute(Attributes, AtLoc, AtEndLoc);
    } else if (parsingNonisolated()) {
      Status |=
          parseNewDeclAttribute(Attributes, {}, DeclAttrKind::Nonisolated);
    } else {
      if (!ifConfigsAreDeclAttrs && !ifConfigContainsOnlyAttributes()) {
        break;
      }
      Status |= parseIfConfigAttributes(Attributes, ifConfigsAreDeclAttrs);
    }
  }
  return Status;
}

/// \verbatim
///   modifier-list
///     /* empty */
//      modifier modifier-list
//    modifier
//      'private'
//      'private' '(' 'set' ')'
//      'fileprivate'
//      'fileprivate' '(' 'set' )'
//      'internal'
//      'internal' '(' 'set' ')'
//      'public'
//      'open'
//      'weak'
//      'unowned'
//      'unowned' '(' 'safe' ')'
//      'unowned' '(' 'unsafe' ')'
//      'optional'
//      'required'
//      'lazy'
//      'final'
//      'dynamic'
//      'prefix'
//      'postfix'
//      'infix'
//      'override'
//      'mutating
//      'nonmutating'
//      '__consuming'
//      'convenience'
//      'actor'
//      'distributed'
ParserStatus Parser::parseDeclModifierList(DeclAttributes &Attributes,
                                           SourceLoc &StaticLoc,
                                           StaticSpellingKind &StaticSpelling,
                                           bool isFromClangAttribute) {
  ParserStatus status;
  while (true) {
    switch (Tok.getKind()) {

    case tok::kw_private:
    case tok::kw_fileprivate:
    case tok::kw_internal:
    case tok::kw_public: {
      // We still model these specifiers as attributes.
      status |= parseNewDeclAttribute(Attributes, /*AtLoc=*/{},
                                      DeclAttrKind::AccessControl);
      continue;
    }

    // Context sensitive keywords.
    case tok::identifier: {
      if (Tok.isEscapedIdentifier())
        break;

      auto Kind = llvm::StringSwitch<std::optional<DeclAttrKind>>(Tok.getText())
#define CONTEXTUAL_CASE(KW, CLASS) .Case(#KW, DeclAttrKind::CLASS)
#define CONTEXTUAL_DECL_ATTR(KW, CLASS, ...) CONTEXTUAL_CASE(KW, CLASS)
#define CONTEXTUAL_DECL_ATTR_ALIAS(KW, CLASS) CONTEXTUAL_CASE(KW, CLASS)
#define CONTEXTUAL_SIMPLE_DECL_ATTR(KW, CLASS, ...) CONTEXTUAL_CASE(KW, CLASS)
#include <swift/AST/DeclAttr.def>
#undef CONTEXTUAL_CASE
                      .Default(std::nullopt);

      if (!Kind)
        break;

      Tok.setKind(tok::contextual_keyword);

      if (Kind == DeclAttrKind::Actor) {
        // If the next token is a startOfSwiftDecl, we are part of the modifier
        // list and should consume the actor token (e.g, actor public class Foo)
        // otherwise, it's the decl keyword (e.g. actor Foo) and shouldn't be.
        // Unfortunately, the BacktrackingScope will eat diagnostics emitted in
        // that scope, so we have to store enough state to emit the diagnostics
        // outside of the scope.
        bool isActorModifier = false;
        SourceLoc actorLoc = Tok.getLoc();

        {
          BacktrackingScope Scope(*this);

          consumeToken(); // consume actor
          isActorModifier = isStartOfSwiftDecl(
              /*allowPoundIfAttributes=*/false);
        }

        if (!isActorModifier)
          break;

        // Actor is a standalone keyword now, so it can't be used
        // as a modifier. Let's diagnose and recover.
        status.setIsParseError();

        consumeToken(); // consume 'actor'

        diagnose(actorLoc, diag::keyword_cant_be_identifier, Tok.getText());
        continue;
      }

      status |= parseNewDeclAttribute(Attributes, /*AtLoc=*/{}, *Kind,
                                      isFromClangAttribute);
      continue;
    }

    case tok::kw_static: {
      // 'static' is not handled as an attribute in AST.
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
    }

    case tok::kw_class: {
      // If 'class' is a modifier on another decl kind, like var or func,
      // then treat it as a modifier.
      {
        BacktrackingScope Scope(*this);
        consumeToken(tok::kw_class);
        // When followed by an 'override' or CC token inside a class,
        // treat 'class' as a modifier; in the case of a following CC
        // token, we cannot be sure there is no intention to override
        // or witness something static.
        if (isStartOfSwiftDecl() || (isa<ClassDecl>(CurDeclContext) &&
                                     (Tok.is(tok::code_complete) ||
                                      Tok.getRawText() == "override"))) {
          /* We're OK */
        } else {
          // This 'class' is a real ClassDecl introducer.
          break;
        }
      }
      if (StaticLoc.isValid()) {
        diagnose(Tok, diag::decl_already_static,
                 StaticSpellingKind::KeywordClass)
            .highlight(StaticLoc)
            .fixItRemove(Tok.getLoc());
      } else {
        StaticLoc = Tok.getLoc();
        StaticSpelling = StaticSpellingKind::KeywordClass;
      }
      consumeToken(tok::kw_class);
      continue;
    }

    case tok::unknown:
      // Eat an invalid token in decl modifier context. Error tokens are
      // diagnosed by the lexer, so we don't need to emit another diagnostic.
      consumeToken(tok::unknown);
      continue;

    default:
      break;
    }

    // If we 'break' out of the switch, modifier list has ended.
    return status;
  }
}

/// This is the internal implementation of \c parseTypeAttributeList,
/// which we expect to be inlined to handle the common case of an absent
/// attribute list.
///
/// \verbatim
///   attribute-list:
///     /*empty*/
///     attribute-list-clause attribute-list
///     'inout' attribute-list-clause attribute-list
///     '__shared' attribute-list-clause attribute-list
///     '__owned' attribute-list-clause attribute-list
///     'some' attribute-list-clause attribute-list
///     'nonisolated(nonsending)' attribute-list-clause attribute-list
///   attribute-list-clause:
///     '@' attribute
///     '@' attribute attribute-list-clause
/// \endverbatim
ParserStatus Parser::ParsedTypeAttributeList::slowParse(Parser &P) {
  ParserStatus status;
  auto &Tok = P.Tok;
  while (P.isParameterSpecifier()) {
    if (Tok.isContextualKeyword("isolated")) {
      Tok.setKind(tok::contextual_keyword);
      auto kwLoc = P.consumeToken();

      if (IsolatedLoc.isValid()) {
        P.diagnose(kwLoc, diag::parameter_specifier_repeated)
            .fixItRemove(SpecifierLoc);
      }
      IsolatedLoc = kwLoc;
      continue;
    }

    if (Tok.isContextualKeyword("_const")) {
      Tok.setKind(tok::contextual_keyword);
      ConstLoc = P.consumeToken();
      continue;
    }

    // nonisolated(nonsending)
    if (Tok.isContextualKeyword("nonisolated")) {
      Tok.setKind(tok::contextual_keyword);

      auto kwLoc = P.consumeToken();

      if (CallerIsolatedLoc.isValid()) {
        P.diagnose(kwLoc, diag::nonisolated_nonsending_repeated)
            .fixItRemove(SpecifierLoc);
      }

      // '('
      if (!P.consumeIfAttributeLParen()) {
        P.diagnose(Tok, diag::nonisolated_nonsending_expected_lparen);
        status.setIsParseError();
        continue;
      }

      if (!Tok.isContextualKeyword("nonsending")) {
        P.diagnose(Tok, diag::nonisolated_nonsending_incorrect_modifier);
        status.setIsParseError();
        continue;
      }

      (void)P.consumeToken();

      // ')'
      if (!P.consumeIf(tok::r_paren)) {
        P.diagnose(Tok, diag::nonisolated_nonsending_expected_rparen);
        status.setIsParseError();
        continue;
      }

      CallerIsolatedLoc = kwLoc;
      continue;
    }

    // Perform an extra check for 'sending'. Since it is a specifier, we use
    // the actual parsing logic below.
    if (Tok.isContextualKeyword("sending")) {
      if (!P.Context.LangOpts.hasFeature(Feature::SendingArgsAndResults)) {
        P.diagnose(Tok, diag::requires_experimental_feature, Tok.getRawText(),
                   false, Feature::SendingArgsAndResults.getName());
      }

      // Only allow for 'sending' to be written once.
      if (SendingLoc.isValid()) {
        P.diagnose(Tok, diag::sending_repeated).fixItRemove(Tok.getLoc());
      }

      // If we already saw a specifier, check if we have borrowing. In such a
      // case, emit an error.
      if (SpecifierLoc.isValid() &&
          Specifier == ParamDecl::Specifier::Borrowing) {
        P.diagnose(Tok, diag::sending_cannot_be_used_with_borrowing,
                   "sending");
      }

      SendingLoc = P.consumeToken();
      continue;
    }

    if (P.isSILLifetimeDependenceToken()) {
      if (!P.Context.LangOpts.hasFeature(Feature::LifetimeDependence) &&
          !P.Context.LangOpts.hasFeature(Feature::Lifetimes)) {
        P.diagnose(Tok, diag::requires_experimental_feature,
                   "lifetime dependence specifier", false,
                   Feature::LifetimeDependence.getName());
      }
      P.consumeToken(); // consume '@'
      auto loc = P.consumeToken(); // consume 'lifetime'
      auto result = P.parseLifetimeEntry(loc);
      if (result.isNull()) {
        status |= result;
        continue;
      }
      lifetimeEntry = result.get();
      continue;
    }

    if (SpecifierLoc.isValid()) {
      P.diagnose(Tok, diag::parameter_specifier_repeated)
          .fixItRemove(SpecifierLoc);
    } else {
      if (Tok.is(tok::kw_inout)) {
        Specifier = ParamDecl::Specifier::InOut;
      } else if (Tok.is(tok::identifier)) {
        if (Tok.getRawText() == "__shared") {
          Specifier = ParamDecl::Specifier::LegacyShared;
        } else if (Tok.getRawText() == "__owned") {
          Specifier = ParamDecl::Specifier::LegacyOwned;
        } else if (Tok.getRawText() == "borrowing") {
          Specifier = ParamDecl::Specifier::Borrowing;
        } else if (Tok.getRawText() == "consuming") {
          Specifier = ParamDecl::Specifier::Consuming;
        }
      }

      // A specifier must come before 'sending'.
      if (bool(Specifier) && SendingLoc.isValid()) {
        P.diagnose(Tok, diag::sending_before_parameter_specifier,
                   getNameForParamSpecifier(Specifier));
      }
    }
    Tok.setKind(tok::contextual_keyword);
    SpecifierLoc = P.consumeToken();
  }

  while (Tok.is(tok::at_sign)) {
    // Ignore @substituted in SIL mode and leave it for the type parser.
    if (P.isInSILMode() && P.peekToken().getText() == "substituted")
      return status;

    TypeOrCustomAttr result;
    SourceLoc AtEndLoc = Tok.getRange().getEnd();
    SourceLoc AtLoc = P.consumeToken();
    status |=
        P.parseTypeAttribute(result, AtLoc, AtEndLoc, ParseReason);
    if (status.isError())
      return status;
    if (result)
      Attributes.push_back(result);
  }
  
  return status;
}

static bool isStartOfOperatorDecl(const Token &Tok, const Token &Tok2) {
  return Tok.isContextualKeyword("operator") &&
         (Tok2.isContextualKeyword("prefix") ||
          Tok2.isContextualKeyword("postfix") ||
          Tok2.isContextualKeyword("infix"));
}

/// Diagnose issues with fixity attributes, if any.
static void diagnoseOperatorFixityAttributes(Parser &P,
                                             DeclAttributes &Attrs,
                                             const Decl *D) {
  auto isFixityAttr = [](DeclAttribute *attr){
    DeclAttrKind kind = attr->getKind();
    return attr->isValid() &&
           (kind == DeclAttrKind::Prefix || kind == DeclAttrKind::Infix ||
            kind == DeclAttrKind::Postfix);
  };
  
  SmallVector<DeclAttribute *, 3> fixityAttrs;
  std::copy_if(Attrs.begin(), Attrs.end(),
               std::back_inserter(fixityAttrs), isFixityAttr);
  std::reverse(fixityAttrs.begin(), fixityAttrs.end());
  
  for (auto it = fixityAttrs.begin(); it != fixityAttrs.end(); ++it) {
    if (it != fixityAttrs.begin()) {
      auto *attr = *it;
      P.diagnose(attr->getLocation(), diag::mutually_exclusive_attrs, attr,
                 fixityAttrs.front(), attr->isDeclModifier())
          .fixItRemove(attr->getRange());
      attr->setInvalid();
    }
  }
  
  // Operator declarations must specify a fixity.
  if (auto *OD = dyn_cast<OperatorDecl>(D)) {
    if (fixityAttrs.empty()) {
      P.diagnose(OD->getOperatorLoc(), diag::operator_decl_no_fixity);
    }
    for (auto it = Attrs.begin(); it != Attrs.end(); ++it) {
      if (isFixityAttr(*it) ||
          (*it)->getKind() == DeclAttrKind::RawDocComment) {
        continue;
      }
      auto *attr = *it;
      P.diagnose(attr->getLocation(),
                 diag::operator_decl_should_not_contain_other_attributes, attr)
          .fixItRemove(attr->getRange());
      attr->setInvalid();
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

static unsigned skipUntilMatchingRBrace(Parser &P, bool &HasPoundDirective,
                                        bool &HasPoundSourceLocation,
                                        bool &HasOperatorDeclarations,
                                        bool &HasNestedClassDeclarations,
                                        bool &HasNestedTypeDeclarations,
                                        bool &HasPotentialRegexLiteral,
                                        bool &HasDerivativeDeclarations) {
  HasPoundDirective = false;
  HasPoundSourceLocation = false;
  HasOperatorDeclarations = false;
  HasNestedClassDeclarations = false;
  HasNestedTypeDeclarations = false;
  HasPotentialRegexLiteral = false;
  HasDerivativeDeclarations = false;

  unsigned OpenBraces = 1;
  unsigned OpenPoundIf = 0;

  bool LastTokenWasFunc = false;

  while ((OpenBraces != 0 || OpenPoundIf != 0) && P.Tok.isNot(tok::eof)) {
    // Detect 'func' followed by an operator identifier.
    if (LastTokenWasFunc) {
      LastTokenWasFunc = false;
      HasOperatorDeclarations |= P.Tok.isAnyOperator();
    } else {
      LastTokenWasFunc = P.Tok.is(tok::kw_func);
    }

    HasNestedClassDeclarations |= P.Tok.is(tok::kw_class);

    HasPoundSourceLocation |= P.Tok.isAny(tok::pound_sourceLocation,
                                          tok::pound_line);
    HasPoundDirective |= HasPoundSourceLocation;
    HasPoundDirective |= P.Tok.isAny(tok::pound_if, tok::pound_else,
                                     tok::pound_endif, tok::pound_elseif);

    HasNestedTypeDeclarations |= P.Tok.isAny(tok::kw_class, tok::kw_struct,
                                             tok::kw_enum, tok::kw_typealias,
                                             tok::kw_protocol)
                              || P.Tok.isContextualKeyword("actor");

    if (P.consumeIf(tok::at_sign)) {
      if (P.Tok.is(tok::identifier)) {
        std::optional<DeclAttrKind> DK =
            DeclAttribute::getAttrKindFromString(P.Tok.getText());
        if (DK && *DK == DeclAttrKind::Derivative) {
          HasDerivativeDeclarations = true;
          P.consumeToken();
        }
      }
      continue;
    }

    // HACK: Bail if we encounter what could potentially be a regex literal.
    // This is necessary as:
    // - We might encounter an invalid Swift token that might be valid in a
    // regex.
    // - Such a literal could contain a literal `}`, which should not be treated
    // as an end brace.
    // FIXME: We should be able to handle `/.../` regex literals in the lexer.
    if (P.L->isPotentialUnskippableBareSlashRegexLiteral(P.Tok)) {
      HasPotentialRegexLiteral = true;
      return OpenBraces;
    }

    // Match opening `#if` with closing `#endif` to match what the parser does,
    // `#if` + `#endif` are considered stronger delimiters than `{` + `}`.
    if (P.consumeIf(tok::pound_if)) {
      OpenPoundIf += 1;
      continue;
    }
    if (OpenPoundIf != 0) {
      // We're in a `#if`, check to see if we've reached the end.
      if (P.consumeIf(tok::pound_endif)) {
        OpenPoundIf -= 1;
        continue;
      }
      // Consume the next token and continue iterating. We can swallow any
      // amount of '{' and '}' while in the `#if`.
      P.consumeToken();
      continue;
    }

    if (P.consumeIf(tok::l_brace)) {
      ++OpenBraces;
      continue;
    }
    if (OpenBraces == 1 && P.Tok.is(tok::r_brace))
      break;
    if (P.consumeIf(tok::r_brace)) {
      --OpenBraces;
      continue;
    }
    P.consumeToken();
  }
  return OpenBraces;
}

bool swift::isKeywordPossibleDeclStart(const LangOptions &options,
                                       const Token &Tok) {
  switch (Tok.getKind()) {
  case tok::kw_inout:
    return options.hasFeature(Feature::ReferenceBindings);
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
  case tok::pound:
  case tok::pound_if:
  case tok::pound_warning:
  case tok::pound_error:
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

static bool
consumeIfParenthesizedModifier(Parser &P, StringRef name,
                               std::initializer_list<StringRef> allowedArguments) {
  assert((P.Tok.getText() == name) && P.peekToken().is(tok::l_paren) &&
         "Invariant violated");

  // Look ahead to parse the parenthesized expression.
  Parser::CancellableBacktrackingScope backtrack(P);
  P.consumeToken(tok::identifier);
  P.consumeToken(tok::l_paren);
  
  if (P.consumeIf(tok::code_complete)) {
    // If a code complete token is present, recover from missing/incorrect argument and missing '('
    P.consumeIf(tok::identifier);
    P.consumeIf(tok::r_paren);
    backtrack.cancelBacktrack();
    return true;
  }
  
  const bool argumentIsAllowed =
      std::find(allowedArguments.begin(), allowedArguments.end(),
                P.Tok.getText()) != allowedArguments.end();
  
  if (argumentIsAllowed && P.consumeIf(tok::identifier) &&
      P.consumeIf(tok::r_paren)) {
    backtrack.cancelBacktrack();
    return true;
  }
  
  return false;
}

/// Given a current token of 'unowned', check to see if it is followed by a
/// "(safe)" or "(unsafe)" specifier and consumes if it is.
static bool consumeIfParenthesizedUnowned(Parser &P) {
  return consumeIfParenthesizedModifier(P, "unowned", {"safe", "unsafe"});
}

/// Given a current token of 'nonisolated', check to see if it is followed by an
/// "(unsafe)" or "(nonsending)" specifier and consumes if it is.
static bool consumeIfParenthesizedNonisolated(Parser &P) {
  return consumeIfParenthesizedModifier(P, "nonisolated",
                                        {"unsafe", "nonsending"});
}

static void skipAttribute(Parser &P) {
  // Consider unexpected tokens to be incomplete attributes.

  // Parse the attribute name, which can be qualified, have
  // generic arguments, and so on.
  do {
    if (!(P.consumeIf(tok::identifier) || P.consumeIf(tok::kw_rethrows)) && 
        !P.consumeIf(tok::code_complete))
      return;

    if (P.startsWithLess(P.Tok)) {
      P.consumeStartingLess();
      P.skipUntilGreaterInTypeList();
    }
  } while (P.consumeIf(tok::period));

  // Skip an argument clause after the attribute name.
  if (P.consumeIf(tok::l_paren)) {
    while (P.Tok.isNot(tok::r_brace, tok::eof, tok::pound_endif)) {
      if (P.consumeIf(tok::r_paren)) break;
      P.skipSingle();
    }
  }
}

bool Parser::isStartOfSwiftDecl(bool allowPoundIfAttributes,
                                bool hadAttrsOrModifiers) {
  if (Tok.is(tok::at_sign) && peekToken().is(tok::kw_rethrows)) {
    // @rethrows does not follow the general rule of @<identifier> so
    // it is needed to short circuit this else there will be an infinite
    // loop on invalid attributes of just rethrows
  } else if (!isKeywordPossibleDeclStart(Context.LangOpts, Tok)) {
    // If this is obviously not the start of a decl, then we're done.
    return false;
  }

  // When 'init' appears inside another 'init', it's likely the user wants to
  // invoke an initializer but forgets to prefix it with 'self.' or 'super.'
  // Otherwise, expect 'init' to be the start of a declaration (and complain
  // when the expectation is not fulfilled).
  if (Tok.is(tok::kw_init)) {
    return !isa<ConstructorDecl>(CurDeclContext);
  }

  // Similarly, when 'case' appears inside a function, it's probably a switch
  // case, not an enum case declaration.
  if (Tok.is(tok::kw_case)) {
    return !isa<AbstractFunctionDecl>(CurDeclContext);
  }

  // The protocol keyword needs more checking to reject "protocol<Int>".
  if (Tok.is(tok::kw_protocol)) {
    const Token &Tok2 = peekToken();
    return !Tok2.isAnyOperator() || Tok2.getText() != "<";
  }

  // The 'try' case is only for simple local recovery, so we only bother to
  // check 'let' and 'var' right now.
  if (Tok.is(tok::kw_try))
    return peekToken().isAny(tok::kw_let, tok::kw_var);

  // Skip an attribute, since it might be a type attribute.  This can't
  // happen at the top level of a scope, but we do use isStartOfSwiftDecl()
  // in positions like generic argument lists.
  if (Tok.is(tok::at_sign)) {
    BacktrackingScope backtrack(*this);
    while (consumeIf(tok::at_sign))
      skipAttribute(*this);

    // If this attribute is the last element in the block,
    // consider it is a start of incomplete decl.
    if (Tok.isAny(tok::r_brace, tok::eof) ||
        (Tok.is(tok::pound_endif) && !allowPoundIfAttributes))
      return true;

    return isStartOfSwiftDecl(allowPoundIfAttributes,
                              /*hadAttrsOrModifiers=*/true);
  }

  if (Tok.is(tok::pound)) {
    if (isStartOfFreestandingMacroExpansion()) {
      if (isInSILMode() || SF.Kind == SourceFileKind::Interface)
        return false;

      // Parse '#<identifier>' after attrs/modifiers as a macro expansion decl.
      if (hadAttrsOrModifiers)
        return true;

      // Macro expansions at the top level of non-script file are declarations.
      return CurDeclContext->isModuleScopeContext() && !allowTopLevelCode();
    }

    // Otherwise, prefer parsing it as an expression.
    return false;
  }

  // Skip a #if that contains only attributes in all branches. These will be
  // parsed as attributes of a declaration, not as separate declarations.
  if (Tok.is(tok::pound_if) && allowPoundIfAttributes) {
    BacktrackingScope backtrack(*this);
    bool sawAnyAttributes = false;
    if (!skipIfConfigOfAttributes(sawAnyAttributes))
      return false;
    if (Tok.is(tok::eof))
      return true;
    if (!sawAnyAttributes)
      return false;
    return isStartOfSwiftDecl(/*allowPoundIfAttributes=*/true,
                              /*hadAttrsOrModifiers=*/true);
  }

  // If we have a decl modifying keyword, check if the next token is a valid
  // decl start. This is necessary to correctly handle Swift keywords that are
  // shared by SIL, e.g 'private' in 'sil private @foo :'. We need to make sure
  // this isn't considered a valid Swift decl start.
  if (Tok.isKeyword()) {
    auto DAK = DeclAttribute::getAttrKindFromString(Tok.getText());
    if (DAK && DeclAttribute::isDeclModifier(*DAK)) {
      BacktrackingScope backtrack(*this);
      consumeToken();

      // Eat paren after modifier name; e.g. private(set)
      if (consumeIf(tok::l_paren)) {
        while (Tok.isNot(tok::r_brace, tok::eof, tok::pound_endif)) {
          if (consumeIf(tok::r_paren))
            break;

          // If we found the start of a decl while trying to skip over the
          // paren, then we have something incomplete like 'private('. Return
          // true for better recovery.
          if (isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false,
                                 /*hadAttrsOrModifiers=*/true))
            return true;

          skipSingle();
        }
      }
      return isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false,
                                /*hadAttrsOrModifiers=*/true);
    }
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
  if (Tok.getText() == "unowned" && Tok2.is(tok::l_paren)) {
    Parser::BacktrackingScope Backtrack(*this);
    if (consumeIfParenthesizedUnowned(*this)) {
      return isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false,
                                /*hadAttrsOrModifiers=*/true);
    }
  }

  // If this is 'nonisolated', check to see if it is valid.
  if (Tok.isContextualKeyword("nonisolated") && Tok2.is(tok::l_paren)) {
    BacktrackingScope backtrack(*this);
    if (consumeIfParenthesizedNonisolated(*this)) {
      return isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false,
                                /*hadAttrsOrModifiers=*/true);
    }
  }

  if (Tok.isContextualKeyword("actor")) {
    if (Tok2.is(tok::identifier)) // actor Foo {}
      return true;
    BacktrackingScope Scope(*this);
    // actor may be somewhere in the modifier list. Eat the tokens until we get
    // to something that isn't the start of a decl. If that is an identifier,
    // it's an actor declaration, otherwise, it isn't.
    do {
      consumeToken();
    } while (isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false,
                                /*hadAttrsOrModifiers=*/true));
    return Tok.is(tok::identifier);
  }

  // 'macro' name
  if (Tok.isContextualKeyword("macro")) {
    return Tok2.is(tok::identifier);
  }

  if (Tok.isContextualKeyword("package")) {
    // If `case` is the next token after `return package` statement,
    // E.g.
    // switch package {
    //   case .x: return package
    //   case .y: return nil
    // }
    // currently it errors (this is also true for `open`).
    //
    // If a non-contextual keyword was used in the above example, this
    // function hits the line from above:
    // ```
    // if (!Tok.isContextualDeclKeyword())
    //   return false;
    // ```
    // thus we return false here as well, i.e. treat it as a non-contextual
    // keyword.
    if (Tok2.getKind() == tok::kw_case)
      return false;
    
    // Handle 'package(set)' access modifier
    auto DAK = DeclAttribute::getAttrKindFromString(Tok.getText());
    if (DAK && DeclAttribute::isDeclModifier(*DAK)) {
      BacktrackingScope backtrack(*this);
      // First consume `package`
      consumeToken();
      // Eat paren after modifier name; e.g. `package(set)`, similar to
      // `private(set)` described above in the `if (Tok.isKeyword())` block
      if (consumeIf(tok::l_paren)) {
        while (Tok.isNot(tok::r_brace, tok::eof, tok::pound_endif)) {
          if (consumeIf(tok::r_paren))
            break;
          // If we found the start of a decl while trying to skip over the
          // paren, then we have something incomplete like 'package('. Return
          // true for better recovery.
          if (isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false,
                                 /*hadAttrsOrModifiers=*/true))
            return true;
          skipSingle();
        }
      }
      return isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false,
                                /*hadAttrsOrModifiers=*/true);
    }
  }

  // `using @<attribute>` or `using <identifier>`.
  if (Tok.isContextualKeyword("using")) {
    // `using` declarations don't support attributes or modifiers.
    if (hadAttrsOrModifiers)
      return false;

    return !Tok2.isAtStartOfLine() &&
           (Tok2.is(tok::at_sign) || Tok2.is(tok::identifier) ||
            Tok2.is(tok::code_complete));
  }

  // If the next token is obviously not the start of a decl, bail early.
  if (!isKeywordPossibleDeclStart(Context.LangOpts, Tok2))
    return false;
  
  // Otherwise, do a recursive parse.
  Parser::BacktrackingScope Backtrack(*this);
  consumeToken(tok::identifier);
  return isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false,
                            /*hadAttrsOrModifiers=*/true);
}

bool Parser::isStartOfSILDecl() {
  switch (Tok.getKind()) {
  case tok::kw_sil:
  case tok::kw_sil_stage:
  case tok::kw_sil_property:
  case tok::kw_sil_vtable:
  case tok::kw_sil_moveonlydeinit:
  case tok::kw_sil_global:
  case tok::kw_sil_witness_table:
  case tok::kw_sil_default_witness_table:
  case tok::kw_sil_default_override_table:
  case tok::kw_sil_differentiability_witness:
  case tok::kw_sil_coverage_map:
  case tok::kw_sil_scope:
    // SIL decls must start on a new line.
    return Tok.isAtStartOfLine();
  case tok::kw_undef:
  case tok::NUM_TOKENS:
    return false;
#define SIL_KEYWORD(Name)
#define TOKEN(Name) case tok:: Name: return false;
#include "swift/AST/TokenKinds.def"
  }
  llvm_unreachable("Unhandled case in switch");
}

bool Parser::isStartOfFreestandingMacroExpansion() {
  // Check if "'#' <identifier>" where the identifier is on the sameline.
  if (!Tok.is(tok::pound))
    return false;
  const Token &Tok2 = peekToken();
  if (Tok2.isAtStartOfLine())
    return false;

  if (Tok2.isAny(tok::identifier, tok::code_complete))
    return true;
  if (Tok2.isKeyword()) {
    // allow keywords right after '#' so we can diagnose it when parsing.
    return Tok.getRange().getEnd() == Tok2.getLoc();
  }
  return false;
}

void Parser::consumeDecl(ParserPosition BeginParserPosition, bool IsTopLevel) {
  SourceLoc CurrentLoc = Tok.getLoc();

  SourceLoc EndLoc = PreviousLoc;
  backtrackToPosition(BeginParserPosition);
  SourceLoc BeginLoc = Tok.getLoc();

  State->setIDEInspectionDelayedDeclState(
      SourceMgr, L->getBufferID(), IDEInspectionDelayedDeclKind::Decl,
      CurDeclContext, {BeginLoc, EndLoc}, BeginParserPosition.PreviousLoc);

  while (SourceMgr.isBeforeInBuffer(Tok.getLoc(), CurrentLoc))
    consumeToken();

  if (IsTopLevel) {
    // Skip the rest of the file to prevent the parser from constructing the
    // AST for it.  Forward references are not allowed at the top level.
    while (Tok.isNot(tok::eof))
      consumeToken();
  }
}

/// Determine the declaration parsing options to use when parsing the decl in
/// the given context.
static Parser::ParseDeclOptions getParseDeclOptions(DeclContext *DC) {
  using ParseDeclOptions = Parser::ParseDeclOptions;

  if (DC->isModuleScopeContext())
    return Parser::PD_AllowTopLevel;

  if (DC->isLocalContext())
    return Parser::PD_Default;

  auto decl = DC->getAsDecl();
  switch (decl->getKind()) {
  case DeclKind::Extension:
    return ParseDeclOptions(Parser::PD_HasContainerType |
                            Parser::PD_InExtension);
  case DeclKind::Enum:
    return ParseDeclOptions(Parser::PD_HasContainerType |
                            Parser::PD_AllowEnumElement | Parser::PD_InEnum);

  case DeclKind::Protocol:
    return ParseDeclOptions(Parser::PD_HasContainerType |
                            Parser::PD_DisallowInit | Parser::PD_InProtocol);

  case DeclKind::Class:
    return ParseDeclOptions(Parser::PD_HasContainerType | Parser::PD_InClass);

  case DeclKind::Struct:
    return ParseDeclOptions(Parser::PD_HasContainerType | Parser::PD_InStruct);

  default:
    llvm_unreachable("Bad iterable decl context kinds.");
  }
}

/// Parse a single syntactic declaration and return a list of decl
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
ParserStatus Parser::parseDecl(bool IsAtStartOfLineOrPreviousHadSemi,
                               bool IfConfigsAreDeclAttrs,
                               llvm::function_ref<void(Decl *)> Handler,
                               bool stubOnly) {
  ParseDeclOptions Flags = getParseDeclOptions(CurDeclContext);
  if (stubOnly) {
    Flags |= PD_StubOnly;
  }
  ParserPosition BeginParserPosition;
  if (isIDEInspectionFirstPass())
    BeginParserPosition = getParserPosition();

  if (Tok.is(tok::pound_if) && !ifConfigContainsOnlyAttributes()) {
    auto IfConfigResult = parseIfConfig(
        IfConfigContext::DeclItems,
        [&](bool IsActive) {
          ParserStatus Status;
          bool PreviousHadSemi = true;
          while (Tok.isNot(tok::pound_else, tok::pound_endif, tok::pound_elseif,
                           tok::r_brace, tok::eof)) {
            Status |= parseDeclItem(PreviousHadSemi,
                                    [&](Decl *D) {
              if (IsActive) {
                Handler(D);
              }
            });
          }
        });
    if (IfConfigResult.hasCodeCompletion() && isIDEInspectionFirstPass()) {
      consumeDecl(BeginParserPosition, CurDeclContext->isModuleScopeContext());
      return makeParserError();
    }

    return makeParserSuccess();
  }
  if (Tok.isAny(tok::pound_warning, tok::pound_error)) {
    return parseDeclPoundDiagnostic();
  }

  // Note that we're parsing a declaration.
  StructureMarkerRAII ParsingDecl(*this, Tok.getLoc(),
                                  StructureMarkerKind::Declaration);

  // Parse attributes.
  DeclAttributes Attributes;
  if (Tok.hasComment())
    Attributes.add(new (Context) RawDocCommentAttr(Tok.getCommentRange()));
  ParserStatus AttrStatus = parseDeclAttributeList(
      Attributes, IfConfigsAreDeclAttrs);

  // Parse modifiers.
  // Keep track of where and whether we see a contextual keyword on the decl.
  SourceLoc StaticLoc;
  StaticSpellingKind StaticSpelling = StaticSpellingKind::None;
  auto ModifierResult =
      parseDeclModifierList(Attributes, StaticLoc, StaticSpelling);
  if (ModifierResult.hasCodeCompletion()) {
    return ModifierResult;
  }

  ParserResult<Decl> DeclResult;

  // We emit diagnostics for 'try let ...' in parseDeclVar().
  SourceLoc tryLoc;
  if (Tok.is(tok::kw_try) && peekToken().isAny(tok::kw_let, tok::kw_var))
    tryLoc = consumeToken(tok::kw_try);

  // Save the original token, in case code-completion needs it.
  auto OrigTok = Tok;
  bool MayNeedOverrideCompletion = false;

  bool HandlerAlreadyCalled = false;

  auto parseBindingIntroducer = [&](bool HasBindingIntroducerKeyword) {
    // Collect all modifiers into a modifier list.
    llvm::SmallVector<Decl *, 4> Entries;
    DeclResult = parseDeclVar(Flags, Attributes, Entries, StaticLoc,
                              StaticSpelling, tryLoc, HasBindingIntroducerKeyword);
    StaticLoc = SourceLoc(); // we handled static if present.
    MayNeedOverrideCompletion = true;
    if ((AttrStatus.hasCodeCompletion() || DeclResult.hasCodeCompletion())
        && isIDEInspectionFirstPass())
      return;
    std::for_each(Entries.begin(), Entries.end(), Handler);
    HandlerAlreadyCalled = true;
  };

  auto parseFunc = [&](bool HasFuncKeyword) {
    // Collect all modifiers into a modifier list.
    DeclResult = parseDeclFunc(StaticLoc, StaticSpelling, Flags, Attributes,
                               HasFuncKeyword);
    StaticLoc = SourceLoc(); // we handled static if present.
    MayNeedOverrideCompletion = true;
  };

  switch (Tok.getKind()) {
  case tok::kw_import:
    DeclResult = parseDeclImport(Flags, Attributes);
    break;
  case tok::kw_extension:
    DeclResult = parseDeclExtension(Flags, Attributes);
    break;
  case tok::kw_let:
  case tok::kw_var: {
    parseBindingIntroducer(/*HasLetOrVarKeyword=*/true);
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
    if ((AttrStatus.hasCodeCompletion() || DeclResult.hasCodeCompletion()) &&
        isIDEInspectionFirstPass())
      break;
    std::for_each(Entries.begin(), Entries.end(), Handler);
    HandlerAlreadyCalled = true;
    break;
  }
  case tok::kw_class:
    DeclResult = parseDeclClass(Flags, Attributes);
    break;
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
    parseFunc(/*HasFuncKeyword=*/true);
    break;
  case tok::kw_subscript: {
    llvm::SmallVector<Decl *, 4> Entries;
    DeclResult = parseDeclSubscript(StaticLoc, StaticSpelling, Flags,
                                    Attributes, Entries);
    StaticLoc = SourceLoc(); // we handled static if present.
    if ((AttrStatus.hasCodeCompletion() || DeclResult.hasCodeCompletion()) &&
        isIDEInspectionFirstPass())
      break;
    std::for_each(Entries.begin(), Entries.end(), Handler);
    MayNeedOverrideCompletion = true;
    HandlerAlreadyCalled = true;
    break;
  }

  case tok::code_complete:
    MayNeedOverrideCompletion = true;
    DeclResult = makeParserError();
    // Handled below.
    break;
  case tok::pound:
    if (!isStartOfFreestandingMacroExpansion()) {
      consumeToken(tok::pound);
      diagnose(Tok.getLoc(),
               diag::macro_expansion_decl_expected_macro_identifier);
      DeclResult = makeParserError();
      break;
    }

    if (peekToken().is(tok::code_complete)) {
      consumeToken();
      if (CodeCompletionCallbacks) {
        CodeCompletionCallbacks->completeAfterPoundDirective();
      }
      consumeToken(tok::code_complete);
      DeclResult = makeParserCodeCompletionResult<Decl>();
      break;
    }

    // Parse as a macro expansion.
    DeclResult = parseDeclMacroExpansion(Flags, Attributes);
    StaticLoc = SourceLoc(); // Ignore 'static' on macro expansion
    break;

  case tok::pound_if:
  case tok::pound_sourceLocation:
  case tok::pound_line:
  case tok::pound_warning:
  case tok::pound_error:
    // We see some attributes right before these pounds.
    // TODO: Emit dedicated errors for them.
    LLVM_FALLTHROUGH;

  // Obvious nonsense.
  default:

    // TODO: Once reference bindings is no longer experimental, move this into
    // kw_let, kw_var.
    if (Context.LangOpts.hasFeature(Feature::ReferenceBindings) &&
        Tok.getKind() == tok::kw_inout) {
      parseBindingIntroducer(/*HasLetOrVarKeyword=*/true);
      break;
    }

    if (Tok.isContextualKeyword("actor") && peekToken().is(tok::identifier)) {
      Tok.setKind(tok::contextual_keyword);
      DeclResult = parseDeclClass(Flags, Attributes);
      break;
    }

    if (Tok.isContextualKeyword("macro") && peekToken().is(tok::identifier)) {
      Tok.setKind(tok::contextual_keyword);
      DeclResult = parseDeclMacro(Attributes);
      break;
    }

    // `using @<attribute>` or `using <identifier>`
    if (Tok.isContextualKeyword("using")) {
      auto nextToken = peekToken();
      if (!nextToken.isAtStartOfLine() &&
          (nextToken.is(tok::at_sign) || nextToken.is(tok::identifier) ||
           nextToken.is(tok::code_complete))) {
        DeclResult = parseDeclUsing(Flags, Attributes);
        break;
      }
    }

    if (Flags.contains(PD_HasContainerType) &&
        IsAtStartOfLineOrPreviousHadSemi) {

      // Emit diagnostics if we meet an identifier/operator where a declaration
      // is expected, perhaps the user forgot the 'func' or 'var' keyword.
      //
      // Must not confuse it with trailing closure syntax, so we only
      // recover in contexts where there can be no statements.

      const bool IsProbablyVarDecl =
          Tok.isIdentifierOrUnderscore() &&
          peekToken().isAny(tok::colon, tok::equal, tok::comma);

      const bool IsProbablyTupleDecl =
          Tok.is(tok::l_paren) && peekToken().isIdentifierOrUnderscore();

      if (IsProbablyVarDecl || IsProbablyTupleDecl) {

        DescriptiveDeclKind DescriptiveKind;

        switch (StaticSpelling) {
        case StaticSpellingKind::None:
          DescriptiveKind = DescriptiveDeclKind::Property;
          break;
        case StaticSpellingKind::KeywordStatic:
          DescriptiveKind = DescriptiveDeclKind::StaticProperty;
          break;
        case StaticSpellingKind::KeywordClass:
          llvm_unreachable("kw_class is only parsed as a modifier if it's "
                           "followed by a keyword");
        }

        diagnose(Tok.getLoc(), diag::expected_keyword_in_decl, "var",
                 DescriptiveKind)
            .fixItInsert(Tok.getLoc(), "var ");
        parseBindingIntroducer(/*HasLetOrVarKeyword=*/false);
        break;
      }

      const bool IsProbablyFuncDecl =
          Tok.isIdentifierOrUnderscore() || Tok.isAnyOperator();

      if (IsProbablyFuncDecl) {

        DescriptiveDeclKind DescriptiveKind;

        if (Tok.isAnyOperator()) {
          DescriptiveKind = DescriptiveDeclKind::OperatorFunction;
        } else {
          switch (StaticSpelling) {
          case StaticSpellingKind::None:
            DescriptiveKind = DescriptiveDeclKind::Method;
            break;
          case StaticSpellingKind::KeywordStatic:
            DescriptiveKind = DescriptiveDeclKind::StaticMethod;
            break;
          case StaticSpellingKind::KeywordClass:
            llvm_unreachable("kw_class is only parsed as a modifier if it's "
                             "followed by a keyword");
          }
        }

        diagnose(Tok.getLoc(), diag::expected_keyword_in_decl, "func",
                 DescriptiveKind)
            .fixItInsert(Tok.getLoc(), "func ");
        parseFunc(/*HasFuncKeyword=*/false);
        break;
      }
    }

    diagnose(Tok, diag::expected_decl);

    if (CurDeclContext) {
      if (auto nominal = dyn_cast<NominalTypeDecl>(CurDeclContext)) {
        diagnose(nominal->getLoc(), diag::note_in_decl_of,
                 nominal->createNameRef());
      } else if (auto extension = dyn_cast<ExtensionDecl>(CurDeclContext)) {
        if (auto repr = extension->getExtendedTypeRepr()) {
          if (auto declRefTR = dyn_cast<DeclRefTypeRepr>(repr)) {
            diagnose(extension->getLoc(), diag::note_in_extension_of,
                     declRefTR);
          }
        }
      }
    }
  }

  if (DeclResult.isParseErrorOrHasCompletion() && Tok.is(tok::code_complete)) {
    if (MayNeedOverrideCompletion && CodeCompletionCallbacks) {
      // If we need to complete an override, collect the keywords already
      // specified so that we do not duplicate them in code completion
      // strings.
      SmallVector<StringRef, 3> Keywords;
      SourceLoc introducerLoc;
      switch (OrigTok.getKind()) {
      case tok::kw_func:
      case tok::kw_subscript:
      case tok::kw_var:
      case tok::kw_let:
      case tok::kw_typealias:
        Keywords.push_back(OrigTok.getText());
        introducerLoc = OrigTok.getLoc();
        break;
      default:
        // Other tokens are already accounted for.
        break;
      }
      if (StaticSpelling == StaticSpellingKind::KeywordStatic) {
        Keywords.push_back(getTokenText(tok::kw_static));
      } else if (StaticSpelling == StaticSpellingKind::KeywordClass) {
        Keywords.push_back(getTokenText(tok::kw_class));
      }
      for (auto attr : Attributes) {
        Keywords.push_back(attr->getAttrName());
      }
      CodeCompletionCallbacks->completeNominalMemberBeginning(Keywords,
                                                              introducerLoc);
    }

    DeclResult = makeParserCodeCompletionStatus();
    consumeToken(tok::code_complete);
  }

  if (AttrStatus.hasCodeCompletion() || DeclResult.hasCodeCompletion()) {
    if (isIDEInspectionFirstPass() &&
        !CurDeclContext->isModuleScopeContext() &&
        !isa<TopLevelCodeDecl>(CurDeclContext) &&
        !isa<AbstractClosureExpr>(CurDeclContext)) {
      // Only consume non-toplevel decls.
      consumeDecl(BeginParserPosition, /*IsTopLevel=*/false);

      return makeParserError();
    }
    if (AttrStatus.hasCodeCompletion() && CodeCompletionCallbacks) {
      std::optional<DeclKind> DK;
      if (DeclResult.isNonNull())
        DK = DeclResult.get()->getKind();
      CodeCompletionCallbacks->setAttrTargetDeclKind(DK);
    }
    DeclResult.setHasCodeCompletionAndIsError();
    if (isIDEInspectionFirstPass())
      return DeclResult;
  }

  if (DeclResult.isNonNull()) {
    Decl *D = DeclResult.get();
    if (!HandlerAlreadyCalled)
      Handler(D);
  }

  if (!DeclResult.isParseErrorOrHasCompletion()) {
    // If we parsed 'class' or 'static', but didn't handle it above, complain
    // about it.
    if (StaticLoc.isValid())
      diagnose(DeclResult.get()->getLoc(), diag::decl_not_static,
               StaticSpelling)
          .fixItRemove(SourceRange(StaticLoc));
  }

  return DeclResult;
}

std::pair<std::vector<Decl *>, std::optional<Fingerprint>>
Parser::parseDeclListDelayed(IterableDeclContext *IDC) {
  Decl *D = const_cast<Decl*>(IDC->getDecl());
  DeclContext *DC = cast<DeclContext>(D);
  SourceRange BodyRange;
  if (auto ext = dyn_cast<ExtensionDecl>(IDC)) {
    BodyRange = ext->getBraces();
  } else {
    auto *ntd = cast<NominalTypeDecl>(IDC);
    BodyRange = ntd->getBraces();
  }

  if (BodyRange.isInvalid()) {
    assert(D->isImplicit());
    return {std::vector<Decl *>(), std::nullopt};
  }

  auto BeginParserPosition = getParserPosition(BodyRange.Start,
                                               /*previousLoc*/ SourceLoc());
  auto EndLexerState = L->getStateForEndOfTokenLoc(BodyRange.End);

  // ParserPositionRAII needs a primed parser to restore to.
  if (Tok.is(tok::NUM_TOKENS))
    consumeTokenWithoutFeedingReceiver();

  // Ensure that we restore the parser state at exit.
  ParserPositionRAII PPR(*this);

  // Create a lexer that cannot go past the end state.
  Lexer LocalLex(*L, BeginParserPosition.LS, EndLexerState);

  // Temporarily swap out the parser's current lexer with our new one.
  llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

  // Rewind to the start of the member list, which is a '{' in well-formed
  // code.
  restoreParserPosition(BeginParserPosition);

  // If there is no left brace, then return an empty list of declarations;
  // we will have already diagnosed this.
  if (!Tok.is(tok::l_brace))
    return {std::vector<Decl *>(), std::nullopt};

  ContextChange CC(*this, DC);
  SourceLoc LBLoc = consumeToken(tok::l_brace);
  (void)LBLoc;
  assert(LBLoc == BodyRange.Start);
  SourceLoc RBLoc;
  Diag<> Id;
  switch (D->getKind()) {
  case DeclKind::Extension: Id = diag::expected_rbrace_extension; break;
  case DeclKind::Enum: Id = diag::expected_rbrace_enum; break;
  case DeclKind::Protocol: Id = diag::expected_rbrace_protocol; break;
  case DeclKind::Class: Id = diag::expected_rbrace_class; break;
  case DeclKind::Struct: Id = diag::expected_rbrace_struct; break;
  default:
    llvm_unreachable("Bad iterable decl context kinds.");
  }
  bool hadError = false;
  return parseDeclList(LBLoc, RBLoc, Id, hadError);
}

/// Parse an 'import' declaration, doing no token skipping on error.
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

  if (!CodeCompletionCallbacks && !DCC.movedToTopLevel() &&
      !(Flags & PD_AllowTopLevel)) {
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

  ImportPath::Builder importPath;
  bool HasNext;
  do {
    if (Tok.is(tok::code_complete)) {
      consumeToken();
      if (CodeCompletionCallbacks) {
        CodeCompletionCallbacks->completeImportDecl(importPath);
      }
      return makeParserCodeCompletionStatus();
    }
    importPath.push_back(Identifier(), Tok.getLoc());
    if (parseAnyIdentifier(importPath.back().Item,
                           /*diagnoseDollarPrefix=*/false,
                           diag::expected_identifier_in_decl, "import"))
      return nullptr;
    if (Tok.is(tok::oper_postfix)) {
        diagnose(Tok, diag::unexpected_operator_in_import_path)
          .fixItRemove(Tok.getLoc());
        return nullptr;
    }
    HasNext = consumeIf(tok::period);
  } while (HasNext);

  if (Tok.is(tok::code_complete)) {
    // We omit the code completion token if it immediately follows the module
    // identifiers.
    auto BufferId = SourceMgr.getIDEInspectionTargetBufferID();
    auto IdEndOffset = SourceMgr.getLocOffsetInBuffer(importPath.back().Loc,
      BufferId) + importPath.back().Item.str().size();
    auto CCTokenOffset = SourceMgr.getLocOffsetInBuffer(SourceMgr.
      getIDEInspectionTargetLoc(), BufferId);
    if (IdEndOffset == CCTokenOffset) {
      consumeToken();
    }
  }

  if (Kind != ImportKind::Module && importPath.size() == 1) {
    diagnose(importPath.front().Loc, diag::decl_expected_module_name);
    return nullptr;
  }

  // Look up if the imported module is being aliased via -module-alias,
  // and check that the module alias appeared in source files instead of
  // its corresponding real name
  auto parsedModuleID = importPath.get().front().Item;
  if (Context.getRealModuleName(parsedModuleID, ASTContext::ModuleAliasLookupOption::realNameFromAlias).empty()) {
    // If reached here, it means the parsed module name is a real module name
    // which appeared in the source file; only a module alias should be allowed
    auto aliasName = Context.getRealModuleName(parsedModuleID, ASTContext::ModuleAliasLookupOption::aliasFromRealName);
    diagnose(importPath.front().Loc, diag::expected_module_alias,
                     parsedModuleID, aliasName)
      .fixItReplace(importPath.front().Loc, aliasName.str());
    return nullptr;
  }

  auto *ID = ImportDecl::create(Context, CurDeclContext, ImportLoc, Kind,
                                KindLoc, importPath.get());
  ID->attachParsedAttrs(Attributes);
  return DCC.fixupParserResult(ID);
}

/// Parse an `using` declaration.
///
/// \verbatim
///   decl-using:
///     'using' (@<attribute> | <modifier>)
/// \endverbatim
ParserResult<UsingDecl> Parser::parseDeclUsing(ParseDeclOptions Flags,
                                               DeclAttributes &Attributes) {
  assert(Tok.isContextualKeyword("using"));
  DebuggerContextChange DCC(*this);

  if (!Context.LangOpts.hasFeature(Feature::DefaultIsolationPerFile)) {
    diagnose(Tok, diag::experimental_using_decl_disabled);
  }

  SourceLoc UsingLoc = consumeToken();

  if (Tok.is(tok::code_complete)) {
    if (CodeCompletionCallbacks) {
      CodeCompletionCallbacks->completeUsingDecl();
    }
    return makeParserCodeCompletionStatus();
  }

  SourceLoc AtLoc;
  // @<<attribute>>
  if (Tok.is(tok::at_sign))
    AtLoc = consumeToken();

  SourceLoc SpecifierLoc;
  Identifier RawSpecifier;

  if (parseIdentifier(RawSpecifier, SpecifierLoc,
                      /*diagnoseDollarPrefix=*/false,
                      diag::expected_identifier_in_decl, "using"))
    return nullptr;

  std::optional<UsingSpecifier> Specifier =
      llvm::StringSwitch<std::optional<UsingSpecifier>>(RawSpecifier.str())
          .Case("MainActor", UsingSpecifier::MainActor)
          .Case("nonisolated", UsingSpecifier::Nonisolated)
          .Default(std::nullopt);

  if (!Specifier) {
    diagnose(SpecifierLoc, diag::using_decl_invalid_specifier);
    return nullptr;
  }

  // Complain the `using` not being at top-level only after the specifier
  // has been consumed, otherwise the specifier is going to be interpreted
  // as a start of another declaration.
  if (!CodeCompletionCallbacks && !DCC.movedToTopLevel() &&
      !(Flags & PD_AllowTopLevel)) {
    diagnose(UsingLoc, diag::decl_inner_scope);
    return nullptr;
  }

  auto *UD = UsingDecl::create(Context, UsingLoc, AtLoc ? AtLoc : SpecifierLoc,
                               *Specifier, CurDeclContext);
  return DCC.fixupParserResult(UD);
}

/// Parse an inheritance clause.
///
/// \verbatim
///   inheritance:
///      ':' inherited (',' inherited)*
///
///   inherited:
///     'class'
///     type-identifier
///     '~' 'Copyable'
/// \endverbatim
ParserStatus Parser::parseInheritance(
    SmallVectorImpl<InheritedEntry> &Inherited,
    bool allowClassRequirement,
    bool allowAnyObject) {
  consumeToken(tok::colon);

  SourceLoc classRequirementLoc;

  ParserStatus Status;
  SourceLoc prevComma;
  bool HasComma;
  bool IsEndOfList;
  do {
    SWIFT_DEFER {
      // Check for a ',', which indicates that there are more protocols coming.
      HasComma = consumeIf(tok::comma, prevComma);
      IsEndOfList = (Context.LangOpts.hasFeature(Feature::TrailingComma) &&
                     (Tok.is(tok::l_brace) || Tok.is(tok::kw_where)));
    };
    // Parse the 'class' keyword for a class requirement.
    if (Tok.is(tok::kw_class)) {
      // If we aren't allowed to have a class requirement here, complain.
      auto classLoc = consumeToken();
      if (!allowClassRequirement) {
        diagnose(classLoc, diag::unexpected_class_constraint);

        // Note that it makes no sense to suggest fixing
        // 'struct S : class' to 'struct S : AnyObject' for
        // example; in that case we just complain about
        // 'class' being invalid here.
        if (allowAnyObject) {
          diagnose(classLoc, diag::suggest_anyobject)
            .fixItReplace(classLoc, "AnyObject");
        }
        continue;
      }

      // If we already saw a class requirement, complain.
      if (classRequirementLoc.isValid()) {
        diagnose(classLoc, diag::redundant_class_requirement)
          .highlight(classRequirementLoc)
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
      classRequirementLoc = classLoc;

      // Add 'AnyObject' to the inherited list.
      Inherited.push_back(InheritedEntry(UnqualifiedIdentTypeRepr::create(
          Context, DeclNameLoc(classLoc),
          DeclNameRef(Context.getIdentifier("AnyObject")))));
      continue;
    }

    auto ParsedTypeResult =
        parseType(diag::expected_type, ParseTypeReason::InheritanceClause);
    Status |= ParsedTypeResult;

    // Record the type if its a single type.
    if (ParsedTypeResult.isNonNull())
      Inherited.push_back(InheritedEntry(ParsedTypeResult.get()));
  } while (HasComma && !IsEndOfList);

  return Status;
}

static ParserStatus
parseIdentifierDeclName(Parser &P, Identifier &Result, SourceLoc &Loc,
                        StringRef DeclKindName,
                        llvm::function_ref<bool(const Token &)> canRecover) {
  if (P.Tok.is(tok::identifier)) {
    Loc = P.consumeIdentifier(Result, /*diagnoseDollarPrefix=*/true);

    // We parsed an identifier for the declaration. If we see another
    // identifier, it might've been a single identifier that got broken by a
    // space or newline accidentally.
    if (P.Tok.isIdentifierOrUnderscore() && !P.Tok.isContextualDeclKeyword())
      P.diagnoseConsecutiveIDs(Result.str(), Loc, DeclKindName);

    // Return success anyway
    return makeParserSuccess();
  }

  P.checkForInputIncomplete();

  if (P.Tok.is(tok::integer_literal) || P.Tok.is(tok::floating_literal) ||
      (P.Tok.is(tok::unknown) && isdigit(P.Tok.getText()[0]))) {
    // Using numbers for identifiers is a common error for beginners, so it's
    // worth handling this in a special way.
    P.diagnose(P.Tok, diag::number_cant_start_decl_name, DeclKindName);

    // Pretend this works as an identifier, which shouldn't be observable since
    // actual uses of it will hit random other errors, e.g. `1()` won't be
    // callable.
    Result = P.Context.getIdentifier(P.Tok.getText());
    Loc = P.Tok.getLoc();
    P.consumeToken();

    // We recovered, so this is a success.
    return makeParserSuccess();
  }

  if (P.Tok.isKeyword()) {
    P.diagnose(P.Tok, diag::keyword_cant_be_identifier, P.Tok.getText());
    P.diagnose(P.Tok, diag::backticks_to_escape)
      .fixItReplace(P.Tok.getLoc(), "`" + P.Tok.getText().str() + "`");

    // Recover if the next token is one of the expected tokens.
    if (canRecover(P.peekToken())) {
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

  // If there is expected tokens after the code completion token, just eat the
  // code completion token. We don't need code completion here.
  // E.g.:
  //   'func' <completion> ('('|'<')
  //   'typealias' <completion> ('='|'<')
  // If there's no expected token after the completion, override completion may
  // kicks in. So leave the token here.
  // E.g.
  //   'func' <completion>
  //   'init' <completion>
  if (P.Tok.is(tok::code_complete) && canRecover(P.peekToken())) {
    P.consumeToken(tok::code_complete);
    return makeParserCodeCompletionStatus();
  }

  P.diagnose(P.Tok, diag::expected_identifier_in_decl, DeclKindName);
  return makeParserError();
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
ParserStatus Parser::parseDeclItem(bool &PreviousHadSemi,
                                   llvm::function_ref<void(Decl *)> handler) {
  if (Tok.is(tok::semi)) {
    // Consume ';' without preceding decl.
    diagnose(Tok, diag::unexpected_separator, ";")
      .fixItRemove(Tok.getLoc());
    consumeToken();
    // Return success because we already recovered.
    return makeParserSuccess();
  }

  // If the previous declaration didn't have a semicolon and this new
  // declaration doesn't start a line, complain.
  const bool IsAtStartOfLineOrPreviousHadSemi =
      PreviousHadSemi || Tok.isAtStartOfLine() || Tok.is(tok::unknown);
  if (!IsAtStartOfLineOrPreviousHadSemi) {
    auto endOfPrevious = getEndOfPreviousLoc();
    diagnose(endOfPrevious, diag::declaration_same_line_without_semi)
      .fixItInsert(endOfPrevious, ";");
  }

  if (Tok.isAny(tok::pound_sourceLocation, tok::pound_line)) {
    auto LineDirectiveStatus = parseLineDirective(Tok.is(tok::pound_line));
    if (LineDirectiveStatus.isErrorOrHasCompletion())
      skipUntilDeclRBrace(tok::semi, tok::pound_endif);
    return LineDirectiveStatus;
  }

  Decl *LastResultDecl = nullptr;
  ParserStatus Result =
      parseDecl(IsAtStartOfLineOrPreviousHadSemi,
                /* IfConfigsAreDeclAttrs=*/false, [&](Decl *decl) {
        handler(decl);
        LastResultDecl = decl;
      });
  if (Result.isErrorOrHasCompletion())
    skipUntilDeclRBrace(tok::semi, tok::pound_endif);
  SourceLoc SemiLoc;
  PreviousHadSemi = consumeIf(tok::semi, SemiLoc);
  if (PreviousHadSemi && LastResultDecl)
    LastResultDecl->TrailingSemiLoc = SemiLoc;
  return Result;
}

bool Parser::parseMemberDeclList(SourceLoc &LBLoc, SourceLoc &RBLoc,
                                 Diag<> LBraceDiag, Diag<> RBraceDiag,
                                 IterableDeclContext *IDC,
                                 ParseDeclOptions Flags) {
  if (Flags.contains(PD_StubOnly) && Tok.isNot(tok::l_brace)) {
    // Hey, look, it really is a stub!
    LBLoc = RBLoc = SourceLoc();

    // Cache the empty result to prevent delayed parsing.
    Context.evaluator.cacheOutput(ParseMembersRequest{IDC},
                                  FingerprintAndMembers{std::nullopt, {}});
    return false;
  }

  if (parseToken(tok::l_brace, LBLoc, LBraceDiag)) {
    LBLoc = RBLoc = PreviousLoc;

    // Cache the empty result to prevent delayed parsing.
    Context.evaluator.cacheOutput(ParseMembersRequest{IDC},
                                  FingerprintAndMembers{std::nullopt, {}});
    return true;
  }

  // Record '{' '}' to the current hash, nothing else.
  recordTokenHash("}");
  llvm::SaveAndRestore<std::optional<StableHasher>> T(CurrentTokenHash,
                                                      std::nullopt);

  bool HasOperatorDeclarations = false;
  bool HasNestedClassDeclarations = false;
  bool HasDerivativeDeclarations = false;

  if (canDelayMemberDeclParsing(HasOperatorDeclarations,
                                HasNestedClassDeclarations,
                                HasDerivativeDeclarations,
                                Flags)) {
    if (HasOperatorDeclarations)
      IDC->setMaybeHasOperatorDeclarations();
    if (HasNestedClassDeclarations)
      IDC->setMaybeHasNestedClassDeclarations();
    if (HasDerivativeDeclarations)
      IDC->setMaybeHasDerivativeDeclarations();
    if (InFreestandingMacroArgument)
      IDC->setInFreestandingMacroArgument();

    if (delayParsingDeclList(LBLoc, RBLoc, IDC))
      return true;
  } else {
    // When forced to eagerly parse, do so and cache the results in the
    // evaluator.
    bool hadError = false;
    auto membersAndHash = parseDeclList(LBLoc, RBLoc, RBraceDiag, hadError);
    IDC->setMaybeHasOperatorDeclarations();
    IDC->setMaybeHasNestedClassDeclarations();
    IDC->setMaybeHasDerivativeDeclarations();

    if (Flags.contains(PD_StubOnly)) {
      diagnose(LBLoc, diag::stub_decl_cannot_have_body,
               IDC->getDecl()->getDescriptiveKind())
        .fixItRemove({LBLoc, RBLoc});

      // Cache the empty result to prevent delayed parsing.
      Context.evaluator.cacheOutput(ParseMembersRequest{IDC},
                                    FingerprintAndMembers{std::nullopt, {}});
      
      return true;
    }

    Context.evaluator.cacheOutput(
        ParseMembersRequest{IDC},
        FingerprintAndMembers{
            membersAndHash.second,
            Context.AllocateCopy(llvm::ArrayRef(membersAndHash.first))});

    if (hadError)
      return true;
  }

  return false;
}

/// Parse the members in a struct/class/enum/protocol/extension.
///
/// \verbatim
///    decl* '}'
/// \endverbatim
std::pair<std::vector<Decl *>, std::optional<Fingerprint>>
Parser::parseDeclList(SourceLoc LBLoc, SourceLoc &RBLoc, Diag<> ErrorDiag,
                      bool &hadError) {

  // Hash the type body separately.
  llvm::SaveAndRestore<std::optional<StableHasher>> MemberHashingScope{
      CurrentTokenHash, StableHasher::defaultHasher()};

  // Record '{' which has been consumed in callers.
  recordTokenHash("{");

  std::vector<Decl *> decls;
  ParserStatus Status;
  bool PreviousHadSemi = true;
  {
    while (Tok.isNot(tok::r_brace)) {
      Status |=
          parseDeclItem(PreviousHadSemi, [&](Decl *D) { decls.push_back(D); });
      if (Tok.isAny(tok::eof, tok::pound_endif, tok::pound_else,
                    tok::pound_elseif)) {
        IsInputIncomplete = true;
        break;
      }
    }
  }
  parseMatchingToken(tok::r_brace, RBLoc, ErrorDiag, LBLoc);

  // Increase counter.
  if (auto *stat = Context.Stats) {
    ++stat->getFrontendCounters().NumIterableDeclContextParsed;
  }
  // If we found the closing brace, then the caller should not care if there
  // were errors while parsing inner decls, because we recovered.
  if (RBLoc.isInvalid())
    hadError = true;

  // Clone the current hasher and extract a Fingerprint.
  StableHasher currentHash{*CurrentTokenHash};
  return std::make_pair(decls, Fingerprint{std::move(currentHash)});
}

bool Parser::canDelayMemberDeclParsing(bool &HasOperatorDeclarations,
                                       bool &HasNestedClassDeclarations,
                                       bool &HasDerivativeDeclarations,
                                       ParseDeclOptions Flags) {
  // If explicitly disabled, respect the flag.
  if (!isDelayedParsingEnabled() || Flags.contains(PD_StubOnly))
    return false;

  // Skip until the matching right curly bracket; if we find a pound directive,
  // we can't lazily parse.
  CancellableBacktrackingScope BackTrack(*this);
  bool HasPoundDirective;
  bool HasPoundSourceLocation;
  bool HasNestedTypeDeclarations;
  bool HasPotentialRegexLiteral;
  skipUntilMatchingRBrace(*this, HasPoundDirective, HasPoundSourceLocation,
                          HasOperatorDeclarations, HasNestedClassDeclarations,
                          HasNestedTypeDeclarations, HasPotentialRegexLiteral,
                          HasDerivativeDeclarations);
  if (!HasPoundDirective && !HasPotentialRegexLiteral) {
    // If we didn't see any pound directive, we must not have seen
    // #sourceLocation either.
    ASSERT(!HasPoundSourceLocation);
    BackTrack.cancelBacktrack();
  }
  return !BackTrack.willBacktrack();
}

bool Parser::delayParsingDeclList(SourceLoc LBLoc, SourceLoc &RBLoc,
                                  IterableDeclContext *IDC) {
  bool error = false;

  if (Tok.is(tok::r_brace)) {
    RBLoc = consumeToken();
  } else {
    // Non-delayed parsing would set the RB location to the LB if it is missing,
    // match that behaviour here
    RBLoc = LBLoc;
    error = true;
  }
  return error;
}

/// Parse an 'extension' declaration.
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
  SmallVector<InheritedEntry, 2> Inherited;
  if (Tok.is(tok::colon))
    status |= parseInheritance(Inherited,
                               /*allowClassRequirement=*/false,
                               /*allowAnyObject=*/false);

  // Parse the optional where-clause.
  TrailingWhereClause *trailingWhereClause = nullptr;
  bool trailingWhereHadCodeCompletion = false;
  if (Tok.is(tok::kw_where)) {
    SourceLoc whereLoc, endLoc;
    SmallVector<RequirementRepr, 4> requirements;
    auto whereStatus = parseGenericWhereClause(whereLoc, endLoc, requirements);
    if (whereStatus.hasCodeCompletion()) {
      if (isIDEInspectionFirstPass())
        return whereStatus;
      trailingWhereHadCodeCompletion = true;
    }
    if (!requirements.empty()) {
      trailingWhereClause =
          TrailingWhereClause::create(Context, whereLoc, endLoc, requirements);
    }
    status |= whereStatus;
  }

  ExtensionDecl *ext = ExtensionDecl::create(Context, ExtensionLoc,
                                             extendedType.getPtrOrNull(),
                                             Context.AllocateCopy(Inherited),
                                             CurDeclContext,
                                             trailingWhereClause);
  ext->attachParsedAttrs(Attributes);
  if (trailingWhereHadCodeCompletion && CodeCompletionCallbacks)
    CodeCompletionCallbacks->setParsedDecl(ext);

  SourceLoc LBLoc, RBLoc;

  {
    ContextChange CC(*this, ext);

    if (parseMemberDeclList(LBLoc, RBLoc,
                            diag::expected_lbrace_extension,
                            diag::expected_rbrace_extension,
                            ext, Flags))
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

ParserStatus Parser::parseDeclPoundDiagnostic() {
  bool isError = Tok.is(tok::pound_error);
  consumeToken(isError ? tok::pound_error : tok::pound_warning);

  SourceLoc lParenLoc = Tok.getLoc();
  bool hadLParen = consumeIf(tok::l_paren);

  if (!Tok.is(tok::string_literal)) {
    // Catch #warning(oops, forgot the quotes)
    SourceLoc wordsStartLoc = Tok.getLoc();

    skipUntilTokenOrEndOfLine(tok::r_paren);

    SourceLoc wordsEndLoc = getEndOfPreviousLoc();

    auto diag = diagnose(wordsStartLoc, 
                          diag::pound_diagnostic_expected_string, isError);
    if (wordsEndLoc != wordsStartLoc) {
      diag.fixItInsert(wordsStartLoc, hadLParen ? "\"" : "(\"")
          .fixItInsert(wordsEndLoc, Tok.is(tok::r_paren) ? "\"" : "\")");
    }

    // Consume the right paren to finish the decl, if it's there.
    consumeIf(tok::r_paren);

    return makeParserError();
  }

  auto string = parseExprStringLiteral();
  if (string.isNull())
    return makeParserError();

  auto messageExpr = string.get();

  bool hadRParen = consumeIf(tok::r_paren);

  if (!Tok.isAtStartOfLine() && Tok.isNot(tok::eof)) {
    diagnose(Tok.getLoc(),
             diag::extra_tokens_pound_diagnostic_directive, isError);
    return makeParserError();
  }

  if (!hadLParen && !hadRParen) {
    // Catch if the user forgot parentheses around the string, e.g.
    // #warning "foo"
    diagnose(lParenLoc, diag::pound_diagnostic_expected_parens, isError)
      .highlight(messageExpr->getSourceRange())
      .fixItInsert(messageExpr->getStartLoc(), "(")
      .fixItInsertAfter(messageExpr->getEndLoc(), ")");
    return makeParserError();
  } else if (hadRParen && !hadLParen) {
    // Catch if the user forgot a left paren before the string, e.g.
    // #warning "foo")
    diagnose(messageExpr->getStartLoc(), diag::pound_diagnostic_expected,
             "(", isError)
      .fixItInsert(messageExpr->getStartLoc(), "(");
    return makeParserError();
  } else if (hadLParen && !hadRParen) {
    // Catch if the user forgot a right paren after the string, e.g.
    // #warning("foo"
    diagnose(messageExpr->getEndLoc(), diag::pound_diagnostic_expected,
             ")", isError)
      .fixItInsertAfter(messageExpr->getEndLoc(), ")");
    return makeParserError();
  }

  if (messageExpr->getKind() == ExprKind::InterpolatedStringLiteral) {
    diagnose(messageExpr->getStartLoc(), diag::pound_diagnostic_interpolation,
             isError)
      .highlight(messageExpr->getSourceRange());
    return makeParserError();
  }

  if (!InInactiveClauseEnvironment) {
    Diags
        .diagnose(messageExpr->getStartLoc(),
                  isError ? diag::pound_error : diag::pound_warning,
                  cast<StringLiteralExpr>(messageExpr)->getValue())
        .highlight(messageExpr->getSourceRange());
  }

  // '#error' and '#warning' don't create an AST node.
  return makeParserSuccess();
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
  std::optional<StringRef> Filename;
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

    {
      if (parseSpecificIdentifier("file", diag::sourceLocation_expected,
                                  "file:") ||
          parseToken(tok::colon, diag::sourceLocation_expected, ":"))
        return makeParserError();

      if (Tok.isNot(tok::string_literal)) {
        diagnose(Tok, diag::expected_line_directive_name);
        return makeParserError();
      }

      Filename =
          getStringLiteralIfNotInterpolated(Loc, "'#sourceLocation'");
      if (!Filename.has_value())
        return makeParserError();
      SourceLoc filenameLoc = consumeToken(tok::string_literal);
      SF.VirtualFilePaths.emplace_back(*Filename, filenameLoc);

      if (parseToken(tok::comma, diag::sourceLocation_expected, ",") ||
          parseSpecificIdentifier("line", diag::sourceLocation_expected,
                                  "line:") ||
          parseToken(tok::colon, diag::sourceLocation_expected, ":"))
        return makeParserError();

      if (Tok.isNot(tok::integer_literal)) {
        diagnose(Tok, diag::expected_line_directive_number);
        return makeParserError();
      }
      SmallString<16> buffer;
      auto text = stripUnderscoresIfNeeded(Tok.getText(), buffer);
      if (text.getAsInteger(0, StartLine)) {
        diagnose(Tok, diag::expected_line_directive_number);
        return makeParserError();
      }
      if (StartLine == 0) {
        diagnose(Tok, diag::line_directive_line_zero);
        return makeParserError();
      }
      consumeToken(tok::integer_literal);
    }

    if (Tok.isNot(tok::r_paren)) {
      diagnose(Tok, diag::sourceLocation_expected, ")");
      return makeParserError();
    }
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
    SmallString<16> buffer;
    auto text = stripUnderscoresIfNeeded(Tok.getText(), buffer);
    if (text.getAsInteger(0, StartLine)) {
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
    
    Filename = getStringLiteralIfNotInterpolated(Loc, "'#line'");
    if (!Filename.has_value())
      return makeParserError();
  }

  const char *LastTokTextEnd = Tok.getText().end();

  // Skip over trailing whitespace and a single \r and/or \n to the start of the
  // next line.
  while (*LastTokTextEnd == ' ' || *LastTokTextEnd == '\t')
    ++LastTokTextEnd;
  bool hadCROrLF = false;
  if (*LastTokTextEnd == '\r') {
    hadCROrLF = true;
    ++LastTokTextEnd;
  }
  if (*LastTokTextEnd == '\n') {
    hadCROrLF = true;
    ++LastTokTextEnd;
  }
  if (!hadCROrLF) {
    diagnose(Tok.getLoc(), diag::extra_tokens_line_directive);
    return makeParserError();
  }
  SourceLoc nextLineStartLoc = Lexer::getSourceLoc(LastTokTextEnd);

  int LineOffset =
      StartLine - SourceMgr.getLineAndColumnInBuffer(nextLineStartLoc).first;

  // Create a new virtual file for the region started by the #line marker.
  bool isNewFile = SourceMgr.openVirtualFile(nextLineStartLoc,
                                             Filename.value(), LineOffset);
  assert(isNewFile);(void)isNewFile;

  // Lexing of next token must be deferred until after virtual file setup.
  consumeToken(isLine ? tok::string_literal : tok::r_paren);

  InPoundLineEnvironment = true;
  return makeParserSuccess();
}

/// Parse a typealias decl.
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

  Status |= parseIdentifierDeclName(
      *this, Id, IdLoc, "typealias",
      [](const Token &next) { return next.isAny(tok::colon, tok::equal); });
  if (Status.isErrorOrHasCompletion()) {
    return Status;
  }
    
  DebuggerContextChange DCC(*this, Id, DeclKind::TypeAlias);

  // Parse a generic parameter list if it is present.
  GenericParamList *genericParams = nullptr;
  if (startsWithLess(Tok)) {
    auto Result = parseGenericParameters();
    if (Result.hasCodeCompletion() && !CodeCompletionCallbacks)
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

  auto *TAD = new (Context) TypeAliasDecl(TypeAliasLoc, EqualLoc, Id, IdLoc,
                                          genericParams, CurDeclContext);
  ParserResult<TypeRepr> UnderlyingTy;

  if (Tok.is(tok::colon) || Tok.is(tok::equal)) {
    ContextChange CC(*this, TAD);

    if (Tok.is(tok::colon)) {
      // It is a common mistake to write "typealias A : Int" instead of = Int.
      // Recognize this and produce a fixit.
      diagnose(Tok, diag::expected_equal_in_typealias)
          .fixItReplace(Tok.getLoc(), " = ");
      EqualLoc = consumeToken(tok::colon);
    } else {
      EqualLoc = consumeToken(tok::equal);
    }

    UnderlyingTy = parseType(diag::expected_type_in_typealias);
    TAD->setTypeEndLoc(PreviousLoc);
    Status |= UnderlyingTy;
  }

  TAD->setUnderlyingTypeRepr(UnderlyingTy.getPtrOrNull());
  TAD->attachParsedAttrs(Attributes);

  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    ContextChange CC(*this, TAD);
    Status |= parseFreestandingGenericWhereClause(TAD);
  }

  if (UnderlyingTy.isNull()) {
    // If there is an attempt to do code completion
    // inside of typealias type, let's just return
    // because we've seen required '=' token.
    if (EqualLoc.isInvalid()) {
      diagnose(Tok, diag::expected_equal_in_typealias);
      Status.setIsParseError();
      return Status;
    }
  }

  return DCC.fixupParserResult(Status, TAD);
}

/// Parse an associatedtype decl.
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

  // Reject variadic associated types with a specific error.
  if (Tok.isContextualKeyword("each")) {
    const auto EachLoc = consumeToken();
    diagnose(EachLoc, diag::associatedtype_cannot_be_variadic)
        .fixItRemoveChars(EachLoc, Tok.getLoc());
  }

  Status = parseIdentifierDeclName(
      *this, Id, IdLoc, "associatedtype",
      [](const Token &next) { return next.isAny(tok::colon, tok::equal); });
  if (Status.isErrorOrHasCompletion())
    return Status;

  DebuggerContextChange DCC(*this, Id, DeclKind::AssociatedType);
  
  // Reject generic parameters with a specific error.
  if (startsWithLess(Tok)) {
    if (auto genericParams = parseGenericParameters().getPtrOrNull()) {
      diagnose(genericParams->getLAngleLoc(),
               diag::associated_type_generic_parameter_list)
      .fixItRemove(genericParams->getSourceRange());
    }
  }

  // Reject (early syntax) variadic associated types with a specific error.
  if (startsWithEllipsis(Tok)) {
    const auto EllipsisLoc = consumeStartingEllipsis();
    const auto EllipsisEnd = Lexer::getLocForEndOfToken(SourceMgr, EllipsisLoc);
    diagnose(EllipsisLoc, diag::associatedtype_cannot_be_variadic)
        .fixItRemoveChars(EllipsisLoc, EllipsisEnd);
  }

  // Parse optional inheritance clause.
  // FIXME: Allow class requirements here.
  SmallVector<InheritedEntry, 2> Inherited;
  if (Tok.is(tok::colon))
    Status |= parseInheritance(Inherited,
                               /*allowClassRequirement=*/false,
                               /*allowAnyObject=*/true);
  
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
    auto whereStatus = parseProtocolOrAssociatedTypeWhereClause(
        TrailingWhere, /*isProtocol=*/false);
    Status |= whereStatus;
    if (whereStatus.hasCodeCompletion() && !CodeCompletionCallbacks) {
      // Trigger delayed parsing, no need to continue.
      return whereStatus;
    }
  }

  if (!Flags.contains(PD_InProtocol)) {
    diagnose(AssociatedTypeLoc, diag::associatedtype_outside_protocol)
        .fixItReplace(AssociatedTypeLoc, "typealias");
    Status.setIsParseError();
    return Status;
  }

  auto assocType = AssociatedTypeDecl::createParsed(
      Context, CurDeclContext, AssociatedTypeLoc, Id, IdLoc,
      UnderlyingTy.getPtrOrNull(), TrailingWhere);
  assocType->attachParsedAttrs(Attributes);
  if (!Inherited.empty())
    assocType->setInherited(Context.AllocateCopy(Inherited));
  return makeParserResult(Status, assocType);
}

/// Parse a "(value)" specifier for "set" or "willSet" if present.  Create a
/// parameter list to represent the spelled argument or return null if none is
/// present.
static ParameterList *parseOptionalAccessorArgument(SourceLoc SpecifierLoc,
                                                    Parser &P,
                                                    AccessorKind Kind) {
  // 'set' and 'willSet' have a (value) parameter, 'didSet' takes an (oldValue)
  // parameter and 'get' and always takes a () parameter.
  if (Kind != AccessorKind::Set && Kind != AccessorKind::WillSet &&
      Kind != AccessorKind::DidSet && Kind != AccessorKind::Init)
    return nullptr;

  SourceLoc StartLoc, NameLoc, EndLoc;
  Identifier Name;

  // If the SpecifierLoc is invalid, then the caller just wants us to synthesize
  // the default, not actually try to parse something.
  if (SpecifierLoc.isValid() && P.Tok.is(tok::l_paren)) {
    StartLoc = P.consumeToken(tok::l_paren);
    if (P.Tok.isNot(tok::identifier)) {
      P.diagnose(P.Tok, diag::expected_accessor_parameter_name,
                 Kind == AccessorKind::Set ? 0 :
                 Kind == AccessorKind::WillSet ? 1 : 2);
      P.skipUntil(tok::r_paren, tok::l_brace);
      if (P.Tok.is(tok::r_paren))
        EndLoc = P.consumeToken();
      else
        EndLoc = StartLoc;
    } else {
      // We have a name.
      NameLoc = P.consumeIdentifier(Name, /*diagnoseDollarPrefix=*/true);

      auto DiagID =
         Kind == AccessorKind::Set ? diag::expected_rparen_set_name :
         Kind == AccessorKind::WillSet ? diag::expected_rparen_willSet_name :
          diag::expected_rparen_didSet_name;
      
      // Look for the closing ')'.
      P.parseMatchingToken(tok::r_paren, EndLoc, DiagID, StartLoc);
    }
  }
  if (Name.empty())
    return nullptr;

  auto *param = new (P.Context) ParamDecl(
      SourceLoc(), SourceLoc(), Identifier(), NameLoc, Name, P.CurDeclContext);

  return ParameterList::create(P.Context, StartLoc, param, EndLoc);
}

bool Parser::canDelayFunctionBodyParsing(bool &HasNestedTypeDeclarations,
                                         ParseDeclOptions Flags) {
  // If explicitly disabled, respect the flag.
  if (!isDelayedParsingEnabled() && !isIDEInspectionFirstPass() &&
        !Flags.contains(PD_StubOnly))
    return false;

  // Skip until the matching right curly bracket; If it has a potential regex
  // literal, we can't skip. If there's a `#sourceLocation`, we also can't skip
  // since we rely on setting and restoring state in the parser. Other cases we
  // can handle.
  CancellableBacktrackingScope BackTrack(*this);
  consumeToken(tok::l_brace);
  bool HasPoundDirectives;
  bool HasPoundSourceLocation;
  bool HasOperatorDeclarations;
  bool HasNestedClassDeclarations;
  bool HasPotentialRegexLiteral;
  bool HasDerivativeDeclarations;
  skipUntilMatchingRBrace(*this, HasPoundDirectives, HasPoundSourceLocation,
                          HasOperatorDeclarations, HasNestedClassDeclarations,
                          HasNestedTypeDeclarations, HasPotentialRegexLiteral,
                          HasDerivativeDeclarations);
  if (HasPoundSourceLocation || HasPotentialRegexLiteral)
    return false;

  BackTrack.cancelBacktrack();
  consumeIf(tok::r_brace);
  return true;
}

void Parser::skipSILUntilSwiftDecl() {
  // Tell the lexer we're about to start lexing SIL.
  Lexer::SILBodyRAII sbr(*L);

  while (!Tok.is(tok::eof) &&
         !isStartOfSwiftDecl(/*allowPoundIfAttributes=*/false)) {
    // SIL pound dotted paths need to be skipped specially as they can contain
    // decl keywords like 'subscript'.
    if (consumeIf(tok::pound)) {
      do {
        consumeToken();
      } while (consumeIf(tok::period));
      continue;
    }

    // SIL types need to be skipped specially as they can contain attributes on
    // tuples which can look like decl attributes.
    if (consumeIf(tok::sil_dollar)) {
      if (Tok.isAnyOperator() && Tok.getText().starts_with("*")) {
        consumeStartingCharacterOfCurrentToken();
      }
      (void)parseType();
      continue;
    }
    skipSingle();
  }
}

void Parser::skipAnyAttribute() {
  consumeToken(tok::at_sign);
  if (!Tok.is(tok::identifier))
    return;

  (void)canParseCustomAttribute();
}

static void diagnoseRedundantAccessors(Parser &P, SourceLoc loc,
                                       AccessorKind accessorKind,
                                       bool isSubscript,
                                       AccessorDecl *previous) {
  assert(accessorKind == previous->getAccessorKind());

  P.diagnose(loc, diag::duplicate_accessor,
             unsigned(isSubscript),
             getAccessorNameForDiagnostic(previous, /*article*/ true));
  P.diagnose(previous->getLoc(), diag::previous_accessor,
             getAccessorNameForDiagnostic(previous, /*article*/ false),
             /*already*/ true);
}

static bool isAllowedWhenParsingLimitedSyntax(AccessorKind kind, bool forSIL) {
  switch (kind) {
  case AccessorKind::Get:
  case AccessorKind::DistributedGet:
  case AccessorKind::Set:
  case AccessorKind::Read2:
    return true;

  case AccessorKind::Address:
  case AccessorKind::MutableAddress:
  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::Read:
  case AccessorKind::Modify:
  case AccessorKind::Modify2:
    return false;

  case AccessorKind::Init:
    return forSIL;
  }
  llvm_unreachable("bad accessor kind");
}

struct Parser::ParsedAccessors {
  SourceLoc LBLoc, RBLoc;
  SmallVector<AccessorDecl*, 16> Accessors;

#define ACCESSOR(ID) AccessorDecl *ID = nullptr;
#include "swift/AST/AccessorKinds.def"

  void record(Parser &P, AbstractStorageDecl *storage, bool invalid);
  void classify(Parser &P, AbstractStorageDecl *storage, bool invalid);

  /// Add an accessor.  If there's an existing accessor of this kind,
  /// return it.  The new accessor is still remembered but will be
  /// ignored.
  AccessorDecl *add(AccessorDecl *accessor);

  /// applies the function to each accessor that is not a 'get'
  void forEachNonGet(llvm::function_ref<void(AccessorDecl *)> func) {
    for (auto accessor : Accessors)
      if (!accessor->isGetter())
        func(accessor);
  }

  /// Find the first accessor that can be used to perform mutation.
  AccessorDecl *findFirstMutator() const {
    if (Init) return Init;
    if (Set) return Set;
    if (Modify) return Modify;
    if (Modify2) return Modify2;
    if (MutableAddress) return MutableAddress;
    return nullptr;
  }
};

static ParserStatus parseAccessorIntroducer(Parser &P,
                                            DeclAttributes &Attributes,
                                            AccessorKind &Kind, SourceLoc &Loc,
                                            bool isFirstAccessor,
                                            bool *featureUnavailable) {
  ParserStatus Status;
  assert(Attributes.isEmpty());
  auto AttributeStatus = P.parseDeclAttributeList(Attributes);
  // Only propagate the `hasCodeCompletion` status outwards. If the attributes
  // have an error, we want to just drop them and continue parsing as if they
  // weren't present.
  if (AttributeStatus.hasCodeCompletion()) {
    Status.setHasCodeCompletion();
  }
  if (Status.hasCodeCompletion() && P.CodeCompletionCallbacks) {
    P.CodeCompletionCallbacks->setAttrTargetDeclKind(DeclKind::Accessor);
  }

  // Parse the contextual keywords for 'mutating' and 'nonmutating' before
  // get and set.
  {
    if (P.Tok.isContextualKeyword("mutating")) {
      P.parseNewDeclAttribute(Attributes, /*AtLoc*/ {}, DeclAttrKind::Mutating);
    } else if (P.Tok.isContextualKeyword("nonmutating")) {
      P.parseNewDeclAttribute(Attributes, /*AtLoc*/ {},
                              DeclAttrKind::NonMutating);
    } else if (P.Tok.isContextualKeyword("__consuming")) {
      P.parseNewDeclAttribute(Attributes, /*AtLoc*/ {},
                              DeclAttrKind::LegacyConsuming);
    } else if (P.Tok.isContextualKeyword("consuming")) {
      P.parseNewDeclAttribute(Attributes, /*AtLoc*/ {},
                              DeclAttrKind::Consuming);
    } else if (P.Tok.isContextualKeyword("borrowing")) {
      P.parseNewDeclAttribute(Attributes, /*AtLoc*/ {},
                              DeclAttrKind::Borrowing);
    }
  }

  if (!(P.Tok.is(tok::identifier) || P.Tok.is(tok::kw_init)) ||
      P.Tok.isEscapedIdentifier()) {
    Status.setIsParseError();
    return Status;
  }
#define ACCESSOR_KEYWORD(KEYWORD)
// Existing source code may have an implicit getter whose first expression is a
// call to a function named experimental_accessor passing a trailing closure.
// To maintain compatibility, if the feature corresponding to
// experimental_accessor is not enabled, do not parse the token as an
// introducer, so that it can be parsed as such a function call.
#define EXPERIMENTAL_COROUTINE_ACCESSOR(ID, KEYWORD, FEATURE)                  \
  else if (P.Tok.getRawText() == #KEYWORD) {                                   \
    if (!P.Context.LangOpts.hasFeature(Feature::FEATURE)) {                    \
      if (featureUnavailable)                                                  \
        *featureUnavailable = true;                                            \
      if (isFirstAccessor) {                                                   \
        Status.setIsParseError();                                              \
        return Status;                                                         \
      }                                                                        \
    }                                                                          \
    Kind = AccessorKind::ID;                                                   \
  }
#define SINGLETON_ACCESSOR(ID, KEYWORD)                                        \
  else if (P.Tok.getRawText() == #KEYWORD) {                                   \
    Kind = AccessorKind::ID;                                                   \
  }
#include "swift/AST/AccessorKinds.def"
  else {
    Status.setIsParseError();
    return Status;
  }
  P.Tok.setKind(tok::contextual_keyword);
  Loc = P.consumeToken();
  return Status;
}

/// Parsing for effects specifiers. We expect the token cursor to be
/// at the point marked by the ^ below:
///
/// \verbatim
///    getter-clause  ::= ... "get" getter-effects? code-block
///                                 ^
///    getter-effects ::= "throws"
///    getter-effects ::= "async" "throws"?
/// \endverbatim
///
/// While only 'get' accessors currently support such specifiers,
/// this routine will also diagnose unsupported effects specifiers on
/// other accessors.
///
/// \param accessors the accessors we've parsed already.
/// \param asyncLoc is mutated with the location of 'async' if found.
/// \param throwsLoc is mutated with the location of 'throws' if found.
/// \param hasEffectfulGet maintains the state of parsing multiple accessors
///                        to track whether we've already seen an effectful
///                        'get' accessor. It is mutated by this function.
/// \param currentKind is the kind of accessor we're parsing.
/// \param currentLoc is the location of the current accessor we're parsing.
/// \return the status of the parser after trying to parse these specifiers.
ParserStatus Parser::parseGetEffectSpecifier(ParsedAccessors &accessors,
                                             SourceLoc &asyncLoc,
                                             SourceLoc &throwsLoc,
                                             TypeRepr *&thrownTy,
                                             bool &hasEffectfulGet,
                                             AccessorKind currentKind,
                                             SourceLoc const& currentLoc) {
  ParserStatus Status;

  if (isEffectsSpecifier(Tok)) {
    if (currentKind == AccessorKind::Get) {
      Status |=
          parseEffectsSpecifiers(/*existingArrowLoc*/ SourceLoc(), asyncLoc,
              /*reasync*/ nullptr, throwsLoc,
              /*rethrows*/ nullptr, thrownTy);

      // If we've previously parsed a non-'get' accessor, raise diagnostics,
      // because we're about to add an effectful 'get' accessor.
      if (!hasEffectfulGet) {
        accessors.forEachNonGet([&](AccessorDecl *nonGetAccessor) {
          diagnose(nonGetAccessor->getLoc(),
                   diag::invalid_accessor_with_effectful_get,
                   accessorKindName(nonGetAccessor->getAccessorKind()));
        });
      }

      hasEffectfulGet = true;
    } else {
      // effects are not valid on this accessor. emit error for each one.
      do {
        diagnose(Tok, diag::invalid_accessor_specifier,
                 accessorKindName(currentKind), Tok.getRawText());
        consumeToken();
      } while (isEffectsSpecifier(Tok));
    }
  }

  // If we're about to set-up a non-'get' accessor when
  // we have an effectful 'get', raise a diagnostic.
  if (hasEffectfulGet && currentKind != AccessorKind::Get) {
    diagnose(currentLoc, diag::invalid_accessor_with_effectful_get,
             accessorKindName(currentKind));
  }

  return Status;
}

bool Parser::parseAccessorAfterIntroducer(
    SourceLoc Loc, AccessorKind Kind, ParsedAccessors &accessors,
    bool &hasEffectfulGet, bool &parsingLimitedSyntax,
    DeclAttributes &Attributes, ParseDeclOptions Flags,
    AbstractStorageDecl *storage, ParserStatus &Status) {
  auto *param = parseOptionalAccessorArgument(Loc, *this, Kind);

  // Next, parse effects specifiers. While it's only valid to have them
  // on 'get' accessors, we also emit diagnostics if they show up on others.
  SourceLoc asyncLoc;
  SourceLoc throwsLoc;
  TypeRepr *thrownTy = nullptr;
  Status |= parseGetEffectSpecifier(accessors, asyncLoc, throwsLoc, thrownTy,
                                    hasEffectfulGet, Kind, Loc);

  // Set up a function declaration.
  auto *accessor = AccessorDecl::createParsed(
      Context, Kind, storage, /*declLoc*/ Loc, /*accessorKeywordLoc*/ Loc,
      param, asyncLoc, throwsLoc, thrownTy, CurDeclContext);
  accessor->attachParsedAttrs(Attributes);

  // Collect this accessor and detect conflicts.
  if (auto existingAccessor = accessors.add(accessor)) {
    diagnoseRedundantAccessors(*this, Loc, Kind, isa<SubscriptDecl>(storage),
                               existingAccessor);
  }

  if (requiresFeatureCoroutineAccessors(Kind) &&
      !Context.LangOpts.hasFeature(Feature::CoroutineAccessors)) {
    diagnose(Tok, diag::accessor_requires_coroutine_accessors,
             getAccessorNameForDiagnostic(Kind, /*article*/ false,
                                          /*underscored*/ false));
  }

  // There should be no body in the limited syntax; diagnose unexpected
  // accessor implementations.
  if (parsingLimitedSyntax) {
    if (Tok.is(tok::l_brace))
      diagnose(Tok, diag::unexpected_getset_implementation_in_protocol,
               getAccessorNameForDiagnostic(Kind, /*article*/ false,
                                            /*underscored*/ false));
    return false;
  }

  // It's okay not to have a body if there's an external asm name.
  if (!Tok.is(tok::l_brace)) {
    // Accessors don't need bodies in module interfaces or @abi attrs
    if (SF.Kind == SourceFileKind::Interface || Flags.contains(PD_StubOnly))
      return false;

    // _silgen_name'd accessors don't need bodies.
    if (!Attributes.hasAttribute<SILGenNameAttr>()) {
      diagnose(Tok, diag::expected_lbrace_accessor,
               getAccessorNameForDiagnostic(accessor, /*article*/ false));
      Status |= makeParserError();
      return true;
    }

    return false;
  }

  parseAbstractFunctionBody(accessor, Flags);
  return false;
}

ParserStatus Parser::parseGetSet(ParseDeclOptions Flags, ParameterList *Indices,
                                 TypeRepr *ResultType,
                                 ParsedAccessors &accessors,
                                 AbstractStorageDecl *storage) {
  assert(Tok.is(tok::l_brace));

  // Properties in protocols use a very limited syntax.
  // SIL mode and module interfaces use the same syntax.
  // Otherwise, we have a normal var or subscript declaration and we need
  // parse the full complement of specifiers, along with their bodies.
  bool parsingLimitedSyntax = Flags.contains(PD_InProtocol) ||
                              SF.Kind == SourceFileKind::SIL;

  auto parseImplicitGetter = [&]() {
    assert(Tok.is(tok::l_brace));
    accessors.LBLoc = Tok.getLoc();
    auto *getter = AccessorDecl::createParsed(
        Context, AccessorKind::Get, storage, /*declLoc*/ Tok.getLoc(),
        /*accessorKeywordLoc*/ SourceLoc(), /*paramList*/ nullptr,
        /*asyncLoc*/ SourceLoc(), /*throwsLoc*/ SourceLoc(),
        /*thrownTy*/ nullptr, CurDeclContext);
    accessors.add(getter);
    parseAbstractFunctionBody(getter, Flags);
    accessors.RBLoc = getter->getEndLoc();
  };

  // If the body is completely empty, preserve it. This is at best a getter with
  // an implicit fallthrough off the end.
  if (peekToken().is(tok::r_brace)) {
    if (parsingLimitedSyntax) {
      // In the limited syntax, fall out and let the caller handle it.
      accessors.LBLoc = consumeToken(tok::l_brace);
      accessors.RBLoc = consumeToken(tok::r_brace);
    } else {
      // Otherwise, treat the empty braces as a valid implicit getter. We need
      // more information to determine whether missing return diagnostics are
      // necessary.
      parseImplicitGetter();
    }

    return makeParserSuccess();
  }

  // Prepare backtracking for implicit getter.
  std::optional<CancellableBacktrackingScope> backtrack;
  backtrack.emplace(*this);

  ParserStatus Status;
  bool accessorHasCodeCompletion = false;
  bool IsFirstAccessor = true;
  bool hasEffectfulGet = false;
  accessors.LBLoc = consumeToken(tok::l_brace);
  while (!Tok.isAny(tok::r_brace, tok::eof)) {
    // Parse introducer if possible.
    DeclAttributes Attributes;
    AccessorKind Kind = AccessorKind::Get;
    SourceLoc Loc;
    bool featureUnavailable = false;
    ParserStatus AccessorStatus = parseAccessorIntroducer(
        *this, Attributes, Kind, Loc, IsFirstAccessor, &featureUnavailable);
    Status |= AccessorStatus;
    if (AccessorStatus.isError() && !AccessorStatus.hasCodeCompletion()) {
      if (Tok.is(tok::code_complete)) {
        // Handle code completion here only if it's not the first accessor.
        // If it's the first accessor, it's handled in function body parsing
        // because it might be an implicit getter.
        if (!IsFirstAccessor || parsingLimitedSyntax) {
          if (CodeCompletionCallbacks) {
            CodeCompletionCallbacks->setParsedDecl(storage);
            CodeCompletionCallbacks->completeAccessorBeginning(nullptr);
          }
          consumeToken(tok::code_complete);
          accessorHasCodeCompletion = true;
          break;
        }
      }

      // parsingLimitedSyntax mode cannot have a body.
      if (parsingLimitedSyntax) {
        auto diag = Context.LangOpts.hasFeature(Feature::CoroutineAccessors)
                        ? diag::expected_getreadset_in_protocol
                        : diag::expected_getset_in_protocol;
        diagnose(Tok, diag);
        Status |= makeParserError();
        break;
      }

      // Cannot have an implicit getter after other accessor.
      if (!IsFirstAccessor) {
        if (!featureUnavailable) {
          diagnose(Tok, diag::expected_accessor_kw);
        }
        skipUntil(tok::r_brace);
        // Don't signal an error since we recovered.
        break;
      }

      // This is an implicit getter. Cancel accessor contexts, backtrack to '{'
      // position.
      backtrack.reset();
      parseImplicitGetter();
      return makeParserSuccess();
    }
    if (IsFirstAccessor) {
      // Continue parsing without backtracking so we can re-use previously
      // parsed nodes for incremental re-parsing, but avoid destructing
      // `backtrack` because its syntax context isn't at the top of the stack at
      // this point.
      backtrack->cancelBacktrack();
      IsFirstAccessor = false;
    }

    // For now, immediately reject illegal accessors in protocols just to
    // avoid having to deal with them everywhere.
    if (parsingLimitedSyntax && !isAllowedWhenParsingLimitedSyntax(
                                    Kind, SF.Kind == SourceFileKind::SIL)) {
      auto diag = Context.LangOpts.hasFeature(Feature::CoroutineAccessors)
                      ? diag::expected_getreadset_in_protocol
                      : diag::expected_getset_in_protocol;
      diagnose(Loc, diag);
      continue;
    }

    // 'set' and 'willSet' can have an optional name.  This isn't valid in a
    // protocol, but we parse and then reject it for better QoI.
    //
    //     set-name    ::= '(' identifier ')'
    if (parsingLimitedSyntax && Tok.is(tok::l_paren)) {
      diagnose(Loc, diag::protocol_setter_name);
    }

    if (parseAccessorAfterIntroducer(Loc, Kind, accessors, hasEffectfulGet,
                                     parsingLimitedSyntax, Attributes, Flags,
                                     storage, Status))
      break;
  }
  backtrack->cancelBacktrack();
  backtrack.reset();
  // Parse the final '}'.
  if (Status.isError())
    skipUntil(tok::r_brace);

  parseMatchingToken(tok::r_brace, accessors.RBLoc,
                     diag::expected_rbrace_in_getset, accessors.LBLoc);
  if (accessorHasCodeCompletion)
    return makeParserCodeCompletionStatus();
  return Status;
}

void Parser::parseTopLevelAccessors(
    AbstractStorageDecl *storage, SmallVectorImpl<ASTNode> &items
) {
  // Prime the lexer.
  if (Tok.is(tok::NUM_TOKENS))
    consumeTokenWithoutFeedingReceiver();

  bool hadLBrace = consumeIf(tok::l_brace);

  // Prepopulate the field for any accessors that were already parsed parsed accessors
  ParsedAccessors accessors;
#define ACCESSOR(ID)                                            \
    if (auto accessor = storage->getAccessor(AccessorKind::ID)) \
      accessors.ID = accessor;
#include "swift/AST/AccessorKinds.def"

  ParserStatus status;
  bool hasEffectfulGet = false;
  bool parsingLimitedSyntax = false;
  bool IsFirstAccessor = true;
  while (!Tok.isAny(tok::r_brace, tok::eof)) {
    DeclAttributes attributes;
    AccessorKind kind = AccessorKind::Get;
    SourceLoc loc;
    ParserStatus accessorStatus =
        parseAccessorIntroducer(*this, attributes, kind, loc, IsFirstAccessor,
                                /*featureUnavailable=*/nullptr);
    if (accessorStatus.isError())
      break;

    (void)parseAccessorAfterIntroducer(loc, kind, accessors, hasEffectfulGet,
                                       parsingLimitedSyntax, attributes,
                                       PD_Default, storage, status);
    if (IsFirstAccessor) {
      IsFirstAccessor = false;
    }
  }

  if (hadLBrace && Tok.is(tok::r_brace)) {
    consumeToken(tok::r_brace);
  }

  // Consume remaining tokens.
  // FIXME: Emit a diagnostic here?
  while (!Tok.is(tok::eof)) {
    consumeToken();
  }

  accessors.record(*this, storage, false);

  // Collect these accessors as top-level decls.
  for (auto accessor : accessors.Accessors)
    items.push_back(accessor);
}

void Parser::parseExpandedAttributeList(SmallVectorImpl<ASTNode> &items,
                                        bool isFromClangAttribute) {
  if (Tok.is(tok::NUM_TOKENS))
    consumeTokenWithoutFeedingReceiver();

  DeclAttributes attributes;
  parseDeclAttributeList(attributes);

  // If this is coming from a Clang attribute, also parse modifiers.
  if (isFromClangAttribute && !Tok.is(tok::eof)) {
    SourceLoc staticLoc;
    StaticSpellingKind staticSpelling;
    parseDeclModifierList(
        attributes, staticLoc, staticSpelling, isFromClangAttribute);
  }

  // Consume remaining tokens.
  while (!Tok.is(tok::eof)) {
    if (!isFromClangAttribute) {
      diagnose(Tok.getLoc(), diag::unexpected_attribute_expansion,
               Tok.getText());
    }
    consumeToken();
  }

  // Create a `MissingDecl` as a placeholder for the declaration the
  // macro will attach the attribute list to.
  MissingDecl *missing =
      MissingDecl::create(Context, CurDeclContext, Tok.getLoc());
  missing->attachParsedAttrs(attributes);

  items.push_back(ASTNode(missing));
  return;
}

void Parser::parseExpandedMemberList(SmallVectorImpl<ASTNode> &items) {
  if (Tok.is(tok::NUM_TOKENS))
    consumeTokenWithoutFeedingReceiver();

  bool previousHadSemi = true;

  while (!Tok.is(tok::eof)) {
    SourceLoc startingLoc = Tok.getLoc();
    parseDeclItem(previousHadSemi, [&](Decl *d) { items.push_back(d); });

    if (Tok.getLoc() == startingLoc)
      break;
  }

  // Consume remaining tokens.
  while (!Tok.is(tok::eof)) {
    diagnose(Tok.getLoc(), diag::unexpected_member_expansion,
             Tok.getText());
    consumeToken();
  }
}

/// Parse the brace-enclosed getter and setter for a variable.
ParserResult<VarDecl>
Parser::parseDeclVarGetSet(PatternBindingEntry &entry, ParseDeclOptions Flags,
                           SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling,
                           SourceLoc VarLoc, bool hasInitializer,
                           const DeclAttributes &Attributes,
                           SmallVectorImpl<Decl *> &Decls) {
  bool Invalid = false;

  auto *pattern = entry.getPattern();

  // The grammar syntactically requires a simple identifier for the variable
  // name. Complain if that isn't what we got. But for recovery purposes,
  // make an effort to look through other things anyway.
  VarDecl *PrimaryVar = nullptr;
  bool primaryVarIsWellFormed = true;
  {
    Pattern *cur = pattern;
    TypedPattern *previousTyped = nullptr;
    while (true) {
      if (auto typed = dyn_cast<TypedPattern>(cur)) {
        if (previousTyped) primaryVarIsWellFormed = false;
        previousTyped = typed;
        cur = typed->getSubPattern();
      } else if (auto paren = dyn_cast<ParenPattern>(cur)) {
        primaryVarIsWellFormed = false;
        cur = paren->getSubPattern();
      } else if (auto var = dyn_cast<BindingPattern>(cur)) {
        primaryVarIsWellFormed = false;
        cur = var->getSubPattern();
      } else {
        break;
      }
    }

    if (auto named = dyn_cast<NamedPattern>(cur)) {
      PrimaryVar = named->getDecl();
    }
  }

  if (!PrimaryVar || !primaryVarIsWellFormed) {
    diagnose(pattern->getLoc(), diag::getset_nontrivial_pattern);
    Invalid = true;
  }

  // Create a fake VarDecl and PBD so that we don't have to weaken the
  // formation rule that an AccessorDecl always has a VarDecl.
  VarDecl *storage = PrimaryVar;
  if (!storage) {
    storage = new (Context) VarDecl(StaticLoc.isValid(),
                                    VarDecl::Introducer::Var,
                                    VarLoc, Identifier(),
                                    CurDeclContext);
    storage->setInvalid();

    pattern =
      TypedPattern::createImplicit(Context, new (Context) NamedPattern(storage),
                                   ErrorType::get(Context));
    entry.setPattern(pattern);
  }

  // Parse getter and setter.
  ParsedAccessors accessors;
  auto typedPattern = dyn_cast<TypedPattern>(pattern);
  auto *resultTypeRepr = typedPattern ? typedPattern->getTypeRepr() : nullptr;
  auto AccessorStatus = parseGetSet(Flags, /*Indices=*/nullptr, resultTypeRepr,
                                    accessors, storage);
  if (AccessorStatus.isError())
    Invalid = true;

  // If we have an invalid case, bail out now.
  if (!PrimaryVar)
    return nullptr;

  if (!typedPattern) {
    if (accessors.Get || accessors.Set || accessors.Address ||
        accessors.MutableAddress) {
      SourceLoc locAfterPattern = pattern->getLoc().getAdvancedLoc(
        pattern->getBoundName().getLength());
      diagnose(pattern->getLoc(), diag::computed_property_missing_type)
        .fixItInsert(locAfterPattern, ": <# Type #>");
      Invalid = true;
    }
  }

  // Reject accessors on 'let's after parsing them (for better recovery).
  if (PrimaryVar->isLet() && !Attributes.hasAttribute<HasStorageAttr>()) {
    Diag<> DiagID;
    if (accessors.WillSet || accessors.DidSet)
      DiagID = diag::let_cannot_be_observing_property;
    else if (accessors.Address || accessors.MutableAddress)
      DiagID = diag::let_cannot_be_addressed_property;
    else
      DiagID = diag::let_cannot_be_computed_property;

    diagnose(accessors.LBLoc, DiagID).fixItReplace(VarLoc, "var");
    Invalid = true;
  }

  // Reject accessors when we're parsing stubs only.
  if (Flags.contains(PD_StubOnly) && !accessors.Accessors.empty()) {
    diagnose(Tok, diag::stub_decl_cannot_have_body,
             PrimaryVar->getDescriptiveKind())
      .fixItRemove({ accessors.LBLoc, accessors.RBLoc });
    Invalid = true;
  }

  accessors.record(*this, PrimaryVar, Invalid);

  return makeParserResult(AccessorStatus, PrimaryVar);
}

/// Add the given accessor to the collection of parsed accessors.  If
/// it's the first accessor of its kind, remember it for that purpose
/// and return null; otherwise, return the existing accessor.
AccessorDecl *Parser::ParsedAccessors::add(AccessorDecl *accessor) {
  Accessors.push_back(accessor);

  switch (accessor->getAccessorKind()) {
#define ACCESSOR(ID)                      \
  case AccessorKind::ID:                  \
    if (ID) {                             \
      return ID;                          \
    } else {                              \
      ID = accessor;                      \
      return nullptr;                     \
    }
#include "swift/AST/AccessorKinds.def"
  }
  llvm_unreachable("bad accessor kind");
}

/// Record a bunch of parsed accessors into the given abstract storage decl.
void Parser::ParsedAccessors::record(Parser &P, AbstractStorageDecl *storage,
                                     bool invalid) {
  classify(P, storage, invalid);
  storage->setAccessors(LBLoc, Accessors, RBLoc);
}

static std::optional<AccessorKind>
getCorrespondingUnderscoredAccessorKind(AccessorKind kind) {
  switch (kind) {
  case AccessorKind::Read2:
    return {AccessorKind::Read};
  case AccessorKind::Modify2:
    return {AccessorKind::Modify};
  case AccessorKind::Get:
  case AccessorKind::DistributedGet:
  case AccessorKind::Set:
  case AccessorKind::Read:
  case AccessorKind::Modify:
  case AccessorKind::WillSet:
  case AccessorKind::DidSet:
  case AccessorKind::Address:
  case AccessorKind::MutableAddress:
  case AccessorKind::Init:
    return std::nullopt;
  }
}

static void diagnoseConflictingAccessors(Parser &P, AccessorDecl *first,
                                         AccessorDecl *&second) {
  if (!second) return;
  bool underscored =
      (getCorrespondingUnderscoredAccessorKind(first->getAccessorKind()) ==
       second->getAccessorKind()) ||
      (getCorrespondingUnderscoredAccessorKind(second->getAccessorKind()) ==
       first->getAccessorKind()) ||
      first->getASTContext().LangOpts.hasFeature(Feature::CoroutineAccessors);
  P.diagnose(
      second->getLoc(), diag::conflicting_accessor,
      isa<SubscriptDecl>(first->getStorage()),
      getAccessorNameForDiagnostic(second, /*article*/ true, underscored),
      getAccessorNameForDiagnostic(first, /*article*/ true, underscored));
  P.diagnose(
      first->getLoc(), diag::previous_accessor,
      getAccessorNameForDiagnostic(first, /*article*/ false, underscored),
      /*already*/ false);
  second->setInvalid();
}

template <class... DiagArgs>
static void diagnoseAndIgnoreObservers(Parser &P,
                                       Parser::ParsedAccessors &accessors,
                                       Diag<unsigned, DiagArgs...> diagnostic,
                        typename std::enable_if<true, DiagArgs>::type... args) {
  if (auto &accessor = accessors.WillSet) {
    P.diagnose(accessor->getLoc(), diagnostic, /*willSet*/ 0, args...);
    accessor->setInvalid();
  }
  if (auto &accessor = accessors.DidSet) {
    P.diagnose(accessor->getLoc(), diagnostic, /*didSet*/ 1, args...);
    accessor->setInvalid();
  }
}

void Parser::ParsedAccessors::classify(Parser &P, AbstractStorageDecl *storage,
                                       bool invalid) {
  // If there was a problem parsing accessors, mark all parsed accessors
  // as invalid to avoid tripping up later invariants.
  // We also want to avoid diagnose missing accessors if something
  // was invalid.
  if (invalid) {
    for (auto accessor : Accessors) {
      accessor->setInvalid();
    }
  }

  // The observing accessors have very specific restrictions.
  // Prefer to ignore them.
  if (WillSet || DidSet) {
    // We don't support the observing accessors on subscripts.
    if (isa<SubscriptDecl>(storage)) {
      diagnoseAndIgnoreObservers(P, *this,
                                 diag::observing_accessor_in_subscript);
    }
  }

  // Okay, observers are out of the way.

  // 'get', '_read', 'read' and a non-mutable addressor are all exclusive.
  if (Get) {
    diagnoseConflictingAccessors(P, Get, Read);
    diagnoseConflictingAccessors(P, Get, Read2);
    diagnoseConflictingAccessors(P, Get, Address);
  } else if (Read) {
    diagnoseConflictingAccessors(P, Read, Read2);
    diagnoseConflictingAccessors(P, Read, Address);
  } else if (Read2) {
    diagnoseConflictingAccessors(P, Read2, Address);
  } else if (Address) {
    // Nothing can go wrong.

  // If there's a writing accessor of any sort, there must also be a
  // reading accessor.
  } else if (auto mutator = findFirstMutator()) {
    if (!invalid) {
      P.diagnose(mutator->getLoc(),
                 // Don't mention the more advanced accessors if the user
                 // only provided a setter without a getter.
                 (MutableAddress || Modify || Modify2)
                     ? diag::missing_reading_accessor
                     : diag::missing_getter,
                 isa<SubscriptDecl>(storage),
                 getAccessorNameForDiagnostic(mutator, /*article*/ true));
    }

  // Subscripts always have to have some sort of accessor; they can't be
  // purely stored.
  } else if (isa<SubscriptDecl>(storage)) {
    if (!invalid) {
      P.diagnose(LBLoc, diag::subscript_without_get);
    }
  }

  // '_modify', 'modify' and 'unsafeMutableAddress' are all mutually exclusive.
  // 'unsafeMutableAddress' and 'set' are mutually exclusive, but 'set' and
  // 'modify' can appear together.
  if (Set) {
    diagnoseConflictingAccessors(P, Set, MutableAddress);
  } else if (Modify) {
    diagnoseConflictingAccessors(P, Modify, Modify2);
    diagnoseConflictingAccessors(P, Modify, MutableAddress);
  }

  if (Init) {
    auto *DC = storage->getDeclContext();
    if (isa<ExtensionDecl>(DC)) {
      P.diagnose(Init->getLoc(),
                 diag::init_accessor_is_not_in_the_primary_declaration);
    } else if (!DC->isTypeContext() || isa<SubscriptDecl>(storage)) {
      P.diagnose(Init->getLoc(), diag::init_accessor_is_not_on_property);
    }
  }
}


/// Parse a 'var', 'let', or 'inout' declaration, doing no token skipping on error.
ParserResult<PatternBindingDecl>
Parser::parseDeclVar(ParseDeclOptions Flags,
                     DeclAttributes &Attributes,
                     SmallVectorImpl<Decl *> &Decls,
                     SourceLoc StaticLoc,
                     StaticSpellingKind StaticSpelling,
                     SourceLoc TryLoc,
                     bool HasBindingKeyword) {
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
        diagnose(Tok, diag::class_var_not_in_class, 
                 Flags.contains(PD_InProtocol))
            .fixItReplace(StaticLoc, "static");
    }
  }

  PatternBindingState newBindingContext = PatternBindingState::NotInBinding;
  if (HasBindingKeyword)
    newBindingContext = PatternBindingState(Tok);
  assert(!HasBindingKeyword || Tok.getKind() == tok::kw_let ||
         Tok.getKind() == tok::kw_var || Tok.getKind() == tok::kw_inout);

  SourceLoc VarLoc = newBindingContext.getIntroducer() ? consumeToken() : Tok.getLoc();

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
  DeclContext *BindingContext = topLevelDecl ? topLevelDecl : CurDeclContext;

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
    auto *PBD = PatternBindingDecl::create(Context, StaticLoc, StaticSpelling,
                                           VarLoc, PBDEntries, BindingContext);

    // If we're setting up a TopLevelCodeDecl, configure it by setting up the
    // body that holds PBD and we're done.  The TopLevelCodeDecl is already set
    // up in Decls to be returned to caller.
    if (topLevelDecl) {
      auto range = PBD->getSourceRangeIncludingAttrs();
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
  bool HasNext;
  bool DisallowInit =
      Flags.contains(PD_DisallowInit) || Flags.contains(PD_StubOnly);
  do {
    Pattern *pattern;
    {
      // In our recursive parse, remember that we're in a var/let pattern.
      llvm::SaveAndRestore<decltype(InBindingPattern)> T(
          InBindingPattern,
          newBindingContext.getPatternBindingStateForIntroducer(VarDecl::Introducer::Var));

      // Track whether we are parsing an 'async let' pattern.
      const auto hasAsyncAttr = Attributes.hasAttribute<AsyncAttr>();
      llvm::SaveAndRestore<bool> AsyncAttr(InPatternWithAsyncAttribute,
                                           hasAsyncAttr);

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
      VD->attachParsedAttrs(Attributes);
      VD->setTopLevelGlobal(topLevelDecl);

      Decls.push_back(VD);
    });

    // Remember this pattern/init pair for our ultimate PatternBindingDecl. The
    // Initializer will be added later when/if it is parsed.
    PatternBindingInitializer *initContext = nullptr;
    PBDEntries.push_back({pattern, /*EqualLoc*/ SourceLoc(), /*Init*/ nullptr,
                          initContext});

    Expr *PatternInit = nullptr;
    
    // Parse an initializer if present.
    if (Tok.is(tok::equal)) {
      // If we have no local context to parse the initial value into, create one
      // for the PBD we'll eventually create.  This allows us to have reasonable
      // DeclContexts for any closures that may live inside of initializers.
      if (!CurDeclContext->isLocalContext() && !topLevelDecl) {
        assert(!initContext && "There cannot be an init context yet");
        initContext = PatternBindingInitializer::create(CurDeclContext);
      }

      // If we're using a local context (either a TopLevelCodeDecl or a
      // PatternBindingContext) install it now so that CurDeclContext is set
      // right when parsing the initializer.
      std::optional<ParseFunctionBody> initParser;
      std::optional<ContextChange> topLevelParser;
      if (topLevelDecl)
        topLevelParser.emplace(*this, topLevelDecl);
      if (initContext)
        initParser.emplace(*this, initContext);
      
      SourceLoc EqualLoc = consumeToken(tok::equal);
      PBDEntries.back().setEqualLoc(EqualLoc);

      ParserResult<Expr> init = parseExpr(diag::expected_init_value);
      PBDEntries.back().setOriginalInit(init.getPtrOrNull());

      // If this Pattern binding was not supposed to have an initializer, but it
      // did, diagnose this and remove it.
      if (DisallowInit && init.isNonNull()) {
        diagnose(EqualLoc, diag::disallowed_init);
        init = nullptr;
      }
      
      // Otherwise, if this pattern binding *was* supposed (or allowed) to have
      // an initializer, but it was a parse error, replace it with ErrorExpr so
      // that downstream clients know that it was present (well, at least the =
      // was present).  This silences downstream diagnostics checking to make
      // sure that some PBD's that require initializers actually had them.
      if (!DisallowInit && init.isNull())
        init = makeParserResult(init, new (Context) ErrorExpr(EqualLoc));
      
      
      // Remember this init for the PatternBindingDecl.
      PatternInit = init.getPtrOrNull();
      PBDEntries.back().setInit(PatternInit);

      // If we set up an initialization context for a property or module-level
      // global, record it.
      PBDEntries.back().setInitContext(initContext);

      if (init.hasCodeCompletion()) {
        Status |= init;
        // If we are doing second pass of code completion, we don't want to
        // suddenly cut off parsing and throw away the declaration.
        if (isIDEInspectionFirstPass())
          return makeResult(makeParserCodeCompletionStatus());
      }

      if (init.isNull())
        return makeResult(makeParserError());
    }
    
    // If we syntactically match the second decl-var production, with a
    // var-get-set clause, parse the var-get-set clause.
    if (Tok.is(tok::l_brace)) {

      // Skip parsing the var-get-set clause if '{' is at start of line
      // and next token is not 'didSet' or 'willSet'. Parsing as 'do'
      // statement gives useful errors for missing 'do' before brace.
      // See https://github.com/apple/swift/issues/57183.
      if (!PatternInit || !Tok.isAtStartOfLine() || isStartOfGetSetAccessor()) {
        HasAccessors = true;
        auto boundVar = parseDeclVarGetSet(
            PBDEntries.back(), Flags, StaticLoc, StaticSpelling, VarLoc,
            PatternInit != nullptr, Attributes, Decls);
        if (boundVar.hasCodeCompletion())
          return makeResult(makeParserCodeCompletionStatus());
      }
    }
    
    // Propagate back types for simple patterns, like "var A, B : T".
    if (auto *TP = dyn_cast<TypedPattern>(pattern)) {
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

          auto *NewTP = TypedPattern::createPropagated(Context, PrevPat,
                                                       TP->getTypeRepr());
          PBDEntries[i-1].setPattern(NewTP);
        }
      }
    }
     HasNext = consumeIf(tok::comma);
  } while (HasNext);

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

/// Parse a 'func' declaration, returning null on error.  The caller
/// handles this case and does recovery as appropriate.
///
/// \verbatim
///   decl-func:
///     attribute-list? ('static' | 'class' | 'distributed')? 'mutating'? 'func'
///               any-identifier generic-params? func-signature where-clause?
///               stmt-brace?
/// \endverbatim
///
/// \note The caller of this method must ensure that the next token is 'func'.
ParserResult<FuncDecl> Parser::parseDeclFunc(SourceLoc StaticLoc,
                                             StaticSpellingKind StaticSpelling,
                                             ParseDeclOptions Flags,
                                             DeclAttributes &Attributes,
                                             bool HasFuncKeyword) {
  assert(StaticLoc.isInvalid() || StaticSpelling != StaticSpellingKind::None);

  if (StaticLoc.isValid()) {
    if (!Flags.contains(PD_HasContainerType)) {
      // Reject static functions at global scope.
      diagnose(Tok, diag::static_func_decl_global_scope, StaticSpelling)
          .fixItRemove(StaticLoc);
      StaticLoc = SourceLoc();
      StaticSpelling = StaticSpellingKind::None;
    } else if (Flags.contains(PD_InStruct) || Flags.contains(PD_InEnum) ||
               Flags.contains(PD_InProtocol)) {
      if (StaticSpelling == StaticSpellingKind::KeywordClass) {
        diagnose(Tok, diag::class_func_not_in_class,
                 Flags.contains(PD_InProtocol))
            .fixItReplace(StaticLoc, "static");

        StaticSpelling = StaticSpellingKind::KeywordStatic;
      }
    }
  }

  ParserStatus Status;
  SourceLoc FuncLoc =
      HasFuncKeyword ? consumeToken(tok::kw_func) : Tok.getLoc();

  // Parse function name.
  Identifier SimpleName;
  SourceLoc NameLoc;
  if (Tok.isAnyOperator() || Tok.isAny(tok::exclaim_postfix, tok::amp_prefix)) {
    // If the name is an operator token that ends in '<' and the following token
    // is an identifier or 'let', split the '<' off as a separate token. This
    // allows things like 'func ==<T>(x:T, y:T) {}' to parse as '==' with
    // generic type variable '<T>' as expected.
    auto NameStr = Tok.getText();
    if (NameStr.size() > 1 && NameStr.back() == '<' &&
        peekToken().isAny(tok::identifier, tok::kw_let)) {
      NameStr = NameStr.slice(0, NameStr.size() - 1);
    }
    SimpleName = Context.getIdentifier(NameStr);
    NameLoc = consumeStartingCharacterOfCurrentToken(tok::oper_binary_spaced,
                                                     NameStr.size());
    // Within a protocol, recover from a missing 'static'.
    if (Flags & PD_InProtocol) {
      switch (StaticSpelling) {
      case StaticSpellingKind::None: {
        diagnose(NameLoc, diag::operator_static_in_protocol, SimpleName.str())
            .fixItInsert(FuncLoc, "static ");
        StaticSpelling = StaticSpellingKind::KeywordStatic;
        break;
      }

      case StaticSpellingKind::KeywordStatic:
        // Okay, this is correct.
        break;

      case StaticSpellingKind::KeywordClass:
        llvm_unreachable("should have been fixed above");
      }
    }
  } else {
    // This non-operator path is quite accepting of what tokens might be a name,
    // because we're aggressive about recovering/providing good diagnostics for
    // beginners.
    auto NameStatus = parseIdentifierDeclName(
        *this, SimpleName, NameLoc, "function", [&](const Token &next) {
          return next.isAny(tok::l_paren, tok::arrow, tok::l_brace) ||
                 startsWithLess(next);
        });
    if (NameStatus.isErrorOrHasCompletion())
      return NameStatus;
  }

  DebuggerContextChange DCC(*this, SimpleName, DeclKind::Func);

  // Parse the generic-params, if present.
  GenericParamList *GenericParams;
  auto GenericParamResult = maybeParseGenericParams();
  GenericParams = GenericParamResult.getPtrOrNull();
  if (GenericParamResult.hasCodeCompletion()) {
    Status.setHasCodeCompletionAndIsError();
    if (!CodeCompletionCallbacks)
      return Status;
  }

  DefaultArgumentInfo DefaultArgs;
  TypeRepr *FuncRetTy = nullptr;
  DeclName FullName;
  ParameterList *BodyParams;
  SourceLoc asyncLoc;
  bool reasync;
  SourceLoc throwsLoc;
  bool rethrows;
  TypeRepr *thrownTy = nullptr;
  Status |= parseFunctionSignature(SimpleName, FullName, BodyParams,
                                   DefaultArgs,
                                   asyncLoc, reasync,
                                   throwsLoc, rethrows, thrownTy,
                                   FuncRetTy);
  if (Status.hasCodeCompletion() && !CodeCompletionCallbacks) {
    // Trigger delayed parsing, no need to continue.
    return Status;
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // If there was an 'async' modifier, put it in the right place for a function.
  bool isAsync = asyncLoc.isValid();
  if (auto asyncAttr = Attributes.getAttribute<AsyncAttr>()) {
    SourceLoc insertLoc = Lexer::getLocForEndOfToken(
        SourceMgr, BodyParams->getRParenLoc());

    diagnose(asyncAttr->getLocation(), diag::async_func_modifier)
      .fixItRemove(asyncAttr->getRange())
      .fixItInsert(insertLoc, " async");
    asyncAttr->setInvalid();
    isAsync = true;
  }

  // Create the decl for the func and add it to the parent scope.
  auto *FD = FuncDecl::create(Context, StaticLoc, StaticSpelling,
                              FuncLoc, FullName, NameLoc,
                              /*Async=*/isAsync, asyncLoc,
                              /*Throws=*/throwsLoc.isValid(), throwsLoc,
                              thrownTy, GenericParams,
                              BodyParams, FuncRetTy,
                              CurDeclContext);

  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    ContextChange CC(*this, FD);

    Status |= parseFreestandingGenericWhereClause(FD);
    if (Status.hasCodeCompletion() && !CodeCompletionCallbacks) {
      // Trigger delayed parsing, no need to continue.
      return Status;
    }
  }
  
  // Protocol method arguments may not have default values.
  if (Flags.contains(PD_InProtocol) && DefaultArgs.HasDefaultArgument) {
    diagnose(FuncLoc, diag::protocol_method_argument_init);
    return nullptr;
  }

  if (reasync) {
    Attributes.add(new (Context) ReasyncAttr(asyncLoc));
  }
  if (rethrows) {
    Attributes.add(new (Context) RethrowsAttr(throwsLoc));
  }

  diagnoseOperatorFixityAttributes(*this, Attributes, FD);
  // Add the attributes here so if we need them while parsing the body
  // they are available.
  FD->attachParsedAttrs(Attributes);

  // Pass the function signature to code completion.
  if (Status.hasCodeCompletion()) {
    assert(CodeCompletionCallbacks && "must be code completion second pass");
    CodeCompletionCallbacks->setParsedDecl(FD);
  }

  if (Flags.contains(PD_InProtocol)) {
    if (Tok.is(tok::l_brace)) {
      diagnose(Tok, diag::protocol_method_with_body);
      skipSingle();
    }
  } else if (!Status.hasCodeCompletion()) {
    parseAbstractFunctionBody(FD, Flags);
  }

  return DCC.fixupParserResult(FD);
}

/// Parse a function body for \p AFD, setting the body to \p AFD before
/// returning it.
BodyAndFingerprint
Parser::parseAbstractFunctionBodyImpl(AbstractFunctionDecl *AFD) {
  assert(Tok.is(tok::l_brace));

  // Establish the new context.
  ParseFunctionBody CC(*this, AFD);

  if (auto *Stats = Context.Stats)
    ++Stats->getFrontendCounters().NumFunctionsParsed;

  // In implicit getter, if a CC token is the first token after '{', it might
  // be a start of an accessor block. Perform special completion for that.
  if (auto accessor = dyn_cast<AccessorDecl>(AFD)) {
    if (CodeCompletionCallbacks && peekToken().is(tok::code_complete) &&
        accessor->isImplicitGetter()) {
      SourceLoc LBraceLoc, RBraceLoc;
      LBraceLoc = consumeToken(tok::l_brace);
      auto *CCE = new (Context) CodeCompletionExpr(Tok.getLoc());
      auto *Return = ReturnStmt::createImplied(Context, CCE);
      CodeCompletionCallbacks->setParsedDecl(accessor);
      CodeCompletionCallbacks->completeAccessorBeginning(CCE);
      RBraceLoc = Tok.getLoc();
      consumeToken(tok::code_complete);
      auto *BS =
          BraceStmt::create(Context, LBraceLoc, ASTNode(Return), RBraceLoc,
                            /*implicit*/ true);
      AFD->setBodyParsed(BS);
      return {BS, Fingerprint::ZERO()};
    }
  }

  llvm::SaveAndRestore<std::optional<StableHasher>> T(
      CurrentTokenHash, StableHasher::defaultHasher());

  ParserResult<BraceStmt> Body = parseBraceItemList(diag::invalid_diagnostic);
  // Since we know 'Tok.is(tok::l_brace)', the body can't be null.
  assert(Body.isNonNull());

  // Clone the current hasher and extract a Fingerprint.
  StableHasher currentHash{*CurrentTokenHash};
  Fingerprint fp(std::move(currentHash));

  BraceStmt *BS = Body.get();
  AFD->setBodyParsed(BS, fp);
  return {BS, fp};
}

/// Parse function body into \p AFD or skip it for delayed parsing.
void Parser::parseAbstractFunctionBody(AbstractFunctionDecl *AFD,
                                       ParseDeclOptions Flags) {
  if (!Tok.is(tok::l_brace)) {
    checkForInputIncomplete();
    return;
  }

  // Record the curly braces but nothing inside.
  recordTokenHash("{");
  recordTokenHash("}");

  llvm::SaveAndRestore<std::optional<StableHasher>> T(CurrentTokenHash,
                                                      std::nullopt);

  // If we can delay parsing this body, or this is the first pass of code
  // completion, skip until the end. If we encounter a code completion token
  // while skipping, we'll make a note of it.
  auto BodyPreviousLoc = PreviousLoc;
  SourceRange BodyRange(Tok.getLoc());
  auto setIDEInspectionDelayedDeclStateIfNeeded = [&] {
    auto CharBodyRange =
        Lexer::getCharSourceRangeFromSourceRange(SourceMgr, BodyRange);
    if (!isIDEInspectionFirstPass() ||
        !SourceMgr.rangeContainsIDEInspectionTarget(CharBodyRange)) {
      return;
    }
    if (State->hasIDEInspectionDelayedDeclState())
      State->takeIDEInspectionDelayedDeclState();
    State->setIDEInspectionDelayedDeclState(
        SourceMgr, L->getBufferID(), IDEInspectionDelayedDeclKind::FunctionBody,
        AFD, BodyRange, BodyPreviousLoc);
  };

  bool HasNestedTypeDeclarations;
  if (canDelayFunctionBodyParsing(HasNestedTypeDeclarations, Flags)) {
    ASSERT(!Flags.contains(PD_StubOnly)
              && "stub-only should parse body immediately");
    BodyRange.End = PreviousLoc;

    assert(SourceMgr.isBeforeInBuffer(BodyRange.Start, BodyRange.End) ||
           BodyRange.Start == BodyRange.End &&
           "At least '{' should be consumed");

    AFD->setBodyDelayed(BodyRange);
    AFD->setHasNestedTypeDeclarations(HasNestedTypeDeclarations);

    setIDEInspectionDelayedDeclStateIfNeeded();
    return;
  }

  (void)parseAbstractFunctionBodyImpl(AFD);
  BodyRange = AFD->getBodySourceRange();
  setIDEInspectionDelayedDeclStateIfNeeded();

  if (Flags.contains(PD_StubOnly)) {
    diagnose(BodyRange.Start, diag::stub_decl_cannot_have_body,
             AFD->getDescriptiveKind())
      .fixItRemove(BodyRange);
    AFD->setBody(nullptr, AbstractFunctionDecl::BodyKind::None);
  }
}

BodyAndFingerprint
Parser::parseAbstractFunctionBodyDelayed(AbstractFunctionDecl *AFD) {
  assert(AFD->getBodyKind() == AbstractFunctionDecl::BodyKind::Unparsed &&
         "function body should be delayed");

  auto bodyRange = AFD->getBodySourceRange();
  auto BeginParserPosition = getParserPosition(bodyRange.Start,
                                               /*previousLoc*/ SourceLoc());
  auto EndLexerState = L->getStateForEndOfTokenLoc(AFD->getEndLoc());

  // ParserPositionRAII needs a primed parser to restore to.
  if (Tok.is(tok::NUM_TOKENS))
    consumeTokenWithoutFeedingReceiver();

  // Ensure that we restore the parser state at exit.
  ParserPositionRAII PPR(*this);

  // Create a lexer that cannot go past the end state.
  Lexer LocalLex(*L, BeginParserPosition.LS, EndLexerState);

  // Temporarily swap out the parser's current lexer with our new one.
  llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

  // Rewind to '{' of the function body.
  restoreParserPosition(BeginParserPosition);

  return parseAbstractFunctionBodyImpl(AFD);
}

/// Parse a 'enum' declaration, returning true (and doing no token
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

  Status |= parseIdentifierDeclName(
      *this, EnumName, EnumNameLoc, "enum", [&](const Token &next) {
        return next.isAny(tok::colon, tok::l_brace) || startsWithLess(next);
      });
  if (Status.isErrorOrHasCompletion())
    return Status;

  DebuggerContextChange DCC(*this, EnumName, DeclKind::Enum);
  
  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    auto Result = maybeParseGenericParams();
    GenericParams = Result.getPtrOrNull();
    if (Result.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
  }

  EnumDecl *ED = new (Context) EnumDecl(EnumLoc, EnumName, EnumNameLoc,
                                        { }, GenericParams, CurDeclContext);
  ED->attachParsedAttrs(Attributes);

  ContextChange CC(*this, ED);

  // Parse optional inheritance clause within the context of the enum.
  if (Tok.is(tok::colon)) {
    SmallVector<InheritedEntry, 2> Inherited;
    Status |= parseInheritance(Inherited,
                               /*allowClassRequirement=*/false,
                               /*allowAnyObject=*/false);
    ED->setInherited(Context.AllocateCopy(Inherited));
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);
  
  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(ED);
    if (whereStatus.hasCodeCompletion() && !CodeCompletionCallbacks) {
      // Trigger delayed parsing, no need to continue.
      return whereStatus;
    }
    Status |= whereStatus;
  }

  SourceLoc LBLoc, RBLoc;
  {
    if (parseMemberDeclList(LBLoc, RBLoc,
                            diag::expected_lbrace_enum,
                            diag::expected_rbrace_enum,
                            ED, Flags))
      Status.setIsParseError();
  }

  ED->setBraces({LBLoc, RBLoc});

  return DCC.fixupParserResult(Status, ED);
}

/// Parse a 'case' of an enum.
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
      break;
    }

    if (Tok.is(tok::identifier)) {
      Status |= parseIdentifierDeclName(
          *this, Name, NameLoc, "enum 'case'", [](const Token &next) {
            return next.isAny(tok::l_paren, tok::kw_case, tok::colon,
                              tok::r_brace);
          });
      assert(Status.isSuccess() && !Status.hasCodeCompletion());
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
      {
        CancellableBacktrackingScope backtrack(*this);
        llvm::SaveAndRestore<decltype(InBindingPattern)> T(
            InBindingPattern, PatternBindingState::InMatchingPattern);
        parseMatchingPattern(/*isExprBasic*/false);

        // Reset async attribute in parser context.
        llvm::SaveAndRestore<bool> AsyncAttr(InPatternWithAsyncAttribute,
                                             false);

        if (consumeIf(tok::colon)) {
          backtrack.cancelBacktrack();
          diagnose(CaseLoc, diag::case_outside_of_switch, "case");
          Status.setIsParseError();
          return Status;
        }
      }
      
      if (NameIsKeyword) {
        diagnose(TokLoc, diag::keyword_cant_be_identifier, TokText);
        diagnose(TokLoc, diag::backticks_to_escape)
          .fixItReplace(TokLoc, "`" + TokText.str() + "`");
        if (!Tok.isAtStartOfLine()) {
          Name = Context.getIdentifier(Tok.getText());
          NameLoc = consumeToken();
        }
      } else if (CommaLoc.isValid()) {
        diagnose(Tok, diag::expected_identifier_after_case_comma);
        break;
      } else {
        diagnose(CaseLoc, diag::expected_identifier_in_decl, "enum 'case'");
      }
    }

    // See if there's a following argument type.
    ParserResult<ParameterList> ArgParams;
    SmallVector<Identifier, 4> argumentNames;
    DefaultArgumentInfo DefaultArgs;
    if (Tok.isFollowingLParen()) {
      ArgParams = parseSingleParameterClause(ParameterContextKind::EnumElement,
                                             &argumentNames, &DefaultArgs);
      if (ArgParams.isNull() || ArgParams.hasCodeCompletion())
        return ParserStatus(ArgParams);
    }

    // See if there's a raw value expression.
    SourceLoc EqualsLoc;
    ParserResult<Expr> RawValueExpr;
    LiteralExpr *LiteralRawValueExpr = nullptr;
    if (Tok.is(tok::equal)) {
      EqualsLoc = consumeToken();
      {
        CodeCompletionCallbacks::InEnumElementRawValueRAII
            InEnumElementRawValue(CodeCompletionCallbacks);
        RawValueExpr = parseExpr(diag::expected_expr_enum_case_raw_value);
      }
      if (RawValueExpr.hasCodeCompletion()) {
        Status.setHasCodeCompletionAndIsError();
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
    DeclName FullName;
    if (ArgParams.isNull()) {
      FullName = Name;
    } else {
      FullName = DeclName(Context, Name, argumentNames);
    }
    auto *result = new (Context) EnumElementDecl(NameLoc, FullName,
                                                 ArgParams.getPtrOrNull(),
                                                 EqualsLoc,
                                                 LiteralRawValueExpr,
                                                 CurDeclContext);

    if (NameLoc == CaseLoc) {
      result->setImplicit(); // Parse error
    }

    result->attachParsedAttrs(Attributes);
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

/// Parse a 'struct' declaration, returning true (and doing no token
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

  Status |= parseIdentifierDeclName(
      *this, StructName, StructNameLoc, "struct", [&](const Token &next) {
        return next.isAny(tok::colon, tok::l_brace) || startsWithLess(next);
      });
  if (Status.isErrorOrHasCompletion())
    return Status;

  DebuggerContextChange DCC (*this, StructName, DeclKind::Struct);
  
  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
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
  SD->attachParsedAttrs(Attributes);

  ContextChange CC(*this, SD);

  // Parse optional inheritance clause within the context of the struct.
  if (Tok.is(tok::colon)) {
    SmallVector<InheritedEntry, 2> Inherited;
    Status |= parseInheritance(Inherited,
                               /*allowClassRequirement=*/false,
                               /*allowAnyObject=*/false);
    SD->setInherited(Context.AllocateCopy(Inherited));
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(SD);
    if (whereStatus.hasCodeCompletion() && !CodeCompletionCallbacks) {
      // Trigger delayed parsing, no need to continue.
      return whereStatus;
    }
    Status |= whereStatus;
  }

  // Make the entities of the struct as a code block.
  SourceLoc LBLoc, RBLoc;
  {
    // Parse the body.
    if (parseMemberDeclList(LBLoc, RBLoc,
                            diag::expected_lbrace_struct,
                            diag::expected_rbrace_struct,
                            SD, Flags))
      Status.setIsParseError();
  }

  SD->setBraces({LBLoc, RBLoc});

  return DCC.fixupParserResult(Status, SD);
}

/// Parse a 'class' declaration, doing no token skipping on error.
///
/// \verbatim
///   decl-class:
///      'class' attribute-list identifier generic-params? inheritance?
///          where-clause? '{' decl-class-body '}
///   decl-class-body:
///      decl*
/// \endverbatim
ParserResult<ClassDecl> Parser::parseDeclClass(ParseDeclOptions Flags,
                                               DeclAttributes &Attributes) {
  bool isExplicitActorDecl = Tok.isContextualKeyword("actor");

  // part of
  SourceLoc ClassLoc;
  if (isExplicitActorDecl) {
    ClassLoc = consumeToken();
  } else {
    ClassLoc = consumeToken(tok::kw_class);
  }

  Identifier ClassName;
  SourceLoc ClassNameLoc;
  ParserStatus Status;

  Status |= parseIdentifierDeclName(
      *this, ClassName, ClassNameLoc, isExplicitActorDecl ? "actor" : "class",
      [&](const Token &next) {
        return next.isAny(tok::colon, tok::l_brace) || startsWithLess(next);
      });
  if (Status.isErrorOrHasCompletion())
    return Status;

  DebuggerContextChange DCC (*this, ClassName, DeclKind::Class);

  // Parse the generic-params, if present.
  GenericParamList *GenericParams = nullptr;
  {
    auto Result = maybeParseGenericParams();
    GenericParams = Result.getPtrOrNull();
    if (Result.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
  }

  // Create the class.
  ClassDecl *CD = new (Context) ClassDecl(ClassLoc, ClassName, ClassNameLoc,
                                          { }, GenericParams, CurDeclContext,
                                          isExplicitActorDecl);
  CD->attachParsedAttrs(Attributes);

  // Parsed classes never have missing vtable entries.
  CD->setHasMissingVTableEntries(false);

  ContextChange CC(*this, CD);

  // Parse optional inheritance clause within the context of the class.
  if (Tok.is(tok::colon)) {
    SmallVector<InheritedEntry, 2> Inherited;
    Status |= parseInheritance(Inherited,
                               /*allowClassRequirement=*/false,
                               /*allowAnyObject=*/false);
    CD->setInherited(Context.AllocateCopy(Inherited));

  // Parse python style inheritance clause and replace parentheses with a colon
  } else if (Tok.is(tok::l_paren)) {
    bool isParenStyleInheritance = false;
    {
      BacktrackingScope backtrack(*this);
      consumeToken(tok::l_paren);
      isParenStyleInheritance = canParseType() &&
        Tok.isAny(tok::r_paren, tok::kw_where, tok::l_brace, tok::eof);
    }
    if(isParenStyleInheritance) {
      SourceLoc LParenLoc = consumeToken(tok::l_paren);
      auto TypeResult = parseType();
      if (TypeResult.isNull()) {
        Status.setIsParseError();
        return Status;
      }
      SourceLoc RParenLoc;
      consumeIf(tok::r_paren, RParenLoc);
      diagnose(LParenLoc, diag::expected_colon_class)
        .fixItReplace(LParenLoc, ": ")
        .fixItRemove(RParenLoc);
    }
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(CD);
    if (whereStatus.hasCodeCompletion() && !CodeCompletionCallbacks) {
      // Trigger delayed parsing, no need to continue.
      return whereStatus;
    }
    Status |= whereStatus;
  }

  SourceLoc LBLoc, RBLoc;
  {
    // Parse the body.
    if (parseMemberDeclList(LBLoc, RBLoc,
                            isExplicitActorDecl ? diag::expected_lbrace_actor
                                                : diag::expected_lbrace_class,
                            isExplicitActorDecl ? diag::expected_rbrace_actor
                                                : diag::expected_rbrace_class,
                            CD, Flags))
      Status.setIsParseError();
  }

  CD->setBraces({LBLoc, RBLoc});

  return DCC.fixupParserResult(Status, CD);
}

ParserStatus Parser::parsePrimaryAssociatedTypes(
    SmallVectorImpl<PrimaryAssociatedTypeName> &AssocTypeNames) {
  SourceLoc LAngleLoc = consumeStartingLess();

  auto Result = parsePrimaryAssociatedTypeList(AssocTypeNames);

  // Parse the closing '>'.
  SourceLoc RAngleLoc;
  if (startsWithGreater(Tok)) {
    RAngleLoc = consumeStartingGreater();
  } else {
    diagnose(Tok, diag::expected_rangle_primary_associated_type_list);
    diagnose(LAngleLoc, diag::opening_angle);

    // Skip until we hit the '>'.
    RAngleLoc = skipUntilGreaterInTypeList();
  }

  return Result;
}

ParserStatus Parser::parsePrimaryAssociatedTypeList(
    SmallVectorImpl<PrimaryAssociatedTypeName> &AssocTypeNames) {
  ParserStatus Result;
  bool HasNextParam = false;
  do {
    // Parse the name of the parameter.
    Identifier Name;
    SourceLoc NameLoc;
    if (parseIdentifier(Name, NameLoc, /*diagnoseDollarPrefix=*/true,
                        diag::expected_primary_associated_type_name)) {
      Result.setIsParseError();
      break;
    }

    AssocTypeNames.emplace_back(Name, NameLoc);

    // Parse the comma, if the list continues.
    HasNextParam = consumeIf(tok::comma);
    
    // The list ends if we find a trailing comma
    if (startsWithGreater(Tok)) {
      break;
    }
  } while (HasNextParam);

  return Result;
}

/// Parse a 'protocol' declaration, doing no token skipping on error.
///
/// \verbatim
///   decl-protocol:
///      protocol-head '{' protocol-member* '}'
///
///   protocol-head:
///     attribute-list 'protocol' identifier primary-associated-type-list?
///     inheritance? 
///
///   primary-associated-type-list:
///     '<' identifier+ '>'
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

  Status |= parseIdentifierDeclName(
      *this, ProtocolName, NameLoc, "protocol",
      [&](const Token &next) { return next.isAny(tok::colon, tok::l_brace); });
  if (Status.isErrorOrHasCompletion())
    return Status;

  SmallVector<PrimaryAssociatedTypeName, 2> PrimaryAssociatedTypeNames;
  if (startsWithLess(Tok)) {
    Status |= parsePrimaryAssociatedTypes(PrimaryAssociatedTypeNames);
  }

  DebuggerContextChange DCC (*this);
  
  // Parse optional inheritance clause.
  SmallVector<InheritedEntry, 4> InheritedProtocols;
  SourceLoc colonLoc;
  if (Tok.is(tok::colon)) {
    colonLoc = Tok.getLoc();
    Status |= parseInheritance(InheritedProtocols,
                               /*allowClassRequirement=*/true,
                               /*allowAnyObject=*/true);
  }

  TrailingWhereClause *TrailingWhere = nullptr;
  bool whereClauseHadCodeCompletion = false;
  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseProtocolOrAssociatedTypeWhereClause(
        TrailingWhere, /*isProtocol=*/true);
    if (whereStatus.hasCodeCompletion()) {
      if (isIDEInspectionFirstPass())
        return whereStatus;
      whereClauseHadCodeCompletion = true;
    }
  }

  ProtocolDecl *Proto = new (Context)
      ProtocolDecl(CurDeclContext, ProtocolLoc, NameLoc, ProtocolName,
                   Context.AllocateCopy(PrimaryAssociatedTypeNames),
                   Context.AllocateCopy(InheritedProtocols), TrailingWhere);

  Proto->attachParsedAttrs(Attributes);
  if (whereClauseHadCodeCompletion && CodeCompletionCallbacks)
    CodeCompletionCallbacks->setParsedDecl(Proto);

  ContextChange CC(*this, Proto);

  // Parse the body.
  {
    SourceLoc LBraceLoc;
    SourceLoc RBraceLoc;
    {
      // Parse the members.
      if (parseMemberDeclList(LBraceLoc, RBraceLoc,
                              diag::expected_lbrace_protocol,
                              diag::expected_rbrace_protocol,
                              Proto, Flags))
        Status.setIsParseError();
    }

    // Install the protocol elements.
    Proto->setBraces({LBraceLoc, RBraceLoc});
  }
  
  return DCC.fixupParserResult(Status, Proto);
}

/// Parse a 'subscript' declaration.
///
/// \verbatim
///   decl-subscript:
///     subscript-head get-set
///   subscript-head
///     attribute-list? 'subscript' parameter-clause '->' type
/// \endverbatim
ParserResult<SubscriptDecl>
Parser::parseDeclSubscript(SourceLoc StaticLoc,
                           StaticSpellingKind StaticSpelling,
                           ParseDeclOptions Flags,
                           DeclAttributes &Attributes,
                           SmallVectorImpl<Decl *> &Decls) {
  assert(StaticLoc.isInvalid() || StaticSpelling != StaticSpellingKind::None);
  
  if (StaticLoc.isValid()) {
    if (Flags.contains(PD_InStruct) || Flags.contains(PD_InEnum) ||
               Flags.contains(PD_InProtocol)) {
      if (StaticSpelling == StaticSpellingKind::KeywordClass) {
        diagnose(Tok, diag::class_subscript_not_in_class,
                 Flags.contains(PD_InProtocol))
        .fixItReplace(StaticLoc, "static");
        
        StaticSpelling = StaticSpellingKind::KeywordStatic;
      }
    }
  }

  ParserStatus Status;
  SourceLoc SubscriptLoc = consumeToken(tok::kw_subscript);

  // Diagnose 'subscript' with name.
  if (Tok.is(tok::identifier) &&
      (peekToken().is(tok::l_paren) || startsWithLess(peekToken()))) {
    diagnose(Tok, diag::subscript_has_name)
      .fixItRemove(Tok.getLoc());
    consumeToken(tok::identifier);
  }

  // Parse the generic-params, if present.
  GenericParamList *GenericParams;

  auto Result = maybeParseGenericParams();
  GenericParams = Result.getPtrOrNull();
  if (Result.hasCodeCompletion()) {
    Status.setHasCodeCompletionAndIsError();
    if (!CodeCompletionCallbacks)
      return Status;
  }

  // Parse the parameter list.
  DefaultArgumentInfo DefaultArgs;
  ParserResult<ParameterList> Indices =
      parseSingleParameterClause(ParameterContextKind::Subscript,
                                 /*argumentNamesOut*/ nullptr, &DefaultArgs);
  Status |= Indices;
  if (Status.hasCodeCompletion() && !CodeCompletionCallbacks)
    return Status;
  
  SourceLoc ArrowLoc;
  ParserResult<TypeRepr> ElementTy;
  {
    // '->'
    if (!consumeIf(tok::arrow, ArrowLoc)) {
      if (!Indices.isParseErrorOrHasCompletion())
        diagnose(Tok, diag::expected_arrow_subscript);
      Status.setIsParseError();
    }

    if (!ArrowLoc.isValid() &&
        (Indices.isNull() || Indices.get()->size() == 0)) {
      // This doesn't look much like a subscript, so let regular recovery take
      // care of it.
      return Status;
    }

    // type
    ElementTy = parseDeclResultType(diag::expected_type_subscript);
    Status |= ElementTy;
    if (Status.hasCodeCompletion() && !CodeCompletionCallbacks)
      return Status;

    if (ElementTy.isNull()) {
      // Always set an element type.
      ElementTy = makeParserResult(ElementTy, ErrorTypeRepr::create(Context));
    }
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  // Protocol requirement arguments may not have default values.
  if (Flags.contains(PD_InProtocol) && DefaultArgs.HasDefaultArgument) {
    diagnose(SubscriptLoc, diag::protocol_subscript_argument_init);
    return nullptr;
  }

  // Build an AST for the subscript declaration.
  auto *const Subscript = SubscriptDecl::createParsed(
      Context, StaticLoc, StaticSpelling, SubscriptLoc, Indices.get(), ArrowLoc,
      ElementTy.get(), CurDeclContext, GenericParams);
  Subscript->attachParsedAttrs(Attributes);

  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    ContextChange CC(*this, Subscript);

    Status |= parseFreestandingGenericWhereClause(Subscript);
    if (Status.hasCodeCompletion() && !CodeCompletionCallbacks) {
      // Trigger delayed parsing, no need to continue.
      return Status;
    }
  }

  // Pass the function signature to code completion.
  if (Status.hasCodeCompletion()) {
    assert(CodeCompletionCallbacks && "must be code completion second pass");
    CodeCompletionCallbacks->setParsedDecl(Subscript);
  }

  Decls.push_back(Subscript);

  bool Invalid = false;
  // Reject 'subscript' functions outside of type decls
  if (!(Flags & PD_HasContainerType)) {
    diagnose(SubscriptLoc, diag::subscript_decl_wrong_scope);
    Invalid = true;
  }

  // '{'
  // Parse getter and setter.
  ParsedAccessors accessors;
  if (Tok.isNot(tok::l_brace)) {
    // Subscript stubs should never have accessors, and this one doesn't, so
    // we're done.
    if (Flags.contains(PD_StubOnly)) {
      return makeParserResult(Status, Subscript);
    }

    // Subscript declarations must always have at least a getter, so they need
    // to be followed by a {.
    if (!Status.isErrorOrHasCompletion()) {
      if (Flags.contains(PD_InProtocol)) {
        diagnose(Tok, diag::expected_lbrace_subscript_protocol)
            .fixItInsertAfter(ElementTy.get()->getEndLoc(), " { get <#set#> }");
      } else {
        diagnose(Tok, diag::expected_lbrace_subscript);
      }
      Status.setIsParseError();
    }
  } else if (!Status.hasCodeCompletion()) {
    Status |= parseGetSet(Flags, Indices.get(), ElementTy.get(), accessors,
                          Subscript);
  }

  accessors.record(*this, Subscript, (Invalid || !Status.isSuccess() ||
                                      Status.hasCodeCompletion()));

  // No need to setLocalDiscriminator because subscripts cannot
  // validly appear outside of type decls.
  return makeParserResult(Status, Subscript);
}

ParserResult<ConstructorDecl>
Parser::parseDeclInit(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  assert(Tok.is(tok::kw_init));
  ParserStatus Status;
  SourceLoc ConstructorLoc = consumeToken();
  bool Failable = false, IUO = false;
  SourceLoc FailabilityLoc;

  const bool ConstructorsNotAllowed = !(Flags & PD_HasContainerType);

  // Reject constructors outside of types.
  if (ConstructorsNotAllowed) {
    diagnose(Tok, diag::initializer_decl_wrong_scope);
  }

  // Parse the '!' or '?' for a failable initializer.
  if (Tok.isAny(tok::exclaim_postfix, tok::sil_exclamation) ||
      (Tok.isAnyOperator() && Tok.getText() == "!")) {
    Failable = true;
    IUO = true;
    FailabilityLoc = consumeToken();
  } else if (Tok.isAny(tok::question_postfix, tok::question_infix)) {
    Failable = true;
    FailabilityLoc = consumeToken();
  }

  // Reject named 'init'. e.g. 'init withString(string: str)'.
  if (Tok.is(tok::identifier) &&
      (peekToken().is(tok::l_paren) || startsWithLess(peekToken()))) {
    diagnose(Tok, diag::initializer_has_name)
      .fixItRemove(Tok.getLoc());
    consumeToken(tok::identifier);
  }

  // Parse the generic-params, if present.
  auto GPResult = maybeParseGenericParams();
  GenericParamList *GenericParams = GPResult.getPtrOrNull();
  if (GPResult.hasCodeCompletion()) {
    Status.setHasCodeCompletionAndIsError();
    if (!CodeCompletionCallbacks)
      return Status;
  }

  DefaultArgumentInfo DefaultArgs;
  TypeRepr *FuncRetTy = nullptr;
  DeclName FullName;
  ParameterList *BodyParams;
  SourceLoc asyncLoc;
  bool reasync;
  SourceLoc throwsLoc;
  bool rethrows;
  TypeRepr *thrownTy = nullptr;
  Status |= parseFunctionSignature(DeclBaseName::createConstructor(), FullName,
                                   BodyParams,
                                   DefaultArgs,
                                   asyncLoc, reasync,
                                   throwsLoc, rethrows, thrownTy,
                                   FuncRetTy);
  if (Status.hasCodeCompletion() && !CodeCompletionCallbacks) {
    // Trigger delayed parsing, no need to continue.
    return Status;
  }

  // Protocol initializer arguments may not have default values.
  if (Flags.contains(PD_InProtocol) && DefaultArgs.HasDefaultArgument) {
    diagnose(ConstructorLoc, diag::protocol_init_argument_init);
    return nullptr;
  }

  // If there was an 'async' modifier, put it in the right place for an
  // initializer.
  bool isAsync = asyncLoc.isValid();
  if (auto asyncAttr = Attributes.getAttribute<AsyncAttr>()) {
    SourceLoc insertLoc = Lexer::getLocForEndOfToken(
        SourceMgr, BodyParams->getRParenLoc());

    diagnose(asyncAttr->getLocation(), diag::async_func_modifier)
      .fixItRemove(asyncAttr->getRange())
      .fixItInsert(insertLoc, " async");
    asyncAttr->setInvalid();
    isAsync = true;
  }

  if (FuncRetTy) {
    diagnose(FuncRetTy->getStartLoc(), diag::initializer_result_type)
      .fixItRemove(FuncRetTy->getSourceRange());
  }

  if (reasync) {
    Attributes.add(new (Context) ReasyncAttr(asyncLoc));
  }
  if (rethrows) {
    Attributes.add(new (Context) RethrowsAttr(throwsLoc));
  }

  diagnoseWhereClauseInGenericParamList(GenericParams);

  auto *CD = new (Context) ConstructorDecl(FullName, ConstructorLoc,
                                           Failable, FailabilityLoc,
                                           isAsync, asyncLoc,
                                           throwsLoc.isValid(), throwsLoc,
                                           thrownTy, BodyParams, GenericParams,
                                           CurDeclContext);
  CD->setImplicitlyUnwrappedOptional(IUO);
  CD->attachParsedAttrs(Attributes);

  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    ContextChange(*this, CD);

    Status |= parseFreestandingGenericWhereClause(CD);
    if (Status.hasCodeCompletion() && !CodeCompletionCallbacks) {
      // Trigger delayed parsing, no need to continue.
      return Status;
    }
  }

  // No need to setLocalDiscriminator.

  // Pass the function signature to code completion.
  if (Status.hasCodeCompletion()) {
    assert(CodeCompletionCallbacks && "must be code completion second pass");
    CodeCompletionCallbacks->setParsedDecl(CD);
  }

  if (ConstructorsNotAllowed) {
    // Tell the type checker not to touch this constructor.
    CD->setInvalid();
  }

  if (Flags.contains(PD_InProtocol)) {
    if (Tok.is(tok::l_brace)) {
      diagnose(Tok, diag::protocol_init_with_body);
      skipSingle();
    }
  } else if(!Status.hasCodeCompletion()) {
    parseAbstractFunctionBody(CD, Flags);
  }

  return makeParserResult(Status, CD);
}

ParserResult<DestructorDecl> Parser::
parseDeclDeinit(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  SourceLoc DestructorLoc = consumeToken(tok::kw_deinit);

  // Parse extraneous parentheses and remove them with a fixit.
  auto skipParameterListIfPresent = [this] {
    SourceLoc LParenLoc;
    if (!consumeIf(tok::l_paren, LParenLoc))
      return;
    SourceLoc RParenLoc;
    skipUntil(tok::r_paren);

    if (Tok.is(tok::r_paren)) {
      SourceLoc RParenLoc = consumeToken();
      diagnose(LParenLoc, diag::destructor_params)
        .fixItRemove(SourceRange(LParenLoc, RParenLoc));
    } else {
      diagnose(Tok, diag::opened_destructor_expected_rparen);
      diagnose(LParenLoc, diag::opening_paren);
    }
  };

  // '{'
  if (!Tok.is(tok::l_brace)) {
    switch (SF.Kind) {
    case SourceFileKind::Interface:
    case SourceFileKind::SIL:
      // It's okay to have no body for SIL code or module interfaces.
      break;
    case SourceFileKind::Library:
    case SourceFileKind::Main:
    case SourceFileKind::MacroExpansion:
    case SourceFileKind::DefaultArgument:
      if (Tok.is(tok::identifier)) {
        diagnose(Tok, diag::destructor_has_name).fixItRemove(Tok.getLoc());
        consumeToken();
      }
      skipParameterListIfPresent();
      if (Tok.is(tok::l_brace))
        break;

      diagnose(Tok, diag::expected_lbrace_destructor);
      return nullptr;
    }
  }

  auto *DD = new (Context) DestructorDecl(DestructorLoc, CurDeclContext);
  parseAbstractFunctionBody(DD, Flags);

  DD->attachParsedAttrs(Attributes);

  // Reject 'destructor' functions outside of structs, enums, classes, or
  // extensions that provide objc implementations.
  //
  // Later in the type checker, we validate that structs/enums are noncopyable
  // and that @objcImplementations are main-body.
  auto rejectDestructor = [](DeclContext *dc) {
    if (isa<StructDecl>(dc) || isa<EnumDecl>(dc) ||
        isa<ClassDecl>(dc))
      return false;

    if (auto *ED = dyn_cast<ExtensionDecl>(dc))
      return !ED->isObjCImplementation();

    return true;
  };
  if (rejectDestructor(CurDeclContext)) {
    diagnose(DestructorLoc, diag::destructor_decl_outside_class_or_noncopyable);

    // Tell the type checker not to touch this destructor.
    DD->setInvalid();
  }

  return makeParserResult(DD);
}

ParserResult<OperatorDecl> 
Parser::parseDeclOperator(ParseDeclOptions Flags, DeclAttributes &Attributes) {
  SourceLoc OperatorLoc = consumeToken(tok::kw_operator);
  bool AllowTopLevel = Flags.contains(PD_AllowTopLevel);

  const auto maybeDiagnoseInvalidCharInOperatorName = [this](const Token &Tk) {
    if (Tk.is(tok::identifier)) {
      if (Tk.getText() == "$" ||
          !DeclAttribute::getAttrKindFromString(Tk.getText())) {
        diagnose(Tk, diag::identifier_within_operator_name, Tk.getText());
        return true;
      }
    } else if (Tk.isNot(tok::colon, tok::l_brace, tok::semi) &&
               Tk.isPunctuation()) {
      diagnose(Tk, diag::operator_name_invalid_char,
               Tk.getText().take_front());
      return true;
    }
    return false;
  };

  // Postfix operators starting with ? or ! conflict with builtin
  // unwrapping operators.
  if (Attributes.hasAttribute<PostfixAttr>())
    if (!Tok.getText().empty() && (Tok.getRawText().front() == '?' ||
                                   Tok.getRawText().front() == '!'))
      diagnose(Tok, diag::postfix_operator_name_cannot_start_with_unwrap);

  // A common error is to try to define an operator with something in the
  // unicode plane considered to be an operator, or to try to define an
  // operator like "not".  Analyze and diagnose this specifically.
  if (Tok.isAnyOperator() ||
      Tok.isAny(tok::exclaim_postfix, tok::question_infix,
                tok::question_postfix, tok::equal, tok::arrow)) {
    if (peekToken().getLoc() == Tok.getRange().getEnd() &&
      maybeDiagnoseInvalidCharInOperatorName(peekToken())) {
      consumeToken();

      // If there's a deprecated body, skip it to improve recovery.
      if (peekToken().is(tok::l_brace)) {
        consumeToken();
        skipSingle();
      }
      return nullptr;
    }
  } else {
    if (maybeDiagnoseInvalidCharInOperatorName(Tok)) {
      // We're done diagnosing.
    } else {
      diagnose(Tok, diag::expected_operator_name_after_operator);
    }

    // If there's a deprecated body, skip it to improve recovery.
    if (peekToken().is(tok::l_brace)) {
      consumeToken();
      skipSingle();
    }
    return nullptr;
  }

  DebuggerContextChange DCC (*this);

  Identifier Name = Context.getIdentifier(Tok.getText());
  SourceLoc NameLoc = consumeToken();

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

  // Parse (or diagnose) a specified precedence group and/or
  // designated protocol. These both look like identifiers, so we
  // parse them both as identifiers here and sort it out in type
  // checking.
  SourceLoc colonLoc, groupLoc;
  Identifier groupName;
  if (Tok.is(tok::colon)) {
    colonLoc = consumeToken();
    if (Tok.is(tok::code_complete)) {
      if (CodeCompletionCallbacks && !isPrefix && !isPostfix) {
        CodeCompletionCallbacks->completeInPrecedenceGroup(
            CodeCompletionCallbacks::PrecedenceGroupCompletionKind::Relation);
      }
      consumeToken();

      return makeParserCodeCompletionResult<OperatorDecl>();
    }

    (void)parseIdentifier(groupName, groupLoc,
                          diag::operator_decl_expected_precedencegroup,
                          /*diagnoseDollarPrefix=*/false);

    if (Context.TypeCheckerOpts.EnableOperatorDesignatedTypes) {
      // Designated types have been removed; consume the list (mainly for source
      // compatibility with old swiftinterfaces) and emit a warning.

      // These SourceLocs point to the ends of the designated type list. If
      // `typesEndLoc` never becomes valid, we didn't find any designated types.
      SourceLoc typesStartLoc = Tok.getLoc();
      SourceLoc typesEndLoc;

      if (isPrefix || isPostfix) {
        // These have no precedence group, so we already parsed the first entry
        // in the designated types list. Retroactively include it in the range.
        typesStartLoc = colonLoc;
        typesEndLoc = groupLoc;
      }

      while (Tok.isNot(tok::eof)) {
        if (!consumeIf(tok::comma, typesEndLoc)) {
          break;
        }

        if (Tok.isNot(tok::eof)) {
          typesEndLoc = consumeToken();
        }
      }

      if (typesEndLoc.isValid())
        diagnose(typesStartLoc, diag::operator_decl_remove_designated_types)
            .fixItRemove({typesStartLoc, typesEndLoc});
    } else {
      if (isPrefix || isPostfix) {
        // If we have nothing after the colon, then just remove the colon.
        auto endLoc = groupLoc.isValid() ? groupLoc : colonLoc;
        diagnose(colonLoc, diag::precedencegroup_not_infix)
            .fixItRemove({colonLoc, endLoc});
      }
      // Nothing to complete here, simply consume the token.
      if (Tok.is(tok::code_complete))
        consumeToken();
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
        SourceLoc lastGoodLoc = groupLoc.isValid() ? groupLoc : NameLoc;
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
  if (isPrefix)
    res = new (Context)
        PrefixOperatorDecl(CurDeclContext, OperatorLoc, Name, NameLoc);
  else if (isPostfix)
    res = new (Context)
        PostfixOperatorDecl(CurDeclContext, OperatorLoc, Name, NameLoc);
  else
    res = new (Context)
        InfixOperatorDecl(CurDeclContext, OperatorLoc, Name, NameLoc, colonLoc,
                          groupName, groupLoc);

  diagnoseOperatorFixityAttributes(*this, Attributes, res);

  res->attachParsedAttrs(Attributes);
  return makeParserResult(res);
}

ParserResult<PrecedenceGroupDecl>
Parser::parseDeclPrecedenceGroup(ParseDeclOptions flags,
                                 DeclAttributes &attributes) {
  SourceLoc precedenceGroupLoc = consumeToken(tok::kw_precedencegroup);
  DebuggerContextChange DCC (*this);

  if (!CodeCompletionCallbacks && !DCC.movedToTopLevel() &&
      !(flags & PD_AllowTopLevel)) {
    diagnose(precedenceGroupLoc, diag::decl_inner_scope);
    return nullptr;
  }

  Identifier name;
  SourceLoc nameLoc;
  if (parseIdentifier(name, nameLoc, /*diagnoseDollarPrefix=*/true,
                      diag::expected_precedencegroup_name)) {
    // If the identifier is missing or a keyword or something, try to
    // skip the entire body.
    if (!Tok.isAtStartOfLine() && Tok.isNot(tok::eof) &&
         peekToken().is(tok::l_brace))
      consumeToken();
    if (Tok.is(tok::l_brace)) {
      consumeToken(tok::l_brace);
      skipUntilDeclRBrace();
      (void) consumeIf(tok::r_brace);
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
  bool hasCodeCompletion = false;

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
    result->attachParsedAttrs(attributes);
    return result;
  };
  auto createInvalid = [&](bool hasCodeCompletion) {
    // Use the last consumed token location as the rbrace to satisfy
    // the AST invariant about a decl's source range including all of
    // its components.
    if (!rbraceLoc.isValid()) rbraceLoc = PreviousLoc;

    auto result = create();
    result->setInvalid();
    if (hasCodeCompletion)
      return makeParserCodeCompletionResult(result);
    return makeParserErrorResult(result);
  };

  // Expect the body to start here.
  if (!consumeIf(tok::l_brace, lbraceLoc)) {
    diagnose(Tok, diag::expected_precedencegroup_lbrace);
    return createInvalid(/*hasCodeCompletion*/false);
  }
  // Empty body.
  if (Tok.is(tok::r_brace)) {
    // Create empty attribute list.
    rbraceLoc = consumeToken(tok::r_brace);
    return makeParserResult(create());
  }

  auto abortBody = [&](bool hasCodeCompletion = false) {
    skipUntilDeclRBrace();
    (void) consumeIf(tok::r_brace, rbraceLoc);
    return createInvalid(hasCodeCompletion);
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

  auto checkCodeCompletion =
      [&](CodeCompletionCallbacks::PrecedenceGroupCompletionKind SK) -> bool {
    if (Tok.is(tok::code_complete)) {
      if (this->CodeCompletionCallbacks)
        this->CodeCompletionCallbacks->completeInPrecedenceGroup(SK);
      consumeToken();
      return true;
    }
    return false;
  };

  // Skips the CC token if it comes without spacing.
  auto skipUnspacedCodeCompleteToken = [&]() -> bool {
    if (Tok.is(tok::code_complete) && getEndOfPreviousLoc() == Tok.getLoc()) {
       consumeToken();
      return true;
    }
    return false;
  };

  // Parse the attributes in the body.
  while (Tok.isNot(tok::r_brace)) {
    if (checkCodeCompletion(CodeCompletionCallbacks::
                                PrecedenceGroupCompletionKind::AttributeList)) {
      hasCodeCompletion = true;
      continue;
    } else if (Tok.isNot(tok::identifier)) {
      diagnose(Tok, diag::expected_precedencegroup_attribute);
      return abortBody();
    }
    auto attrName = Tok.getText();

    if (attrName == "associativity") {
      // "associativity" is considered as a contextual keyword.
      TokReceiver->registerTokenKindChange(Tok.getLoc(),
                                           tok::contextual_keyword);
      parseAttributePrefix(associativityKeywordLoc);

      if (checkCodeCompletion(CodeCompletionCallbacks::
                                  PrecedenceGroupCompletionKind::Associativity))
        return abortBody(/*hasCodeCompletion*/true);

      if (Tok.isNot(tok::identifier)) {
        diagnose(Tok, diag::expected_precedencegroup_associativity);
        return abortBody();
      }

      auto parsedAssociativity =
          llvm::StringSwitch<std::optional<Associativity>>(Tok.getText())
              .Case("none", Associativity::None)
              .Case("left", Associativity::Left)
              .Case("right", Associativity::Right)
              .Default(std::nullopt);

      if (!parsedAssociativity) {
        diagnose(Tok, diag::expected_precedencegroup_associativity);
        parsedAssociativity = Associativity::None;
        invalid = true;
      } else {
        // "left", "right" or "none" are considered contextual keywords.
        TokReceiver->registerTokenKindChange(Tok.getLoc(),
                                             tok::contextual_keyword);
      }
      associativity = *parsedAssociativity;
      associativityValueLoc = consumeToken();

      if (skipUnspacedCodeCompleteToken())
        return abortBody(/*hasCodeCompletion*/true);
      continue;
    }
    
    if (attrName == "assignment") {
      parseAttributePrefix(assignmentKeywordLoc);

      // "assignment" is considered as a contextual keyword.
      TokReceiver->registerTokenKindChange(assignmentKeywordLoc,
                                           tok::contextual_keyword);
      if (checkCodeCompletion(CodeCompletionCallbacks::
                                  PrecedenceGroupCompletionKind::Assignment))
        return abortBody(/*hasCodeCompletion*/true);

      if (consumeIf(tok::kw_true, assignmentValueLoc)) {
        assignment = true;
      } else if (consumeIf(tok::kw_false, assignmentValueLoc)) {
        assignment = false;
      } else {
        diagnose(Tok, diag::expected_precedencegroup_assignment);
        return abortBody();
      }
      if (skipUnspacedCodeCompleteToken())
        return abortBody(/*hasCodeCompletion*/true);
      continue;
    }

    bool isLowerThan = false;
    if (attrName == "higherThan" ||
        (isLowerThan = (attrName == "lowerThan"))) {
      // "lowerThan" and "higherThan" are contextual keywords.
      TokReceiver->registerTokenKindChange(Tok.getLoc(),
                                                  tok::contextual_keyword);
      parseAttributePrefix(isLowerThan ? lowerThanKeywordLoc
                                       : higherThanKeywordLoc);
      auto &relations = (isLowerThan ? lowerThan : higherThan);

      do {
        if (checkCodeCompletion(CodeCompletionCallbacks::
                                    PrecedenceGroupCompletionKind::Relation)) {
          return abortBody(/*hasCodeCompletion*/true);
        }

        if (Tok.isNot(tok::identifier)) {
          diagnose(Tok, diag::expected_precedencegroup_relation, attrName);
          return abortBody();
        }
        Identifier name;
        SourceLoc nameLoc = consumeIdentifier(name,
                                              /*diagnoseDollarPrefix=*/false);
        relations.push_back({nameLoc, name, nullptr});

        if (skipUnspacedCodeCompleteToken())
          return abortBody(/*hasCodeCompletion*/true);
        if (!consumeIf(tok::comma))
          break;
      } while (true);
      continue;
    }

    diagnose(Tok, diag::unknown_precedencegroup_attribute, attrName);
    return abortBody();
  }
  rbraceLoc = consumeToken(tok::r_brace);

  auto result = create();
  if (invalid) result->setInvalid();
  if (hasCodeCompletion)
    return makeParserCodeCompletionResult(result);
  return makeParserResult(result);
}

ParserResult<MacroDecl> Parser::parseDeclMacro(DeclAttributes &attributes) {
  assert(Tok.isContextualKeyword("macro"));
  SourceLoc macroLoc = consumeToken(); // 'macro'

  Identifier macroName;
  SourceLoc macroNameLoc;
  ParserStatus status;

  status |= parseIdentifierDeclName(
      *this, macroName, macroNameLoc, "macro",
      [&](const Token &next) {
        return next.isAny(tok::colon, tok::l_paren) || startsWithLess(next);
      });
  if (status.isErrorOrHasCompletion())
    return status;

  DebuggerContextChange dcc (*this, macroName, DeclKind::Macro);

  // Parse the generic-params, if present.
  GenericParamList *genericParams = nullptr;
  {
    auto result = maybeParseGenericParams();
    genericParams = result.getPtrOrNull();
    if (result.hasCodeCompletion())
      return makeParserCodeCompletionStatus();
  }

  // Parse the macro signature.
  ParameterList *parameterList = nullptr;
  SourceLoc arrowLoc;
  TypeRepr *resultType = nullptr;
  DeclName macroFullName;

  // Parameter list.
  DefaultArgumentInfo defaultArgs;
  SmallVector<Identifier, 2> namePieces;
  auto parameterResult = parseSingleParameterClause(
      ParameterContextKind::Macro, &namePieces, &defaultArgs);
  status |= parameterResult;
  parameterList = parameterResult.getPtrOrNull();

  // ->
  if (consumeIf(tok::arrow, arrowLoc)) {
    // Result type.
    auto parsedResultType =
        parseDeclResultType(diag::expected_type_macro_result);
    resultType = parsedResultType.getPtrOrNull();
    status |= parsedResultType;
    if (status.isErrorOrHasCompletion())
      return status;
  }

  macroFullName = DeclName(Context, macroName, namePieces);

  // Parse '=' <expression>
  Expr *definition = nullptr;
  if (consumeIf(tok::equal)) {
    ParserResult<Expr> parsedDefinition =
        parseExpr(diag::macro_decl_expected_macro_definition);
    status |= parsedDefinition;

    definition = parsedDefinition.getPtrOrNull();
  }

  // Create the macro declaration.
  auto *macro = new (Context) MacroDecl(
      macroLoc, macroFullName, macroNameLoc, genericParams, parameterList,
      arrowLoc, resultType, definition, CurDeclContext);
  macro->attachParsedAttrs(attributes);

  // Parse a 'where' clause if present.
  if (Tok.is(tok::kw_where)) {
    auto whereStatus = parseFreestandingGenericWhereClause(macro);
    if (whereStatus.hasCodeCompletion() && !CodeCompletionCallbacks) {
      // Trigger delayed parsing, no need to continue.
      return whereStatus;
    }
    status |= whereStatus;
  }

  return dcc.fixupParserResult(status, macro);
}

ParserResult<MacroExpansionDecl>
Parser::parseDeclMacroExpansion(ParseDeclOptions flags,
                                DeclAttributes &attributes) {
  SourceLoc poundLoc;
  DeclNameLoc macroNameLoc;
  DeclNameRef macroNameRef;
  SourceLoc leftAngleLoc, rightAngleLoc;
  SmallVector<TypeRepr *, 4> genericArgs;
  ArgumentList *argList = nullptr;
  ParserStatus status = parseFreestandingMacroExpansion(
      poundLoc, macroNameLoc, macroNameRef, leftAngleLoc, genericArgs,
      rightAngleLoc, argList, /*isExprBasic=*/false,
      diag::macro_expansion_decl_expected_macro_identifier);
  if (!macroNameRef)
    return status;

  auto *med = MacroExpansionDecl::create(
      CurDeclContext, poundLoc, macroNameRef, macroNameLoc, leftAngleLoc,
      Context.AllocateCopy(genericArgs), rightAngleLoc, argList);
  med->attachParsedAttrs(attributes);

  return makeParserResult(status, med);
}

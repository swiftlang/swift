//===--- Refactoring.cpp ---------------------------------------------------===//
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

#include "swift/IDE/Refactoring.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsRefactoring.h"
#include "swift/AST/Expr.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Edit.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Index/Index.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace swift;
using namespace swift::ide;
using namespace swift::index;

namespace {
class ContextFinder : public SourceEntityWalker {
  SourceFile &SF;
  ASTContext &Ctx;
  SourceManager &SM;
  SourceRange Target;
  llvm::function_ref<bool(ASTNode)> IsContext;
  SmallVector<ASTNode, 4> AllContexts;
  bool contains(ASTNode Enclosing) {
    auto Result = SM.rangeContains(Enclosing.getSourceRange(), Target);
    if (Result && IsContext(Enclosing))
      AllContexts.push_back(Enclosing);
    return Result;
  }
public:
  ContextFinder(SourceFile &SF, ASTNode TargetNode,
                llvm::function_ref<bool(ASTNode)> IsContext =
                  [](ASTNode N) { return true; }) :
                  SF(SF), Ctx(SF.getASTContext()), SM(Ctx.SourceMgr),
                  Target(TargetNode.getSourceRange()), IsContext(IsContext) {}
  ContextFinder(SourceFile &SF, SourceLoc TargetLoc,
                llvm::function_ref<bool(ASTNode)> IsContext =
                  [](ASTNode N) { return true; }) :
                  SF(SF), Ctx(SF.getASTContext()), SM(Ctx.SourceMgr),
                  Target(TargetLoc), IsContext(IsContext) {
                    assert(TargetLoc.isValid() && "Invalid loc to find");
                  }
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override { return contains(D); }
  bool walkToStmtPre(Stmt *S) override { return contains(S); }
  bool walkToExprPre(Expr *E) override { return contains(E); }
  void resolve() { walk(SF); }
  llvm::ArrayRef<ASTNode> getContexts() const {
    return llvm::makeArrayRef(AllContexts);
  }
};

class Renamer {
protected:
  const SourceManager &SM;

protected:
  Renamer(const SourceManager &SM, StringRef OldName) : SM(SM), Old(OldName) {}

  // Implementor's interface.
  virtual void doRenameLabel(CharSourceRange Label,
                             RefactoringRangeKind RangeKind,
                             unsigned NameIndex) = 0;
  virtual void doRenameBase(CharSourceRange Range,
                            RefactoringRangeKind RangeKind) = 0;

public:
  const DeclNameViewer Old;

public:
  virtual ~Renamer() {}

  /// Adds a replacement to rename the given base name range
  /// \return true if the given range does not match the old name
  bool renameBase(CharSourceRange Range, RefactoringRangeKind RangeKind) {
    assert(Range.isValid());

    StringRef Existing = Range.str();
    if (Existing != Old.base())
      return true;
    doRenameBase(Range, RangeKind);
    return false;
  }

  /// Adds replacements to rename the given label ranges
  /// \return true if the label ranges do not match the old name
  bool renameLabels(ArrayRef<CharSourceRange> LabelRanges,
                    LabelRangeType RangeType, bool isCallSite) {
    if (isCallSite)
      return renameLabelsLenient(LabelRanges, RangeType);

    ArrayRef<StringRef> OldLabels = Old.args();

    if (OldLabels.size() != LabelRanges.size())
      return true;

    size_t Index = 0;
    for (const auto &LabelRange : LabelRanges) {
      assert(LabelRange.isValid());
      if (!labelRangeMatches(LabelRange, RangeType, OldLabels[Index]))
        return true;
      splitAndRenameLabel(LabelRange, RangeType, Index++);
    }
    return false;
  }

  bool isOperator() const { return Lexer::isOperator(Old.base()); }

private:
  void splitAndRenameLabel(CharSourceRange Range, LabelRangeType RangeType,
                           size_t NameIndex) {
    switch (RangeType) {
    case LabelRangeType::CallArg:
      return splitAndRenameCallArg(Range, NameIndex);
    case LabelRangeType::Param:
      return splitAndRenameParamLabel(Range, NameIndex, /*IsCollapsible=*/true);
    case LabelRangeType::NoncollapsibleParam:
      return splitAndRenameParamLabel(Range, NameIndex, /*IsCollapsible=*/false);
    case LabelRangeType::Selector:
      return doRenameLabel(
          Range, RefactoringRangeKind::SelectorArgumentLabel, NameIndex);
    case LabelRangeType::None:
      llvm_unreachable("expected a label range");
    }
  }

  void splitAndRenameParamLabel(CharSourceRange Range, size_t NameIndex, bool IsCollapsible) {
    // Split parameter range foo([a b]: Int) into decl argument label [a] and
    // parameter name [b] or noncollapsible parameter name [b] if IsCollapsible
    // is false (as for subscript decls). If we have only foo([a]: Int), then we
    // add an empty range for the local name, or for the decl argument label if
    // IsCollapsible is false.
    StringRef Content = Range.str();
    size_t ExternalNameEnd = Content.find_first_of(" \t\n\v\f\r/");

    if (ExternalNameEnd == StringRef::npos) { // foo([a]: Int)
      if (IsCollapsible) {
        doRenameLabel(Range, RefactoringRangeKind::DeclArgumentLabel, NameIndex);
        doRenameLabel(CharSourceRange{Range.getEnd(), 0},
                      RefactoringRangeKind::ParameterName, NameIndex);
      } else {
        doRenameLabel(CharSourceRange{Range.getStart(), 0},
                      RefactoringRangeKind::DeclArgumentLabel, NameIndex);
        doRenameLabel(Range, RefactoringRangeKind::NoncollapsibleParameterName,
                      NameIndex);
      }
    } else { // foo([a b]: Int)
      CharSourceRange Ext{Range.getStart(), unsigned(ExternalNameEnd)};

      // Note: we consider the leading whitespace part of the parameter name
      // if the parameter is collapsible, since if the parameter is collapsed
      // into a matching argument label, we want to remove the whitespace too.
      // FIXME: handle comments foo(a /*...*/b: Int).
      size_t LocalNameStart = Content.find_last_of(" \t\n\v\f\r/");
      assert(LocalNameStart != StringRef::npos);
      if (!IsCollapsible)
        ++LocalNameStart;
      auto LocalLoc = Range.getStart().getAdvancedLocOrInvalid(LocalNameStart);
      CharSourceRange Local{LocalLoc, unsigned(Content.size() - LocalNameStart)};

      doRenameLabel(Ext, RefactoringRangeKind::DeclArgumentLabel, NameIndex);
      if (IsCollapsible) {
        doRenameLabel(Local, RefactoringRangeKind::ParameterName, NameIndex);
      } else {
        doRenameLabel(Local, RefactoringRangeKind::NoncollapsibleParameterName, NameIndex);
      }
    }
  }

  void splitAndRenameCallArg(CharSourceRange Range, size_t NameIndex) {
    // Split call argument foo([a: ]1) into argument name [a] and the remainder
    // [: ].
    StringRef Content = Range.str();
    size_t Colon = Content.find(':'); // FIXME: leading whitespace?
    if (Colon == StringRef::npos) {
      assert(Content.empty());
      doRenameLabel(Range, RefactoringRangeKind::CallArgumentCombined,
                    NameIndex);
      return;
    }

    // Include any whitespace before the ':'.
    assert(Colon == Content.substr(0, Colon).size());
    Colon = Content.substr(0, Colon).rtrim().size();

    CharSourceRange Arg{Range.getStart(), unsigned(Colon)};
    doRenameLabel(Arg, RefactoringRangeKind::CallArgumentLabel, NameIndex);

    auto ColonLoc = Range.getStart().getAdvancedLocOrInvalid(Colon);
    assert(ColonLoc.isValid());
    CharSourceRange Rest{ColonLoc, unsigned(Content.size() - Colon)};
    doRenameLabel(Rest, RefactoringRangeKind::CallArgumentColon, NameIndex);
  }

  bool labelRangeMatches(CharSourceRange Range, LabelRangeType RangeType, StringRef Expected) {
    if (Range.getByteLength()) {
      CharSourceRange ExistingLabelRange =
          Lexer::getCharSourceRangeFromSourceRange(SM, Range.getStart());
      StringRef ExistingLabel = ExistingLabelRange.str();

      switch (RangeType) {
      case LabelRangeType::NoncollapsibleParam:
        if (ExistingLabelRange == Range && Expected.empty()) // subscript([x]: Int)
          return true;
        LLVM_FALLTHROUGH;
      case LabelRangeType::CallArg:
      case LabelRangeType::Param:
      case LabelRangeType::Selector:
        return ExistingLabel == (Expected.empty() ? "_" : Expected);
      case LabelRangeType::None:
        llvm_unreachable("Unhandled label range type");
      }
    }
    return Expected.empty();
  }

  bool renameLabelsLenient(ArrayRef<CharSourceRange> LabelRanges,
                           LabelRangeType RangeType) {

    ArrayRef<StringRef> OldNames = Old.args();

    size_t NameIndex = 0;

    for (CharSourceRange Label : LabelRanges) {
      // empty label
      if (!Label.getByteLength()) {

        // first name pos
        if (!NameIndex) {
          while (!OldNames[NameIndex].empty()) {
            if (++NameIndex >= OldNames.size())
              return true;
          }
          splitAndRenameLabel(Label, RangeType, NameIndex++);
          continue;
        }

        // other name pos
        if (NameIndex >= OldNames.size() || !OldNames[NameIndex].empty()) {
          // FIXME: only allow one variadic param
          continue; // allow for variadic
        }
        splitAndRenameLabel(Label, RangeType, NameIndex++);
        continue;
      }

      // non-empty label
      if (NameIndex >= OldNames.size())
        return true;

      while (!labelRangeMatches(Label, RangeType, OldNames[NameIndex])) {
        if (++NameIndex >= OldNames.size())
          return true;
      };
      splitAndRenameLabel(Label, RangeType, NameIndex++);
    }
    return false;
  }

  static RegionType getSyntacticRenameRegionType(const ResolvedLoc &Resolved) {
    if (Resolved.Node.isNull())
      return RegionType::Comment;

    if (Expr *E = Resolved.Node.getAsExpr()) {
      if (isa<StringLiteralExpr>(E))
        return RegionType::String;
    }
    if (Resolved.IsInSelector)
      return RegionType::Selector;
    if (Resolved.IsActive)
      return RegionType::ActiveCode;
    return RegionType::InactiveCode;
  }

public:
  RegionType addSyntacticRenameRanges(const ResolvedLoc &Resolved,
                                      const RenameLoc &Config) {

    if (!Resolved.Range.isValid())
      return RegionType::Unmatched;

    auto RegionKind = getSyntacticRenameRegionType(Resolved);
    // Don't include unknown references coming from active code; if we don't
    // have a semantic NameUsage for them, then they're likely unrelated symbols
    // that happen to have the same name.
    if (RegionKind == RegionType::ActiveCode &&
        Config.Usage == NameUsage::Unknown)
      return RegionType::Unmatched;

    assert(Config.Usage != NameUsage::Call || Config.IsFunctionLike);

    // FIXME: handle escaped keyword names `init`
    bool IsSubscript = Old.base() == "subscript" && Config.IsFunctionLike;
    bool IsInit = Old.base() == "init" && Config.IsFunctionLike;
    bool IsKeywordBase = IsInit || IsSubscript;
    
    // Filter out non-semantic keyword basename locations with no labels.
    // We've already filtered out those in active code, so these are
    // any appearance of just 'init' or 'subscript' in strings, comments, and
    // inactive code.
    if (IsKeywordBase && (Config.Usage == NameUsage::Unknown &&
                           Resolved.LabelType == LabelRangeType::None))
      return RegionType::Unmatched;

    if (!Config.IsFunctionLike || !IsKeywordBase) {
      if (renameBase(Resolved.Range, RefactoringRangeKind::BaseName))
        return RegionType::Mismatch;

    } else if (IsKeywordBase && Config.Usage == NameUsage::Definition) {
      if (renameBase(Resolved.Range, RefactoringRangeKind::KeywordBaseName))
        return RegionType::Mismatch;
    }

    bool HandleLabels = false;
    if (Config.IsFunctionLike) {
      switch (Config.Usage) {
      case NameUsage::Call:
        HandleLabels = !isOperator();
        break;
      case NameUsage::Definition:
        HandleLabels = true;
        break;
      case NameUsage::Reference:
        HandleLabels = Resolved.LabelType == LabelRangeType::Selector || IsSubscript;
        break;
      case NameUsage::Unknown:
        HandleLabels = Resolved.LabelType != LabelRangeType::None;
        break;
      }
    } else if (Resolved.LabelType != LabelRangeType::None &&
               !Config.IsNonProtocolType &&
               // FIXME: Workaround for enum case labels until we support them
               Config.Usage != NameUsage::Definition) {
      return RegionType::Mismatch;
    }

    if (HandleLabels) {
      bool isCallSite = Config.Usage != NameUsage::Definition &&
                        (Config.Usage != NameUsage::Reference || IsSubscript) &&
                        Resolved.LabelType == LabelRangeType::CallArg;

      if (renameLabels(Resolved.LabelRanges, Resolved.LabelType, isCallSite))
        return Config.Usage == NameUsage::Unknown ?
            RegionType::Unmatched : RegionType::Mismatch;
    }

    return RegionKind;
  }
};

class RenameRangeDetailCollector : public Renamer {
  void doRenameLabel(CharSourceRange Label, RefactoringRangeKind RangeKind,
                     unsigned NameIndex) override {
    Ranges.push_back({Label, RangeKind, NameIndex});
  }
  void doRenameBase(CharSourceRange Range,
                    RefactoringRangeKind RangeKind) override {
    Ranges.push_back({Range, RangeKind, None});
  }

public:
  RenameRangeDetailCollector(const SourceManager &SM, StringRef OldName)
      : Renamer(SM, OldName) {}
  std::vector<RenameRangeDetail> Ranges;
};

class TextReplacementsRenamer : public Renamer {
  llvm::StringSet<> &ReplaceTextContext;
  std::vector<Replacement> Replacements;

public:
  const DeclNameViewer New;

private:
  StringRef registerText(StringRef Text) {
    if (Text.empty())
      return Text;
    return ReplaceTextContext.insert(Text).first->getKey();
  }

  StringRef getCallArgLabelReplacement(StringRef OldLabelRange,
                                       StringRef NewLabel) {
    return NewLabel.empty() ? "" : NewLabel;
  }

  StringRef getCallArgColonReplacement(StringRef OldLabelRange,
                                       StringRef NewLabel) {
    // Expected OldLabelRange: foo( []3, a[: ]2,  b[ : ]3 ...)
    // FIXME: Preserve comments: foo([a/*:*/ : /*:*/ ]2, ...)
    if (NewLabel.empty())
      return "";
    if (OldLabelRange.empty())
      return ": ";
    return registerText(OldLabelRange);
  }

  StringRef getCallArgCombinedReplacement(StringRef OldArgLabel,
                                          StringRef NewArgLabel) {
    // This case only happens when going from foo([]1) to foo([a: ]1).
    assert(OldArgLabel.empty());
    if (NewArgLabel.empty())
      return "";
    return registerText((llvm::Twine(NewArgLabel) + ": ").str());
  }

  StringRef getParamNameReplacement(StringRef OldParam, StringRef OldArgLabel,
                                    StringRef NewArgLabel) {
    // We don't want to get foo(a a: Int), so drop the parameter name if the
    // argument label will match the original name.
    // Note: the leading whitespace is part of the parameter range.
    if (!NewArgLabel.empty() && OldParam.ltrim() == NewArgLabel)
      return "";

    // If we're renaming foo(x: Int) to foo(_:), then use the original argument
    // label as the parameter name so as to not break references in the body.
    if (NewArgLabel.empty() && !OldArgLabel.empty() && OldParam.empty())
      return registerText((llvm::Twine(" ") + OldArgLabel).str());

    return registerText(OldParam);
  }

  StringRef getDeclArgumentLabelReplacement(StringRef OldLabelRange,
                                            StringRef NewArgLabel) {
      // OldLabelRange is subscript([]a: Int), foo([a]: Int) or foo([a] b: Int)
      if (NewArgLabel.empty())
        return OldLabelRange.empty() ? "" : "_";

      if (OldLabelRange.empty())
        return registerText((llvm::Twine(NewArgLabel) + " ").str());
      return registerText(NewArgLabel);
  }

  StringRef getReplacementText(StringRef LabelRange,
                               RefactoringRangeKind RangeKind,
                               StringRef OldLabel, StringRef NewLabel) {
    switch (RangeKind) {
    case RefactoringRangeKind::CallArgumentLabel:
      return getCallArgLabelReplacement(LabelRange, NewLabel);
    case RefactoringRangeKind::CallArgumentColon:
      return getCallArgColonReplacement(LabelRange, NewLabel);
    case RefactoringRangeKind::CallArgumentCombined:
      return getCallArgCombinedReplacement(LabelRange, NewLabel);
    case RefactoringRangeKind::ParameterName:
      return getParamNameReplacement(LabelRange, OldLabel, NewLabel);
    case RefactoringRangeKind::NoncollapsibleParameterName:
      return LabelRange;
    case RefactoringRangeKind::DeclArgumentLabel:
      return getDeclArgumentLabelReplacement(LabelRange, NewLabel);
    case RefactoringRangeKind::SelectorArgumentLabel:
      return NewLabel.empty() ? "_" : registerText(NewLabel);
    default:
      llvm_unreachable("label range type is none but there are labels");
    }
  }

  void addReplacement(CharSourceRange LabelRange,
                      RefactoringRangeKind RangeKind, StringRef OldLabel,
                      StringRef NewLabel) {
    StringRef ExistingLabel = LabelRange.str();
    StringRef Text =
        getReplacementText(ExistingLabel, RangeKind, OldLabel, NewLabel);
    if (Text != ExistingLabel)
      Replacements.push_back({LabelRange, Text, {}});
  }

  void doRenameLabel(CharSourceRange Label, RefactoringRangeKind RangeKind,
                     unsigned NameIndex) override {
    addReplacement(Label, RangeKind, Old.args()[NameIndex],
                   New.args()[NameIndex]);
  }

  void doRenameBase(CharSourceRange Range, RefactoringRangeKind) override {
    if (Old.base() != New.base())
      Replacements.push_back({Range, registerText(New.base()), {}});
  }

public:
  TextReplacementsRenamer(const SourceManager &SM, StringRef OldName,
                          StringRef NewName,
                          llvm::StringSet<> &ReplaceTextContext)
      : Renamer(SM, OldName), ReplaceTextContext(ReplaceTextContext),
        New(NewName) {
    assert(Old.isValid() && New.isValid());
    assert(Old.partsCount() == New.partsCount());
  }

  std::vector<Replacement> getReplacements() const {
    return std::move(Replacements);
  }
};

static const ValueDecl *getRelatedSystemDecl(const ValueDecl *VD) {
  if (VD->getModuleContext()->isSystemModule())
    return VD;
  for (auto *Req : VD->getSatisfiedProtocolRequirements()) {
    if (Req->getModuleContext()->isSystemModule())
      return Req;
  }
  for (auto Over = VD->getOverriddenDecl(); Over;
       Over = Over->getOverriddenDecl()) {
    if (Over->getModuleContext()->isSystemModule())
      return Over;
  }
  return nullptr;
}

static Optional<RefactoringKind> getAvailableRenameForDecl(const ValueDecl *VD) {
  std::vector<RenameAvailabiliyInfo> Scratch;
  for (auto &Info : collectRenameAvailabilityInfo(VD, Scratch)) {
    if (Info.AvailableKind == RenameAvailableKind::Available)
      return Info.Kind;
  }
  return None;
}

class RenameRangeCollector : public IndexDataConsumer {
public:
  RenameRangeCollector(StringRef USR, StringRef newName)
      : USR(USR.str()), newName(newName.str()) {}

  RenameRangeCollector(const ValueDecl *D, StringRef newName)
      : newName(newName.str()) {
    llvm::raw_string_ostream OS(USR);
    printDeclUSR(D, OS);
  }

  ArrayRef<RenameLoc> results() const { return locations; }

private:
  bool indexLocals() override { return true; }
  void failed(StringRef error) override {}
  bool recordHash(StringRef hash, bool isKnown) override { return true; }
  bool startDependency(StringRef name, StringRef path, bool isClangModule,
                       bool isSystem, StringRef hash) override {
    return true;
  }
  bool finishDependency(bool isClangModule) override { return true; }

  Action startSourceEntity(const IndexSymbol &symbol) override {
    if (symbol.USR == USR) {
      if (auto loc = indexSymbolToRenameLoc(symbol, newName)) {
        locations.push_back(std::move(*loc));
      }
    }
    return IndexDataConsumer::Continue;
  }

  bool finishSourceEntity(SymbolInfo symInfo, SymbolRoleSet roles) override {
    return true;
  }

  Optional<RenameLoc> indexSymbolToRenameLoc(const index::IndexSymbol &symbol,
                                             StringRef NewName);

private:
  std::string USR;
  std::string newName;
  StringScratchSpace stringStorage;
  std::vector<RenameLoc> locations;
};

Optional<RenameLoc>
RenameRangeCollector::indexSymbolToRenameLoc(const index::IndexSymbol &symbol,
                                             StringRef newName) {
  if (symbol.roles & (unsigned)index::SymbolRole::Implicit) {
    return None;
  }

  NameUsage usage = NameUsage::Unknown;
  if (symbol.roles & (unsigned)index::SymbolRole::Call) {
    usage = NameUsage::Call;
  } else if (symbol.roles & (unsigned)index::SymbolRole::Definition) {
    usage = NameUsage::Definition;
  } else if (symbol.roles & (unsigned)index::SymbolRole::Reference) {
    usage = NameUsage::Reference;
  } else {
    llvm_unreachable("unexpected role");
  }

  bool isFunctionLike = false;
  bool isNonProtocolType = false;

  switch (symbol.symInfo.Kind) {
  case index::SymbolKind::EnumConstant:
  case index::SymbolKind::Function:
  case index::SymbolKind::Constructor:
  case index::SymbolKind::ConversionFunction:
  case index::SymbolKind::InstanceMethod:
  case index::SymbolKind::ClassMethod:
  case index::SymbolKind::StaticMethod:
    isFunctionLike = true;
    break;
  case index::SymbolKind::Class:
  case index::SymbolKind::Enum:
  case index::SymbolKind::Struct:
    isNonProtocolType = true;
    break;
  default:
    break;
  }
  StringRef oldName = stringStorage.copyString(symbol.name);
  return RenameLoc{symbol.line,    symbol.column,    usage, oldName, newName,
                   isFunctionLike, isNonProtocolType};
}

ArrayRef<SourceFile*>
collectSourceFiles(ModuleDecl *MD, llvm::SmallVectorImpl<SourceFile*> &Scratch) {
  for (auto Unit : MD->getFiles()) {
    if (auto SF = dyn_cast<SourceFile>(Unit)) {
      Scratch.push_back(SF);
    }
  }
  return llvm::makeArrayRef(Scratch);
}

/// Get the source file that contains the given range and belongs to the module.
SourceFile *getContainingFile(ModuleDecl *M, RangeConfig Range) {
  llvm::SmallVector<SourceFile*, 4> Files;
  for (auto File : collectSourceFiles(M, Files)) {
    if (File->getBufferID()) {
      if (File->getBufferID().getValue() == Range.BufferId) {
        return File;
      }
    }
  }
  return nullptr;
}

class RefactoringAction {
protected:
  ModuleDecl *MD;
  SourceFile *TheFile;
  SourceEditConsumer &EditConsumer;
  ASTContext &Ctx;
  SourceManager &SM;
  DiagnosticEngine DiagEngine;
  SourceLoc StartLoc;
  StringRef PreferredName;
public:
  RefactoringAction(ModuleDecl *MD, RefactoringOptions &Opts,
                    SourceEditConsumer &EditConsumer,
                    DiagnosticConsumer &DiagConsumer);
  virtual ~RefactoringAction() = default;
  virtual bool performChange() = 0;
};

RefactoringAction::
RefactoringAction(ModuleDecl *MD, RefactoringOptions &Opts,
                  SourceEditConsumer &EditConsumer,
                  DiagnosticConsumer &DiagConsumer): MD(MD),
    TheFile(getContainingFile(MD, Opts.Range)),
    EditConsumer(EditConsumer), Ctx(MD->getASTContext()),
    SM(MD->getASTContext().SourceMgr), DiagEngine(SM),
    StartLoc(Lexer::getLocForStartOfToken(SM, Opts.Range.getStart(SM))),
    PreferredName(Opts.PreferredName) {
  DiagEngine.addConsumer(DiagConsumer);
}

/// Different from RangeBasedRefactoringAction, TokenBasedRefactoringAction takes
/// the input of a given token, e.g., a name or an "if" key word. Contextual
/// refactoring kinds can suggest applicable refactorings on that token, e.g.
/// rename or reverse if statement.
class TokenBasedRefactoringAction : public RefactoringAction {
protected:
  ResolvedCursorInfo CursorInfo;
public:
  TokenBasedRefactoringAction(ModuleDecl *MD, RefactoringOptions &Opts,
                              SourceEditConsumer &EditConsumer,
                              DiagnosticConsumer &DiagConsumer) :
  RefactoringAction(MD, Opts, EditConsumer, DiagConsumer) {
    // We can only proceed with valid location and source file.
    if (StartLoc.isValid() && TheFile) {
      // Resolve the sema token and save it for later use.
      CursorInfoResolver Resolver(*TheFile);
      CursorInfo = Resolver.resolve(StartLoc);
    }
  }
};

#define CURSOR_REFACTORING(KIND, NAME, ID)                                    \
class RefactoringAction##KIND: public TokenBasedRefactoringAction {           \
  public:                                                                     \
  RefactoringAction##KIND(ModuleDecl *MD, RefactoringOptions &Opts,           \
                          SourceEditConsumer &EditConsumer,                   \
                          DiagnosticConsumer &DiagConsumer) :                 \
    TokenBasedRefactoringAction(MD, Opts, EditConsumer, DiagConsumer) {}      \
  bool performChange() override;                                              \
  static bool isApplicable(ResolvedCursorInfo Tok, DiagnosticEngine &Diag);   \
  bool isApplicable() {                                                       \
    return RefactoringAction##KIND::isApplicable(CursorInfo, DiagEngine) ;    \
  }                                                                           \
};
#include "swift/IDE/RefactoringKinds.def"

class RangeBasedRefactoringAction : public RefactoringAction {
  RangeResolver Resolver;
protected:
  ResolvedRangeInfo RangeInfo;
public:
  RangeBasedRefactoringAction(ModuleDecl *MD, RefactoringOptions &Opts,
                              SourceEditConsumer &EditConsumer,
                              DiagnosticConsumer &DiagConsumer) :
  RefactoringAction(MD, Opts, EditConsumer, DiagConsumer),
  Resolver(*TheFile, Opts.Range.getStart(SM), Opts.Range.getEnd(SM)),
  RangeInfo(Resolver.resolve()) {}
};

#define RANGE_REFACTORING(KIND, NAME, ID)                                     \
class RefactoringAction##KIND: public RangeBasedRefactoringAction {           \
  public:                                                                     \
  RefactoringAction##KIND(ModuleDecl *MD, RefactoringOptions &Opts,           \
                          SourceEditConsumer &EditConsumer,                   \
                          DiagnosticConsumer &DiagConsumer) :                 \
    RangeBasedRefactoringAction(MD, Opts, EditConsumer, DiagConsumer) {}      \
  bool performChange() override;                                              \
  static bool isApplicable(ResolvedRangeInfo Info, DiagnosticEngine &Diag);   \
  bool isApplicable() {                                                       \
    return RefactoringAction##KIND::isApplicable(RangeInfo, DiagEngine) ;     \
  }                                                                           \
};
#include "swift/IDE/RefactoringKinds.def"

bool RefactoringActionLocalRename::
isApplicable(ResolvedCursorInfo CursorInfo, DiagnosticEngine &Diag) {
  if (CursorInfo.Kind != CursorInfoKind::ValueRef)
    return false;
  auto RenameOp = getAvailableRenameForDecl(CursorInfo.ValueD);
  return RenameOp.hasValue() &&
    RenameOp.getValue() == RefactoringKind::LocalRename;
}

static void analyzeRenameScope(ValueDecl *VD, DiagnosticEngine &Diags,
                               llvm::SmallVectorImpl<DeclContext *> &Scopes) {
  Scopes.clear();
  if (!getAvailableRenameForDecl(VD).hasValue()) {
    Diags.diagnose(SourceLoc(), diag::value_decl_no_loc, VD->getFullName());
    return;
  }

  auto *Scope = VD->getDeclContext();
  // If the context is a top-level code decl, there may be other sibling
  // decls that the renamed symbol is visible from
  if (isa<TopLevelCodeDecl>(Scope))
    Scope = Scope->getParent();

  Scopes.push_back(Scope);
}

bool RefactoringActionLocalRename::performChange() {
  if (StartLoc.isInvalid()) {
    DiagEngine.diagnose(SourceLoc(), diag::invalid_location);
    return true;
  }
  if (!DeclNameViewer(PreferredName).isValid()) {
    DiagEngine.diagnose(SourceLoc(), diag::invalid_name, PreferredName);
    return true;
  }
  if (!TheFile) {
    DiagEngine.diagnose(StartLoc, diag::location_module_mismatch,
                        MD->getNameStr());
    return true;
  }
  CursorInfoResolver Resolver(*TheFile);
  ResolvedCursorInfo CursorInfo = Resolver.resolve(StartLoc);
  if (CursorInfo.isValid() && CursorInfo.ValueD) {
    ValueDecl *VD = CursorInfo.ValueD;
    llvm::SmallVector<DeclContext *, 8> Scopes;
    analyzeRenameScope(VD, DiagEngine, Scopes);
    if (Scopes.empty())
      return true;
    RenameRangeCollector rangeCollector(VD, PreferredName);
    for (DeclContext *DC : Scopes)
      indexDeclContext(DC, rangeCollector);

    auto consumers = DiagEngine.takeConsumers();
    assert(consumers.size() == 1);
    return syntacticRename(TheFile, rangeCollector.results(), EditConsumer,
                           *consumers[0]);
  } else {
    DiagEngine.diagnose(StartLoc, diag::unresolved_location);
    return true;
  }
}

StringRef getDefaultPreferredName(RefactoringKind Kind) {
  switch(Kind) {
    case RefactoringKind::None:
      llvm_unreachable("Should be a valid refactoring kind");
    case RefactoringKind::GlobalRename:
    case RefactoringKind::LocalRename:
      return "newName";
    case RefactoringKind::ExtractExpr:
    case RefactoringKind::ExtractRepeatedExpr:
      return "extractedExpr";
    case RefactoringKind::ExtractFunction:
      return "extractedFunc";
    default:
      return "";
  }
}

enum class CannotExtractReason {
  Literal,
  VoidType,
};

class ExtractCheckResult {
  bool KnownFailure;
  llvm::SmallVector<CannotExtractReason, 2> AllReasons;

public:
  ExtractCheckResult(): KnownFailure(true) {}
  ExtractCheckResult(ArrayRef<CannotExtractReason> AllReasons):
    KnownFailure(false), AllReasons(AllReasons.begin(), AllReasons.end()) {}
  bool success() { return success({}); }
  bool success(llvm::ArrayRef<CannotExtractReason> ExpectedReasons) {
    if (KnownFailure)
      return false;
    bool Result = true;

    // Check if any reasons aren't covered by the list of expected reasons
    // provided by the client.
    for (auto R: AllReasons) {
      Result &= llvm::is_contained(ExpectedReasons, R);
    }
    return Result;
  }
};

/// Check whether a given range can be extracted.
/// Return true on successful condition checking,.
/// Return false on failed conditions.
ExtractCheckResult checkExtractConditions(ResolvedRangeInfo &RangeInfo,
                                          DiagnosticEngine &DiagEngine) {
  llvm::SmallVector<CannotExtractReason, 2> AllReasons;
  // If any declared declaration is refered out of the given range, return false.
  auto Declared = RangeInfo.DeclaredDecls;
  auto It = std::find_if(Declared.begin(), Declared.end(),
                         [](DeclaredDecl DD) { return DD.ReferredAfterRange; });
  if (It != Declared.end()) {
    DiagEngine.diagnose(It->VD->getLoc(),
                        diag::value_decl_referenced_out_of_range,
                        It->VD->getFullName());
    return ExtractCheckResult();
  }

  // We cannot extract a range with multi entry points.
  if (!RangeInfo.HasSingleEntry) {
    DiagEngine.diagnose(SourceLoc(), diag::multi_entry_range);
    return ExtractCheckResult();
  }

  // We cannot extract code that is not sure to exit or not.
  if (RangeInfo.exit() == ExitState::Unsure) {
    return ExtractCheckResult();
  }

  // We cannot extract expressions of l-value type.
  if (auto Ty = RangeInfo.getType()) {
    if (Ty->hasLValueType() || Ty->is<InOutType>())
      return ExtractCheckResult();

    // Disallow extracting error type expressions/statements
    // FIXME: diagnose what happened?
    if (Ty->hasError())
      return ExtractCheckResult();

    if (Ty->isVoid()) {
      AllReasons.emplace_back(CannotExtractReason::VoidType);
    }
  }

  // We cannot extract a range with orphaned loop keyword.
  switch (RangeInfo.Orphan) {
  case swift::ide::OrphanKind::Continue:
    DiagEngine.diagnose(SourceLoc(), diag::orphan_loop_keyword, "continue");
    return ExtractCheckResult();
  case swift::ide::OrphanKind::Break:
    DiagEngine.diagnose(SourceLoc(), diag::orphan_loop_keyword, "break");
    return ExtractCheckResult();
  case swift::ide::OrphanKind::None:
    break;
  }

  // Guard statement can not be extracted.
  if (llvm::any_of(RangeInfo.ContainedNodes,
                  [](ASTNode N) { return N.isStmt(StmtKind::Guard); })) {
    return ExtractCheckResult();
  }

  // Disallow extracting certain kinds of statements.
  if (RangeInfo.Kind == RangeKind::SingleStatement) {
    Stmt *S = RangeInfo.ContainedNodes[0].get<Stmt *>();

    // These aren't independent statement.
    if (isa<BraceStmt>(S) || isa<CatchStmt>(S) || isa<CaseStmt>(S))
      return ExtractCheckResult();
  }

  // Disallow extracting literals.
  if (RangeInfo.Kind == RangeKind::SingleExpression) {
    Expr *E = RangeInfo.ContainedNodes[0].get<Expr*>();

    // Until implementing the performChange() part of extracting trailing
    // closures, we disable them for now.
    if (isa<AbstractClosureExpr>(E))
      return ExtractCheckResult();

    if (isa<LiteralExpr>(E))
      AllReasons.emplace_back(CannotExtractReason::Literal);
  }

  switch (RangeInfo.RangeContext->getContextKind()) {
  case swift::DeclContextKind::Initializer:
  case swift::DeclContextKind::SubscriptDecl:
  case swift::DeclContextKind::AbstractFunctionDecl:
  case swift::DeclContextKind::AbstractClosureExpr:
  case swift::DeclContextKind::TopLevelCodeDecl:
    break;

  case swift::DeclContextKind::SerializedLocal:
  case swift::DeclContextKind::Module:
  case swift::DeclContextKind::FileUnit:
  case swift::DeclContextKind::GenericTypeDecl:
  case swift::DeclContextKind::ExtensionDecl:
    return ExtractCheckResult();
  }
  return ExtractCheckResult(AllReasons);
}

bool RefactoringActionExtractFunction::
isApplicable(ResolvedRangeInfo Info, DiagnosticEngine &Diag) {
  switch (Info.Kind) {
  case RangeKind::PartOfExpression:
  case RangeKind::SingleDecl:
  case RangeKind::MultiTypeMemberDecl:
  case RangeKind::Invalid:
    return false;
  case RangeKind::SingleExpression:
  case RangeKind::SingleStatement:
  case RangeKind::MultiStatement: {
    return checkExtractConditions(Info, Diag).
      success({CannotExtractReason::VoidType});
  }
  }
  llvm_unreachable("unhandled kind");
}

static StringRef correctNameInternal(ASTContext &Ctx, StringRef Name,
                                     ArrayRef<ValueDecl*> AllVisibles) {
  // If we find the collision.
  bool FoundCollision = false;

  // The suffixes we cannot use by appending to the original given name.
  llvm::StringSet<> UsedSuffixes;
  for (auto VD : AllVisibles) {
    StringRef S = VD->getBaseName().userFacingName();
    if (!S.startswith(Name))
      continue;
    StringRef Suffix = S.substr(Name.size());
    if (Suffix.empty())
      FoundCollision = true;
    else
      UsedSuffixes.insert(Suffix);
  }
  if (!FoundCollision)
    return Name;

  // Find the first suffix we can use.
  std::string SuffixToUse;
  for (unsigned I = 1; ; I ++) {
    SuffixToUse = std::to_string(I);
    if (UsedSuffixes.count(SuffixToUse) == 0)
      break;
  }
  return Ctx.getIdentifier((llvm::Twine(Name) + SuffixToUse).str()).str();
}

static StringRef correctNewDeclName(DeclContext *DC, StringRef Name) {

  // Collect all visible decls in the decl context.
  llvm::SmallVector<ValueDecl*, 16> AllVisibles;
  VectorDeclConsumer Consumer(AllVisibles);
  ASTContext &Ctx = DC->getASTContext();
  lookupVisibleDecls(Consumer, DC, Ctx.getLazyResolver(), true);
  return correctNameInternal(Ctx, Name, AllVisibles);
}

static Type sanitizeType(Type Ty) {
  // Transform lvalue type to inout type so that we can print it properly.
  return Ty.transform([](Type Ty) {
    if (Ty->is<LValueType>()) {
      return Type(InOutType::get(Ty->getRValueType()->getCanonicalType()));
    }
    return Ty;
  });
}

static SourceLoc
getNewFuncInsertLoc(DeclContext *DC, DeclContext*& InsertToContext) {
  if (auto D = DC->getInnermostDeclarationDeclContext()) {

    // If extracting from a getter/setter, we should skip both the immediate
    // getter/setter function and the individual var decl. The pattern binding
    // decl is the position before which we should insert the newly extracted
    // function.
    if (auto *FD = dyn_cast<AccessorDecl>(D)) {
      ValueDecl *SD = FD->getStorage();
      switch (SD->getKind()) {
      case DeclKind::Var:
        if (auto *PBD = cast<VarDecl>(SD)->getParentPatternBinding())
          D = PBD;
        break;
      case DeclKind::Subscript:
        D = SD;
        break;
      default:
        break;
      }
    }

    auto Result = D->getStartLoc();
    assert(Result.isValid());

    // The insert loc should be before every decl attributes.
    for (auto Attr : D->getAttrs()) {
      auto Loc = Attr->getRangeWithAt().Start;
      if (Loc.isValid() &&
          Loc.getOpaquePointerValue() < Result.getOpaquePointerValue())
        Result = Loc;
    }

    // The insert loc should be before the doc comments associated with this decl.
    if (!D->getRawComment().Comments.empty()) {
      auto Loc = D->getRawComment().Comments.front().Range.getStart();
      if (Loc.isValid() &&
          Loc.getOpaquePointerValue() < Result.getOpaquePointerValue()) {
        Result = Loc;
      }
    }
    InsertToContext = D->getDeclContext();
    return Result;
  }
  return SourceLoc();
}

static std::vector<NoteRegion>
getNotableRegions(StringRef SourceText, unsigned NameOffset, StringRef Name,
                    bool IsFunctionLike = false, bool IsNonProtocolType = false) {
  auto InputBuffer = llvm::MemoryBuffer::getMemBufferCopy(SourceText,"<extract>");

  CompilerInvocation Invocation{};

  Invocation.getFrontendOptions().InputsAndOutputs.addInput(
      InputFile("<extract>", true, InputBuffer.get()));
  Invocation.getFrontendOptions().ModuleName = "extract";

  auto Instance = llvm::make_unique<swift::CompilerInstance>();
  if (Instance->setup(Invocation))
    llvm_unreachable("Failed setup");

  Instance->performParseOnly();

  unsigned BufferId = Instance->getPrimarySourceFile()->getBufferID().getValue();
  SourceManager &SM = Instance->getSourceMgr();
  SourceLoc NameLoc = SM.getLocForOffset(BufferId, NameOffset);
  auto LineAndCol = SM.getLineAndColumn(NameLoc);

  UnresolvedLoc UnresoledName{NameLoc, true};

  NameMatcher Matcher(*Instance->getPrimarySourceFile());
  auto Resolved = Matcher.resolve(llvm::makeArrayRef(UnresoledName), None);
  assert(!Resolved.empty() && "Failed to resolve generated func name loc");

  RenameLoc RenameConfig = {
    LineAndCol.first, LineAndCol.second,
    NameUsage::Definition, /*OldName=*/Name, /*NewName=*/"",
    IsFunctionLike, IsNonProtocolType
  };
  RenameRangeDetailCollector Renamer(SM, Name);
  Renamer.addSyntacticRenameRanges(Resolved.back(), RenameConfig);
  auto Ranges = Renamer.Ranges;

  std::vector<NoteRegion> NoteRegions(Renamer.Ranges.size());
  std::transform(Ranges.begin(), Ranges.end(), NoteRegions.begin(),
                 [&SM](RenameRangeDetail &Detail) -> NoteRegion {
    auto Start = SM.getLineAndColumn(Detail.Range.getStart());
    auto End = SM.getLineAndColumn(Detail.Range.getEnd());
    return {Detail.RangeKind, Start.first, Start.second, End.first, End.second, Detail.Index};
  });

  return NoteRegions;
}

bool RefactoringActionExtractFunction::performChange() {
  // Check if the new name is ok.
  if (!Lexer::isIdentifier(PreferredName)) {
    DiagEngine.diagnose(SourceLoc(), diag::invalid_name, PreferredName);
    return true;
  }
  DeclContext *DC = RangeInfo.RangeContext;
  DeclContext *InsertToDC = nullptr;
  SourceLoc InsertLoc = getNewFuncInsertLoc(DC, InsertToDC);

  // Complain about no inserting position.
  if (InsertLoc.isInvalid()) {
    DiagEngine.diagnose(SourceLoc(), diag::no_insert_position);
    return true;
  }

  // Correct the given name if collision happens.
  PreferredName = correctNewDeclName(InsertToDC, PreferredName);

  // Collect the paramters to pass down to the new function.
  std::vector<ReferencedDecl> Parameters;
  for (auto &RD: RangeInfo.ReferencedDecls) {
    // If the referenced decl is declared elsewhere, no need to pass as parameter
    if (RD.VD->getDeclContext() != DC)
      continue;

    // We don't need to pass down implicitly declared variables, e.g. error in
    // a catch block.
    if (RD.VD->isImplicit()) {
      SourceLoc Loc = RD.VD->getStartLoc();
      if (Loc.isValid() &&
          SM.isBeforeInBuffer(RangeInfo.ContentRange.getStart(), Loc) &&
          SM.isBeforeInBuffer(Loc, RangeInfo.ContentRange.getEnd()))
        continue;
    }

    // If the referenced decl is declared inside the range, no need to pass
    // as parameter.
    if (RangeInfo.DeclaredDecls.end() !=
      std::find_if(RangeInfo.DeclaredDecls.begin(), RangeInfo.DeclaredDecls.end(),
        [RD](DeclaredDecl DD) { return RD.VD == DD.VD; }))
      continue;

    // We don't need to pass down self.
    if (auto PD = dyn_cast<ParamDecl>(RD.VD)) {
      if (PD->isSelfParameter()) {
        continue;
      }
    }

    Parameters.emplace_back(RD.VD, sanitizeType(RD.Ty));
  }
  SmallString<64> Buffer;
  unsigned FuncBegin = Buffer.size();
  unsigned FuncNameOffset;
  {
    llvm::raw_svector_ostream OS(Buffer);

    if (!InsertToDC->isLocalContext()) {
      // Default to be file private.
      OS << tok::kw_fileprivate << " ";
    }

    // Inherit static if the containing function is.
    if (DC->getContextKind() == DeclContextKind::AbstractFunctionDecl) {
      if (auto FD = dyn_cast<FuncDecl>(static_cast<AbstractFunctionDecl*>(DC))) {
        if (FD->isStatic()) {
          OS << tok::kw_static << " ";
        }
      }
    }

    OS << tok::kw_func << " ";
    FuncNameOffset = Buffer.size() - FuncBegin;
    OS << PreferredName;
    OS << "(";
    for (auto &RD : Parameters) {
      OS << "_ " << RD.VD->getBaseName().userFacingName() << ": ";
      RD.Ty->reconstituteSugar(/*Recursive*/true)->print(OS);
      if (&RD != &Parameters.back())
        OS << ", ";
    }
    OS << ")";

    if (RangeInfo.ThrowingUnhandledError)
      OS << " " << tok::kw_throws;

    bool InsertedReturnType = false;
    if (auto Ty = RangeInfo.getType()) {
      // If the type of the range is not void, specify the return type.
      if (!Ty->isVoid()) {
        OS << " " << tok::arrow << " ";
        sanitizeType(Ty)->reconstituteSugar(/*Recursive*/true)->print(OS);
        InsertedReturnType = true;
      }
    }

    OS << " {\n";

    // Add "return" if the extracted entity is an expression.
    if (RangeInfo.Kind == RangeKind::SingleExpression && InsertedReturnType)
      OS << tok::kw_return << " ";
    OS << RangeInfo.ContentRange.str() << "\n}\n\n";
  }
  unsigned FuncEnd = Buffer.size();

  unsigned ReplaceBegin = Buffer.size();
  unsigned CallNameOffset;
  {
    llvm::raw_svector_ostream OS(Buffer);
    if (RangeInfo.exit() == ExitState::Positive)
      OS << tok::kw_return <<" ";
    CallNameOffset = Buffer.size() - ReplaceBegin;
    OS << PreferredName << "(";
    for (auto &RD : Parameters) {

      // Inout argument needs "&".
      if (RD.Ty->is<InOutType>())
        OS << "&";
      OS << RD.VD->getBaseName().userFacingName();
      if (&RD != &Parameters.back())
        OS << ", ";
    }
    OS << ")";
  }
  unsigned ReplaceEnd = Buffer.size();

  std::string ExtractedFuncName = PreferredName.str() + "(";
  for (size_t i = 0; i < Parameters.size(); ++i) {
    ExtractedFuncName += "_:";
  }
  ExtractedFuncName += ")";

  StringRef DeclStr(Buffer.begin() + FuncBegin, FuncEnd - FuncBegin);
  auto NotableFuncRegions = getNotableRegions(DeclStr, FuncNameOffset,
                                              ExtractedFuncName,
                                              /*IsFunctionLike=*/true);

  StringRef CallStr(Buffer.begin() + ReplaceBegin, ReplaceEnd - ReplaceBegin);
  auto NotableCallRegions = getNotableRegions(CallStr, CallNameOffset,
                                              ExtractedFuncName,
                                              /*IsFunctionLike=*/true);

  // Insert the new function's declaration.
  EditConsumer.accept(SM, InsertLoc, DeclStr, NotableFuncRegions);

  // Replace the code to extract with the function call.
  EditConsumer.accept(SM, RangeInfo.ContentRange, CallStr, NotableCallRegions);

  return false;
}

class RefactoringActionExtractExprBase {
  SourceFile *TheFile;
  ResolvedRangeInfo RangeInfo;
  DiagnosticEngine &DiagEngine;
  const bool ExtractRepeated;
  StringRef PreferredName;
  SourceEditConsumer &EditConsumer;

  ASTContext &Ctx;
  SourceManager &SM;

public:
  RefactoringActionExtractExprBase(SourceFile *TheFile,
                                   ResolvedRangeInfo RangeInfo,
                                   DiagnosticEngine &DiagEngine,
                                   bool ExtractRepeated,
                                   StringRef PreferredName,
                                   SourceEditConsumer &EditConsumer) :
    TheFile(TheFile), RangeInfo(RangeInfo), DiagEngine(DiagEngine),
    ExtractRepeated(ExtractRepeated), PreferredName(PreferredName),
    EditConsumer(EditConsumer), Ctx(TheFile->getASTContext()),
    SM(Ctx.SourceMgr){}
  bool performChange();
};

/// This is to ensure all decl references in two expressions are identical.
struct ReferenceCollector: public SourceEntityWalker {
  llvm::SmallVector<ValueDecl*, 4> References;

  ReferenceCollector(Expr *E) { walk(E); }
  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
                          Type T, ReferenceMetaData Data) override {
    References.emplace_back(D);
    return true;
  }
  bool operator==(const ReferenceCollector &Other) const {
    if (References.size() != Other.References.size())
      return false;
    return std::equal(References.begin(), References.end(),
                      Other.References.begin());
  }
};

struct SimilarExprCollector: public SourceEntityWalker {
  SourceManager &SM;

  /// The expression under selection.
  Expr *SelectedExpr;
  llvm::ArrayRef<Token> AllTokens;
  llvm::SetVector<Expr*> &Bucket;

  /// The tokens included in the expression under selection.
  llvm::ArrayRef<Token> SelectedTokens;

  /// The referenced decls in the expression under selection.
  ReferenceCollector SelectedReferences;

  bool compareTokenContent(ArrayRef<Token> Left, ArrayRef<Token> Right) {
    if (Left.size() != Right.size())
      return false;
    return std::equal(Left.begin(), Left.end(), Right.begin(),
                      [](const Token &L, const Token& R) {
                        return L.getText() == R.getText();
                      });
  }

  /// Find all tokens included by an expression.
  llvm::ArrayRef<Token> getExprSlice(Expr *E) {
    return slice_token_array(AllTokens, E->getStartLoc(), E->getEndLoc());
  }

  SimilarExprCollector(SourceManager &SM, Expr* SelectedExpr,
                       llvm::ArrayRef<Token> AllTokens,
    llvm::SetVector<Expr*> &Bucket): SM(SM), SelectedExpr(SelectedExpr),
    AllTokens(AllTokens), Bucket(Bucket),
    SelectedTokens(getExprSlice(SelectedExpr)),
    SelectedReferences(SelectedExpr){}

  bool walkToExprPre(Expr *E) override {
    // We don't extract implicit expressions.
    if (E->isImplicit())
      return true;
    if (E->getKind() != SelectedExpr->getKind())
      return true;

    // First check the underlying token arrays have the same content.
    if (compareTokenContent(getExprSlice(E), SelectedTokens)) {
      ReferenceCollector CurrentReferences(E);

      // Next, check the referenced decls are same.
      if (CurrentReferences == SelectedReferences)
        Bucket.insert(E);
    }
    return true;
  }
};

bool RefactoringActionExtractExprBase::performChange() {
  // Check if the new name is ok.
  if (!Lexer::isIdentifier(PreferredName)) {
    DiagEngine.diagnose(SourceLoc(), diag::invalid_name, PreferredName);
    return true;
  }

  // Find the enclosing brace statement;
  ContextFinder Finder(*TheFile, RangeInfo.ContainedNodes.front(),
                       [](ASTNode N) { return N.isStmt(StmtKind::Brace); });

  auto *SelectedExpr = RangeInfo.ContainedNodes[0].get<Expr*>();
  Finder.resolve();
  SourceLoc InsertLoc;
  llvm::SetVector<ValueDecl*> AllVisibleDecls;
  struct DeclCollector: public SourceEntityWalker {
    llvm::SetVector<ValueDecl*> &Bucket;
    DeclCollector(llvm::SetVector<ValueDecl*> &Bucket): Bucket(Bucket) {}
    bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
      if (auto *VD = dyn_cast<ValueDecl>(D))
        Bucket.insert(VD);
      return true;
    }
  } Collector(AllVisibleDecls);

  llvm::SetVector<Expr*> AllExpressions;

  if (!Finder.getContexts().empty()) {

    // Get the innermost brace statement.
    auto BS = static_cast<BraceStmt*>(Finder.getContexts().back().get<Stmt*>());

    // Collect all value decls inside the brace statement.
    Collector.walk(BS);

    if (ExtractRepeated) {
      // Collect all expressions we are going to extract.
      SimilarExprCollector(SM, SelectedExpr,
                           slice_token_array(TheFile->getAllTokens(),
                                             BS->getStartLoc(),
                                             BS->getEndLoc()),
                           AllExpressions).walk(BS);
    } else {
      AllExpressions.insert(SelectedExpr);
    }

    assert(!AllExpressions.empty() && "at least one expression is extracted.");
    for (auto Ele : BS->getElements()) {
      // Find the element that encloses the first expression under extraction.
      if (SM.rangeContains(Ele.getSourceRange(),
                           (*AllExpressions.begin())->getSourceRange())) {

        // Insert before the enclosing element.
        InsertLoc = Ele.getStartLoc();
      }
    }
  }

  // Complain about no inserting position.
  if (InsertLoc.isInvalid()) {
    DiagEngine.diagnose(SourceLoc(), diag::no_insert_position);
    return true;
  }

  // Correct name if collision happens.
  PreferredName = correctNameInternal(TheFile->getASTContext(), PreferredName,
                                      AllVisibleDecls.getArrayRef());

  // Print the type name of this expression.
  llvm::SmallString<16> TyBuffer;

  // We are not sure about the type of repeated expressions.
  if (!ExtractRepeated) {
    if (auto Ty = RangeInfo.getType()) {
      llvm::raw_svector_ostream OS(TyBuffer);
      OS << ": ";
      Ty->getRValueType()->reconstituteSugar(true)->print(OS);
    }
  }

  llvm::SmallString<64> DeclBuffer;
  llvm::raw_svector_ostream OS(DeclBuffer);
  unsigned StartOffset, EndOffset;
  OS << tok::kw_let << " ";
  StartOffset = DeclBuffer.size();
  OS << PreferredName;
  EndOffset = DeclBuffer.size();
  OS << TyBuffer.str() <<  " = " << RangeInfo.ContentRange.str() << "\n";

  NoteRegion DeclNameRegion{
    RefactoringRangeKind::BaseName,
    /*StartLine=*/1, /*StartColumn=*/StartOffset + 1,
    /*EndLine=*/1, /*EndColumn=*/EndOffset + 1,
    /*ArgIndex*/None
  };

  // Perform code change.
  EditConsumer.accept(SM, InsertLoc, DeclBuffer.str(), {DeclNameRegion});

  // Replace all occurrences of the extracted expression.
  for (auto *E : AllExpressions) {
    EditConsumer.accept(SM,
      Lexer::getCharSourceRangeFromSourceRange(SM, E->getSourceRange()),
      PreferredName,
      {{
        RefactoringRangeKind::BaseName,
        /*StartLine=*/1, /*StartColumn-*/1, /*EndLine=*/1,
        /*EndColumn=*/static_cast<unsigned int>(PreferredName.size() + 1),
        /*ArgIndex*/None
      }});
  }
  return false;
}

bool RefactoringActionExtractExpr::
isApplicable(ResolvedRangeInfo Info, DiagnosticEngine &Diag) {
  switch (Info.Kind) {
    case RangeKind::SingleExpression:
      // We disallow extract literal expression for two reasons:
      // (1) since we print the type for extracted expression, the type of a
      // literal may print as "int2048" where it is not typically users' choice;
      // (2) Extracting one literal provides little value for users.
      return checkExtractConditions(Info, Diag).success();
    case RangeKind::PartOfExpression:
    case RangeKind::SingleDecl:
    case RangeKind::MultiTypeMemberDecl:
    case RangeKind::SingleStatement:
    case RangeKind::MultiStatement:
    case RangeKind::Invalid:
      return false;
  }
  llvm_unreachable("unhandled kind");
}

bool RefactoringActionExtractExpr::performChange() {
  return RefactoringActionExtractExprBase(TheFile, RangeInfo,
                                          DiagEngine, false, PreferredName,
                                          EditConsumer).performChange();
}

bool RefactoringActionExtractRepeatedExpr::
isApplicable(ResolvedRangeInfo Info, DiagnosticEngine &Diag) {
  switch (Info.Kind) {
    case RangeKind::SingleExpression:
      return checkExtractConditions(Info, Diag).
        success({CannotExtractReason::Literal});
    case RangeKind::PartOfExpression:
    case RangeKind::SingleDecl:
    case RangeKind::MultiTypeMemberDecl:
    case RangeKind::SingleStatement:
    case RangeKind::MultiStatement:
    case RangeKind::Invalid:
      return false;
  }
  llvm_unreachable("unhandled kind");
}
bool RefactoringActionExtractRepeatedExpr::performChange() {
  return RefactoringActionExtractExprBase(TheFile, RangeInfo,
                                          DiagEngine, true, PreferredName,
                                          EditConsumer).performChange();
}


bool RefactoringActionMoveMembersToExtension::isApplicable(
    ResolvedRangeInfo Info, DiagnosticEngine &Diag) {
  switch (Info.Kind) {
  case RangeKind::SingleDecl:
  case RangeKind::MultiTypeMemberDecl: {
    DeclContext *DC = Info.RangeContext;

    // The the common decl context is not a nomial type, we cannot create an
    // extension for it
    if (!DC || !DC->getInnermostDeclarationDeclContext() ||
        !isa<NominalTypeDecl>(DC->getInnermostDeclarationDeclContext()))
      return false;


    // Members of types not declared at top file level cannot be extracted
    // to an extension at top file level
    if (DC->getParent()->getContextKind() != DeclContextKind::FileUnit)
      return false;

    // Check if contained nodes are all allowed decls.
    for (auto Node : Info.ContainedNodes) {
      Decl *D = Node.dyn_cast<Decl*>();
      if (!D)
        return false;

      if (isa<AccessorDecl>(D) || isa<DestructorDecl>(D) ||
          isa<EnumCaseDecl>(D) || isa<EnumElementDecl>(D))
        return false;
    }

    // We should not move instance variables with storage into the extension
    // because they are not allowed to be declared there
    for (auto DD : Info.DeclaredDecls) {
      if (auto ASD = dyn_cast<AbstractStorageDecl>(DD.VD)) {
        // Only disallow storages in the common decl context, allow them in
        // any subtypes
        if (ASD->hasStorage() && ASD->getDeclContext() == DC) {
          return false;
        }
      }
    }

    return true;
  }
  case RangeKind::SingleExpression:
  case RangeKind::PartOfExpression:
  case RangeKind::SingleStatement:
  case RangeKind::MultiStatement:
  case RangeKind::Invalid:
    return false;
  }
  llvm_unreachable("unhandled kind");
}

bool RefactoringActionMoveMembersToExtension::performChange() {
  DeclContext *DC = RangeInfo.RangeContext;

  auto CommonTypeDecl =
      dyn_cast<NominalTypeDecl>(DC->getInnermostDeclarationDeclContext());
  assert(CommonTypeDecl && "Not applicable if common parent is no nomial type");

  SmallString<64> Buffer;
  llvm::raw_svector_ostream OS(Buffer);
  OS << "\n\n";
  OS << "extension " << CommonTypeDecl->getName() << " {\n";
  OS << RangeInfo.ContentRange.str().trim();
  OS << "\n}";

  // Insert extension after the type declaration
  EditConsumer.insertAfter(SM, CommonTypeDecl->getEndLoc(), Buffer);
  EditConsumer.remove(SM, RangeInfo.ContentRange);

  return false;
}

namespace {
// A SingleDecl range may not include all decls actually declared in that range:
// a var decl has accessors that aren't included. This will find those missing
// decls.
class FindAllSubDecls : public SourceEntityWalker {
  llvm::SmallPtrSetImpl<Decl *> &Found;
  public:
  FindAllSubDecls(llvm::SmallPtrSetImpl<Decl *> &found)
    : Found(found) {}

  bool walkToDeclPre(Decl *D, CharSourceRange range) {
    // Record this Decl, and skip its contents if we've already touched it.
    if (!Found.insert(D).second)
      return false;

    if (auto ASD = dyn_cast<AbstractStorageDecl>(D)) {
      auto accessors = ASD->getAllAccessors();
      Found.insert(accessors.begin(), accessors.end());
    }
    return true;
  }
};
}
bool RefactoringActionReplaceBodiesWithFatalError::isApplicable(
  ResolvedRangeInfo Info, DiagnosticEngine &Diag) {
  switch (Info.Kind) {
  case RangeKind::SingleDecl:
  case RangeKind::MultiTypeMemberDecl: {
    llvm::SmallPtrSet<Decl *, 16> Found;
    for (auto decl : Info.DeclaredDecls) {
      FindAllSubDecls(Found).walk(decl.VD);
    }
    for (auto decl : Found) {
      auto AFD = dyn_cast<AbstractFunctionDecl>(decl);
      if (AFD && !AFD->isImplicit())
        return true;
    }

    return false;
 }
  case RangeKind::SingleExpression:
  case RangeKind::PartOfExpression:
  case RangeKind::SingleStatement:
  case RangeKind::MultiStatement:
  case RangeKind::Invalid:
    return false;
  }
  llvm_unreachable("unhandled kind");
}

bool RefactoringActionReplaceBodiesWithFatalError::performChange() {
  const StringRef replacement = "{\nfatalError()\n}";
  llvm::SmallPtrSet<Decl *, 16> Found;
  for (auto decl : RangeInfo.DeclaredDecls) {
    FindAllSubDecls(Found).walk(decl.VD);
  }
  for (auto decl : Found) {
    auto AFD = dyn_cast<AbstractFunctionDecl>(decl);
    if (!AFD || AFD->isImplicit())
      continue;

    auto range = AFD->getBodySourceRange();
    // If we're in replacement mode (i.e. have an edit consumer), we can
    // rewrite the function body.
    auto charRange = Lexer::getCharSourceRangeFromSourceRange(SM, range);
    EditConsumer.accept(SM, charRange, replacement);

  }
  return false;
}

static std::pair<IfStmt *, IfStmt *>
findCollapseNestedIfTarget(ResolvedCursorInfo CursorInfo) {
  if (CursorInfo.Kind != CursorInfoKind::StmtStart)
    return {};

  // Ensure the statement is 'if' statement. It must not have 'else' clause.
  IfStmt *OuterIf = dyn_cast<IfStmt>(CursorInfo.TrailingStmt);
  if (!OuterIf)
    return {};
  if (OuterIf->getElseStmt())
    return {};

  // The body must contain a sole inner 'if' statement.
  auto Body = dyn_cast_or_null<BraceStmt>(OuterIf->getThenStmt());
  if (!Body || Body->getNumElements() != 1)
    return {};

  IfStmt *InnerIf =
      dyn_cast_or_null<IfStmt>(Body->getElement(0).dyn_cast<Stmt *>());
  if (!InnerIf)
    return {};

  // Inner 'if' statement also cannot have 'else' clause.
  if (InnerIf->getElseStmt())
    return {};

  return {OuterIf, InnerIf};
}

bool RefactoringActionCollapseNestedIfStmt::
isApplicable(ResolvedCursorInfo CursorInfo, DiagnosticEngine &Diag) {
  return findCollapseNestedIfTarget(CursorInfo).first;
}

bool RefactoringActionCollapseNestedIfStmt::performChange() {
  auto Target = findCollapseNestedIfTarget(CursorInfo);
  if (!Target.first)
    return true;
  auto OuterIf = Target.first;
  auto InnerIf = Target.second;

  EditorConsumerInsertStream OS(
      EditConsumer, SM,
      Lexer::getCharSourceRangeFromSourceRange(SM, OuterIf->getSourceRange()));

  OS << tok::kw_if << " "; 

  // Emit conditions.
  bool first = true;
  for (auto &C : llvm::concat<StmtConditionElement>(OuterIf->getCond(),
                                                    InnerIf->getCond())) {
    if (first)
      first = false;
    else
      OS << ", ";
    OS << Lexer::getCharSourceRangeFromSourceRange(SM, C.getSourceRange())
              .str();
  }

  // Emit body.
  OS << " ";
  OS << Lexer::getCharSourceRangeFromSourceRange(
            SM, InnerIf->getThenStmt()->getSourceRange())
            .str();
  return false;
}

static std::unique_ptr<llvm::SetVector<Expr*>>
findConcatenatedExpressions(ResolvedRangeInfo Info, ASTContext &Ctx) {
  Expr *E = nullptr;

  switch (Info.Kind) {
  case RangeKind::SingleExpression:
    // FIXME: the range info kind should imply non-empty list.
    if (!Info.ContainedNodes.empty())
      E = Info.ContainedNodes[0].get<Expr*>();
    else
      return nullptr;
    break;
  case RangeKind::PartOfExpression:
    E = Info.CommonExprParent;
    break;
  default:
    return nullptr;
  }

  assert(E);

  struct StringInterpolationExprFinder: public SourceEntityWalker {
    std::unique_ptr<llvm::SetVector<Expr*>> Bucket = llvm::
    make_unique<llvm::SetVector<Expr*>>();
    ASTContext &Ctx;

    bool IsValidInterpolation = true;
    StringInterpolationExprFinder(ASTContext &Ctx): Ctx(Ctx) {}

    bool isConcatenationExpr(DeclRefExpr* Expr) {
      if (!Expr)
        return false;
      auto *FD = dyn_cast<FuncDecl>(Expr->getDecl());
      if (FD == nullptr || (FD != Ctx.getPlusFunctionOnString() &&
          FD != Ctx.getPlusFunctionOnRangeReplaceableCollection())) {
        return false;
      }
      return true;
    }

    bool walkToExprPre(Expr *E) {
      if (E->isImplicit())
        return true;
      // FIXME: we should have ErrorType instead of null.
      if (E->getType().isNull())
        return true;
      auto ExprType = E->getType()->getNominalOrBoundGenericNominal();
      //Only binary concatenation operators should exist in expression
      if (E->getKind() == ExprKind::Binary) {
        auto *BE = dyn_cast<BinaryExpr>(E);
        auto *OperatorDeclRef = BE->getSemanticFn()->getMemberOperatorRef();
        if (!(isConcatenationExpr(OperatorDeclRef)
            && ExprType == Ctx.getStringDecl())) {
          IsValidInterpolation = false;
          return false;
        }
        return true;
      }
      // Everything that evaluates to string should be gathered.
      if (ExprType == Ctx.getStringDecl()) {
        Bucket->insert(E);
        return false;
      }
      if (auto *DR = dyn_cast<DeclRefExpr>(E)) {
        // Checks whether all function references in expression are concatenations.
        auto *FD = dyn_cast<FuncDecl>(DR->getDecl());
        auto IsConcatenation = isConcatenationExpr(DR);
        if (FD && IsConcatenation) {
          return false;
        }
      }
      // There was non-expected expression, it's not valid interpolation then.
      IsValidInterpolation = false;
      return false;
    }
  } Walker(Ctx);
  Walker.walk(E);

  // There should be two or more expressions to convert.
  if (!Walker.IsValidInterpolation || Walker.Bucket->size() < 2)
    return nullptr;

  return std::move(Walker.Bucket);
}

static void interpolatedExpressionForm(Expr *E, SourceManager &SM,
                                              llvm::raw_ostream &OS) {
  if (auto *Literal = dyn_cast<StringLiteralExpr>(E)) {
    OS << Literal->getValue();
    return;
  }
  auto ExpStr = Lexer::getCharSourceRangeFromSourceRange(SM,
    E->getSourceRange()).str().str();
  if (isa<InterpolatedStringLiteralExpr>(E)) {
    ExpStr.erase(0, 1);
    ExpStr.pop_back();
    OS << ExpStr;
    return;
  }
  OS << "\\(" << ExpStr << ")";
}

bool RefactoringActionConvertStringsConcatenationToInterpolation::
isApplicable(ResolvedRangeInfo Info, DiagnosticEngine &Diag) {
  auto RangeContext = Info.RangeContext;
  if (RangeContext) {
    auto &Ctx = Info.RangeContext->getASTContext();
    return findConcatenatedExpressions(Info, Ctx) != nullptr;
  }
  return false;
}

bool RefactoringActionConvertStringsConcatenationToInterpolation::performChange() {
  auto Expressions = findConcatenatedExpressions(RangeInfo, Ctx);
  if (!Expressions)
    return true;
  EditorConsumerInsertStream OS(EditConsumer, SM, RangeInfo.ContentRange);
  OS << "\"";
  for (auto It = Expressions->begin(); It != Expressions->end(); It++) {
    interpolatedExpressionForm(*It, SM, OS);
  }
  OS << "\"";
  return false;
}

/// Abstract helper class containing info about an IfExpr
/// that can be expanded into an IfStmt.
class ExpandableTernaryExprInfo {

public:
  virtual ~ExpandableTernaryExprInfo() {}

  virtual IfExpr *getIf() = 0;

  virtual SourceRange getNameRange() = 0;

  virtual Type getType() = 0;

  virtual bool shouldDeclareNameAndType() {
    return !getType().isNull();
  }

  virtual bool isValid() {

    //Ensure all public properties are non-nil and valid
    if (!getIf() || !getNameRange().isValid())
      return false;
    if (shouldDeclareNameAndType() && getType().isNull())
      return false;

    return true; //valid
  }

  CharSourceRange getNameCharRange(const SourceManager &SM) {
    return Lexer::getCharSourceRangeFromSourceRange(SM, getNameRange());
  }
};

/// Concrete subclass containing info about an AssignExpr
/// where the source is the expandable IfExpr.
class ExpandableAssignTernaryExprInfo: public ExpandableTernaryExprInfo {

public:
  ExpandableAssignTernaryExprInfo(AssignExpr *Assign): Assign(Assign) {}

  IfExpr *getIf() {
    if (!Assign)
      return nullptr;

    return dyn_cast<IfExpr>(Assign->getSrc());
  }

  SourceRange getNameRange() {
    auto Invalid = SourceRange();

    if (!Assign)
      return Invalid;

    if (auto dest = Assign->getDest())
      return dest->getSourceRange();

    return Invalid;
  }

  Type getType() {
    return nullptr;
  }

private:
  AssignExpr *Assign = nullptr;
};

/// Concrete subclass containing info about a PatternBindingDecl
/// where the pattern initializer is the expandable IfExpr.
class ExpandableBindingTernaryExprInfo: public ExpandableTernaryExprInfo {

public:
  ExpandableBindingTernaryExprInfo(PatternBindingDecl *Binding):
  Binding(Binding) {}

  IfExpr *getIf() {
    if (Binding && Binding->getNumPatternEntries() == 1) {
      if (auto *Init = Binding->getInit(0)) {
        return dyn_cast<IfExpr>(Init);
      }
    }

    return nullptr;
  }

  SourceRange getNameRange() {
    if (auto Pattern = getNamePattern())
      return Pattern->getSourceRange();

    return SourceRange();
  }

  Type getType() {
    if (auto Pattern = getNamePattern())
      return Pattern->getType();

    return nullptr;
  }

private:
  Pattern *getNamePattern() {
    if (!Binding || Binding->getNumPatternEntries() != 1)
      return nullptr;

    auto Pattern = Binding->getPattern(0);

    if (!Pattern)
      return nullptr;

    if (auto TyPattern = dyn_cast<TypedPattern>(Pattern))
      Pattern = TyPattern->getSubPattern();

    return Pattern;
  }

  PatternBindingDecl *Binding = nullptr;
};

std::unique_ptr<ExpandableTernaryExprInfo>
findExpandableTernaryExpression(ResolvedRangeInfo Info) {

  if (Info.Kind != RangeKind::SingleDecl
      && Info.Kind != RangeKind:: SingleExpression)
    return nullptr;

  if (Info.ContainedNodes.size() != 1)
    return nullptr;

  if (auto D = Info.ContainedNodes[0].dyn_cast<Decl*>())
    if (auto Binding = dyn_cast<PatternBindingDecl>(D))
      return llvm::make_unique<ExpandableBindingTernaryExprInfo>(Binding);

  if (auto E = Info.ContainedNodes[0].dyn_cast<Expr*>())
    if (auto Assign = dyn_cast<AssignExpr>(E))
      return llvm::make_unique<ExpandableAssignTernaryExprInfo>(Assign);

  return nullptr;
}

bool RefactoringActionExpandTernaryExpr::
isApplicable(ResolvedRangeInfo Info, DiagnosticEngine &Diag) {
  auto Target = findExpandableTernaryExpression(Info);
  return Target && Target->isValid();
}

bool RefactoringActionExpandTernaryExpr::performChange() {
  auto Target = findExpandableTernaryExpression(RangeInfo);

  if (!Target || !Target->isValid())
    return true; //abort

  auto NameCharRange = Target->getNameCharRange(SM);

  auto IfRange = Target->getIf()->getSourceRange();
  auto IfCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, IfRange);

  auto CondRange = Target->getIf()->getCondExpr()->getSourceRange();
  auto CondCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, CondRange);

  auto ThenRange = Target->getIf()->getThenExpr()->getSourceRange();
  auto ThenCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, ThenRange);

  auto ElseRange = Target->getIf()->getElseExpr()->getSourceRange();
  auto ElseCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, ElseRange);

  llvm::SmallString<64> DeclBuffer;
  llvm::raw_svector_ostream OS(DeclBuffer);

  llvm::StringRef Space = " ";
  llvm::StringRef NewLine = "\n";

  if (Target->shouldDeclareNameAndType()) {
    //Specifier will not be replaced; append after specifier
    OS << NameCharRange.str() << tok::colon << Space;
    OS << Target->getType() << NewLine;
  }

  OS << tok::kw_if << Space;
  OS << CondCharRange.str() << Space;
  OS << tok::l_brace << NewLine;

  OS << NameCharRange.str() << Space;
  OS << tok::equal << Space;
  OS << ThenCharRange.str() << NewLine;

  OS << tok::r_brace << Space;
  OS << tok::kw_else << Space;
  OS << tok::l_brace << NewLine;

  OS << NameCharRange.str() << Space;
  OS << tok::equal << Space;
  OS << ElseCharRange.str() << NewLine;

  OS << tok::r_brace;

  //Start replacement with name range, skip the specifier
  auto ReplaceRange(NameCharRange);
  ReplaceRange.widen(IfCharRange);

  EditConsumer.accept(SM, ReplaceRange, DeclBuffer.str());

  return false; //don't abort
}

/// Struct containing info about an IfStmt that can be converted into an IfExpr.
struct ConvertToTernaryExprInfo {
  ConvertToTernaryExprInfo() {}

  Expr *AssignDest() {

    if (!Then || !Then->getDest() || !Else || !Else->getDest())
      return nullptr;

    auto ThenDest = Then->getDest();
    auto ElseDest = Else->getDest();

    if (ThenDest->getKind() != ElseDest->getKind())
      return nullptr;

    switch (ThenDest->getKind()) {
      case ExprKind::DeclRef: {
        auto ThenRef = dyn_cast<DeclRefExpr>(Then->getDest());
        auto ElseRef = dyn_cast<DeclRefExpr>(Else->getDest());

        if (!ThenRef || !ThenRef->getDecl() || !ElseRef || !ElseRef->getDecl())
          return nullptr;

        auto ThenName = ThenRef->getDecl()->getFullName();
        auto ElseName = ElseRef->getDecl()->getFullName();

        if (ThenName.compare(ElseName) != 0)
          return nullptr;

        return Then->getDest();
      }
      case ExprKind::Tuple: {
        auto ThenTuple = dyn_cast<TupleExpr>(Then->getDest());
        auto ElseTuple = dyn_cast<TupleExpr>(Else->getDest());

        if (!ThenTuple || !ElseTuple)
          return nullptr;

        auto ThenNames = ThenTuple->getElementNames();
        auto ElseNames = ElseTuple->getElementNames();

        if (!ThenNames.equals(ElseNames))
          return nullptr;

        return ThenTuple;
      }
      default:
        return nullptr;
    }
  }

  Expr *ThenSrc() {
    if (!Then)
      return nullptr;
    return Then->getSrc();
  }

  Expr *ElseSrc() {
    if (!Else)
      return nullptr;
    return Else->getSrc();
  }

  bool isValid() {
    if (!Cond || !AssignDest() || !ThenSrc() || !ElseSrc()
        || !IfRange.isValid())
      return false;

    return true;
  }

  PatternBindingDecl *Binding = nullptr; //optional

  Expr *Cond = nullptr; //required
  AssignExpr *Then = nullptr; //required
  AssignExpr *Else = nullptr; //required
  SourceRange IfRange;
};

ConvertToTernaryExprInfo
findConvertToTernaryExpression(ResolvedRangeInfo Info) {

  auto notFound = ConvertToTernaryExprInfo();

  if (Info.Kind != RangeKind::SingleStatement
      && Info.Kind != RangeKind::MultiStatement)
    return notFound;

  if (Info.ContainedNodes.empty())
    return notFound;

  struct AssignExprFinder: public SourceEntityWalker {

    AssignExpr *Assign = nullptr;

    AssignExprFinder(Stmt* S) {
      if (S)
        walk(S);
    }

    virtual bool walkToExprPre(Expr *E) {
      Assign = dyn_cast<AssignExpr>(E);
      return false;
    }
  };

  ConvertToTernaryExprInfo Target;

  IfStmt *If = nullptr;

  if (Info.ContainedNodes.size() == 1) {
    if (auto S = Info.ContainedNodes[0].dyn_cast<Stmt*>())
      If = dyn_cast<IfStmt>(S);
  }

  if (Info.ContainedNodes.size() == 2) {
    if (auto D = Info.ContainedNodes[0].dyn_cast<Decl*>())
      Target.Binding = dyn_cast<PatternBindingDecl>(D);
    if (auto S = Info.ContainedNodes[1].dyn_cast<Stmt*>())
      If = dyn_cast<IfStmt>(S);
  }

  if (!If)
    return notFound;

  auto CondList = If->getCond();

  if (CondList.size() != 1)
    return notFound;

  Target.Cond = CondList[0].getBooleanOrNull();
  Target.IfRange = If->getSourceRange();

  Target.Then = AssignExprFinder(If->getThenStmt()).Assign;
  Target.Else = AssignExprFinder(If->getElseStmt()).Assign;

  return Target;
}

bool RefactoringActionConvertToTernaryExpr::
isApplicable(ResolvedRangeInfo Info, DiagnosticEngine &Diag) {
  return findConvertToTernaryExpression(Info).isValid();
}

bool RefactoringActionConvertToTernaryExpr::performChange() {
  auto Target = findConvertToTernaryExpression(RangeInfo);

  if (!Target.isValid())
    return true; //abort

  llvm::SmallString<64> DeclBuffer;
  llvm::raw_svector_ostream OS(DeclBuffer);

  llvm::StringRef Space = " ";

  auto IfRange = Target.IfRange;
  auto ReplaceRange = Lexer::getCharSourceRangeFromSourceRange(SM, IfRange);

  auto CondRange = Target.Cond->getSourceRange();
  auto CondCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, CondRange);

  auto ThenRange = Target.ThenSrc()->getSourceRange();
  auto ThenCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, ThenRange);

  auto ElseRange = Target.ElseSrc()->getSourceRange();
  auto ElseCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, ElseRange);

  CharSourceRange DestCharRange;

  if (Target.Binding) {
    auto DestRange = Target.Binding->getSourceRange();
    DestCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, DestRange);
    ReplaceRange.widen(DestCharRange);
  } else {
    auto DestRange = Target.AssignDest()->getSourceRange();
    DestCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, DestRange);
  }

  OS << DestCharRange.str() << Space << tok::equal << Space;
  OS << CondCharRange.str() << Space << tok::question_postfix << Space;
  OS << ThenCharRange.str() << Space << tok::colon << Space;
  OS << ElseCharRange.str();

  EditConsumer.accept(SM, ReplaceRange, DeclBuffer.str());

  return false; //don't abort
}

/// The helper class analyzes a given nominal decl or an extension decl to
/// decide whether stubs are required to filled in and the context in which
/// these stubs should be filled.
class FillProtocolStubContext {

  std::vector<ValueDecl*> getUnsatisfiedRequirements(const DeclContext *DC);

  /// Context in which the content should be filled; this could be either a
  /// nominal type declaraion or an extension declaration.
  DeclContext *DC;

  /// The type that adopts the required protocol stubs. For nominal type decl, this
  /// should be the declared type itself; for extension decl, this should be the
  /// extended type at hand.
  Type Adopter;

  /// The start location of the decl, either nominal type or extension, for the
  /// printer to figure out the right indentation.
  SourceLoc StartLoc;

  /// The location of '{' for the decl, thus we know where to insert the filling
  /// stubs.
  SourceLoc BraceStartLoc;

  /// The value decls that should be satisfied; this could be either function
  /// decls, property decls, or required type alias.
  std::vector<ValueDecl*> FillingContents;

public:
  FillProtocolStubContext(ExtensionDecl *ED) : DC(ED),
    Adopter(ED->getExtendedType()), StartLoc(ED->getStartLoc()),
    BraceStartLoc(ED->getBraces().Start),
    FillingContents(getUnsatisfiedRequirements(ED)) {};

  FillProtocolStubContext(NominalTypeDecl *ND) : DC(ND),
    Adopter(ND->getDeclaredType()), StartLoc(ND->getStartLoc()),
    BraceStartLoc(ND->getBraces().Start),
    FillingContents(getUnsatisfiedRequirements(ND)) {};

  FillProtocolStubContext() : DC(nullptr), Adopter(), FillingContents({}) {};

  static FillProtocolStubContext getContextFromCursorInfo(ResolvedCursorInfo Tok);

  ArrayRef<ValueDecl*> getFillingContents() const {
    return llvm::makeArrayRef(FillingContents);
  }

  DeclContext *getFillingContext() const { return DC; }

  bool canProceed() const {
    return StartLoc.isValid() && BraceStartLoc.isValid() &&
      !getFillingContents().empty();
  }

  Type getAdopter() const { return Adopter; }
  SourceLoc getContextStartLoc() const { return StartLoc; }
  SourceLoc getBraceStartLoc() const { return BraceStartLoc; }
};

FillProtocolStubContext FillProtocolStubContext::
getContextFromCursorInfo(ResolvedCursorInfo CursorInfo) {
  if(!CursorInfo.isValid())
    return FillProtocolStubContext();
  if (!CursorInfo.IsRef) {
    // If the type name is on the declared nominal, e.g. "class A {}"
    if (auto ND = dyn_cast<NominalTypeDecl>(CursorInfo.ValueD)) {
      return FillProtocolStubContext(ND);
    }
  } else if (auto *ED = CursorInfo.ExtTyRef) {
    // If the type ref is on a declared extension, e.g. "extension A {}"
    return FillProtocolStubContext(ED);
  }
  return FillProtocolStubContext();
}

std::vector<ValueDecl*> FillProtocolStubContext::
getUnsatisfiedRequirements(const DeclContext *DC) {
  // The results to return.
  std::vector<ValueDecl*> NonWitnessedReqs;

  // For each conformance of the extended nominal.
  for(ProtocolConformance *Con : DC->getLocalConformances()) {

    // Collect non-witnessed requirements.
    Con->forEachNonWitnessedRequirement(DC->getASTContext().getLazyResolver(),
      [&](ValueDecl *VD) { NonWitnessedReqs.push_back(VD); });
  }

  return NonWitnessedReqs;
}

bool RefactoringActionFillProtocolStub::
isApplicable(ResolvedCursorInfo Tok, DiagnosticEngine &Diag) {
  return FillProtocolStubContext::getContextFromCursorInfo(Tok).canProceed();
};

bool RefactoringActionFillProtocolStub::performChange() {
  // Get the filling protocol context from the input token.
  FillProtocolStubContext Context = FillProtocolStubContext::
    getContextFromCursorInfo(CursorInfo);

  assert(Context.canProceed());
  assert(!Context.getFillingContents().empty());
  assert(Context.getFillingContext());
  llvm::SmallString<128> Text;
  {
    llvm::raw_svector_ostream SS(Text);
    Type Adopter = Context.getAdopter();
    SourceLoc Loc = Context.getContextStartLoc();
    auto Contents = Context.getFillingContents();

    // For each unsatisfied requirement, print the stub to the buffer.
    std::for_each(Contents.begin(), Contents.end(), [&](ValueDecl *VD) {
      printRequirementStub(VD, Context.getFillingContext(), Adopter, Loc, SS);
    });
  }

  // Insert all stubs after '{' in the extension/nominal type decl.
  EditConsumer.insertAfter(SM, Context.getBraceStartLoc(), Text);
  return false;
}

ArrayRef<RefactoringKind>
collectAvailableRefactoringsAtCursor(SourceFile *SF, unsigned Line,
                                     unsigned Column,
                                     std::vector<RefactoringKind> &Scratch,
                            llvm::ArrayRef<DiagnosticConsumer*> DiagConsumers) {
  // Prepare the tool box.
  ASTContext &Ctx = SF->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;
  DiagnosticEngine DiagEngine(SM);
  std::for_each(DiagConsumers.begin(), DiagConsumers.end(),
                [&](DiagnosticConsumer *Con) { DiagEngine.addConsumer(*Con); });
  CursorInfoResolver Resolver(*SF);
  SourceLoc Loc = SM.getLocForLineCol(SF->getBufferID().getValue(), Line, Column);
  if (Loc.isInvalid())
    return {};
  ResolvedCursorInfo Tok = Resolver.resolve(Lexer::getLocForStartOfToken(SM, Loc));
  return collectAvailableRefactorings(SF, Tok, Scratch, /*Exclude rename*/false);
}

static EnumDecl* getEnumDeclFromSwitchStmt(SwitchStmt *SwitchS) {
  if (auto SubjectTy = SwitchS->getSubjectExpr()->getType()) {
    // FIXME: Support more complex subject like '(Enum1, Enum2)'.
    return dyn_cast_or_null<EnumDecl>(SubjectTy->getAnyNominal());
  }
  return nullptr;
}

static bool performCasesExpansionInSwitchStmt(SwitchStmt *SwitchS,
                                              DiagnosticEngine &DiagEngine,
                                              SourceLoc ExpandedStmtLoc,
                                              EditorConsumerInsertStream &OS
                                              ) {
  // Assume enum elements are not handled in the switch statement.
  auto EnumDecl = getEnumDeclFromSwitchStmt(SwitchS);
  assert(EnumDecl);
  llvm::DenseSet<EnumElementDecl*> UnhandledElements;
  EnumDecl->getAllElements(UnhandledElements);
  for (auto Current : SwitchS->getCases()) {
    if (Current->isDefault()) {
      continue;
    }
    // For each handled enum element, remove it from the bucket.
    for (auto Item : Current->getCaseLabelItems()) {
      if (auto *EEP = dyn_cast_or_null<EnumElementPattern>(Item.getPattern())) {
        UnhandledElements.erase(EEP->getElementDecl());
      }
    }
  }

  // If all enum elements are handled in the switch statement, issue error.
  if (UnhandledElements.empty()) {
    DiagEngine.diagnose(ExpandedStmtLoc, diag::no_remaining_cases);
    return true;
  }

  printEnumElementsAsCases(UnhandledElements, OS);
  return false;
}

// Finds SwitchStmt that contains given CaseStmt.
static SwitchStmt* findEnclosingSwitchStmt(CaseStmt *CS,
                                           SourceFile *SF,
                                           DiagnosticEngine &DiagEngine) {
  auto IsSwitch = [](ASTNode Node) {
    return Node.is<Stmt*>() &&
    Node.get<Stmt*>()->getKind() == StmtKind::Switch;
  };
  ContextFinder Finder(*SF, CS, IsSwitch);
  Finder.resolve();

  // If failed to find the switch statement, issue error.
  if (Finder.getContexts().empty()) {
    DiagEngine.diagnose(CS->getStartLoc(), diag::no_parent_switch);
    return nullptr;
  }
  auto *SwitchS = static_cast<SwitchStmt*>(Finder.getContexts().back().
                                           get<Stmt*>());
  // Make sure that CaseStmt is included in switch that was found.
  auto Cases = SwitchS->getCases();
  auto Default = std::find(Cases.begin(), Cases.end(), CS);
  if (Default == Cases.end()) {
    DiagEngine.diagnose(CS->getStartLoc(), diag::no_parent_switch);
    return nullptr;
  }
  return SwitchS;
}

bool RefactoringActionExpandDefault::
isApplicable(ResolvedCursorInfo CursorInfo, DiagnosticEngine &Diag) {
  auto Exit = [&](bool Applicable) {
    if (!Applicable)
      Diag.diagnose(SourceLoc(), diag::invalid_default_location);
    return Applicable;
  };
  if (CursorInfo.Kind != CursorInfoKind::StmtStart)
    return Exit(false);
  if (auto *CS = dyn_cast<CaseStmt>(CursorInfo.TrailingStmt)) {
    auto EnclosingSwitchStmt = findEnclosingSwitchStmt(CS,
                                                       CursorInfo.SF,
                                                       Diag);
    if (!EnclosingSwitchStmt)
      return false;
    auto EnumD = getEnumDeclFromSwitchStmt(EnclosingSwitchStmt);
    auto IsApplicable = CS->isDefault() && EnumD != nullptr;
    return IsApplicable;
  }
  return Exit(false);
}

bool RefactoringActionExpandDefault::performChange() {
  // If we've not seen the default statement inside the switch statement, issue
  // error.
  auto *CS = static_cast<CaseStmt*>(CursorInfo.TrailingStmt);
  auto *SwitchS = findEnclosingSwitchStmt(CS, TheFile, DiagEngine);
  assert(SwitchS);
  EditorConsumerInsertStream OS(EditConsumer, SM,
                                Lexer::getCharSourceRangeFromSourceRange(SM,
                                  CS->getLabelItemsRange()));
  return performCasesExpansionInSwitchStmt(SwitchS,
                                           DiagEngine,
                                           CS->getStartLoc(),
                                           OS);
}

bool RefactoringActionExpandSwitchCases::
isApplicable(ResolvedCursorInfo CursorInfo, DiagnosticEngine &DiagEngine) {
  if (!CursorInfo.TrailingStmt)
    return false;
  if (auto *Switch = dyn_cast<SwitchStmt>(CursorInfo.TrailingStmt)) {
    return getEnumDeclFromSwitchStmt(Switch);
  }
  return false;
}

bool RefactoringActionExpandSwitchCases::performChange() {
  auto *SwitchS = dyn_cast<SwitchStmt>(CursorInfo.TrailingStmt);
  assert(SwitchS);

  auto InsertRange = CharSourceRange();
  auto Cases = SwitchS->getCases();
  auto Default = std::find_if(Cases.begin(), Cases.end(), [](CaseStmt *Stmt) {
    return Stmt->isDefault();
  });
  if (Default != Cases.end()) {
    auto DefaultRange = (*Default)->getLabelItemsRange();
    InsertRange = Lexer::getCharSourceRangeFromSourceRange(SM, DefaultRange);
  } else {
    auto RBraceLoc = SwitchS->getRBraceLoc();
    InsertRange = CharSourceRange(SM, RBraceLoc, RBraceLoc);
  }
  EditorConsumerInsertStream OS(EditConsumer, SM, InsertRange);
  if (SM.getLineNumber(SwitchS->getLBraceLoc()) ==
      SM.getLineNumber(SwitchS->getRBraceLoc())) {
    OS << "\n";
  }
  auto Result = performCasesExpansionInSwitchStmt(SwitchS,
                                           DiagEngine,
                                           SwitchS->getStartLoc(),
                                           OS);
  return Result;
}

static Expr *findLocalizeTarget(ResolvedCursorInfo CursorInfo) {
  if (CursorInfo.Kind != CursorInfoKind::ExprStart)
    return nullptr;
  struct StringLiteralFinder: public SourceEntityWalker {
    SourceLoc StartLoc;
    Expr *Target;
    StringLiteralFinder(SourceLoc StartLoc): StartLoc(StartLoc), Target(nullptr) {}
    bool walkToExprPre(Expr *E) {
      if (E->getStartLoc() != StartLoc)
        return false;
      if (E->getKind() == ExprKind::InterpolatedStringLiteral)
        return false;
      if (E->getKind() == ExprKind::StringLiteral) {
        Target = E;
        return false;
      }
      return true;
    }
  } Walker(CursorInfo.TrailingExpr->getStartLoc());
  Walker.walk(CursorInfo.TrailingExpr);
  return Walker.Target;
}

bool RefactoringActionLocalizeString::
isApplicable(ResolvedCursorInfo Tok, DiagnosticEngine &Diag) {
  return findLocalizeTarget(Tok);
}

bool RefactoringActionLocalizeString::performChange() {
  Expr* Target = findLocalizeTarget(CursorInfo);
   if (!Target)
    return true;
  EditConsumer.accept(SM, Target->getStartLoc(), "NSLocalizedString(");
  EditConsumer.insertAfter(SM, Target->getEndLoc(), ", comment: \"\")");
  return false;
}

static void generateMemberwiseInit(SourceEditConsumer &EditConsumer,
                            SourceManager &SM,
                            SmallVectorImpl<std::string>& memberNameVector,
                            SmallVectorImpl<std::string>& memberTypeVector,
                            SourceLoc targetLocation) {
  
  assert(!memberTypeVector.empty());
  assert(memberTypeVector.size() == memberNameVector.size());
  
  EditConsumer.accept(SM, targetLocation, "\ninternal init(");
  
  for (size_t i = 0, n = memberTypeVector.size(); i < n ; i++) {
    EditConsumer.accept(SM, targetLocation, memberNameVector[i] + ": " +
                        memberTypeVector[i]);
    
    if (i != memberTypeVector.size() - 1) {
      EditConsumer.accept(SM, targetLocation, ", ");
    }
  }
  
  EditConsumer.accept(SM, targetLocation, ") {\n");
  
  for (auto varName: memberNameVector) {
    EditConsumer.accept(SM, targetLocation,
                        "self." + varName + " = " + varName + "\n");
  }
  
  EditConsumer.accept(SM, targetLocation, "}\n");
}
  
static SourceLoc collectMembersForInit(ResolvedCursorInfo CursorInfo,
                           SmallVectorImpl<std::string>& memberNameVector,
                           SmallVectorImpl<std::string>& memberTypeVector) {
  
  if (!CursorInfo.ValueD)
    return SourceLoc();
  
  ClassDecl *classDecl = dyn_cast<ClassDecl>(CursorInfo.ValueD);
  if (!classDecl || classDecl->getStoredProperties().empty() ||
      CursorInfo.IsRef) {
    return SourceLoc();
  }
  
  SourceLoc bracesStart = classDecl->getBraces().Start;
  if (!bracesStart.isValid())
    return SourceLoc();
  
  SourceLoc targetLocation = bracesStart.getAdvancedLoc(1);
  if (!targetLocation.isValid())
    return SourceLoc();
  
  for (auto varDecl : classDecl->getStoredProperties()) {
    auto parentPatternBinding = varDecl->getParentPatternBinding();
    if (!parentPatternBinding)
      continue;
    
    auto varDeclIndex =
      parentPatternBinding->getPatternEntryIndexForVarDecl(varDecl);
    
    if (auto init = varDecl->getParentPatternBinding()->getInit(varDeclIndex)) {
      if (init->getStartLoc().isValid())
        continue;
    }
    
    StringRef memberName = varDecl->getName().str();
    memberNameVector.push_back(memberName.str());
    
    std::string memberType = varDecl->getType().getString();
    memberTypeVector.push_back(memberType);
  }
  
  if (memberNameVector.empty() || memberTypeVector.empty()) {
    return SourceLoc();
  }
  
  return targetLocation;
}
  
bool RefactoringActionMemberwiseInitLocalRefactoring::
isApplicable(ResolvedCursorInfo Tok, DiagnosticEngine &Diag) {
  
  SmallVector<std::string, 8> memberNameVector;
  SmallVector<std::string, 8> memberTypeVector;
  
  return collectMembersForInit(Tok, memberNameVector,
                               memberTypeVector).isValid();
}
    
bool RefactoringActionMemberwiseInitLocalRefactoring::performChange() {
  
  SmallVector<std::string, 8> memberNameVector;
  SmallVector<std::string, 8> memberTypeVector;
  
  SourceLoc targetLocation = collectMembersForInit(CursorInfo, memberNameVector,
                                         memberTypeVector);
  if (targetLocation.isInvalid())
    return true;
  
  generateMemberwiseInit(EditConsumer, SM, memberNameVector,
                         memberTypeVector, targetLocation);
  
  return false;
}
  
bool RefactoringActionCustomizeEquatableConformance::
isApplicable(ResolvedCursorInfo Tok, DiagnosticEngine &Diag) {
  return true;
}

bool RefactoringActionCustomizeEquatableConformance::performChange() {
  return false;
}

static CharSourceRange
  findSourceRangeToWrapInCatch(ResolvedCursorInfo CursorInfo,
                               SourceFile *TheFile,
                               SourceManager &SM) {
  Expr *E = CursorInfo.TrailingExpr;
  if (!E)
    return CharSourceRange();
  auto Node = ASTNode(E);
  auto NodeChecker = [](ASTNode N) { return N.isStmt(StmtKind::Brace); };
  ContextFinder Finder(*TheFile, Node, NodeChecker);
  Finder.resolve();
  auto Contexts = Finder.getContexts();
  if (Contexts.empty())
    return CharSourceRange();
  auto TargetNode = Contexts.back();
  BraceStmt *BStmt = dyn_cast<BraceStmt>(TargetNode.dyn_cast<Stmt*>());
  auto ConvertToCharRange = [&SM](SourceRange SR) {
    return Lexer::getCharSourceRangeFromSourceRange(SM, SR);
  };
  assert(BStmt);
  auto ExprRange = ConvertToCharRange(E->getSourceRange());
  // Check elements of the deepest BraceStmt, pick one that covers expression.
  for (auto Elem: BStmt->getElements()) {
    auto ElemRange = ConvertToCharRange(Elem.getSourceRange());
    if (ElemRange.contains(ExprRange))
      TargetNode = Elem;
  }
  return ConvertToCharRange(TargetNode.getSourceRange());
}

bool RefactoringActionConvertToDoCatch::
isApplicable(ResolvedCursorInfo Tok, DiagnosticEngine &Diag) {
  if (!Tok.TrailingExpr)
    return false;
  return isa<ForceTryExpr>(Tok.TrailingExpr);
}

bool RefactoringActionConvertToDoCatch::performChange() {
  auto *TryExpr = dyn_cast<ForceTryExpr>(CursorInfo.TrailingExpr);
  assert(TryExpr);
  auto Range = findSourceRangeToWrapInCatch(CursorInfo, TheFile, SM);
  if (!Range.isValid())
    return true;
  // Wrap given range in do catch block.
  EditConsumer.accept(SM, Range.getStart(), "do {\n");
  EditorConsumerInsertStream OS(EditConsumer, SM, Range.getEnd());
  OS << "\n} catch {\n" << getCodePlaceholder() << "\n}";

  // Delete ! from try! expression
  auto ExclaimLen = getKeywordLen(tok::exclaim_postfix);
  auto ExclaimRange = CharSourceRange(TryExpr->getExclaimLoc(), ExclaimLen);
  EditConsumer.remove(SM, ExclaimRange);
  return false;
}

/// Given a cursor position, this function tries to collect a number literal
/// expression immediately following the cursor.
static NumberLiteralExpr *getTrailingNumberLiteral(ResolvedCursorInfo Tok) {
  // This cursor must point to the start of an expression.
  if (Tok.Kind != CursorInfoKind::ExprStart)
    return nullptr;
  Expr *Parent = Tok.TrailingExpr;
  assert(Parent);

  // Check if an expression is a number literal.
  auto IsLiteralNumber = [&](Expr *E) -> NumberLiteralExpr* {
    if (auto *NL = dyn_cast<NumberLiteralExpr>(E)) {

      // The sub-expression must have the same start loc with the outermost
      // expression, i.e. the cursor position.
      if (Parent->getStartLoc().getOpaquePointerValue() ==
        E->getStartLoc().getOpaquePointerValue()) {
        return NL;
      }
    }
    return nullptr;
  };
  // For every sub-expression, try to find the literal expression that matches
  // our criteria.
  for (auto Pair: Parent->getDepthMap()) {
    if (auto Result = IsLiteralNumber(Pair.getFirst())) {
      return Result;
    }
  }
  return nullptr;
}

static std::string insertUnderscore(StringRef Text) {
  llvm::SmallString<64> Buffer;
  llvm::raw_svector_ostream OS(Buffer);
  for (auto It = Text.begin(); It != Text.end(); It++) {
    unsigned Distance = It - Text.begin();
    if (Distance && !(Distance % 3)) {
      OS << '_';
    }
    OS << *It;
  }
  return OS.str().str();
}

static void insertUnderscoreInDigits(StringRef Digits,
                                     llvm::raw_ostream &OS) {
  std::string BeforePoint, AfterPoint;
  std::tie(BeforePoint, AfterPoint) = Digits.split('.');

  // Insert '_' for the part before the decimal point.
  std::reverse(BeforePoint.begin(), BeforePoint.end());
  BeforePoint = insertUnderscore(BeforePoint);
  std::reverse(BeforePoint.begin(), BeforePoint.end());
  OS << BeforePoint;

  // Insert '_' for the part after the decimal point, if necessary.
  if (!AfterPoint.empty()) {
    OS << '.';
    OS << insertUnderscore(AfterPoint);
  }
}

bool RefactoringActionSimplifyNumberLiteral::
isApplicable(ResolvedCursorInfo Tok, DiagnosticEngine &Diag) {
  if (auto *Literal = getTrailingNumberLiteral(Tok)) {
    llvm::SmallString<64> Buffer;
    llvm::raw_svector_ostream OS(Buffer);
    StringRef Digits = Literal->getDigitsText();
    insertUnderscoreInDigits(Digits, OS);

    // If inserting '_' results in a different digit sequence, this refactoring
    // is applicable.
    return OS.str() != Digits;
  }
  return false;
}

bool RefactoringActionSimplifyNumberLiteral::performChange() {
  if (auto *Literal = getTrailingNumberLiteral(CursorInfo)) {

    EditorConsumerInsertStream OS(EditConsumer, SM,
                                  CharSourceRange(SM, Literal->getDigitsLoc(),
                                  Lexer::getLocForEndOfToken(SM,
                                    Literal->getEndLoc())));
    StringRef Digits = Literal->getDigitsText();
    insertUnderscoreInDigits(Digits, OS);
    return false;
  }
  return true;
}

static CallExpr *findTrailingClosureTarget(SourceManager &SM,
                                           ResolvedCursorInfo CursorInfo) {
  if (CursorInfo.Kind == CursorInfoKind::StmtStart)
    // StmtStart postion can't be a part of CallExpr.
    return nullptr;

  // Find inner most CallExpr
  ContextFinder
  Finder(*CursorInfo.SF, CursorInfo.Loc,
         [](ASTNode N) {
           return N.isStmt(StmtKind::Brace) || N.isExpr(ExprKind::Call);
         });
  Finder.resolve();
  if (Finder.getContexts().empty()
      || !Finder.getContexts().back().is<Expr*>())
    return nullptr;
  CallExpr *CE = cast<CallExpr>(Finder.getContexts().back().get<Expr*>());

  if (CE->hasTrailingClosure())
    // Call expression already has a trailing closure.
    return nullptr;

  // The last arugment is a closure?
  Expr *Args = CE->getArg();
  if (!Args)
    return nullptr;
  Expr *LastArg;
  if (auto *TSE = dyn_cast<TupleShuffleExpr>(Args))
    Args = TSE->getSubExpr();
  if (auto *PE = dyn_cast<ParenExpr>(Args)) {
    LastArg = PE->getSubExpr();
  } else {
    auto *TE = cast<TupleExpr>(Args);
    if (TE->getNumElements() == 0)
      return nullptr;
    LastArg = TE->getElements().back();
  }

  if (auto *ICE = dyn_cast<ImplicitConversionExpr>(LastArg))
    LastArg = ICE->getSyntacticSubExpr();

  if (isa<ClosureExpr>(LastArg) || isa<CaptureListExpr>(LastArg))
    return CE;
  return nullptr;
}

bool RefactoringActionTrailingClosure::
isApplicable(ResolvedCursorInfo CursorInfo, DiagnosticEngine &Diag) {
  SourceManager &SM = CursorInfo.SF->getASTContext().SourceMgr;
  return findTrailingClosureTarget(SM, CursorInfo);
}

bool RefactoringActionTrailingClosure::performChange() {
  auto *CE = findTrailingClosureTarget(SM, CursorInfo);
  if (!CE)
    return true;
  Expr *Args = CE->getArg();
  if (auto *TSE = dyn_cast<TupleShuffleExpr>(Args))
    Args = TSE->getSubExpr();

  Expr *ClosureArg = nullptr;
  Expr *PrevArg = nullptr;
  SourceLoc LPLoc, RPLoc;

  if (auto *PE = dyn_cast<ParenExpr>(Args)) {
    ClosureArg = PE->getSubExpr();
    LPLoc = PE->getLParenLoc();
    RPLoc = PE->getRParenLoc();
  } else {
    auto *TE = cast<TupleExpr>(Args);
    auto NumArgs = TE->getNumElements();
    if (NumArgs == 0)
      return true;
    LPLoc = TE->getLParenLoc();
    RPLoc = TE->getRParenLoc();
    ClosureArg = TE->getElement(NumArgs - 1);
    if (NumArgs > 1)
      PrevArg = TE->getElement(NumArgs - 2);
  }
  if (auto *ICE = dyn_cast<ImplicitConversionExpr>(ClosureArg))
    ClosureArg = ICE->getSyntacticSubExpr();

  if (LPLoc.isInvalid() || RPLoc.isInvalid())
    return true;

  // Replace:
  //   * Open paren with ' ' if the closure is sole argument.
  //   * Comma with ') ' otherwise.
  if (PrevArg) {
    CharSourceRange PreRange(
        SM,
        Lexer::getLocForEndOfToken(SM, PrevArg->getEndLoc()),
        ClosureArg->getStartLoc());
    EditConsumer.accept(SM, PreRange, ") ");
  } else {
    CharSourceRange PreRange(
        SM, LPLoc, ClosureArg->getStartLoc());
    EditConsumer.accept(SM, PreRange, " ");
  }
  // Remove original closing paren.
  CharSourceRange PostRange(
      SM,
      Lexer::getLocForEndOfToken(SM, ClosureArg->getEndLoc()),
      Lexer::getLocForEndOfToken(SM, RPLoc));
  EditConsumer.remove(SM, PostRange);
  return false;
}

static bool rangeStartMayNeedRename(ResolvedRangeInfo Info) {
  switch(Info.Kind) {
    case RangeKind::SingleExpression: {
      Expr *E = Info.ContainedNodes[0].get<Expr*>();
      // We should show rename for the selection of "foo()"
      if (auto *CE = dyn_cast<CallExpr>(E)) {
        if (CE->getFn()->getKind() == ExprKind::DeclRef)
          return true;

        // When callling an instance method inside another instance method,
        // we have a dot syntax call whose dot and base are both implicit. We
        // need to explicitly allow the specific case here.
        if (auto *DSC = dyn_cast<DotSyntaxCallExpr>(CE->getFn())) {
          if (DSC->getBase()->isImplicit() &&
              DSC->getFn()->getStartLoc() == Info.TokensInRange.front().getLoc())
            return true;
        }
      }
      return false;
    }
    case RangeKind::PartOfExpression: {
      if (auto *CE = dyn_cast<CallExpr>(Info.CommonExprParent)) {
        if (auto *DSC = dyn_cast<DotSyntaxCallExpr>(CE->getFn())) {
          if (DSC->getFn()->getStartLoc() == Info.TokensInRange.front().getLoc())
            return true;
        }
      }
      return false;
    }
    case RangeKind::SingleDecl:
    case RangeKind::MultiTypeMemberDecl:
    case RangeKind::SingleStatement:
    case RangeKind::MultiStatement:
    case RangeKind::Invalid:
      return false;
  }
  llvm_unreachable("unhandled kind");
}
}// end of anonymous namespace

StringRef swift::ide::
getDescriptiveRefactoringKindName(RefactoringKind Kind) {
    switch(Kind) {
      case RefactoringKind::None:
        llvm_unreachable("Should be a valid refactoring kind");
#define REFACTORING(KIND, NAME, ID) case RefactoringKind::KIND: return NAME;
#include "swift/IDE/RefactoringKinds.def"
    }
    llvm_unreachable("unhandled kind");
  }

  StringRef swift::ide::
  getDescriptiveRenameUnavailableReason(RenameAvailableKind Kind) {
    switch(Kind) {
      case RenameAvailableKind::Available:
        return "";
      case RenameAvailableKind::Unavailable_system_symbol:
        return "symbol from system module cannot be renamed";
      case RenameAvailableKind::Unavailable_has_no_location:
        return "symbol without a declaration location cannot be renamed";
      case RenameAvailableKind::Unavailable_has_no_name:
        return "cannot find the name of the symbol";
      case RenameAvailableKind::Unavailable_has_no_accessibility:
        return "cannot decide the accessibility of the symbol";
      case RenameAvailableKind::Unavailable_decl_from_clang:
        return "cannot rename a Clang symbol from its Swift reference";
    }
    llvm_unreachable("unhandled kind");
  }

SourceLoc swift::ide::RangeConfig::getStart(SourceManager &SM) {
  return SM.getLocForLineCol(BufferId, Line, Column);
}

SourceLoc swift::ide::RangeConfig::getEnd(SourceManager &SM) {
  return getStart(SM).getAdvancedLoc(Length);
}

struct swift::ide::FindRenameRangesAnnotatingConsumer::Implementation {
  std::unique_ptr<SourceEditConsumer> pRewriter;
  Implementation(SourceManager &SM, unsigned BufferId, llvm::raw_ostream &OS)
  : pRewriter(new SourceEditOutputConsumer(SM, BufferId, OS)) {}
  static StringRef tag(RefactoringRangeKind Kind) {
    switch (Kind) {
      case RefactoringRangeKind::BaseName:
        return "base";
      case RefactoringRangeKind::KeywordBaseName:
        return "keywordBase";
      case RefactoringRangeKind::ParameterName:
        return "param";
      case RefactoringRangeKind::NoncollapsibleParameterName:
        return "noncollapsibleparam";
      case RefactoringRangeKind::DeclArgumentLabel:
        return "arglabel";
      case RefactoringRangeKind::CallArgumentLabel:
        return "callarg";
      case RefactoringRangeKind::CallArgumentColon:
        return "callcolon";
      case RefactoringRangeKind::CallArgumentCombined:
        return "callcombo";
      case RefactoringRangeKind::SelectorArgumentLabel:
        return "sel";
    }
    llvm_unreachable("unhandled kind");
  }
  void accept(SourceManager &SM, const RenameRangeDetail &Range) {
    std::string NewText;
    llvm::raw_string_ostream OS(NewText);
    StringRef Tag = tag(Range.RangeKind);
    OS << "<" << Tag;
    if (Range.Index.hasValue())
      OS << " index=" << *Range.Index;
    OS << ">" << Range.Range.str() << "</" << Tag << ">";
    pRewriter->accept(SM, {Range.Range, OS.str(), {}});
  }
};

swift::ide::FindRenameRangesAnnotatingConsumer::
FindRenameRangesAnnotatingConsumer(SourceManager &SM, unsigned BufferId,
   llvm::raw_ostream &OS): Impl(*new Implementation(SM, BufferId, OS)) {}

swift::ide::FindRenameRangesAnnotatingConsumer::~FindRenameRangesAnnotatingConsumer() {
  delete &Impl;
}

void swift::ide::FindRenameRangesAnnotatingConsumer::
accept(SourceManager &SM, RegionType RegionType,
       ArrayRef<RenameRangeDetail> Ranges) {
  if (RegionType == RegionType::Mismatch || RegionType == RegionType::Unmatched)
    return;
  for (const auto &Range : Ranges) {
    Impl.accept(SM, Range);
  }
}
ArrayRef<RenameAvailabiliyInfo>
swift::ide::collectRenameAvailabilityInfo(const ValueDecl *VD,
                                std::vector<RenameAvailabiliyInfo> &Scratch) {
  RenameAvailableKind AvailKind = RenameAvailableKind::Available;
  if (getRelatedSystemDecl(VD)){
    AvailKind = RenameAvailableKind::Unavailable_system_symbol;
  } else if (VD->getClangDecl()) {
    AvailKind = RenameAvailableKind::Unavailable_decl_from_clang;
  } else if (VD->getStartLoc().isInvalid()) {
    AvailKind = RenameAvailableKind::Unavailable_has_no_location;
  } else if (!VD->hasName()) {
    AvailKind = RenameAvailableKind::Unavailable_has_no_name;
  }

  if (isa<AbstractFunctionDecl>(VD)) {
    // Disallow renaming accessors.
    if (isa<AccessorDecl>(VD))
      return Scratch;

    // Disallow renaming deinit.
    if (isa<DestructorDecl>(VD))
      return Scratch;
    
    // Disallow renaming init with no arguments.
    if (auto CD = dyn_cast<ConstructorDecl>(VD)) {
      if (!CD->getParameters()->size())
        return Scratch;
    }
  }

  // Always return local rename for parameters.
  // FIXME: if the cursor is on the argument, we should return global rename.
  if (isa<ParamDecl>(VD)) {
    Scratch.emplace_back(RefactoringKind::LocalRename, AvailKind);
    return Scratch;
  }

  // If the indexer considers VD a global symbol, then we apply global rename.
  if (index::isLocalSymbol(VD))
    Scratch.emplace_back(RefactoringKind::LocalRename, AvailKind);
  else
    Scratch.emplace_back(RefactoringKind::GlobalRename, AvailKind);

  return llvm::makeArrayRef(Scratch);
}

ArrayRef<RefactoringKind> swift::ide::
collectAvailableRefactorings(SourceFile *SF,
                             ResolvedCursorInfo CursorInfo,
                             std::vector<RefactoringKind> &Scratch,
                             bool ExcludeRename) {
  llvm::SmallVector<RefactoringKind, 2> AllKinds;
  switch(CursorInfo.Kind) {
  case CursorInfoKind::ModuleRef:
  case CursorInfoKind::Invalid:
  case CursorInfoKind::StmtStart:
  case CursorInfoKind::ExprStart:
    break;
  case CursorInfoKind::ValueRef: {
    auto RenameOp = getAvailableRenameForDecl(CursorInfo.ValueD);
    if (RenameOp.hasValue() &&
        RenameOp.getValue() == RefactoringKind::GlobalRename)
      AllKinds.push_back(RenameOp.getValue());
    }
  }
  DiagnosticEngine DiagEngine(SF->getASTContext().SourceMgr);
#define CURSOR_REFACTORING(KIND, NAME, ID)                                     \
  if (RefactoringAction##KIND::isApplicable(CursorInfo, DiagEngine))           \
    AllKinds.push_back(RefactoringKind::KIND);
#include "swift/IDE/RefactoringKinds.def"

  // Exclude renames.
  for(auto Kind: AllKinds) {
    switch (Kind) {
    case RefactoringKind::LocalRename:
    case RefactoringKind::GlobalRename:
      if (ExcludeRename)
        break;
      LLVM_FALLTHROUGH;
    default:
      Scratch.push_back(Kind);
    }
  }

  return llvm::makeArrayRef(Scratch);
}



ArrayRef<RefactoringKind> swift::ide::
collectAvailableRefactorings(SourceFile *SF, RangeConfig Range,
                             bool &RangeStartMayNeedRename,
                             std::vector<RefactoringKind> &Scratch,
                             llvm::ArrayRef<DiagnosticConsumer*> DiagConsumers) {

  if (Range.Length == 0) {
    return collectAvailableRefactoringsAtCursor(SF, Range.Line, Range.Column,
                                                Scratch, DiagConsumers);
  }
  // Prepare the tool box.
  ASTContext &Ctx = SF->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;
  DiagnosticEngine DiagEngine(SM);
  std::for_each(DiagConsumers.begin(), DiagConsumers.end(),
    [&](DiagnosticConsumer *Con) { DiagEngine.addConsumer(*Con); });

  RangeResolver Resolver(*SF, Range.getStart(SF->getASTContext().SourceMgr),
                         Range.getEnd(SF->getASTContext().SourceMgr));
  ResolvedRangeInfo Result = Resolver.resolve();

  bool enableInternalRefactoring = getenv("SWIFT_ENABLE_INTERNAL_REFACTORING_ACTIONS");

#define RANGE_REFACTORING(KIND, NAME, ID)                                     \
  if (RefactoringAction##KIND::isApplicable(Result, DiagEngine))              \
    Scratch.push_back(RefactoringKind::KIND);
#define INTERNAL_RANGE_REFACTORING(KIND, NAME, ID)                            \
  if (enableInternalRefactoring)                                              \
    RANGE_REFACTORING(KIND, NAME, ID)
#include "swift/IDE/RefactoringKinds.def"

  RangeStartMayNeedRename = rangeStartMayNeedRename(Result);
  return Scratch;
}

bool swift::ide::
refactorSwiftModule(ModuleDecl *M, RefactoringOptions Opts,
                    SourceEditConsumer &EditConsumer,
                    DiagnosticConsumer &DiagConsumer) {
  assert(Opts.Kind != RefactoringKind::None && "should have a refactoring kind.");

  // Use the default name if not specified.
  if (Opts.PreferredName.empty()) {
    Opts.PreferredName = getDefaultPreferredName(Opts.Kind);
  }

  switch (Opts.Kind) {
#define SEMANTIC_REFACTORING(KIND, NAME, ID)                                   \
case RefactoringKind::KIND: {                                                  \
      RefactoringAction##KIND Action(M, Opts, EditConsumer, DiagConsumer);     \
      if (RefactoringKind::KIND == RefactoringKind::LocalRename ||             \
          Action.isApplicable())                                               \
        return Action.performChange();                                         \
      return true;                                                             \
  }
#include "swift/IDE/RefactoringKinds.def"
    case RefactoringKind::GlobalRename:
    case RefactoringKind::FindGlobalRenameRanges:
    case RefactoringKind::FindLocalRenameRanges:
      llvm_unreachable("not a valid refactoring kind");
    case RefactoringKind::None:
      llvm_unreachable("should not enter here.");
  }
  llvm_unreachable("unhandled kind");
}

static std::vector<ResolvedLoc>
resolveRenameLocations(ArrayRef<RenameLoc> RenameLocs, SourceFile &SF,
                       DiagnosticEngine &Diags) {
  SourceManager &SM = SF.getASTContext().SourceMgr;
  unsigned BufferID = SF.getBufferID().getValue();

  std::vector<UnresolvedLoc> UnresolvedLocs;
  for (const RenameLoc &RenameLoc : RenameLocs) {
    DeclNameViewer OldName(RenameLoc.OldName);
    SourceLoc Location = SM.getLocForLineCol(BufferID, RenameLoc.Line,
                                             RenameLoc.Column);

    if (!OldName.isValid()) {
      Diags.diagnose(Location, diag::invalid_name, RenameLoc.OldName);
      return {};
    }

    if (!RenameLoc.NewName.empty()) {
      DeclNameViewer NewName(RenameLoc.NewName);
      ArrayRef<StringRef> ParamNames = NewName.args();
      bool newOperator = Lexer::isOperator(NewName.base());
      bool NewNameIsValid = NewName.isValid() &&
        (Lexer::isIdentifier(NewName.base()) || newOperator) &&
        std::all_of(ParamNames.begin(), ParamNames.end(), [](StringRef Label) {
          return Label.empty() || Lexer::isIdentifier(Label);
        });

      if (!NewNameIsValid) {
        Diags.diagnose(Location, diag::invalid_name, RenameLoc.NewName);
        return {};
      }

      if (NewName.partsCount() != OldName.partsCount()) {
        Diags.diagnose(Location, diag::arity_mismatch, RenameLoc.NewName,
                       RenameLoc.OldName);
        return {};
      }

      if (RenameLoc.Usage == NameUsage::Call && !RenameLoc.IsFunctionLike) {
        Diags.diagnose(Location, diag::name_not_functionlike, RenameLoc.NewName);
        return {};
      }
    }

    bool isOperator = Lexer::isOperator(OldName.base());
    UnresolvedLocs.push_back({
      Location,
      (RenameLoc.Usage == NameUsage::Unknown ||
       (RenameLoc.Usage == NameUsage::Call && !isOperator))
    });
  }

  NameMatcher Resolver(SF);
  return Resolver.resolve(UnresolvedLocs, SF.getAllTokens());
}

int swift::ide::syntacticRename(SourceFile *SF, ArrayRef<RenameLoc> RenameLocs,
                                SourceEditConsumer &EditConsumer,
                                DiagnosticConsumer &DiagConsumer) {

  assert(SF && "null source file");

  SourceManager &SM = SF->getASTContext().SourceMgr;
  DiagnosticEngine DiagEngine(SM);
  DiagEngine.addConsumer(DiagConsumer);

  auto ResolvedLocs = resolveRenameLocations(RenameLocs, *SF, DiagEngine);
  if (ResolvedLocs.size() != RenameLocs.size())
    return true; // Already diagnosed.

  size_t index = 0;
  llvm::StringSet<> ReplaceTextContext;
  for(const RenameLoc &Rename: RenameLocs) {
    ResolvedLoc &Resolved = ResolvedLocs[index++];
    TextReplacementsRenamer Renamer(SM, Rename.OldName, Rename.NewName,
                                    ReplaceTextContext);
    RegionType Type = Renamer.addSyntacticRenameRanges(Resolved, Rename);
    if (Type == RegionType::Mismatch) {
      DiagEngine.diagnose(Resolved.Range.getStart(), diag::mismatched_rename,
                          Rename.NewName);
      EditConsumer.accept(SM, Type, None);
    } else {
      EditConsumer.accept(SM, Type, Renamer.getReplacements());
    }
  }

  return false;
}

int swift::ide::findSyntacticRenameRanges(
    SourceFile *SF, llvm::ArrayRef<RenameLoc> RenameLocs,
    FindRenameRangesConsumer &RenameConsumer,
    DiagnosticConsumer &DiagConsumer) {
  assert(SF && "null source file");

  SourceManager &SM = SF->getASTContext().SourceMgr;
  DiagnosticEngine DiagEngine(SM);
  DiagEngine.addConsumer(DiagConsumer);

  auto ResolvedLocs = resolveRenameLocations(RenameLocs, *SF, DiagEngine);
  if (ResolvedLocs.size() != RenameLocs.size())
    return true; // Already diagnosed.

  size_t index = 0;
  for (const RenameLoc &Rename : RenameLocs) {
    ResolvedLoc &Resolved = ResolvedLocs[index++];
    RenameRangeDetailCollector Renamer(SM, Rename.OldName);
    RegionType Type = Renamer.addSyntacticRenameRanges(Resolved, Rename);
    if (Type == RegionType::Mismatch) {
      DiagEngine.diagnose(Resolved.Range.getStart(), diag::mismatched_rename,
                          Rename.NewName);
      RenameConsumer.accept(SM, Type, None);
    } else {
      RenameConsumer.accept(SM, Type, Renamer.Ranges);
    }
  }

  return false;
}

int swift::ide::findLocalRenameRanges(
    SourceFile *SF, RangeConfig Range,
    FindRenameRangesConsumer &RenameConsumer,
    DiagnosticConsumer &DiagConsumer) {
  assert(SF && "null source file");

  SourceManager &SM = SF->getASTContext().SourceMgr;
  DiagnosticEngine Diags(SM);
  Diags.addConsumer(DiagConsumer);

  auto StartLoc = Lexer::getLocForStartOfToken(SM, Range.getStart(SM));

  CursorInfoResolver Resolver(*SF);
  ResolvedCursorInfo CursorInfo = Resolver.resolve(StartLoc);
  if (!CursorInfo.isValid() || !CursorInfo.ValueD) {
    Diags.diagnose(StartLoc, diag::unresolved_location);
    return true;
  }
  ValueDecl *VD = CursorInfo.ValueD;
  llvm::SmallVector<DeclContext *, 8> Scopes;
  analyzeRenameScope(VD, Diags, Scopes);
  if (Scopes.empty())
    return true;
  RenameRangeCollector RangeCollector(VD, StringRef());
  for (DeclContext *DC : Scopes)
    indexDeclContext(DC, RangeCollector);

  return findSyntacticRenameRanges(SF, RangeCollector.results(), RenameConsumer,
                                   DiagConsumer);
}

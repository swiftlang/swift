//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Lexer.h"
#include "swift/Refactoring/Refactoring.h"

using namespace swift;
using namespace swift::ide;

namespace {

class RenameRangeDetailCollector {
  const SourceManager &SM;
  const DeclNameViewer Old;
  /// The ranges that have been collect.
  ///
  /// This is the result of the `RenameRangeDetailCollector` and can be
  /// retrieved with `getResult`.
  std::vector<RenameRangeDetail> Ranges;

public:
  RenameRangeDetailCollector(const SourceManager &SM, StringRef OldName)
      : SM(SM), Old(OldName) {}

  virtual ~RenameRangeDetailCollector() {}

  RegionType addSyntacticRenameRanges(const ResolvedLoc &Resolved,
                                      const RenameLoc &Config);

  std::vector<RenameRangeDetail> &&takeResult() { return std::move(Ranges); }

private:
  void addRenameRange(CharSourceRange Label, RefactoringRangeKind RangeKind,
                      llvm::Optional<unsigned> NameIndex);

  /// Adds a replacement to rename the given base name range
  /// \return true if the given range does not match the old name
  bool renameBase(CharSourceRange Range, RefactoringRangeKind RangeKind);

  /// Adds replacements to rename the given label ranges
  /// \return true if the label ranges do not match the old name
  bool renameLabels(ArrayRef<CharSourceRange> LabelRanges,
                    llvm::Optional<unsigned> FirstTrailingLabel,
                    LabelRangeType RangeType, bool isCallSite);

  bool isOperator() const { return Lexer::isOperator(Old.base()); }

private:
  /// Returns the range of the  (possibly escaped) identifier at the start of
  /// \p Range and updates \p IsEscaped to indicate whether it's escaped or not.
  CharSourceRange getLeadingIdentifierRange(CharSourceRange Range,
                                            bool &IsEscaped);

  CharSourceRange stripBackticks(CharSourceRange Range);

  void splitAndRenameLabel(CharSourceRange Range, LabelRangeType RangeType,
                           size_t NameIndex);

  void splitAndRenameParamLabel(CharSourceRange Range, size_t NameIndex,
                                bool IsCollapsible);

  void splitAndRenameCallArg(CharSourceRange Range, size_t NameIndex);

  bool labelRangeMatches(CharSourceRange Range, LabelRangeType RangeType,
                         StringRef Expected);

  bool renameLabelsLenient(ArrayRef<CharSourceRange> LabelRanges,
                           llvm::Optional<unsigned> FirstTrailingLabel,
                           LabelRangeType RangeType);

  static RegionType getSyntacticRenameRegionType(const ResolvedLoc &Resolved);
};

void RenameRangeDetailCollector::addRenameRange(
    CharSourceRange Label, RefactoringRangeKind RangeKind,
    llvm::Optional<unsigned> NameIndex) {
  Ranges.push_back({Label, RangeKind, NameIndex});
}

bool RenameRangeDetailCollector::renameBase(CharSourceRange Range,
                                            RefactoringRangeKind RangeKind) {
  assert(Range.isValid());

  if (stripBackticks(Range).str() != Old.base())
    return true;
  addRenameRange(Range, RangeKind, llvm::None);
  return false;
}

bool RenameRangeDetailCollector::renameLabels(
    ArrayRef<CharSourceRange> LabelRanges,
    llvm::Optional<unsigned> FirstTrailingLabel, LabelRangeType RangeType,
    bool isCallSite) {
  if (isCallSite)
    return renameLabelsLenient(LabelRanges, FirstTrailingLabel, RangeType);

  assert(!FirstTrailingLabel);
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

CharSourceRange
RenameRangeDetailCollector::getLeadingIdentifierRange(CharSourceRange Range,
                                                      bool &IsEscaped) {
  assert(Range.isValid() && Range.getByteLength());
  IsEscaped = Range.str().front() == '`';
  SourceLoc Start = Range.getStart();
  if (IsEscaped)
    Start = Start.getAdvancedLoc(1);
  return Lexer::getCharSourceRangeFromSourceRange(SM, Start);
}

CharSourceRange
RenameRangeDetailCollector::stripBackticks(CharSourceRange Range) {
  StringRef Content = Range.str();
  if (Content.size() < 3 || Content.front() != '`' || Content.back() != '`') {
    return Range;
  }
  return CharSourceRange(Range.getStart().getAdvancedLoc(1),
                         Range.getByteLength() - 2);
}

void RenameRangeDetailCollector::splitAndRenameLabel(CharSourceRange Range,
                                                     LabelRangeType RangeType,
                                                     size_t NameIndex) {
  switch (RangeType) {
  case LabelRangeType::CallArg:
    return splitAndRenameCallArg(Range, NameIndex);
  case LabelRangeType::Param:
    return splitAndRenameParamLabel(Range, NameIndex, /*IsCollapsible=*/true);
  case LabelRangeType::NoncollapsibleParam:
    return splitAndRenameParamLabel(Range, NameIndex,
                                    /*IsCollapsible=*/false);
  case LabelRangeType::Selector:
    return addRenameRange(Range, RefactoringRangeKind::SelectorArgumentLabel,
                          NameIndex);
  case LabelRangeType::None:
    llvm_unreachable("expected a label range");
  }
}

void RenameRangeDetailCollector::splitAndRenameParamLabel(CharSourceRange Range,
                                                          size_t NameIndex,
                                                          bool IsCollapsible) {
  // Split parameter range foo([a b]: Int) into decl argument label [a] and
  // parameter name [b] or noncollapsible parameter name [b] if IsCollapsible
  // is false (as for subscript decls). If we have only foo([a]: Int), then we
  // add an empty range for the local name, or for the decl argument label if
  // IsCollapsible is false.
  StringRef Content = Range.str();
  size_t ExternalNameEnd = Content.find_first_of(" \t\n\v\f\r/");

  if (ExternalNameEnd == StringRef::npos) { // foo([a]: Int)
    if (IsCollapsible) {
      addRenameRange(Range, RefactoringRangeKind::DeclArgumentLabel, NameIndex);
      addRenameRange(CharSourceRange{Range.getEnd(), 0},
                     RefactoringRangeKind::ParameterName, NameIndex);
    } else {
      addRenameRange(CharSourceRange{Range.getStart(), 0},
                     RefactoringRangeKind::DeclArgumentLabel, NameIndex);
      addRenameRange(Range, RefactoringRangeKind::NoncollapsibleParameterName,
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

    addRenameRange(Ext, RefactoringRangeKind::DeclArgumentLabel, NameIndex);
    if (IsCollapsible) {
      addRenameRange(Local, RefactoringRangeKind::ParameterName, NameIndex);
    } else {
      addRenameRange(Local, RefactoringRangeKind::NoncollapsibleParameterName,
                     NameIndex);
    }
  }
}

void RenameRangeDetailCollector::splitAndRenameCallArg(CharSourceRange Range,
                                                       size_t NameIndex) {
  // Split call argument foo([a: ]1) into argument name [a] and the remainder
  // [: ].
  StringRef Content = Range.str();
  size_t Colon = Content.find(':'); // FIXME: leading whitespace?
  if (Colon == StringRef::npos) {
    assert(Content.empty());
    addRenameRange(Range, RefactoringRangeKind::CallArgumentCombined,
                   NameIndex);
    return;
  }

  // Include any whitespace before the ':'.
  assert(Colon == Content.substr(0, Colon).size());
  Colon = Content.substr(0, Colon).rtrim().size();

  CharSourceRange Arg{Range.getStart(), unsigned(Colon)};
  addRenameRange(Arg, RefactoringRangeKind::CallArgumentLabel, NameIndex);

  auto ColonLoc = Range.getStart().getAdvancedLocOrInvalid(Colon);
  assert(ColonLoc.isValid());
  CharSourceRange Rest{ColonLoc, unsigned(Content.size() - Colon)};
  addRenameRange(Rest, RefactoringRangeKind::CallArgumentColon, NameIndex);
}

bool RenameRangeDetailCollector::labelRangeMatches(CharSourceRange Range,
                                                   LabelRangeType RangeType,
                                                   StringRef Expected) {
  if (Range.getByteLength()) {
    bool IsEscaped = false;
    CharSourceRange ExistingLabelRange =
        getLeadingIdentifierRange(Range, IsEscaped);
    StringRef ExistingLabel = ExistingLabelRange.str();
    bool IsSingleName =
        Range == ExistingLabelRange ||
        (IsEscaped && Range.getByteLength() == ExistingLabel.size() + 2);

    switch (RangeType) {
    case LabelRangeType::NoncollapsibleParam:
      if (IsSingleName && Expected.empty()) // subscript([x]: Int)
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

bool RenameRangeDetailCollector::renameLabelsLenient(
    ArrayRef<CharSourceRange> LabelRanges,
    llvm::Optional<unsigned> FirstTrailingLabel, LabelRangeType RangeType) {

  ArrayRef<StringRef> OldNames = Old.args();

  // First, match trailing closure arguments in reverse
  if (FirstTrailingLabel) {
    auto TrailingLabels = LabelRanges.drop_front(*FirstTrailingLabel);
    LabelRanges = LabelRanges.take_front(*FirstTrailingLabel);

    for (auto LabelIndex : llvm::reverse(indices(TrailingLabels))) {
      CharSourceRange Label = TrailingLabels[LabelIndex];

      if (Label.getByteLength()) {
        if (OldNames.empty())
          return true;

        while (!labelRangeMatches(Label, LabelRangeType::Selector,
                                  OldNames.back())) {
          if ((OldNames = OldNames.drop_back()).empty())
            return true;
        }
        splitAndRenameLabel(Label, LabelRangeType::Selector,
                            OldNames.size() - 1);
        OldNames = OldNames.drop_back();
        continue;
      }

      // empty labelled trailing closure label
      if (LabelIndex) {
        if (OldNames.empty())
          return true;

        while (!OldNames.back().empty()) {
          if ((OldNames = OldNames.drop_back()).empty())
            return true;
        }
        splitAndRenameLabel(Label, LabelRangeType::Selector,
                            OldNames.size() - 1);
        OldNames = OldNames.drop_back();
        continue;
      }

      // unlabelled trailing closure label
      OldNames = OldNames.drop_back();
      continue;
    }
  }

  // Next, match the non-trailing arguments.
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

RegionType RenameRangeDetailCollector::getSyntacticRenameRegionType(
    const ResolvedLoc &Resolved) {
  switch (Resolved.context) {
  case ResolvedLocContext::Default:
    if (Resolved.isActive) {
      return RegionType::ActiveCode;
    } else {
      return RegionType::InactiveCode;
    }
  case ResolvedLocContext::Selector:
    return RegionType::Selector;
  case ResolvedLocContext::Comment:
    return RegionType::Comment;
  case ResolvedLocContext::StringLiteral:
    return RegionType::String;
  }
}

RegionType RenameRangeDetailCollector::addSyntacticRenameRanges(
    const ResolvedLoc &Resolved, const RenameLoc &Config) {

  if (!Resolved.range.isValid())
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

  // FIXME: this should only be treated specially for instance methods.
  bool IsCallAsFunction =
      Old.base() == "callAsFunction" && Config.IsFunctionLike;

  bool IsSpecialBase = IsInit || IsSubscript || IsCallAsFunction;

  // Filter out non-semantic special basename locations with no labels.
  // We've already filtered out those in active code, so these are
  // any appearance of just 'init', 'subscript', or 'callAsFunction' in
  // strings, comments, and inactive code.
  if (IsSpecialBase && (Config.Usage == NameUsage::Unknown &&
                        Resolved.labelType == LabelRangeType::None))
    return RegionType::Unmatched;

  if (!Config.IsFunctionLike || !IsSpecialBase) {
    if (renameBase(Resolved.range, RefactoringRangeKind::BaseName))
      return RegionType::Mismatch;

  } else if (IsInit || IsCallAsFunction) {
    if (renameBase(Resolved.range, RefactoringRangeKind::KeywordBaseName)) {
      // The base name doesn't need to match (but may) for calls, but
      // it should for definitions and references.
      if (Config.Usage == NameUsage::Definition ||
          Config.Usage == NameUsage::Reference) {
        return RegionType::Mismatch;
      }
    }
  } else if (IsSubscript && Config.Usage == NameUsage::Definition) {
    if (renameBase(Resolved.range, RefactoringRangeKind::KeywordBaseName))
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
      HandleLabels =
          Resolved.labelType == LabelRangeType::Selector || IsSubscript;
      break;
    case NameUsage::Unknown:
      HandleLabels = Resolved.labelType != LabelRangeType::None;
      break;
    }
  }

  if (HandleLabels) {
    bool isCallSite = Config.Usage != NameUsage::Definition &&
                      (Config.Usage != NameUsage::Reference || IsSubscript) &&
                      Resolved.labelType == LabelRangeType::CallArg;

    if (renameLabels(Resolved.labelRanges, Resolved.firstTrailingLabel,
                     Resolved.labelType, isCallSite))
      return Config.Usage == NameUsage::Unknown ? RegionType::Unmatched
                                                : RegionType::Mismatch;
  }

  return RegionKind;
}

} // end anonymous namespace

SyntacticRenameRangeDetails swift::ide::getSyntacticRenameRangeDetails(
    const SourceManager &SM, StringRef OldName, const ResolvedLoc &Resolved,
    const RenameLoc &Config) {
  RenameRangeDetailCollector RangeCollector(SM, OldName);
  RegionType Type = RangeCollector.addSyntacticRenameRanges(Resolved, Config);
  return {Type, RangeCollector.takeResult()};
}

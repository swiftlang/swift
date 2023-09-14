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

#ifndef SWIFT_REFACTORING_RENAME_H
#define SWIFT_REFACTORING_RENAME_H

#include "swift/Basic/SourceManager.h"
#include "swift/IDE/Utils.h"
#include "swift/Parse/Lexer.h"
#include "swift/Refactoring/Refactoring.h"

namespace swift {
namespace refactoring {

using namespace swift::ide;

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

public:
  RegionType addSyntacticRenameRanges(const ResolvedLoc &Resolved,
                                      const RenameLoc &Config);
};

class RenameRangeDetailCollector : public Renamer {
  void doRenameLabel(CharSourceRange Label, RefactoringRangeKind RangeKind,
                     unsigned NameIndex) override {
    Ranges.push_back({Label, RangeKind, NameIndex});
  }
  void doRenameBase(CharSourceRange Range,
                    RefactoringRangeKind RangeKind) override {
    Ranges.push_back({Range, RangeKind, llvm::None});
  }

public:
  RenameRangeDetailCollector(const SourceManager &SM, StringRef OldName)
      : Renamer(SM, OldName) {}
  std::vector<RenameRangeDetail> Ranges;
};

} // namespace refactoring
} // namespace swift

#endif

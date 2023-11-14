//===--- Refactoring.h - APIs for refactoring --------*- C++ -*-===//
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

#ifndef SWIFT_IDE_REFACTORING_H
#define SWIFT_IDE_REFACTORING_H

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/StringExtras.h"
#include "swift/IDE/Utils.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
  class ModuleDecl;
  class SourceLoc;
  class SourceManager;

namespace ide {
  struct ResolvedCursorInfo;

enum class RefactoringKind : int8_t {
  None,
#define REFACTORING(KIND, NAME, ID) KIND,
#include "RefactoringKinds.def"
};

enum class RefactorAvailableKind {
  Available,
  Unavailable_system_symbol,
  Unavailable_has_no_location,
  Unavailable_has_no_name,
  Unavailable_has_no_accessibility,
  Unavailable_decl_from_clang,
  Unavailable_decl_in_macro,
};

struct RefactorAvailabilityInfo {
  RefactoringKind Kind;
  RefactorAvailableKind AvailableKind;
  RefactorAvailabilityInfo(RefactoringKind Kind,
                           RefactorAvailableKind AvailableKind)
      : Kind(Kind), AvailableKind(AvailableKind) {}
  RefactorAvailabilityInfo(RefactoringKind Kind)
      : RefactorAvailabilityInfo(Kind, RefactorAvailableKind::Available) {}
};

struct RenameInfo {
  ValueDecl *VD;
  RefactorAvailabilityInfo Availability;
};

llvm::Optional<RenameInfo> getRenameInfo(ResolvedCursorInfoPtr cursorInfo);

enum class NameUsage {
  Unknown,
  Reference,
  Definition,
  Call
};

struct RenameLoc {
  unsigned Line;
  unsigned Column;
  NameUsage Usage;
  StringRef OldName;
  const bool IsFunctionLike;
};

/// An array of \c RenameLoc that also keeps the underlying string storage of
/// the \c StringRef inside the \c RenameLoc alive.
class RenameLocs {
  std::vector<RenameLoc> locs;
  std::unique_ptr<StringScratchSpace> stringStorage;

public:
  ArrayRef<RenameLoc> getLocations() { return locs; }

  RenameLocs(std::vector<RenameLoc> locs,
             std::unique_ptr<StringScratchSpace> stringStorage)
      : locs(locs), stringStorage(std::move(stringStorage)) {}

  RenameLocs() {}
};

/// Return the location to rename when renaming the identifier at \p startLoc
/// in \p sourceFile.
///
/// - Parameters:
///   - sourceFile: The source file in which to perform local rename
///   - renameInfo: Information about the symbol to rename. See `getRenameInfo`
RenameLocs localRenameLocs(SourceFile *sourceFile, RenameInfo renameInfo);

/// Given a list of `RenameLoc`s, get the corresponding `ResolveLoc`s.
///
/// These resolve locations contain more structured information, such as the
/// range of the base name to rename and the ranges of the argument labels.
///
/// If a \p newName is passed, it is used to verify that all \p renameLocs can
/// be renamed to this name. If any the names cannot be renamed, an empty vector
/// is returned and the issue is diagnosed via \p diags.
std::vector<ResolvedLoc> resolveRenameLocations(ArrayRef<RenameLoc> renameLocs,
                                                StringRef newName,
                                                SourceFile &sourceFile,
                                                DiagnosticEngine &diags);

struct RangeConfig {
  unsigned BufferID;
  unsigned Line;
  unsigned Column;
  unsigned Length;
  SourceLoc getStart(SourceManager &SM);
  SourceLoc getEnd(SourceManager &SM);
};

struct RefactoringOptions {
  RefactoringKind Kind;
  RangeConfig Range;
  std::string PreferredName;
  RefactoringOptions(RefactoringKind Kind) : Kind(Kind) {}
};

// TODO: Merge with NoteRegion – range needs to change to start/end line/column
struct RenameRangeDetail {
  CharSourceRange Range;
  RefactoringRangeKind RangeKind;
  llvm::Optional<unsigned> Index;
};

class FindRenameRangesConsumer {
public:
  virtual void accept(SourceManager &SM, RegionType RegionType,
                      ArrayRef<RenameRangeDetail> Ranges) = 0;
  virtual ~FindRenameRangesConsumer() = default;
};

class FindRenameRangesAnnotatingConsumer : public FindRenameRangesConsumer {
  std::unique_ptr<SourceEditConsumer> pRewriter;

public:
  FindRenameRangesAnnotatingConsumer(SourceManager &SM, unsigned BufferId,
                                     llvm::raw_ostream &OS);
  void accept(SourceManager &SM, RegionType RegionType,
              ArrayRef<RenameRangeDetail> Ranges) override;
};

StringRef getDescriptiveRefactoringKindName(RefactoringKind Kind);

StringRef getDescriptiveRenameUnavailableReason(RefactorAvailableKind Kind);

bool refactorSwiftModule(ModuleDecl *M, RefactoringOptions Opts,
                         SourceEditConsumer &EditConsumer,
                         DiagnosticConsumer &DiagConsumer);

/// Based on the given \p RenameLocs, finds the ranges (including argument
/// labels) that need to be renamed and reports those to \p RenameConsumer.
///
/// If \p NewName is passed, it is validated that all locations can be renamed
/// to that new name. If not, no ranges are reported and an error is emitted
/// via \p DiagConsumer.
int findSyntacticRenameRanges(SourceFile *SF,
                              llvm::ArrayRef<RenameLoc> RenameLocs,
                              StringRef NewName,
                              FindRenameRangesConsumer &RenameConsumer,
                              DiagnosticConsumer &DiagConsumer);

int findLocalRenameRanges(SourceFile *SF, RangeConfig Range,
                          FindRenameRangesConsumer &RenameConsumer,
                          DiagnosticConsumer &DiagConsumer);

SmallVector<RefactorAvailabilityInfo, 0>
collectRefactorings(SourceFile *SF, RangeConfig Range,
                    bool &RangeStartMayNeedRename,
                    llvm::ArrayRef<DiagnosticConsumer *> DiagConsumers);

SmallVector<RefactorAvailabilityInfo, 0>
collectRefactorings(ResolvedCursorInfoPtr CursorInfo, bool ExcludeRename);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_REFACTORING_H

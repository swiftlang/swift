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

#ifndef SWIFT_REFACTORING_REFACTORING_H
#define SWIFT_REFACTORING_REFACTORING_H

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/StringExtras.h"
#include "swift/IDE/CancellableResult.h"
#include "swift/IDE/Utils.h"
#include "swift/Refactoring/RenameLoc.h"
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

std::optional<RenameInfo> getRenameInfo(ResolvedCursorInfoPtr cursorInfo);

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

/// Return the locations to rename when renaming the \p valueDecl
/// in \p sourceFile.
///
/// - Parameters:
///   - sourceFile: The source file in which to perform local rename
///   - valueDecl: The declaration that should be renamed
RenameLocs localRenameLocs(SourceFile *sourceFile, const ValueDecl *valueDecl);

#if SWIFT_BUILD_SWIFT_SYNTAX
/// A `RenameLoc` together with the `ResolvedLoc` that it resolved to.
struct ResolvedAndRenameLoc {
  RenameLoc renameLoc;
  ResolvedLoc resolved;
};

/// Given a list of `RenameLoc`s, get the corresponding `ResolveLoc`s.
///
/// These resolve locations contain more structured information, such as the
/// range of the base name to rename and the ranges of the argument labels.
///
/// If a \p newName is passed, it is used to verify that all \p renameLocs can
/// be renamed to this name. If any the names cannot be renamed, an empty vector
/// is returned and the issue is diagnosed via \p diags.
std::vector<ResolvedAndRenameLoc>
resolveRenameLocations(ArrayRef<RenameLoc> renameLocs, StringRef newName,
                       SourceFile &sourceFile, DiagnosticEngine &diags);
#endif

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
  std::optional<unsigned> Index;
};

StringRef getDescriptiveRefactoringKindName(RefactoringKind Kind);

StringRef getDescriptiveRenameUnavailableReason(RefactorAvailableKind Kind);

bool refactorSwiftModule(ModuleDecl *M, RefactoringOptions Opts,
                         SourceEditConsumer &EditConsumer,
                         DiagnosticConsumer &DiagConsumer);

/// Describes the different ranges that need to be renamed during a rename
/// operation. For a function these are the base name and the argument labels.
struct SyntacticRenameRangeDetails {
  RegionType Type;
  std::vector<RenameRangeDetail> Ranges;
};

/// Return the \c SyntacticRenameRangeDetails for the given \c ResolvedLoc and
/// \c RenameLoc.
SyntacticRenameRangeDetails
getSyntacticRenameRangeDetails(const SourceManager &SM, StringRef OldName,
                               const ResolvedLoc &Resolved,
                               const RenameLoc &Config);

/// Based on the given \p RenameLocs, finds the ranges (including argument
/// labels) that need to be renamed.
///
/// If \p NewName is passed, it is validated that all locations can be renamed
/// to that new name. If not, no ranges are reported and an error is emitted
/// via \p DiagConsumer.
CancellableResult<std::vector<SyntacticRenameRangeDetails>>
findSyntacticRenameRanges(SourceFile *SF, llvm::ArrayRef<RenameLoc> RenameLocs,
                          StringRef NewName);

CancellableResult<std::vector<SyntacticRenameRangeDetails>>
findLocalRenameRanges(SourceFile *SF, RangeConfig Range);

SmallVector<RefactorAvailabilityInfo, 0>
collectRefactorings(SourceFile *SF, RangeConfig Range,
                    bool &RangeStartMayNeedRename,
                    llvm::ArrayRef<DiagnosticConsumer *> DiagConsumers);

SmallVector<RefactorAvailabilityInfo, 0>
collectRefactorings(ResolvedCursorInfoPtr CursorInfo, bool ExcludeRename);

} // namespace ide
} // namespace swift

#endif // SWIFT_REFACTORING_REFACTORING_H

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

#include "llvm/ADT/StringRef.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/IDE/Utils.h"

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

struct RangeConfig {
  unsigned BufferId;
  unsigned Line;
  unsigned Column;
  unsigned Length;
  SourceLoc getStart(SourceManager &SM);
  SourceLoc getEnd(SourceManager &SM);
};

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
  StringRef NewName; // May be empty.
  const bool IsFunctionLike;
  const bool IsNonProtocolType;
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
  Optional<unsigned> Index;
};

enum class RenameAvailableKind {
  Available,
  Unavailable_system_symbol,
  Unavailable_has_no_location,
  Unavailable_has_no_name,
  Unavailable_has_no_accessibility,
  Unavailable_decl_from_clang,
};

struct RenameAvailabiliyInfo {
  RefactoringKind Kind;
  RenameAvailableKind AvailableKind;
  RenameAvailabiliyInfo(RefactoringKind Kind, RenameAvailableKind AvailableKind) :
    Kind(Kind), AvailableKind(AvailableKind) {}
  RenameAvailabiliyInfo(RefactoringKind Kind) :
    RenameAvailabiliyInfo(Kind, RenameAvailableKind::Available) {}
};

class FindRenameRangesConsumer {
public:
  virtual void accept(SourceManager &SM, RegionType RegionType,
                      ArrayRef<RenameRangeDetail> Ranges) = 0;
  virtual ~FindRenameRangesConsumer() = default;
};

class FindRenameRangesAnnotatingConsumer : public FindRenameRangesConsumer {
  struct Implementation;
  Implementation &Impl;

public:
  FindRenameRangesAnnotatingConsumer(SourceManager &SM, unsigned BufferId,
                                     llvm::raw_ostream &OS);
  ~FindRenameRangesAnnotatingConsumer();
  void accept(SourceManager &SM, RegionType RegionType,
              ArrayRef<RenameRangeDetail> Ranges) override;
};

StringRef getDescriptiveRefactoringKindName(RefactoringKind Kind);

StringRef getDescriptiveRenameUnavailableReason(RenameAvailableKind Kind);

bool refactorSwiftModule(ModuleDecl *M, RefactoringOptions Opts,
                         SourceEditConsumer &EditConsumer,
                         DiagnosticConsumer &DiagConsumer);

int syntacticRename(SourceFile *SF, llvm::ArrayRef<RenameLoc> RenameLocs,
                    SourceEditConsumer &EditConsumer,
                    DiagnosticConsumer &DiagConsumer);

int findSyntacticRenameRanges(SourceFile *SF,
                              llvm::ArrayRef<RenameLoc> RenameLocs,
                              FindRenameRangesConsumer &RenameConsumer,
                              DiagnosticConsumer &DiagConsumer);

int findLocalRenameRanges(SourceFile *SF, RangeConfig Range,
                          FindRenameRangesConsumer &RenameConsumer,
                          DiagnosticConsumer &DiagConsumer);

ArrayRef<RefactoringKind>
collectAvailableRefactorings(SourceFile *SF, RangeConfig Range,
                             bool &RangeStartMayNeedRename,
                             std::vector<RefactoringKind> &Scratch,
                             llvm::ArrayRef<DiagnosticConsumer*> DiagConsumers);

ArrayRef<RefactoringKind>
collectAvailableRefactorings(SourceFile *SF, ResolvedCursorInfo CursorInfo,
                             std::vector<RefactoringKind> &Scratch,
                             bool ExcludeRename);

/// Stores information about the reference that rename availability is being
/// queried on.
struct RenameRefInfo {
  SourceFile *SF; ///< The source file containing the reference.
  SourceLoc Loc; ///< The reference's source location.
  bool IsArgLabel; ///< Whether Loc is on an arg label, rather than base name.
};

ArrayRef<RenameAvailabiliyInfo>
collectRenameAvailabilityInfo(const ValueDecl *VD,
                              Optional<RenameRefInfo> RefInfo,
                              std::vector<RenameAvailabiliyInfo> &Scratch);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_REFACTORING_H

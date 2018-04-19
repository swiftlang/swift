//===--- TestOptions.h - ----------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKITD_TEST_TESTOPTIONS_H
#define LLVM_SOURCEKITD_TEST_TESTOPTIONS_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include <string>

namespace sourcekitd_test {

enum class SourceKitRequest {
  None,
  ProtocolVersion,
  DemangleNames,
  MangleSimpleClasses,
  Index,
  CodeComplete,
  CodeCompleteOpen,
  CodeCompleteClose,
  CodeCompleteUpdate,
  CodeCompleteCacheOnDisk,
  CodeCompleteSetPopularAPI,
  CursorInfo,
  RangeInfo,
  RelatedIdents,
  SyntaxMap,
  Structure,
  Format,
  ExpandPlaceholder,
  DocInfo,
  SemanticInfo,
  InterfaceGen,
  InterfaceGenOpen,
  FindUSR,
  FindInterfaceDoc,
  Open,
  Close,
  Edit,
  PrintAnnotations,
  PrintDiags,
  ExtractComment,
  ModuleGroups,
  SyntacticRename,
  FindRenameRanges,
  FindLocalRenameRanges,
  NameTranslation,
  MarkupToXML,
  Statistics,
  SyntaxTree,
  EnableCompileNotifications,
#define SEMANTIC_REFACTORING(KIND, NAME, ID) KIND,
#include "swift/IDE/RefactoringKinds.def"
};

struct TestOptions {
  SourceKitRequest Request = SourceKitRequest::None;
  std::vector<std::string> Inputs;
  std::string SourceFile;
  std::string TextInputFile;
  std::string JsonRequestPath;
  std::string RenameSpecPath;
  llvm::Optional<std::string> SourceText;
  std::string ModuleGroupName;
  std::string InterestedUSR;
  unsigned Line = 0;
  unsigned Col = 0;
  unsigned EndLine = 0;
  unsigned EndCol = 0;
  unsigned Offset = 0;
  unsigned Length = 0;
  std::string SwiftVersion;
  bool PassVersionAsString = false;
  llvm::Optional<std::string> ReplaceText;
  std::string ModuleName;
  std::string HeaderPath;
  bool PassAsSourceText = false;
  std::string CachePath;
  llvm::SmallVector<std::string, 4> RequestOptions;
  llvm::ArrayRef<const char *> CompilerArgs;
  bool UsingSwiftArgs;
  std::string USR;
  std::string SwiftName;
  std::string ObjCName;
  std::string ObjCSelector;
  std::string Name;
  bool CheckInterfaceIsASCII = false;
  bool UsedSema = false;
  bool PrintRequest = true;
  bool PrintResponseAsJSON = false;
  bool PrintRawResponse = false;
  bool PrintResponse = true;
  bool SimplifiedDemangling = false;
  bool SynthesizedExtensions = false;
  bool CollectActionables = false;
  bool isAsyncRequest = false;
  bool timeRequest = false;
  unsigned repeatRequest = 1;
  llvm::Optional<bool> CancelOnSubsequentRequest;
  bool parseArgs(llvm::ArrayRef<const char *> Args);
  void printHelp(bool ShowHidden) const;
};

}

#endif

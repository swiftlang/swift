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
  Edit,
  PrintAnnotations,
  PrintDiags,
  ExtractComment,
  ModuleGroups,
  NameTranslation,
  MarkupToXML,
};

struct TestOptions {
  SourceKitRequest Request = SourceKitRequest::None;
  std::vector<std::string> Inputs;
  std::string SourceFile;
  std::string TextInputFile;
  std::string JsonRequestPath;
  llvm::Optional<std::string> SourceText;
  std::string ModuleGroupName;
  std::string InterestedUSR;
  unsigned Line = 0;
  unsigned Col = 0;
  unsigned EndLine = 0;
  unsigned EndCol = 0;
  unsigned Offset = 0;
  unsigned Length = 0;
  llvm::Optional<std::string> ReplaceText;
  std::string ModuleName;
  std::string HeaderPath;
  bool PassAsSourceText = false;
  std::string CachePath;
  llvm::SmallVector<std::string, 4> RequestOptions;
  llvm::ArrayRef<const char *> CompilerArgs;
  std::string USR;
  std::string SwiftName;
  std::string ObjCName;
  std::string ObjCSelector;
  bool CheckInterfaceIsASCII = false;
  bool UsedSema = false;
  bool PrintRequest = true;
  bool PrintResponseAsJSON = false;
  bool PrintRawResponse = false;
  bool SimplifiedDemangling = false;
  bool SynthesizedExtensions = false;
  bool CollectActionables = false;
  bool isAsyncRequest = false;
  bool parseArgs(llvm::ArrayRef<const char *> Args);
};

}

#endif

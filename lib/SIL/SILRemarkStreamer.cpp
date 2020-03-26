//===--- SILRemarkStreamer.cpp --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILRemarkStreamer.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/LLVMContext.h"
#include "llvm/IR/LLVMContext.h"

using namespace swift;

std::pair<std::unique_ptr<llvm::raw_fd_ostream>,
          std::unique_ptr<SILRemarkStreamer>>
swift::createSILRemarkStreamer(SILModule &silModule, StringRef filename,
                               StringRef passes, llvm::remarks::Format format,
                               DiagnosticEngine &diagEngine,
                               SourceManager &sourceMgr) {
  if (filename.empty())
    return {nullptr, nullptr};

  std::error_code errorCode;
  auto file = std::make_unique<llvm::raw_fd_ostream>(filename, errorCode,
                                                     llvm::sys::fs::F_None);
  if (errorCode) {
    diagEngine.diagnose(SourceLoc(), diag::cannot_open_file, filename,
                        errorCode.message());
    return {nullptr, nullptr};
  }

  llvm::Expected<std::unique_ptr<llvm::remarks::RemarkSerializer>>
      remarkSerializerOrErr = llvm::remarks::createRemarkSerializer(
          format, llvm::remarks::SerializerMode::Separate, *file);
  if (llvm::Error err = remarkSerializerOrErr.takeError()) {
    diagEngine.diagnose(SourceLoc(), diag::error_creating_remark_serializer,
                        toString(std::move(err)));
    return {nullptr, nullptr};
  }

  auto mainRS = std::make_unique<llvm::remarks::RemarkStreamer>(
      std::move(*remarkSerializerOrErr), filename);
  llvm::remarks::RemarkStreamer &mainRemarkStreamer = *mainRS;
  getGlobalLLVMContext().setMainRemarkStreamer(std::move(mainRS));

  if (!passes.empty()) {
    if (llvm::Error err = mainRemarkStreamer.setFilter(passes)) {
      diagEngine.diagnose(SourceLoc(), diag::error_creating_remark_serializer,
                          toString(std::move(err)));
      return {nullptr, nullptr};
    }
  }

  auto remarkStreamer =
      std::make_unique<SILRemarkStreamer>(mainRemarkStreamer, sourceMgr);
  return {std::move(file), std::move(remarkStreamer)};
}

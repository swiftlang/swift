//===--- SwiftSignatureHelp.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SwiftASTManager.h"
#include "SwiftLangSupport.h"
#include "swift/IDE/SignatureHelp.h"
#include "swift/IDE/SignatureHelpFormatter.h"
#include "swift/IDETool/IDEInspectionInstance.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

static void deliverResults(SourceKit::SignatureHelpConsumer &SKConsumer,
                           CancellableResult<SignatureHelpResults> Result) {
  switch (Result.getKind()) {
  case CancellableResultKind::Success: {
    SKConsumer.setReusingASTContext(Result->DidReuseAST);

    if (!Result->Result) {
      // If we have no results, don't call SKConsumer.handleResult which causes
      // empty results to be delivered.
      break;
    }

    llvm::BumpPtrAllocator Allocator;
    SignatureHelpFormatter Formatter(Allocator);

    SKConsumer.handleResult(Formatter.format(*Result->Result));
    break;
  }
  case CancellableResultKind::Failure:
    SKConsumer.failed(Result.getError());
    break;
  case CancellableResultKind::Cancelled:
    SKConsumer.cancelled();
    break;
  }
}

void SwiftLangSupport::getSignatureHelp(
    StringRef PrimaryFilePath, unsigned Offset, ArrayRef<const char *> Args,
    SourceKitCancellationToken CancellationToken,
    SignatureHelpConsumer &SKConsumer, std::optional<VFSOptions> vfsOptions) {
  std::string error;

  auto fileSystem = getFileSystem(vfsOptions, PrimaryFilePath, error);
  if (!fileSystem) {
    return SKConsumer.failed(error);
  }

  std::string InputFileError;
  std::unique_ptr<llvm::MemoryBuffer> InputBuffer =
      getASTManager()->getMemoryBuffer(PrimaryFilePath, fileSystem,
                                       InputFileError);
  if (!InputBuffer) {
    return SKConsumer.failed(InputFileError);
  }

  performWithParamsToCompletionLikeOperation(
      InputBuffer.get(), Offset, /*InsertCodeCompletionToken=*/true, Args,
      fileSystem, CancellationToken,
      [&](CancellableResult<CompletionLikeOperationParams> ParmsResult) {
        ParmsResult.mapAsync<SignatureHelpResults>(
            [&](auto &Params, auto DeliverTransformed) {
              getIDEInspectionInstance()->signatureHelp(
                  Params.Invocation, Args, fileSystem, Params.completionBuffer,
                  Offset, Params.DiagC, Params.CancellationFlag,
                  DeliverTransformed);
            },
            [&](auto Result) { deliverResults(SKConsumer, Result); });
      });
}

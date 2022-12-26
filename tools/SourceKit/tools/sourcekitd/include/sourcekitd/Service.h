//===--- Service.h - --------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKITD_SERVICE_H
#define LLVM_SOURCEKITD_SERVICE_H

#include "SourceKit/Support/CancellationToken.h"
#include "sourcekitd/sourcekitd.h"
#include "llvm/ADT/StringRef.h"

#include <functional>

namespace sourcekitd {
using SourceKit::SourceKitCancellationToken;

/// Initialize the service. Must be called before attempting to handle requests.
/// \param swiftExecutablePath The path of the swift-frontend executable.
///                            Used to find clang relative to it.
/// \param runtimeLibPath The path to the toolchain's library directory.
/// \param diagnosticDocumentationPath The path to diagnostics documentation.
/// \param postNotification Callback to post a notification.
void initializeService(
    llvm::StringRef swiftExecutablePath, llvm::StringRef runtimeLibPath,
    llvm::StringRef diagnosticDocumentationPath,
    std::function<void(sourcekitd_response_t)> postNotification);
/// Shutdown the service.
void shutdownService();

typedef std::function<void(sourcekitd_response_t)> ResponseReceiver;

void handleRequest(sourcekitd_object_t Request,
                   SourceKitCancellationToken CancellationToken,
                   ResponseReceiver Receiver);

void cancelRequest(SourceKitCancellationToken CancellationToken);

void disposeCancellationToken(SourceKitCancellationToken CancellationToken);

} // namespace sourcekitd

#endif // LLVM_SOURCEKITD_SERVICE_H

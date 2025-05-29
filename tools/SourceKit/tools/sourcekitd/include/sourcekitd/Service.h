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
#include "sourcekitd/plugin.h"
#include "sourcekitd/sourcekitd.h"
#include "llvm/ADT/StringRef.h"

#include <functional>

namespace sourcekitd {
using SourceKit::SourceKitCancellationToken;

/// Initialize the service. Must be called before attempting to handle requests.
/// \param swiftExecutablePath The path of the swift-frontend executable.
///                            Used to find clang relative to it.
/// \param runtimeLibPath The path to the toolchain's library directory.
/// \param postNotification Callback to post a notification.
void initializeService(
    llvm::StringRef swiftExecutablePath, llvm::StringRef runtimeLibPath,
    std::function<void(sourcekitd_response_t)> postNotification);
/// Shutdown the service.
void shutdownService();

/// Register a custom request handler. Must be called only during plugin
/// loading.
void pluginRegisterRequestHandler(
    sourcekitd_cancellable_request_handler_t handler);

/// Register a cancellation handler that will be called when a request is
/// cancelled.
/// This function is called even for cancelled requests that are handled by
/// sourcekitd itself and not the plugin. If the plugin doesn't know the request
/// handle to be cancelled, it should ignore the cancellation request.
/// Must be called only during plugin loading.
void pluginRegisterCancellationHandler(
    sourcekitd_cancellation_handler_t handler);

void *pluginGetOpaqueSwiftIDEInspectionInstance();

typedef std::function<void(sourcekitd_response_t)> ResponseReceiver;

void handleRequest(sourcekitd_object_t Request,
                   SourceKitCancellationToken CancellationToken,
                   ResponseReceiver Receiver);

void cancelRequest(SourceKitCancellationToken CancellationToken);

void disposeCancellationToken(SourceKitCancellationToken CancellationToken);

/// Returns \c true if \p Request is of a request kind that should be issued as
/// a dispatch barrier of the message handling queue. In practice, this returns
/// \c true for open, edit and close requests.
///
/// This does not check if dispatch barriers have been enabled by the sourckitd
/// client.
bool requestIsBarrier(sourcekitd_object_t Request);

/// Returns \c true if this is a request to enable dispatch barriers in
/// sourcekitd.
bool requestIsEnableBarriers(sourcekitd_object_t Request);

/// Send the response that request barriers have been enabled to \p Receiver.
void sendBarriersEnabledResponse(ResponseReceiver Receiver);

} // namespace sourcekitd

#endif // LLVM_SOURCEKITD_SERVICE_H

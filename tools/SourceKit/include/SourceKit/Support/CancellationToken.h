//===--- CancellationToken.h - ----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKIT_SUPPORT_CANCELLATION_TOKEN_H
#define LLVM_SOURCEKIT_SUPPORT_CANCELLATION_TOKEN_H

namespace SourceKit {

/// A token that uniquely identifies a SourceKit request that's served by a
/// \c SwiftASTConsumer. Used to cancel the request.
/// If the cancellation token is \c nullptr, it means that cancellation is not
/// supported.
typedef const void *SourceKitCancellationToken;

} // namespace SourceKit

#endif

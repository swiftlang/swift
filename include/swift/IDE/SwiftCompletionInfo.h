//===--- SwiftCompletionInfo.h --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_SWIFTCOMPLETIONINFO_H
#define SWIFT_IDE_SWIFTCOMPLETIONINFO_H

#include "swift/Frontend/Frontend.h"
#include "swift/IDE/CodeCompletionContext.h"

namespace swift {
namespace ide {

struct SwiftCompletionInfo {
  std::shared_ptr<CompilerInstance> compilerInstance = nullptr;
  CodeCompletionContext *completionContext = nullptr;
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_SWIFTCOMPLETIONINFO_H

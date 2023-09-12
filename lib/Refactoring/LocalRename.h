//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFACTORING_LOCALRENAME_H
#define SWIFT_REFACTORING_LOCALRENAME_H

#include "swift/Refactoring/Refactoring.h"

namespace swift {
namespace refactoring {
using namespace swift::ide;

struct RenameInfo {
  ValueDecl *VD;
  RefactorAvailabilityInfo Availability;
};

llvm::Optional<RenameInfo> getRenameInfo(ResolvedCursorInfoPtr cursorInfo);
} // namespace refactoring
} // namespace swift

#endif

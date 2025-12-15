//===--- WarningGroupBehaviorRule.h -----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_WARNINGBEHAVIORRULE_H
#define SWIFT_BASIC_WARNINGBEHAVIORRULE_H

#include "swift/AST/DiagnosticGroups.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/WarningGroupBehavior.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"
#include <string>
#include <variant>
#include <vector>

namespace swift {

/// Describes a rule how to treat a warning group or all warnings.
class WarningGroupBehaviorRule {
public:
  WarningGroupBehaviorRule(WarningGroupBehavior behavior,
                           std::optional<DiagGroupID> targetGroup = std::nullopt)
    : behavior(behavior), targetGroup(targetGroup) {}

  WarningGroupBehavior getBehavior() const { return behavior; }

  bool hasGroup() const { return targetGroup.has_value(); }
  DiagGroupID getGroup() const {
    ASSERT(hasGroup());
    return *targetGroup;
  }

private:
  WarningGroupBehavior behavior;
  std::optional<DiagGroupID> targetGroup;
};
} // end namespace swift

#endif // SWIFT_BASIC_WARNINGBEHAVIORRULE_H

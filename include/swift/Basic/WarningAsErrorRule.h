//===--- WarningAsErrorRule.h -----------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_WARNINGASERRORRULE_H
#define SWIFT_BASIC_WARNINGASERRORRULE_H

#include <string>
#include <variant>

namespace swift {

/// Describes a rule how to treat a warning or all warnings.
class WarningAsErrorRule {
public:
  enum class Action { Disable, Enable };
  struct TargetAll {};
  struct TargetGroup {
    std::string name;
  };
  using Target = std::variant<TargetAll, TargetGroup>;

  /// Init as a rule targeting all diagnostic groups
  WarningAsErrorRule(Action action) : action(action), target(TargetAll()) {}
  /// Init as a rule targeting a specific diagnostic group
  WarningAsErrorRule(Action action, const std::string &group)
      : action(action), target(TargetGroup{group}) {}

  Action getAction() const { return action; }

  Target getTarget() const { return target; }

private:
  Action action;
  Target target;
};

} // end namespace swift

#endif // SWIFT_BASIC_WARNINGASERRORRULE_H
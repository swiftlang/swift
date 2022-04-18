//===--- PossibleParamInfo.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_POSSIBLEPARAMINFO_H
#define SWIFT_IDE_POSSIBLEPARAMINFO_H

#include "swift/AST/Types.h"

namespace swift {
namespace ide {

struct PossibleParamInfo {
  /// Expected parameter.
  ///
  /// 'nullptr' indicates that the code completion position is at out of
  /// expected argument position. E.g.
  ///   func foo(x: Int) {}
  ///   foo(x: 1, <HERE>)
  const AnyFunctionType::Param *Param;
  bool IsRequired;

  PossibleParamInfo(const AnyFunctionType::Param *Param, bool IsRequired)
      : Param(Param), IsRequired(IsRequired) {
    assert((Param || !IsRequired) &&
           "nullptr with required flag is not allowed");
  };

  friend bool operator==(const PossibleParamInfo &lhs,
                         const PossibleParamInfo &rhs) {
    bool ParamsMatch;
    if (lhs.Param == nullptr && rhs.Param == nullptr) {
      ParamsMatch = true;
    } else if (lhs.Param == nullptr || rhs.Param == nullptr) {
      // One is nullptr but the other is not.
      ParamsMatch = false;
    } else {
      // Both are not nullptr.
      ParamsMatch = (*lhs.Param == *rhs.Param);
    }
    return ParamsMatch && (lhs.IsRequired == rhs.IsRequired);
  }
};

} // end namespace ide
} // end namespace swift

#endif // SWIFT_IDE_POSSIBLEPARAMINFO_H

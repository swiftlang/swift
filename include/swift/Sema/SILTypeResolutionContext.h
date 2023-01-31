//===--- SILTypeResolutionContext.h -----------------------------*- C++ -*-===//
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

#ifndef SWIFT_SEMA_SILTYPERESOLUTIONCONTEXT_H
#define SWIFT_SEMA_SILTYPERESOLUTIONCONTEXT_H

namespace swift {
class GenericParamList;

class SILTypeResolutionContext {
public:
  /// Are we requesting a SIL type?
  bool IsSILType;

  /// Look up types in the given parameter list.
  GenericParamList *GenericParams;

  SILTypeResolutionContext(bool isSILType,
                           GenericParamList *genericParams)
    : IsSILType(isSILType), GenericParams(genericParams) {}
};

}

#endif

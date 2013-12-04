//===--- Failure.h - Swift Language Runtime Failure Traps -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift runtime entry points for triggering runtime failures.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_FAILURE_H
#define SWIFT_RUNTIME_FAILURE_H

namespace swift {

/// Triggers a runtime failure if the condition is true.
extern "C" void swift_conditionalFailure(bool condition);

} // end namespace swift
  
#endif

//===--- Exclusivity.cpp - Exclusivity tracking ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This implements the runtime support for dynamically tracking exclusivity.
//
//===----------------------------------------------------------------------===//
#include <cinttypes>
#include "swift/Runtime/Exclusivity.h"

using namespace swift;

void swift::swift_task_enterThreadLocalContextBackDeploy(char *state) { }

void swift::swift_task_exitThreadLocalContextBackDeploy(char *state) { }

// Forcibly disable exclusivity checking, because the back-deployed concurrency
// library cannot communicate with older Swift runtimes effectively.
__attribute__((constructor)) static void disableExclusivityChecking() {
  _swift_disableExclusivityChecking = true;
}

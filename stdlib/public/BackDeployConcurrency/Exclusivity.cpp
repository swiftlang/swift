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
#include "../runtime/ExclusivityPrivate.h"
#include "../runtime/SwiftTLSContext.h"

using namespace swift;
using namespace swift::runtime;

// Thread-local storage used by the back-deployed concurrency library.
namespace {

static thread_local SwiftTLSContext TLSContext;

} // anonymous namespace

SwiftTLSContext &SwiftTLSContext::get() { return TLSContext; }

// Bring in the concurrency-specific exclusivity code.
#include "../runtime/ConcurrencyExclusivity.inc"

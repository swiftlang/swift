//===--- Bincompat.cpp - Binary compatibility checks. -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Checks for enabling binary compatibility workarounds.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Bincompat.h"

namespace swift {

namespace runtime {

namespace bincompat {

bool workaroundProtocolConformanceReverseIteration() { return false; }

} // namespace bincompat

} // namespace runtime

} // namespace swift

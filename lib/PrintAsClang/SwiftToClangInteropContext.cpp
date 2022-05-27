//===--- SwiftToClangInteropContext.cpp - Interop context -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SwiftToClangInteropContext.h"
#include "swift/IRGen/IRABIDetailsProvider.h"

using namespace swift;

SwiftToClangInteropContext::SwiftToClangInteropContext(
    ModuleDecl &mod, const IRGenOptions &irGenOpts)
    : mod(mod), irGenOpts(irGenOpts) {}

SwiftToClangInteropContext::~SwiftToClangInteropContext() {}

IRABIDetailsProvider &SwiftToClangInteropContext::getIrABIDetails() {
  if (!irABIDetails)
    irABIDetails = std::make_unique<IRABIDetailsProvider>(mod, irGenOpts);
  return *irABIDetails;
}

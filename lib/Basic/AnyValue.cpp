//===--- AnyValue.cpp - Out-of-line code for AnyValue ---------------------===//
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
//  This file implements support code for AnyValue.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/AnyValue.h"
using namespace swift;

AnyValue::HolderBase::~HolderBase() { }

std::string AnyValue::getAsString() const {
  std::string result;
  {
    llvm::raw_string_ostream out(result);
    simple_display(out, *this);
  }
  return result;
}

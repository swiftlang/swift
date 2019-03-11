//===--- SwiftRuntime.cpp - Swift Runtime Handling --------------*- C++ -*-===//
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

#include "swift/Badic/SwiftRuntime.h"

using namespace swift;

bool SwiftRuntime::tryParse(StringRef S) {
  Version = VersionTuple(0);
  return !Version.tryParse(S);
}

std::string SwiftRuntime::getAsString() const {
  std::string Result;
  {
    llvm::raw_string_ostream OS(Result);
    OS << *this;
  }
  return Result;
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &OS, const SwiftRuntime &V) {
  if (V.getVersion() > VersionTuple(0))
    OS << V.getVersion();
  return OS;
}


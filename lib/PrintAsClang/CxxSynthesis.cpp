//===--- CxxSynthesis.cpp - Rules for synthesizing C++ code -----*- C++ -*-===//
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

#include "CxxSynthesis.h"

using namespace swift;
using namespace cxx_synthesis;

StringRef cxx_synthesis::getCxxImplNamespaceName() { return "_impl"; }

/// Print a C++ namespace declaration with the give name and body.
void CxxPrinter::printNamespace(
    llvm::function_ref<void(raw_ostream &OS)> namePrinter,
    llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const {
  os << "namespace ";
  namePrinter(os);
  os << " {\n\n";
  bodyPrinter(os);
  os << "\n} // namespace ";
  namePrinter(os);
  os << "\n\n";
}

void CxxPrinter::printNamespace(
    StringRef name,
    llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const {
  printNamespace([&](raw_ostream &os) { os << name; }, bodyPrinter);
}

//===--- CxxSynthesis.h - Rules for synthesizing C++ code -------*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_CXXSYNTHESIS_H
#define SWIFT_PRINTASCLANG_CXXSYNTHESIS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

namespace cxx_synthesis {

/// Return the name of the implementation namespace that is used to hide
/// declarations from the namespace that corresponds to the imported Swift
/// module in C++.
StringRef getCxxImplNamespaceName();

class CxxPrinter {
public:
  CxxPrinter(raw_ostream &os) : os(os) {}

  /// Print a C++ namespace declaration with the give name and body.
  void
  printNamespace(llvm::function_ref<void(raw_ostream &OS)> namePrinter,
                 llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  void
  printNamespace(StringRef name,
                 llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

private:
  raw_ostream &os;
};

} // end namespace cxx_synthesis
} // end namespace swift

#endif

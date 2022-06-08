//===--- PrintClangValueType.h - Printer for C/C++ value types --*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_PRINTCLANGVALUETYPE_H
#define SWIFT_PRINTASCLANG_PRINTCLANGVALUETYPE_H

#include "swift/Basic/LLVM.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class StructDecl;
class SwiftToClangInteropContext;

/// Responsible for printing a Swift struct or enum decl or in C or C++ mode, to
/// be included in a Swift module's generated clang header.
class ClangValueTypePrinter {
public:
  ClangValueTypePrinter(raw_ostream &os,
                        SwiftToClangInteropContext &interopContext)
      : os(os), interopContext(interopContext) {}

  /// Print the C struct thunk or the C++ class definition that
  /// corresponds to the given structure declaration.
  void printStructDecl(const StructDecl *SD);

private:
  raw_ostream &os;
  SwiftToClangInteropContext &interopContext;
};

} // end namespace swift

#endif

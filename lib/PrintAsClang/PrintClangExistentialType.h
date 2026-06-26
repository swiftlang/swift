//===--- PrintClangExistentialType.h - Print existential types ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTASCLANG_PRINTCLANGEXISTENTIALTYPE_H
#define SWIFT_PRINTASCLANG_PRINTCLANGEXISTENTIALTYPE_H

#include "swift/Basic/LLVM.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class DeclAndTypePrinter;
class ProtocolDecl;

/// Prints a C++ wrapper class for a Swift protocol existential type.
/// Non-marker protocols inherit from SwiftExistentialType with a witness
/// table pointer. Marker protocols inherit from swift::Any (zero WTs).
class ClangExistentialTypePrinter {
public:
  ClangExistentialTypePrinter(raw_ostream &os) : os(os) {}

  void printExistentialTypeDecl(const ProtocolDecl *PD,
                                DeclAndTypePrinter &declAndTypePrinter);

private:
  void printMarkerProtocolDecl(const ProtocolDecl *PD,
                               DeclAndTypePrinter &declAndTypePrinter);

  raw_ostream &os;
};

} // end namespace swift

#endif

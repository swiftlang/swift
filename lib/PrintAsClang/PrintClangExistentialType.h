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
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"
#include <optional>
#include <string>

namespace swift {

class DeclAndTypePrinter;
class FuncDecl;
class ProtocolDecl;
class Type;

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

  /// Emits C++ methods for protocol requirements, including inherited
  /// requirements from base protocols (flattened into the wrapper).
  void printProtocolRequirementMethods(const ProtocolDecl *PD,
                                       DeclAndTypePrinter &declAndTypePrinter);

  /// A step in the chain of base witness table loads needed to reach
  /// an inherited protocol's witness table from _witnessTable.
  struct BaseWTStep {
    size_t offset;
  };

  /// Recursively emits methods for PD and all its base protocols.
  /// baseChain describes the sequence of WT loads from _witnessTable
  /// needed to reach PD's witness table.
  void emitMethodsForProtocol(
      const ProtocolDecl *PD, ArrayRef<BaseWTStep> baseChain,
      llvm::SmallPtrSetImpl<const FuncDecl *> &emittedMethods,
      DeclAndTypePrinter &declAndTypePrinter);

  /// Emits a single C++ method that dispatches through a witness table.
  void emitExistentialMethod(const FuncDecl *FD, size_t methodOffset,
                             uint16_t ptrAuthDisc,
                             ArrayRef<BaseWTStep> baseChain,
                             DeclAndTypePrinter &declAndTypePrinter);

  /// Emits asBaseProtocol() conversion methods for each direct base
  /// protocol conformance.
  void printConversionMethods(const ProtocolDecl *PD,
                              DeclAndTypePrinter &declAndTypePrinter);

  /// Emits the _fromExistential factory in the _impl class body.
  void printImplFromExistentialFactory(const ProtocolDecl *PD,
                                       DeclAndTypePrinter &declAndTypePrinter);

  /// Returns the C++ type name for a Swift type if it is a simple
  /// C-representable primitive, or None otherwise.
  std::optional<std::string> getCxxTypeName(Type ty,
                                            DeclAndTypePrinter &printer);

  /// Returns true if the given FuncDecl can be emitted as a C++ method
  /// on an existential wrapper (non-throwing, non-mutating, non-static,
  /// all params and return C-primitive).
  bool canEmitExistentialMethod(const FuncDecl *FD,
                                DeclAndTypePrinter &printer);

  raw_ostream &os;
};

} // end namespace swift

#endif

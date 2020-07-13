//===--- SILParser.h - SIL Undef Value Representation -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_PARSER_H
#define SWIFT_SIL_PARSER_H

#include "swift/AST/DeclContext.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Parser.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {
namespace syntax {

//===----------------------------------------------------------------------===//
// SILParser local value container
//===----------------------------------------------------------------------===//

struct UnresolvedValueName {
  StringRef Name;
  SourceLoc NameLoc;

  bool isUndef() const { return Name == "undef"; }
};

/// Holds SIL local values and forward references.
class SILParsedLocalValueMap {
  SILModule &module;

  llvm::StringMap<SILValue> localValues;
  /// Used to keep track of forward referenced sil operands. These may be used
  /// in basic blocks that aren't dominance ordered.
  llvm::MapVector<StringRef, SourceLoc> forwardReferences;

  /// Convenience method for diagnosing errors. Uses the module's ASTContext
  /// diagnosis engine.
  template <class... DiagArgs, class... Args>
  InFlightDiagnostic diagnose(SourceLoc loc, Diag<DiagArgs...> diagID,
                              Args &&... args);

public:
  std::function<void()> hadError = []() {
    llvm_unreachable("hadError must be initialized.");
  };

  explicit SILParsedLocalValueMap(SILModule &module) : module(module) {}

  /// Get a reference to the forward references stored in this class.
  llvm::ArrayRef<std::pair<StringRef, SourceLoc>> getForwardRefs();

  /// Get a reference to a local value with the specified name and type.
  SILValue getLocalValue(UnresolvedValueName name, SILType type,
                         SILLocation loc, SILBuilder &builder);

  /// When an instruction or block argument is defined, this method is used to
  /// register it and update our symbol table.
  void setLocalValue(SILValue val, StringRef name, SourceLoc loc);
};

} // namespace syntax
} // namespace swift

#endif // SWIFT_SIL_PARSER_H

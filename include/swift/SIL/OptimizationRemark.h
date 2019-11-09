//===--- OptimizationRemark.h - Optimization diagnostics --------*- C++ -*-===//
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
/// \file
/// This file defines the remark type and the emitter class that passes can use
/// to emit optimization diagnostics.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_OPTIMIZATIONREMARKEMITTER_H
#define SWIFT_SIL_OPTIMIZATIONREMARKEMITTER_H

#include "swift/Basic/SourceLoc.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class SILFunction;

namespace OptRemark {

/// Used in the streaming interface as the general argument type.  It
/// internally converts everything into a key-value pair.
struct Argument {
  std::string Key;
  std::string Val;
  /// If set, the debug location corresponding to the value.
  SourceLoc Loc;

  explicit Argument(StringRef Str = "") : Key("String"), Val(Str) {}
  Argument(StringRef Key, StringRef Val) : Key(Key), Val(Val) {}

  Argument(StringRef Key, int N);
  Argument(StringRef Key, long N);
  Argument(StringRef Key, long long N);
  Argument(StringRef Key, unsigned N);
  Argument(StringRef Key, unsigned long N);
  Argument(StringRef Key, unsigned long long N);

  Argument(StringRef Key, SILFunction *F);
  Argument(StringRef Key, SILType Ty);
  Argument(StringRef Key, CanType Ty);
};

/// Shorthand to insert named-value pairs.
using NV = Argument;

/// Inserting this into a Remark indents the text when printed as a debug
/// message.
struct IndentDebug {
  explicit IndentDebug(unsigned Width) : Width(Width) {}
  unsigned Width;
};

/// The base class for remarks.  This can be created by optimization passed to
/// report successful and unsuccessful optimizations. CRTP is used to preserve
/// the underlying type encoding the remark kind in the insertion operator.
template <typename DerivedT> class Remark {
  /// Arguments collected via the streaming interface.
  SmallVector<Argument, 4> Args;

  /// The name of the pass generating the remark.
  StringRef PassName;

  /// Textual identifier for the remark (single-word, camel-case). Can be used
  /// by external tools reading the YAML output file for optimization remarks to
  /// identify the remark.
  StringRef Identifier;

  /// Source location for the diagnostics.
  SourceLoc Location;

  /// The function for the diagnostics.
  SILFunction *Function;

  /// Indentation used if this remarks is printed as a debug message.
  unsigned IndentDebugWidth = 0;

protected:
  Remark(StringRef Identifier, SILInstruction &I)
      : Identifier(Identifier), Location(I.getLoc().getSourceLoc()),
        Function(I.getParent()->getParent()) {}

public:
  DerivedT &operator<<(StringRef S) {
    Args.emplace_back(S);
    return *static_cast<DerivedT *>(this);
  }

  DerivedT &operator<<(Argument A) {
    Args.push_back(std::move(A));
    return *static_cast<DerivedT *>(this);
  }

  DerivedT &operator<<(IndentDebug ID) {
    IndentDebugWidth = ID.Width;
    return *static_cast<DerivedT *>(this);
  }

  StringRef getPassName() const { return PassName; }
  StringRef getIdentifier() const { return Identifier; }
  SILFunction *getFunction() const { return Function; }
  SourceLoc getLocation() const { return Location; }
  std::string getMsg() const;
  std::string getDebugMsg() const;
  Remark<DerivedT> &getRemark() { return *this; }
  SmallVector<Argument, 4> &getArgs() { return Args; }

  void setPassName(StringRef PN) { PassName = PN; }
};

/// Remark to report a successful optimization.
struct RemarkPassed : public Remark<RemarkPassed> {
  RemarkPassed(StringRef Id, SILInstruction &I) : Remark(Id, I) {}
};
/// Remark to report a unsuccessful optimization.
struct RemarkMissed : public Remark<RemarkMissed> {
  RemarkMissed(StringRef Id, SILInstruction &I) : Remark(Id, I) {}
};

/// Used to emit the remarks.  Passes reporting remarks should create an
/// instance of this.
class Emitter {
  SILModule &Module;
  std::string PassName;
  bool PassedEnabled;
  bool MissedEnabled;

  // Making these non-generic allows out-of-line definition.
  void emit(const RemarkPassed &R);
  void emit(const RemarkMissed &R);
  static void emitDebug(const RemarkPassed &R);
  static void emitDebug(const RemarkMissed &R);

  template <typename RemarkT> bool isEnabled();

public:
  Emitter(StringRef PassName, SILModule &M);

  /// Take a lambda that returns a remark which will be emitted.  The
  /// lambda is not evaluated unless remarks are enabled.  Second argument is
  /// only used to restrict this to functions.
  template <typename T>
  void emit(T RemarkBuilder, decltype(RemarkBuilder()) * = nullptr) {
    using RemarkT = decltype(RemarkBuilder());
    // Avoid building the remark unless remarks are enabled.
    if (isEnabled<RemarkT>() || Module.getOptRecordStream()) {
      auto rb = RemarkBuilder();
      rb.setPassName(PassName);
      emit(rb);
    }
  }

  /// Emit an optimization remark or debug message.
  template <typename T>
  static void emitOrDebug(const char *PassName, Emitter *ORE, T RemarkBuilder,
                          decltype(RemarkBuilder()) * = nullptr) {
    using RemarkT = decltype(RemarkBuilder());
    // Avoid building the remark unless remarks are enabled.
    bool EmitRemark =
        ORE && (ORE->isEnabled<RemarkT>() || ORE->Module.getOptRecordStream());
    // Same for DEBUG.
    bool EmitDebug = false;
#ifndef NDEBUG
    EmitDebug |= llvm::DebugFlag && llvm::isCurrentDebugType(PassName);
#endif // NDEBUG

    if (EmitRemark || EmitDebug) {
      auto R = RemarkBuilder();
      if (EmitDebug)
        emitDebug(R);
      if (EmitRemark) {
        // If we have ORE use the PassName that was set up with ORE. DEBUG_TYPE
        // may be different if a pass is calling other modules.
        R.setPassName(ORE->PassName);
        ORE->emit(R);
      }
    }
  }
};

#define REMARK_OR_DEBUG(...)                                                   \
  OptRemark::Emitter::emitOrDebug(DEBUG_TYPE, __VA_ARGS__)

template <> inline bool Emitter::isEnabled<RemarkMissed>() {
  return MissedEnabled;
}
template <> inline bool Emitter::isEnabled<RemarkPassed>() {
  return PassedEnabled;
}
} // namespace OptRemark
} // namespace swift
#endif

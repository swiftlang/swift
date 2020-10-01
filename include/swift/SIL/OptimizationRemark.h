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

#include "swift/AST/SemanticAttrs.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Demangling/Demangler.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class SILFunction;

namespace OptRemark {

struct ArgumentKeyKind {
  enum InnerTy {
    // Just assume this is a normal msg that we are emitting.
    Default,

    // Assume this is a note that should be emitted as a separate
    // diagnostic when emitting diagnostics. Do nothing special
    // along the backend path.
    Note,

    // Assume that this is a note that should be emitted as a separate
    // diagnostic but that doesn't have its own source loc: we should reuse the
    // one for the original remark.
    //
    // This is intended to be used in situations where one needs to emit a
    // "note" warning due to us not being able to infer a part of our
    // opt-remark.
    ParentLocNote,
  };

  InnerTy innerValue;

  ArgumentKeyKind(InnerTy value) : innerValue(value) {}

  operator InnerTy() const { return innerValue; }

  /// Return true if this argument is meant to be a separate diagnostic when we
  /// emit diagnostics but when we emit to the remark streamer (due to not
  /// having support for this), we just emit the remark inline.
  ///
  /// TODO: Unfortunate that this needs to be done.
  bool isSeparateDiagnostic() const {
    switch (innerValue) {
    case InnerTy::Default:
      return false;
    case InnerTy::Note:
    case InnerTy::ParentLocNote:
      return true;
    }

    llvm_unreachable("Covered switch isn't covered?!");
  }
};

struct ArgumentKey {
  ArgumentKeyKind kind;
  std::string data;

  ArgumentKey(ArgumentKeyKind kind, StringRef data) : kind(kind), data(data) {}
  ArgumentKey(ArgumentKeyKind::InnerTy kind, StringRef data)
      : kind(kind), data(data) {}
  ArgumentKey(ArgumentKey kind, StringRef data) : kind(kind.kind), data(data) {}
};

/// Used in the streaming interface as the general argument type.  It
/// internally converts everything into a key-value pair.
struct Argument {
  ArgumentKey key;
  std::string val;
  /// If set, the debug location corresponding to the value.
  SourceLoc loc;

  explicit Argument(StringRef str = "")
      : Argument({ArgumentKeyKind::Default, "String"}, str) {}

  Argument(StringRef key, StringRef val)
      : Argument({ArgumentKeyKind::Default, key}, val) {}
  Argument(ArgumentKey key, StringRef val) : key(key), val(val) {}
  Argument(StringRef key, int n);
  Argument(StringRef key, long n);
  Argument(StringRef key, long long n);
  Argument(StringRef key, unsigned n);
  Argument(StringRef key, unsigned long n);
  Argument(StringRef key, unsigned long long n);

  Argument(StringRef key, SILFunction *f)
      : Argument(ArgumentKey(ArgumentKeyKind::Default, key), f) {}
  Argument(ArgumentKey key, SILFunction *f);
  Argument(StringRef key, SILType ty);
  Argument(StringRef key, CanType ty);

  Argument(StringRef key, StringRef msg, const ValueDecl *decl)
      : Argument(ArgumentKey(ArgumentKeyKind::Default, key), msg, decl) {}
  Argument(ArgumentKey key, StringRef msg, const ValueDecl *decl)
      : key(key), val(msg), loc(decl->getLoc()) {}

  Argument(StringRef key, llvm::Twine &&msg, SILLocation loc)
      : Argument(ArgumentKey(ArgumentKeyKind::Default, key), std::move(msg),
                 loc) {}
  Argument(ArgumentKey key, llvm::Twine &&msg, SILLocation loc)
      : key(key), val(msg.str()), loc(loc.getSourceLoc()) {}
};

/// Shorthand to insert named-value pairs.
using NV = Argument;

/// Inserting this into a Remark indents the text when printed as a debug
/// message.
struct IndentDebug {
  explicit IndentDebug(unsigned width) : width(width) {}
  unsigned width;
};

enum class SourceLocInferenceBehavior : unsigned {
  None = 0,
  ForwardScan = 0x1,
  BackwardScan = 0x2,
  ForwardScan2nd = 0x4,
  AlwaysInfer = 0x8,

  ForwardThenBackwards = ForwardScan | BackwardScan,
  BackwardsThenForwards = BackwardScan | ForwardScan2nd,
  ForwardScanAlwaysInfer = ForwardScan | AlwaysInfer,
  BackwardScanAlwaysInfer = BackwardScan | AlwaysInfer,
};

inline SourceLocInferenceBehavior operator&(SourceLocInferenceBehavior lhs,
                                            SourceLocInferenceBehavior rhs) {
  auto lhsVal = std::underlying_type<SourceLocInferenceBehavior>::type(lhs);
  auto rhsVal = std::underlying_type<SourceLocInferenceBehavior>::type(rhs);
  return SourceLocInferenceBehavior(lhsVal & rhsVal);
}

/// Infer the proper SourceLoc to use for the given SILInstruction.
///
/// This means that if we have a valid location for the instruction, we just
/// return that. Otherwise, we have a runtime instruction that does not have a
/// valid source location. In such a case, we infer the source location from the
/// surrounding code. If we can not find any surrounding code, we return an
/// invalid SourceLoc.
SourceLoc inferOptRemarkSourceLoc(SILInstruction &i,
                                  SourceLocInferenceBehavior inferBehavior);

/// The base class for remarks.  This can be created by optimization passed to
/// report successful and unsuccessful optimizations. CRTP is used to preserve
/// the underlying type encoding the remark kind in the insertion operator.
template <typename DerivedT>
class Remark {
  /// Arguments collected via the streaming interface.
  SmallVector<Argument, 4> args;

  /// The name of the pass generating the remark.
  StringRef passName;

  /// Textual identifier for the remark (single-word, camel-case). Can be used
  /// by external tools reading the output file for optimization remarks to
  /// identify the remark.
  SmallString<32> identifier;

  /// Source location for the diagnostics.
  SourceLoc location;

  /// The function for the diagnostics.
  SILFunction *function;

  /// The demangled name of \p Function.
  SmallString<64> demangledFunctionName;

  /// Indentation used if this remarks is printed as a debug message.
  unsigned indentDebugWidth = 0;

protected:
  Remark(StringRef identifier, SILInstruction &i,
         SourceLocInferenceBehavior inferenceBehavior)
      : identifier((Twine("sil.") + identifier).str()),
        location(inferOptRemarkSourceLoc(i, inferenceBehavior)),
        function(i.getParent()->getParent()),
        demangledFunctionName(Demangle::demangleSymbolAsString(
            function->getName(),
            Demangle::DemangleOptions::SimplifiedUIDemangleOptions())) {}

public:
  DerivedT &operator<<(StringRef s) {
    args.emplace_back(s);
    return *static_cast<DerivedT *>(this);
  }

  DerivedT &operator<<(Argument a) {
    args.push_back(std::move(a));
    return *static_cast<DerivedT *>(this);
  }

  DerivedT &operator<<(IndentDebug indent) {
    indentDebugWidth = indent.width;
    return *static_cast<DerivedT *>(this);
  }

  StringRef getPassName() const { return passName; }
  StringRef getIdentifier() const { return identifier; }
  SILFunction *getFunction() const { return function; }
  StringRef getDemangledFunctionName() const { return demangledFunctionName; }
  SourceLoc getLocation() const { return location; }
  std::string getMsg() const;
  std::string getDebugMsg() const;
  Remark<DerivedT> &getRemark() { return *this; }
  SmallVector<Argument, 4> &getArgs() { return args; }
  ArrayRef<Argument> getArgs() const { return args; }

  void setPassName(StringRef name) { passName = name; }
};

/// Remark to report a successful optimization.
struct RemarkPassed : public Remark<RemarkPassed> {
  RemarkPassed(StringRef id, SILInstruction &i)
      : Remark(id, i, SourceLocInferenceBehavior::None) {}
  RemarkPassed(StringRef id, SILInstruction &i,
               SourceLocInferenceBehavior inferenceBehavior)
      : Remark(id, i, inferenceBehavior) {}
};
/// Remark to report a unsuccessful optimization.
struct RemarkMissed : public Remark<RemarkMissed> {
  RemarkMissed(StringRef id, SILInstruction &i)
      : Remark(id, i, SourceLocInferenceBehavior::None) {}
  RemarkMissed(StringRef id, SILInstruction &i,
               SourceLocInferenceBehavior inferenceBehavior)
      : Remark(id, i, inferenceBehavior) {}
};

/// Used to emit the remarks.  Passes reporting remarks should create an
/// instance of this.
class Emitter {
  SILFunction &fn;
  std::string passName;
  bool passedEnabled;
  bool missedEnabled;

  // Making these non-generic allows out-of-line definition.
  void emit(const RemarkPassed &remark);
  void emit(const RemarkMissed &remark);
  static void emitDebug(const RemarkPassed &remark);
  static void emitDebug(const RemarkMissed &remark);

  template <typename RemarkT> bool isEnabled();

public:
  Emitter(StringRef passName, SILFunction &fn);

  /// Take a lambda that returns a remark which will be emitted.  The
  /// lambda is not evaluated unless remarks are enabled.  Second argument is
  /// only used to restrict this to functions.
  template <typename T>
  void emit(T remarkBuilder, decltype(remarkBuilder()) * = nullptr) {
    using RemarkT = decltype(remarkBuilder());
    // Avoid building the remark unless remarks are enabled.
    if (isEnabled<RemarkT>() || fn.getModule().getSILRemarkStreamer()) {
      auto rb = remarkBuilder();
      rb.setPassName(passName);
      emit(rb);
    }
  }

  /// Emit an optimization remark or debug message.
  template <typename T>
  static void emitOrDebug(const char *passName, Emitter *emitter,
                          T remarkBuilder,
                          decltype(remarkBuilder()) * = nullptr) {
    using RemarkT = decltype(remarkBuilder());
    // Avoid building the remark unless remarks are enabled.
    bool emitRemark =
        emitter && (emitter->isEnabled<RemarkT>() ||
                    emitter->fn.getModule().getSILRemarkStreamer());
    // Same for DEBUG.
    bool shouldEmitDebug = false;
#ifndef NDEBUG
    shouldEmitDebug |= llvm::DebugFlag && llvm::isCurrentDebugType(passName);
#endif // NDEBUG

    if (emitRemark || shouldEmitDebug) {
      auto r = remarkBuilder();
      if (shouldEmitDebug)
        emitDebug(r);
      if (emitRemark) {
        // If we have an Emitter use the PassName that was set up. DEBUG_TYPE
        // may be different if a pass is calling other modules.
        r.setPassName(emitter->passName);
        emitter->emit(r);
      }
    }
  }
};

#define REMARK_OR_DEBUG(...)                                                   \
  OptRemark::Emitter::emitOrDebug(DEBUG_TYPE, __VA_ARGS__)

template <> inline bool Emitter::isEnabled<RemarkMissed>() {
  return missedEnabled;
}
template <> inline bool Emitter::isEnabled<RemarkPassed>() {
  return passedEnabled;
}
} // namespace OptRemark
} // namespace swift
#endif

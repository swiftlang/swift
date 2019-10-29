//===--- OptimizationRemark.cpp - Optimization diagnostics ------*- C++ -*-===//
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

#include "swift/SIL/OptimizationRemark.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Demangling/Demangler.h"
#include "swift/SIL/SILRemarkStreamer.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace OptRemark;

Argument::Argument(StringRef key, int n) : key(key), val(llvm::itostr(n)) {}

Argument::Argument(StringRef key, long n) : key(key), val(llvm::itostr(n)) {}

Argument::Argument(StringRef key, long long n)
    : key(key), val(llvm::itostr(n)) {}

Argument::Argument(StringRef key, unsigned n)
    : key(key), val(llvm::utostr(n)) {}

Argument::Argument(StringRef key, unsigned long n)
    : key(key), val(llvm::utostr(n)) {}

Argument::Argument(StringRef key, unsigned long long n)
    : key(key), val(llvm::utostr(n)) {}

Argument::Argument(StringRef key, SILFunction *f) : key(key) {
  auto options = Demangle::DemangleOptions::SimplifiedUIDemangleOptions();
  // Enable module names so that we have a way of filtering out
  // stdlib-related remarks.
  options.DisplayModuleNames = true;

  val = (Twine("\"") + Demangle::demangleSymbolAsString(f->getName(), options) +
         "\"")
            .str();

  if (f->hasLocation())
    loc = f->getLocation().getSourceLoc();
}

Argument::Argument(StringRef key, SILType ty) : key(key) {
  llvm::raw_string_ostream stream(val);
  ty.print(stream);
}

Argument::Argument(StringRef key, CanType ty) : key(key) {
  llvm::raw_string_ostream stream(val);
  ty.print(stream);
}

template <typename DerivedT> std::string Remark<DerivedT>::getMsg() const {
  std::string str;
  llvm::raw_string_ostream stream(str);
  for (const Argument &arg : args)
    stream << arg.val;
  return stream.str();
}

template <typename DerivedT> std::string Remark<DerivedT>::getDebugMsg() const {
  std::string str;
  llvm::raw_string_ostream stream(str);

  if (indentDebugWidth)
    stream << std::string(" ", indentDebugWidth);

  for (const Argument &arg : args)
    stream << arg.val;

  stream << "\n";
  return stream.str();
}

Emitter::Emitter(StringRef passName, SILModule &m)
    : module(m), passName(passName),
      passedEnabled(
          m.getASTContext().LangOpts.OptimizationRemarkPassedPattern &&
          m.getASTContext().LangOpts.OptimizationRemarkPassedPattern->match(
              passName)),
      missedEnabled(
          m.getASTContext().LangOpts.OptimizationRemarkMissedPattern &&
          m.getASTContext().LangOpts.OptimizationRemarkMissedPattern->match(
              passName)) {}

template <typename RemarkT, typename... ArgTypes>
static void emitRemark(SILModule &module, const Remark<RemarkT> &remark,
                       Diag<ArgTypes...> id, bool diagEnabled) {
  if (remark.getLocation().isInvalid())
    return;
  if (auto *remarkStreamer = module.getSILRemarkStreamer())
    remarkStreamer->emit(remark);
  if (diagEnabled)
    module.getASTContext().Diags.diagnose(remark.getLocation(), id,
                                          remark.getMsg());
}

void Emitter::emit(const RemarkPassed &remark) {
  emitRemark(module, remark, diag::opt_remark_passed,
             isEnabled<RemarkPassed>());
}

void Emitter::emit(const RemarkMissed &remark) {
  emitRemark(module, remark, diag::opt_remark_missed,
             isEnabled<RemarkMissed>());
}

void Emitter::emitDebug(const RemarkPassed &remark) {
  llvm::dbgs() << remark.getDebugMsg();
}

void Emitter::emitDebug(const RemarkMissed &remark) {
  llvm::dbgs() << remark.getDebugMsg();
}

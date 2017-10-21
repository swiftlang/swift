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
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace OptRemark;

Argument::Argument(StringRef Key, int N) : Key(Key), Val(llvm::itostr(N)) {}

Argument::Argument(StringRef Key, long N) : Key(Key), Val(llvm::itostr(N)) {}

Argument::Argument(StringRef Key, long long N)
    : Key(Key), Val(llvm::itostr(N)) {}

Argument::Argument(StringRef Key, unsigned N)
    : Key(Key), Val(llvm::utostr(N)) {}

Argument::Argument(StringRef Key, unsigned long N)
    : Key(Key), Val(llvm::utostr(N)) {}

Argument::Argument(StringRef Key, unsigned long long N)
    : Key(Key), Val(llvm::utostr(N)) {}

Argument::Argument(StringRef Key, SILFunction *F)
    : Key(Key), Val(F->getName()) {
  if (F->hasLocation())
    Loc = F->getLocation().getSourceLoc();
}

template <typename DerivedT> std::string Remark<DerivedT>::getMsg() const {
  std::string Str;
  llvm::raw_string_ostream OS(Str);
  for (const Argument &Arg : Args)
    OS << Arg.Val;
  return OS.str();
}

Emitter::Emitter(StringRef PassName, ASTContext &Ctx)
    : Ctx(Ctx), PassName(PassName),
      PassedEnabled(
          Ctx.LangOpts.OptimizationRemarkPassedPattern &&
          Ctx.LangOpts.OptimizationRemarkPassedPattern->match(PassName)),
      MissedEnabled(
          Ctx.LangOpts.OptimizationRemarkMissedPattern &&
          Ctx.LangOpts.OptimizationRemarkMissedPattern->match(PassName)) {}

template <typename RemarkT, typename... ArgTypes>
static void emitRemark(ASTContext &Ctx, const RemarkT &R,
                       Diag<ArgTypes...> ID) {
  Ctx.Diags.diagnose(R.getLocation(), ID, R.getMsg());
}

void Emitter::emit(const RemarkPassed &R) {
  emitRemark(Ctx, R, diag::opt_remark_passed);
}

void Emitter::emit(const RemarkMissed &R) {
  emitRemark(Ctx, R, diag::opt_remark_missed);
}

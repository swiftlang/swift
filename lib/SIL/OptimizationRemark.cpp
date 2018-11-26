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
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/YAMLTraits.h"
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
    : Key(Key) {
      auto DO = Demangle::DemangleOptions::SimplifiedUIDemangleOptions();
      // Enable module names so that we have a way of filtering out
      // stdlib-related remarks.
      DO.DisplayModuleNames = true;

      Val = (Twine("\"") + Demangle::demangleSymbolAsString(F->getName(), DO) +
             "\"")
                .str();

      if (F->hasLocation())
        Loc = F->getLocation().getSourceLoc();
}

Argument::Argument(StringRef Key, SILType *Ty) : Key(Key) {
  llvm::raw_string_ostream OS(Val);
  Ty->print(OS);
}

template <typename DerivedT> std::string Remark<DerivedT>::getMsg() const {
  std::string Str;
  llvm::raw_string_ostream OS(Str);
  for (const Argument &Arg : Args)
    OS << Arg.Val;
  return OS.str();
}

template <typename DerivedT> std::string Remark<DerivedT>::getDebugMsg() const {
  std::string Str;
  llvm::raw_string_ostream OS(Str);

  if (IndentDebugWidth)
    OS << std::string(" ", IndentDebugWidth);

  for (const Argument &Arg : Args)
    OS << Arg.Val;

  OS << "\n";
  return OS.str();
}

Emitter::Emitter(StringRef PassName, SILModule &M)
    : Module(M), PassName(PassName),
      PassedEnabled(
          M.getASTContext().LangOpts.OptimizationRemarkPassedPattern &&
          M.getASTContext().LangOpts.OptimizationRemarkPassedPattern->match(
              PassName)),
      MissedEnabled(
          M.getASTContext().LangOpts.OptimizationRemarkMissedPattern &&
          M.getASTContext().LangOpts.OptimizationRemarkMissedPattern->match(
              PassName)) {}

template <typename RemarkT, typename... ArgTypes>
static void emitRemark(SILModule &Module, const Remark<RemarkT> &R,
                       Diag<ArgTypes...> ID, bool DiagEnabled) {
  if (R.getLocation().isInvalid())
    return;
  if (auto *Out = Module.getOptRecordStream())
    // YAMLTraits takes a non-const reference even when outputting.
    *Out << const_cast<Remark<RemarkT> &>(R);
  if (DiagEnabled)
    Module.getASTContext().Diags.diagnose(R.getLocation(), ID, R.getMsg());
}

void Emitter::emit(const RemarkPassed &R) {
  emitRemark(Module, R, diag::opt_remark_passed, isEnabled<RemarkPassed>());
}

void Emitter::emit(const RemarkMissed &R) {
  emitRemark(Module, R, diag::opt_remark_missed, isEnabled<RemarkMissed>());
}

void Emitter::emitDebug(const RemarkPassed &R) {
  llvm::dbgs() << R.getDebugMsg();
}

void Emitter::emitDebug(const RemarkMissed &R) {
  llvm::dbgs() << R.getDebugMsg();
}

namespace llvm {
namespace yaml {

template <typename KindT> struct MappingTraits<Remark<KindT>> {
  static void mapping(llvm::yaml::IO &io, Remark<KindT> &R) {
    assert(io.outputting() && "input not implemented");

    if (io.mapTag("!Passed", std::is_same<KindT, RemarkPassed>::value))
      ;
    else if (io.mapTag("!Missed", std::is_same<KindT, RemarkMissed>::value))
      ;
    else
      llvm_unreachable("Unknown remark type");

    // The attributes are read-only for now since we're only support outputting
    // them.
    StringRef PassName = R.getPassName();
    io.mapRequired("Pass", PassName);
    std::string Id = (Twine("sil.") + R.getIdentifier()).str();
    io.mapRequired("Name", Id);

    SourceLoc Loc = R.getLocation();
    if (!io.outputting() || Loc.isValid())
      io.mapOptional("DebugLoc", Loc);

    std::string FN = Demangle::demangleSymbolAsString(
        R.getFunction()->getName(),
        Demangle::DemangleOptions::SimplifiedUIDemangleOptions());
    io.mapRequired("Function", FN);
    io.mapOptional("Args", R.getArgs());
  }
};

template <> struct MappingTraits<SourceLoc> {
  static void mapping(IO &io, SourceLoc &Loc) {
    assert(io.outputting() && "input not yet implemented");

    SourceManager *SM = static_cast<SourceManager *>(io.getContext());
    StringRef File = SM->getDisplayNameForLoc(Loc);
    unsigned Line, Col;
    std::tie(Line, Col) = SM->getLineAndColumn(Loc);

    io.mapRequired("File", File);
    io.mapRequired("Line", Line);
    io.mapRequired("Column", Col);
  }
};

// Implement this as a mapping for now to get proper quotation for the value.
template <> struct MappingTraits<OptRemark::Argument> {
  static void mapping(IO &io, OptRemark::Argument &A) {
    assert(io.outputting() && "input not yet implemented");
    io.mapRequired(A.Key.data(), A.Val);
    if (A.Loc.isValid())
      io.mapOptional("DebugLoc", A.Loc);
  }
};

} // end namespace yaml
} // end namespace llvm

LLVM_YAML_IS_SEQUENCE_VECTOR(OptRemark::Argument)

//===--- Job.cpp - Command to Execute -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/STLExtras.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/PrettyStackTrace.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Option/Arg.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::driver;

StringRef CommandOutput::getOutputForInputAndType(StringRef PrimaryInputFile,
                                                  types::ID Type) const {
  auto const *M = DerivedOutputMap.getOutputMapForInput(PrimaryInputFile);
  if (!M)
    return StringRef();
  auto const Out = M->find(Type);
  if (Out == M->end())
    return StringRef();
  return StringRef(Out->second);
}

void CommandOutput::ensureEntry(StringRef PrimaryInputFile,
                                types::ID Type,
                                StringRef OutputFile,
                                bool Overwrite) {
  auto &M = DerivedOutputMap.getOrCreateOutputMapForInput(PrimaryInputFile);
  if (Overwrite) {
    M[Type] = OutputFile;
  } else {
    auto res = M.insert(std::make_pair(Type, OutputFile));
    if (res.second) {
      // New entry, no need to compare.
    } else {
      // Existing entry, check equality with request.
      assert(res.first->getSecond() == OutputFile);
    }
  }
}

bool CommandOutput::hasSameAdditionalOutputTypes(
    CommandOutput const &other) const {
  bool sameAdditionalOutputTypes = true;
  types::forAllTypes([&](types::ID Type) {
      bool a = AdditionalOutputTypes.count(Type) == 0;
      bool b = other.AdditionalOutputTypes.count(Type) == 0;
      if (a != b)
        sameAdditionalOutputTypes = false;
    });
  return sameAdditionalOutputTypes;
}

void CommandOutput::addOutputs(CommandOutput const &other) {
  assert(PrimaryOutputType == other.PrimaryOutputType);
  assert(&DerivedOutputMap == &other.DerivedOutputMap);
  Inputs.append(other.Inputs.begin(),
                other.Inputs.end());
  // Should only be called with an empty AdditionalOutputTypes
  // or one populated with the same types as other.
  if (AdditionalOutputTypes.empty()) {
    AdditionalOutputTypes = other.AdditionalOutputTypes;
  } else {
    assert(hasSameAdditionalOutputTypes(other));
  }
}

CommandOutput::CommandOutput(types::ID PrimaryOutputType,
                             OutputFileMap &Derived)
    : PrimaryOutputType(PrimaryOutputType), DerivedOutputMap(Derived) {}

types::ID CommandOutput::getPrimaryOutputType() const {
  return PrimaryOutputType;
}

void CommandOutput::addPrimaryOutput(CommandInputPair Input,
                                     StringRef PrimaryOutputFile) {
  Inputs.push_back(Input);
  PrettyStackTraceDriverCommandOutputAddition CrashInfo(
      "primary", this, Input.Primary, PrimaryOutputType, PrimaryOutputFile);
  ensureEntry(Input.Primary, PrimaryOutputType, PrimaryOutputFile, false);
}

StringRef CommandOutput::getPrimaryOutputFilename() const {
  assert(Inputs.size() >= 1);
  return getOutputForInputAndType(Inputs[0].Primary, PrimaryOutputType);
}

SmallVector<StringRef, 16> CommandOutput::getPrimaryOutputFilenames() const {
  SmallVector<StringRef, 16> V;
  for (auto const &I : Inputs) {
    auto Out = getOutputForInputAndType(I.Primary, PrimaryOutputType);
    V.push_back(Out);
  }
  return V;
}

void CommandOutput::setAdditionalOutputForType(types::ID Type,
                                               StringRef OutputFilename) {
  PrettyStackTraceDriverCommandOutputAddition CrashInfo(
      "additional", this, Inputs[0].Primary, Type, OutputFilename);
  assert(Inputs.size() >= 1);

  // If we're given an "additional" output with the same type as the primary,
  // and we've not yet had such an additional type added, we treat it as a
  // request to overwrite the primary choice (which happens early and is
  // sometimes just inferred) with a refined value (eg. -emit-module-path).
  bool Overwrite = (Type == PrimaryOutputType &&
                    AdditionalOutputTypes.count(Type) == 0);
  ensureEntry(Inputs[0].Primary, Type, OutputFilename, Overwrite);
  AdditionalOutputTypes.insert(Type);
}

StringRef CommandOutput::getAdditionalOutputForType(types::ID Type) const {
  if (AdditionalOutputTypes.count(Type) == 0)
    return StringRef();
  assert(Inputs.size() >= 1);
  return getOutputForInputAndType(Inputs[0].Primary, Type);
}

StringRef CommandOutput::getAnyOutputForType(types::ID Type) const {
  if (PrimaryOutputType == Type)
    return getPrimaryOutputFilename();
  return getAdditionalOutputForType(Type);
}

StringRef CommandOutput::getBaseInput(size_t Index) const {
  assert(Index < Inputs.size());
  return Inputs[Index].Base;
}

static void escapeAndPrintString(llvm::raw_ostream &os, StringRef Str) {
  if (Str.empty()) {
    // Special-case the empty string.
    os << "\"\"";
    return;
  }

  bool NeedsEscape = Str.find_first_of(" \"\\$") != StringRef::npos;

  if (!NeedsEscape) {
    // This string doesn't have anything we need to escape, so print it directly
    os << Str;
    return;
  }

  // Quote and escape. This isn't really complete, but is good enough, and
  // matches how Clang's Command handles escaping arguments.
  os << '"';
  for (const char c : Str) {
    switch (c) {
    case '"':
    case '\\':
    case '$':
      // These characters need to be escaped.
      os << '\\';
      // Fall-through to the default case, since we still need to print the
      // character.
      LLVM_FALLTHROUGH;
    default:
      os << c;
    }
  }
  os << '"';
}

void
CommandOutput::print(raw_ostream &out) const {
  out
    << "{\n"
    << "    PrimaryOutputType = " << types::getTypeName(PrimaryOutputType)
    << ";\n"
    << "    Inputs = [\n";
  interleave(Inputs,
             [&](CommandInputPair const &P) {
             out << "        CommandInputPair {\n"
                 << "            Base = ";
             escapeAndPrintString(out, P.Base);
             out << ", \n"
                 << "            Primary = ";
             escapeAndPrintString(out, P.Primary);
             out << "\n        }";
           },
           [&] { out << ",\n"; });
  out << "];\n"
      << "    DerivedOutputFileMap = {\n";
  DerivedOutputMap.dump(out, true);
  out << "\n    };\n}";
}

void
CommandOutput::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

Job::~Job() = default;

void Job::printArguments(raw_ostream &os,
                         const llvm::opt::ArgStringList &Args) {
  interleave(Args,
             [&](const char *Arg) { escapeAndPrintString(os, Arg); },
             [&] { os << ' '; });
}

void Job::dump() const {
  printCommandLineAndEnvironment(llvm::errs());
}

void Job::printCommandLineAndEnvironment(raw_ostream &Stream,
                                         StringRef Terminator) const {
  printCommandLine(Stream, /*Terminator=*/"");
  if (!ExtraEnvironment.empty()) {
    Stream << "  #";
    for (auto &pair : ExtraEnvironment) {
      Stream << " " << pair.first << "=" << pair.second;
    }
  }
  Stream << "\n";
}

void Job::printCommandLine(raw_ostream &os, StringRef Terminator) const {
  escapeAndPrintString(os, Executable);
  os << ' ';
  printArguments(os, Arguments);
  os << Terminator;
}

void Job::printSummary(raw_ostream &os) const {
  // Deciding how to describe our inputs is a bit subtle; if we are a Job built
  // from a JobAction that itself has InputActions sources, then we collect
  // those up. Otherwise it's more correct to talk about our inputs as the
  // outputs of our input-jobs.
  SmallVector<std::string, 4> Inputs;

  for (const Action *A : getSource().getInputs())
    if (const auto *IA = dyn_cast<InputAction>(A))
      Inputs.push_back(IA->getInputArg().getValue());

  for (const Job *J : getInputs())
    for (const std::string &f : J->getOutput().getPrimaryOutputFilenames())
      Inputs.push_back(f);

  size_t limit = 3;
  size_t actual = Inputs.size();
  if (actual > limit) {
    Inputs.erase(Inputs.begin() + limit, Inputs.end());
  }

  os << "{" << getSource().getClassName() << ": ";
  interleave(getOutput().getPrimaryOutputFilenames(),
             [&](const std::string &Arg) {
               os << llvm::sys::path::filename(Arg);
             },
             [&] { os << ' '; });
  os << " <= ";
  interleave(Inputs,
             [&](const std::string &Arg) {
               os << llvm::sys::path::filename(Arg);
             },
             [&] { os << ' '; });
  if (actual > limit) {
    os << " ... " << (actual-limit) << " more";
  }
  os << "}";
}

BatchJob::BatchJob(const JobAction &Source,
                   SmallVectorImpl<const Job *> &&Inputs,
                   std::unique_ptr<CommandOutput> Output,
                   const char *Executable, llvm::opt::ArgStringList Arguments,
                   EnvironmentVector ExtraEnvironment,
                   std::vector<FilelistInfo> Infos,
                   ArrayRef<const Job *> Combined)
    : Job(Source, std::move(Inputs), std::move(Output), Executable, Arguments,
          ExtraEnvironment, Infos),
      CombinedJobs(Combined.begin(), Combined.end()) {}

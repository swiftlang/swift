//===-------- ParseableOutput.cpp - Helpers for parseable output ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/ParseableOutput.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Job.h"
#include "llvm/Option/Arg.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#include <sstream>

using namespace swift::parseable_output;
using namespace swift::driver;
using namespace swift::sys;
using namespace swift;

namespace {
struct CommandInput {
  std::string Path;
  CommandInput() {}
  CommandInput(StringRef Path) : Path(Path) {}
};

using OutputPair = std::pair<file_types::ID, std::string>;
} // end anonymous namespace

namespace swift {
namespace json {
template <> struct ScalarTraits<CommandInput> {
  static void output(const CommandInput &value, llvm::raw_ostream &os) {
    os << value.Path;
  }
  static bool mustQuote(StringRef) { return true; }
};

template <> struct ScalarEnumerationTraits<file_types::ID> {
  static void enumeration(Output &out, file_types::ID &value) {
    file_types::forAllTypes([&](file_types::ID ty) {
      std::string typeName = file_types::getTypeName(ty).str();
      out.enumCase(value, typeName.c_str(), ty);
    });
  }
};

template <> struct ObjectTraits<std::pair<file_types::ID, std::string>> {
  static void mapping(Output &out,
                      std::pair<file_types::ID, std::string> &value) {
    out.mapRequired("type", value.first);
    out.mapRequired("path", value.second);
  }
};

template <typename T, unsigned N> struct ArrayTraits<SmallVector<T, N>> {
  static size_t size(Output &out, SmallVector<T, N> &seq) { return seq.size(); }

  static T &element(Output &out, SmallVector<T, N> &seq, size_t index) {
    if (index >= seq.size())
      seq.resize(index + 1);
    return seq[index];
  }
};
} // namespace json
} // namespace swift

namespace {

Action::Kind
mapFrontendInvocationToActionKind(const CompilerInvocation &Invocation) {
  auto Executable = llvm::sys::path::filename(
      Invocation.getFrontendOptions().MainExecutablePath);
  assert(Executable.str() == "swift-frontend" &&
         "Expected a swift-frontend invocation.");

  Action::Kind ActionKind;
  FrontendOptions::ActionType ActionType =
      Invocation.getFrontendOptions().RequestedAction;
  switch (ActionType) {
  case FrontendOptions::ActionType::REPL:
    ActionKind = Action::Kind::REPLJob;
    break;
  case FrontendOptions::ActionType::MergeModules:
    ActionKind = Action::Kind::MergeModuleJob;
    break;
  case FrontendOptions::ActionType::Immediate:
    ActionKind = Action::Kind::InterpretJob;
    break;
  case FrontendOptions::ActionType::TypecheckModuleFromInterface:
    ActionKind = Action::Kind::VerifyModuleInterfaceJob;
    break;
  case FrontendOptions::ActionType::EmitPCH:
    ActionKind = Action::Kind::GeneratePCHJob;
    break;
  case FrontendOptions::ActionType::EmitIR:
  case FrontendOptions::ActionType::EmitBC:
  case FrontendOptions::ActionType::EmitAssembly:
  case FrontendOptions::ActionType::EmitObject: {
    // Whether or not these actions correspond to a "compile" job or a
    // "backend" job, depends on the input kind.
    if (Invocation.getFrontendOptions().InputsAndOutputs.shouldTreatAsLLVM())
      ActionKind = Action::Kind::BackendJob;
    else
      ActionKind = Action::Kind::CompileJob;
  } break;
  default:
    ActionKind = Action::Kind::CompileJob;
    break;
  }
  // The following Driver actions do not correspond to possible Frontend
  // invocations:
  // ModuleWrapJob, AutolinkExtractJob, GenerateDSYMJob, VerifyDebugInfoJob,
  // StaticLinkJob, DynamicLinkJob
  return ActionKind;
}

class Message {
  std::string Kind;
  std::string Name;

public:
  Message(StringRef Kind, StringRef Name) : Kind(Kind), Name(Name) {}
  virtual ~Message() = default;

  virtual void provideMapping(swift::json::Output &out) {
    out.mapRequired("kind", Kind);
    out.mapRequired("name", Name);
  }
};

class CommandBasedMessage : public Message {
public:
  CommandBasedMessage(StringRef Kind, const driver::Job &Cmd)
      : Message(Kind, Cmd.getSource().getClassName()) {}
};

class InvocationBasedMessage : public Message {
public:
  InvocationBasedMessage(StringRef Kind, const CompilerInvocation &Invocation)
      : Message(Kind, Action::getClassName(
                          mapFrontendInvocationToActionKind(Invocation))) {}
};

class DetailedCommandBasedMessage : public CommandBasedMessage {
  std::string Executable;
  SmallVector<std::string, 16> Arguments;
  std::string CommandLine;
  SmallVector<CommandInput, 4> Inputs;
  SmallVector<OutputPair, 8> Outputs;

public:
  DetailedCommandBasedMessage(StringRef Kind, const driver::Job &Cmd)
      : CommandBasedMessage(Kind, Cmd) {
    Executable = Cmd.getExecutable();
    for (const auto &A : Cmd.getArguments()) {
      Arguments.push_back(A);
    }
    llvm::raw_string_ostream wrapper(CommandLine);
    Cmd.printCommandLine(wrapper, "");
    wrapper.flush();

    for (const Action *A : Cmd.getSource().getInputs()) {
      if (const auto *IA = dyn_cast<InputAction>(A))
        Inputs.push_back(CommandInput(IA->getInputArg().getValue()));
    }

    for (const driver::Job *J : Cmd.getInputs()) {
      auto OutFiles = J->getOutput().getPrimaryOutputFilenames();
      if (const auto *BJAction = dyn_cast<BackendJobAction>(&Cmd.getSource())) {
        Inputs.push_back(CommandInput(OutFiles[BJAction->getInputIndex()]));
      } else {
        for (llvm::StringRef FileName : OutFiles) {
          Inputs.push_back(CommandInput(FileName));
        }
      }
    }

    // TODO: set up Outputs appropriately.
    file_types::ID PrimaryOutputType = Cmd.getOutput().getPrimaryOutputType();
    if (PrimaryOutputType != file_types::TY_Nothing) {
      for (llvm::StringRef OutputFileName :
           Cmd.getOutput().getPrimaryOutputFilenames()) {
        Outputs.push_back(OutputPair(PrimaryOutputType, OutputFileName.str()));
      }
    }
    file_types::forAllTypes([&](file_types::ID Ty) {
      for (auto Output : Cmd.getOutput().getAdditionalOutputsForType(Ty)) {
        Outputs.push_back(OutputPair(Ty, Output.str()));
      }
    });
  }

  void provideMapping(swift::json::Output &out) override {
    Message::provideMapping(out);
    out.mapRequired("command", CommandLine); // Deprecated, do not document
    out.mapRequired("command_executable", Executable);
    out.mapRequired("command_arguments", Arguments);
    out.mapOptional("inputs", Inputs);
    out.mapOptional("outputs", Outputs);
  }
};

class DetailedInvocationBasedMessage : public InvocationBasedMessage {
  ArrayRef<const char *> Args;
  std::string Executable;
  SmallVector<std::string, 16> Arguments;
  std::string CommandLine;
  SmallVector<CommandInput, 4> Inputs;
  SmallVector<OutputPair, 8> Outputs;

public:
  DetailedInvocationBasedMessage(StringRef Kind,
                                 const CompilerInvocation &Invocation,
                                 const InputFile &PrimaryInput,
                                 ArrayRef<const char *> Args)
      : InvocationBasedMessage(Kind, Invocation), Args(Args) {
    // Command line and arguments
    Executable = Invocation.getFrontendOptions().MainExecutablePath;
    CommandLine += Executable;
    for (const auto &A : Args) {
      Arguments.push_back(A);
      CommandLine += std::string(" ") + A;
    }

    // Primary Input only
    Inputs.push_back(CommandInput(PrimaryInput.getFileName()));

    // Output for this Primary
    auto OutputFile = PrimaryInput.outputFilename();
    Outputs.push_back(OutputPair(file_types::lookupTypeForExtension(
                                     llvm::sys::path::extension(OutputFile)),
                                 OutputFile));

    // Supplementary outputs
    const auto &primarySpecificFiles = PrimaryInput.getPrimarySpecificPaths();
    const auto &supplementaryOutputPaths =
        primarySpecificFiles.SupplementaryOutputs;
    supplementaryOutputPaths.forEachSetOutput([&](const std::string &output) {
      Outputs.push_back(OutputPair(file_types::lookupTypeForExtension(
                                       llvm::sys::path::extension(output)),
                                   output));
    });
  }

  void provideMapping(swift::json::Output &out) override {
    Message::provideMapping(out);
    out.mapRequired("command", CommandLine); // Deprecated, do not document
    out.mapRequired("command_executable", Executable);
    out.mapRequired("command_arguments", Arguments);
    out.mapOptional("inputs", Inputs);
    out.mapOptional("outputs", Outputs);
  }
};

class TaskBasedMessage : public CommandBasedMessage {
  int64_t Pid;

public:
  TaskBasedMessage(StringRef Kind, const driver::Job &Cmd, int64_t Pid)
      : CommandBasedMessage(Kind, Cmd), Pid(Pid) {}

  void provideMapping(swift::json::Output &out) override {
    CommandBasedMessage::provideMapping(out);
    out.mapRequired("pid", Pid);
  }
};

class BeganCommandMessage : public DetailedCommandBasedMessage {
  int64_t Pid;
  TaskProcessInformation ProcInfo;

public:
  BeganCommandMessage(const driver::Job &Cmd, int64_t Pid,
                      TaskProcessInformation ProcInfo)
      : DetailedCommandBasedMessage("began", Cmd), Pid(Pid),
        ProcInfo(ProcInfo) {}

  void provideMapping(swift::json::Output &out) override {
    DetailedCommandBasedMessage::provideMapping(out);
    out.mapRequired("pid", Pid);
    out.mapRequired("process", ProcInfo);
  }
};

class BeganInvocationMessage : public DetailedInvocationBasedMessage {
  int64_t Pid;
  TaskProcessInformation ProcInfo;

public:
  BeganInvocationMessage(const CompilerInvocation &Invocation,
                         const InputFile &PrimaryInput,
                         ArrayRef<const char *> Args, int64_t Pid,
                         TaskProcessInformation ProcInfo)
      : DetailedInvocationBasedMessage("began", Invocation, PrimaryInput, Args),
        Pid(Pid), ProcInfo(ProcInfo) {}

  void provideMapping(swift::json::Output &out) override {
    DetailedInvocationBasedMessage::provideMapping(out);
    out.mapRequired("pid", Pid);
    out.mapRequired("process", ProcInfo);
  }
};

class FinishedInvocationMessage : public InvocationBasedMessage {
  int64_t Pid;
  int ExitStatus;
  std::string Output;
  TaskProcessInformation ProcInfo;

public:
  FinishedInvocationMessage(const CompilerInvocation &Invocation, int64_t Pid,
                            StringRef Output, int ExitStatus,
                            TaskProcessInformation ProcInfo)
      : InvocationBasedMessage("finished", Invocation), Pid(Pid),
        ExitStatus(ExitStatus), Output(Output), ProcInfo(ProcInfo) {}
  void provideMapping(swift::json::Output &out) override {
    InvocationBasedMessage::provideMapping(out);
    out.mapOptional("output", Output, std::string());
    out.mapRequired("process", ProcInfo);
    out.mapRequired("pid", Pid);
    out.mapRequired("exit-status", ExitStatus);
  }
};

class TaskOutputMessage : public TaskBasedMessage {
  std::string Output;
  TaskProcessInformation ProcInfo;

public:
  TaskOutputMessage(StringRef Kind, const driver::Job &Cmd, int64_t Pid,
                    StringRef Output, TaskProcessInformation ProcInfo)
      : TaskBasedMessage(Kind, Cmd, Pid), Output(Output), ProcInfo(ProcInfo) {}

  void provideMapping(swift::json::Output &out) override {
    TaskBasedMessage::provideMapping(out);
    out.mapOptional("output", Output, std::string());
    out.mapRequired("process", ProcInfo);
  }
};

class FinishedCommandMessage : public TaskOutputMessage {
  int ExitStatus;

public:
  FinishedCommandMessage(const driver::Job &Cmd, int64_t Pid, StringRef Output,
                         TaskProcessInformation ProcInfo, int ExitStatus)
      : TaskOutputMessage("finished", Cmd, Pid, Output, ProcInfo),
        ExitStatus(ExitStatus) {}

  void provideMapping(swift::json::Output &out) override {
    TaskOutputMessage::provideMapping(out);
    out.mapRequired("exit-status", ExitStatus);
  }
};

class SignalledMessage : public TaskOutputMessage {
  std::string ErrorMsg;
  Optional<int> Signal;

public:
  SignalledMessage(const driver::Job &Cmd, int64_t Pid, StringRef Output,
                   StringRef ErrorMsg, Optional<int> Signal,
                   TaskProcessInformation ProcInfo)
      : TaskOutputMessage("signalled", Cmd, Pid, Output, ProcInfo),
        ErrorMsg(ErrorMsg), Signal(Signal) {}

  void provideMapping(swift::json::Output &out) override {
    TaskOutputMessage::provideMapping(out);
    out.mapOptional("error-message", ErrorMsg, std::string());
    out.mapOptional("signal", Signal);
  }
};

class SkippedMessage : public DetailedCommandBasedMessage {
public:
  SkippedMessage(const driver::Job &Cmd)
      : DetailedCommandBasedMessage("skipped", Cmd) {}
};

} // end anonymous namespace

namespace swift {
namespace json {

template <> struct ObjectTraits<Message> {
  static void mapping(Output &out, Message &msg) { msg.provideMapping(out); }
};

} // namespace json
} // namespace swift

static void emitMessage(raw_ostream &os, Message &msg) {
  std::string JSONString;
  llvm::raw_string_ostream BufferStream(JSONString);
  json::Output yout(BufferStream);
  yout << msg;
  BufferStream.flush();
  os << JSONString.length() << '\n';
  os << JSONString << '\n';
}

void parseable_output::emitBeganMessage(raw_ostream &os, const driver::Job &Cmd,
                                        int64_t Pid,
                                        TaskProcessInformation ProcInfo) {
  BeganCommandMessage msg(Cmd, Pid, ProcInfo);
  emitMessage(os, msg);
}

void parseable_output::emitBeganMessage(raw_ostream &os,
                                        const CompilerInvocation &Invocation,
                                        ArrayRef<const char *> Args,
                                        int64_t OSPid) {
  const auto &IO = Invocation.getFrontendOptions().InputsAndOutputs;
  const auto ProcInfo = sys::TaskProcessInformation(OSPid);

  // Parseable output clients may not understand the idea of a batch
  // compilation. We assign each primary in a batch job a quasi process id,
  // making sure it cannot collide with a real PID (always positive). Non-batch
  // compilation gets a real OS PID.
  int64_t Pid = IO.hasUniquePrimaryInput() ? OSPid : QUASI_PID_START;
  IO.forEachPrimaryInputWithIndex([&](const InputFile &Input,
                                      unsigned idx) -> bool {
    BeganInvocationMessage msg(Invocation, Input, Args, Pid - idx, ProcInfo);
    emitMessage(os, msg);
    return false;
  });
}

void parseable_output::emitFinishedMessage(raw_ostream &os,
                                           const driver::Job &Cmd, int64_t Pid,
                                           int ExitStatus, StringRef Output,
                                           TaskProcessInformation ProcInfo) {
  FinishedCommandMessage msg(Cmd, Pid, Output, ProcInfo, ExitStatus);
  emitMessage(os, msg);
}

void parseable_output::emitFinishedMessage(
    raw_ostream &os, const CompilerInvocation &Invocation, int ExitStatus,
    const llvm::StringMap<std::vector<std::string>> &FileSpecificDiagnostics,
    int64_t OSPid) {
  const auto &IO = Invocation.getFrontendOptions().InputsAndOutputs;
  const auto ProcInfo = sys::TaskProcessInformation(OSPid);

  // Parseable output clients may not understand the idea of a batch
  // compilation. We assign each primary in a batch job a quasi process id,
  // making sure it cannot collide with a real PID (always positive). Non-batch
  // compilation gets a real OS PID.
  int64_t Pid = IO.hasUniquePrimaryInput() ? OSPid : QUASI_PID_START;
  IO.forEachPrimaryInputWithIndex(
      [&](const InputFile &Input, unsigned idx) -> bool {
        assert(FileSpecificDiagnostics.count(Input.getFileName()) != 0 &&
               "Expected diagnostic collection for input.");

        // Join all diagnostics produced for this file into a single output.
        auto PrimaryDiags = FileSpecificDiagnostics.lookup(Input.getFileName());
        const char *const Delim = "\n";
        std::ostringstream JoinedDiags;
        std::copy(PrimaryDiags.begin(), PrimaryDiags.end(),
                  std::ostream_iterator<std::string>(JoinedDiags, Delim));
        FinishedInvocationMessage msg(Invocation, Pid - idx, JoinedDiags.str(),
                                      ExitStatus, ProcInfo);
        emitMessage(os, msg);
        return false;
      });
}

void parseable_output::emitSignalledMessage(
    raw_ostream &os, const driver::Job &Cmd, int64_t Pid, StringRef ErrorMsg,
    StringRef Output, Optional<int> Signal, TaskProcessInformation ProcInfo) {
  SignalledMessage msg(Cmd, Pid, Output, ErrorMsg, Signal, ProcInfo);
  emitMessage(os, msg);
}

void parseable_output::emitSkippedMessage(raw_ostream &os,
                                          const driver::Job &Cmd) {
  SkippedMessage msg(Cmd);
  emitMessage(os, msg);
}

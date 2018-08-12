//===--- ParseableOutput.cpp - Helpers for parseable output ---------------===//
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

#include "swift/Driver/ParseableOutput.h"

#include "swift/Basic/FileTypes.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/Basic/TaskQueue.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Job.h"
#include "llvm/Option/Arg.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift::driver::parseable_output;
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
  template<>
  struct ScalarTraits<CommandInput> {
    static void output(const CommandInput &value, llvm::raw_ostream &os) {
      os << value.Path;
    }
    static bool mustQuote(StringRef) { return true; }
  };

  template <> struct ScalarEnumerationTraits<file_types::ID> {
    static void enumeration(Output &out, file_types::ID &value) {
      file_types::forAllTypes([&](file_types::ID ty) {
        std::string typeName = file_types::getTypeName(ty);
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

  template<typename T, unsigned N>
  struct ArrayTraits<SmallVector<T, N>> {
    static size_t size(Output &out, SmallVector<T, N> &seq) {
      return seq.size();
    }

    static T &element(Output &out, SmallVector<T, N> &seq, size_t index) {
      if (index >= seq.size())
        seq.resize(index+1);
      return seq[index];
    }
  };
} // namespace json
} // namespace swift

namespace {

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
  CommandBasedMessage(StringRef Kind, const Job &Cmd) :
      Message(Kind, Cmd.getSource().getClassName()) {}
};

class DetailedCommandBasedMessage : public CommandBasedMessage {
  std::string Executable;
  SmallVector<std::string, 16> Arguments;
  std::string CommandLine;
  SmallVector<CommandInput, 4> Inputs;
  SmallVector<OutputPair, 8> Outputs;
public:
  DetailedCommandBasedMessage(StringRef Kind, const Job &Cmd) :
      CommandBasedMessage(Kind, Cmd) {
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

    for (const Job *J : Cmd.getInputs()) {
      auto OutFiles = J->getOutput().getPrimaryOutputFilenames();
      if (const auto *BJAction = dyn_cast<BackendJobAction>(&Cmd.getSource())) {
        Inputs.push_back(CommandInput(OutFiles[BJAction->getInputIndex()]));
      } else {
        for (const std::string &FileName : OutFiles) {
          Inputs.push_back(CommandInput(FileName));
        }
      }
    }

    // TODO: set up Outputs appropriately.
    file_types::ID PrimaryOutputType = Cmd.getOutput().getPrimaryOutputType();
    if (PrimaryOutputType != file_types::TY_Nothing) {
      for (const std::string &OutputFileName : Cmd.getOutput().
                                                 getPrimaryOutputFilenames()) {
        Outputs.push_back(OutputPair(PrimaryOutputType, OutputFileName));
      }
    }
    file_types::forAllTypes([&](file_types::ID Ty) {
      for (auto Output : Cmd.getOutput().getAdditionalOutputsForType(Ty)) {
        Outputs.push_back(OutputPair(Ty, Output));
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

class TaskBasedMessage : public CommandBasedMessage {
  int64_t Pid;
public:
  TaskBasedMessage(StringRef Kind, const Job &Cmd, int64_t Pid) :
      CommandBasedMessage(Kind, Cmd), Pid(Pid) {}

  void provideMapping(swift::json::Output &out) override {
    CommandBasedMessage::provideMapping(out);
    out.mapRequired("pid", Pid);
  }
};

class BeganMessage : public DetailedCommandBasedMessage {
  int64_t Pid;
  TaskProcessInformation ProcInfo;

public:
  BeganMessage(const Job &Cmd, int64_t Pid, TaskProcessInformation ProcInfo)
      : DetailedCommandBasedMessage("began", Cmd), Pid(Pid),
        ProcInfo(ProcInfo) {}

  void provideMapping(swift::json::Output &out) override {
    DetailedCommandBasedMessage::provideMapping(out);
    out.mapRequired("pid", Pid);
    out.mapRequired("process", ProcInfo);
  }
};

class TaskOutputMessage : public TaskBasedMessage {
  std::string Output;
  TaskProcessInformation ProcInfo;

public:
  TaskOutputMessage(StringRef Kind, const Job &Cmd, int64_t Pid,
                    StringRef Output, TaskProcessInformation ProcInfo)
      : TaskBasedMessage(Kind, Cmd, Pid), Output(Output), ProcInfo(ProcInfo) {}

  void provideMapping(swift::json::Output &out) override {
    TaskBasedMessage::provideMapping(out);
    out.mapOptional("output", Output, std::string());
    out.mapRequired("process", ProcInfo);
  }
};

class FinishedMessage : public TaskOutputMessage {
  int ExitStatus;
public:
  FinishedMessage(const Job &Cmd, int64_t Pid, StringRef Output,
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
  SignalledMessage(const Job &Cmd, int64_t Pid, StringRef Output,
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
  SkippedMessage(const Job &Cmd) :
      DetailedCommandBasedMessage("skipped", Cmd) {}
};

} // end anonymous namespace

namespace swift {
namespace json {

template<>
struct ObjectTraits<Message> {
  static void mapping(Output &out, Message &msg) {
    msg.provideMapping(out);
  }
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

void parseable_output::emitBeganMessage(raw_ostream &os, const Job &Cmd,
                                        int64_t Pid,
                                        TaskProcessInformation ProcInfo) {
  BeganMessage msg(Cmd, Pid, ProcInfo);
  emitMessage(os, msg);
}

void parseable_output::emitFinishedMessage(raw_ostream &os, const Job &Cmd,
                                           int64_t Pid, int ExitStatus,
                                           StringRef Output,
                                           TaskProcessInformation ProcInfo) {
  FinishedMessage msg(Cmd, Pid, Output, ProcInfo, ExitStatus);
  emitMessage(os, msg);
}

void parseable_output::emitSignalledMessage(raw_ostream &os, const Job &Cmd,
                                            int64_t Pid, StringRef ErrorMsg,
                                            StringRef Output,
                                            Optional<int> Signal,
                                            TaskProcessInformation ProcInfo) {
  SignalledMessage msg(Cmd, Pid, Output, ErrorMsg, Signal, ProcInfo);
  emitMessage(os, msg);
}

void parseable_output::emitSkippedMessage(raw_ostream &os, const Job &Cmd) {
  SkippedMessage msg(Cmd);
  emitMessage(os, msg);
}

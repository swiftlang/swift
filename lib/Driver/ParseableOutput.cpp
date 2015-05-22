//===--- ParseableOutput.cpp - Helpers for parseable output ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Driver/ParseableOutput.h"

#include "swift/Basic/JSONSerialization.h"
#include "swift/Driver/Action.h"
#include "swift/Driver/Job.h"
#include "swift/Driver/Types.h"
#include "llvm/Option/Arg.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift::driver::parseable_output;
using namespace swift::driver;
using namespace swift;

namespace {
  struct CommandInput {
    std::string Path;
    CommandInput() {}
    CommandInput(StringRef Path) : Path(Path) {}
  };

  typedef std::pair<types::ID, std::string> OutputPair;
}

namespace swift {
namespace json {
  template<>
  struct ScalarTraits<CommandInput> {
    static void output(const CommandInput &value, llvm::raw_ostream &os) {
      os << value.Path;
    }
    static bool mustQuote(StringRef) { return true; }
  };

  template<>
  struct ScalarEnumerationTraits<types::ID> {
    static void enumeration(Output &out, types::ID &value) {
      types::forAllTypes([&](types::ID ty) {
        std::string typeName = types::getTypeName(ty);
        out.enumCase(value, typeName.c_str(), ty);
      });
    }
  };

  template<>
  struct ObjectTraits<std::pair<types::ID, std::string>> {
    static void mapping(Output &out, std::pair<types::ID, std::string> &value) {
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
}
}

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
  std::string CommandLine;
  SmallVector<CommandInput, 4> Inputs;
  SmallVector<OutputPair, 8> Outputs;
public:
  DetailedCommandBasedMessage(StringRef Kind, const Job &Cmd) :
      CommandBasedMessage(Kind, Cmd) {
    llvm::raw_string_ostream wrapper(CommandLine);
    Cmd.printCommandLine(wrapper, "");
    wrapper.flush();

    for (const Action *A : Cmd.getSource().getInputs()) {
      if (const InputAction *IA = dyn_cast<InputAction>(A))
        Inputs.push_back(CommandInput(IA->getInputArg().getValue()));
    }

    for (const Job *J : Cmd.getInputs()) {
      ArrayRef<std::string> OutFiles = J->getOutput().getPrimaryOutputFilenames();
      if (const auto *BJAction = dyn_cast<BackendJobAction>(&Cmd.getSource())) {
        Inputs.push_back(CommandInput(OutFiles[BJAction->getInputIndex()]));
      } else {
        for (const std::string &FileName : OutFiles) {
          Inputs.push_back(CommandInput(FileName));
        }
      }
    }

    // TODO: set up Outputs appropriately.
    types::ID PrimaryOutputType = Cmd.getOutput().getPrimaryOutputType();
    if (PrimaryOutputType != types::TY_Nothing) {
      for (const std::string &OutputFileName : Cmd.getOutput().
                                                 getPrimaryOutputFilenames()) {
        Outputs.push_back(OutputPair(PrimaryOutputType, OutputFileName));
      }
    }
    types::forAllTypes([&](types::ID Ty) {
      const std::string &Output =
          Cmd.getOutput().getAdditionalOutputForType(Ty);
      if (!Output.empty())
        Outputs.push_back(OutputPair(Ty, Output));
    });
  }

  virtual void provideMapping(swift::json::Output &out) {
    Message::provideMapping(out);
    out.mapRequired("command", CommandLine);
    out.mapOptional("inputs", Inputs);
    out.mapOptional("outputs", Outputs);
  }
};

class TaskBasedMessage : public CommandBasedMessage {
  ProcessId Pid;
public:
  TaskBasedMessage(StringRef Kind, const Job &Cmd, ProcessId Pid) :
      CommandBasedMessage(Kind, Cmd), Pid(Pid) {}

  virtual void provideMapping(swift::json::Output &out) {
    CommandBasedMessage::provideMapping(out);
    out.mapRequired("pid", Pid);
  }
};

class BeganMessage : public DetailedCommandBasedMessage {
  ProcessId Pid;
public:
  BeganMessage(const Job &Cmd, ProcessId Pid) :
      DetailedCommandBasedMessage("began", Cmd), Pid(Pid) {}

  virtual void provideMapping(swift::json::Output &out) {
    DetailedCommandBasedMessage::provideMapping(out);
    out.mapRequired("pid", Pid);
  }
};

class TaskOutputMessage : public TaskBasedMessage {
  std::string Output;
public:
  TaskOutputMessage(StringRef Kind, const Job &Cmd, ProcessId Pid,
                    StringRef Output) : TaskBasedMessage(Kind, Cmd, Pid),
                                        Output(Output) {}

  virtual void provideMapping(swift::json::Output &out) {
    TaskBasedMessage::provideMapping(out);
    out.mapOptional("output", Output, std::string());
  }
};

class FinishedMessage : public TaskOutputMessage {
  int ExitStatus;
public:
  FinishedMessage(const Job &Cmd, ProcessId Pid, StringRef Output,
                  int ExitStatus) : TaskOutputMessage("finished", Cmd, Pid,
                                                      Output),
                                    ExitStatus(ExitStatus) {}

  virtual void provideMapping(swift::json::Output &out) {
    TaskOutputMessage::provideMapping(out);
    out.mapRequired("exit-status", ExitStatus);
  }
};

class SignalledMessage : public TaskOutputMessage {
  std::string ErrorMsg;
public:
  SignalledMessage(const Job &Cmd, ProcessId Pid, StringRef Output,
                   StringRef ErrorMsg) : TaskOutputMessage("signalled", Cmd,
                                                           Pid, Output),
                                         ErrorMsg(ErrorMsg) {}

  virtual void provideMapping(swift::json::Output &out) {
    TaskOutputMessage::provideMapping(out);
    out.mapOptional("error-message", ErrorMsg, std::string());
  }
};

class SkippedMessage : public DetailedCommandBasedMessage {
public:
  SkippedMessage(const Job &Cmd) :
      DetailedCommandBasedMessage("skipped", Cmd) {}
};

}

namespace swift {
namespace json {

template<>
struct ObjectTraits<Message> {
  static void mapping(Output &out, Message &msg) {
    msg.provideMapping(out);
  }
};

} // end namespace yaml
} // end namespace llvm

static void emitMessage(raw_ostream &os, Message &msg) {
  std::string JSONString;
  llvm::raw_string_ostream BufferStream(JSONString);
  json::Output yout(BufferStream);
  yout << msg;
  BufferStream.flush();
  os << JSONString.length() << '\n';
  os << JSONString << '\n';
}

void parseable_output::emitBeganMessage(raw_ostream &os,
                                        const Job &Cmd, ProcessId Pid) {
  BeganMessage msg(Cmd, Pid);
  emitMessage(os, msg);
}

void parseable_output::emitFinishedMessage(raw_ostream &os,
                                           const Job &Cmd, ProcessId Pid,
                                           int ExitStatus, StringRef Output) {
  FinishedMessage msg(Cmd, Pid, Output, ExitStatus);
  emitMessage(os, msg);
}

void parseable_output::emitSignalledMessage(raw_ostream &os,
                                            const Job &Cmd, ProcessId Pid,
                                            StringRef ErrorMsg,
                                            StringRef Output) {
  SignalledMessage msg(Cmd, Pid, Output, ErrorMsg);
  emitMessage(os, msg);
}

void parseable_output::emitSkippedMessage(raw_ostream &os, const Job &Cmd) {
  SkippedMessage msg(Cmd);
  emitMessage(os, msg);
}

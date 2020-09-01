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

#include "swift/Frontend/ParseableOutput.h"

#include "swift/Basic/FileTypes.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/Basic/TaskQueue.h"
#include "llvm/Option/Arg.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift::parseable_output;
using namespace swift::sys;
using namespace swift;

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
  CommandBasedMessage(StringRef Kind, StringRef Name) :
      Message(Kind, Name) {}
};

class DetailedCommandBasedMessage : public CommandBasedMessage {
  JobInfo Info;
public:
  DetailedCommandBasedMessage(
      StringRef kind, StringRef name, const JobInfo &info
  ) : CommandBasedMessage(kind, name), Info(info) { }

  void provideMapping(swift::json::Output &out) override {
    Message::provideMapping(out);
    out.mapRequired("command", Info.CommandLine); // Deprecated, do not document
    out.mapRequired("command_executable", Info.Executable);
    out.mapRequired("command_arguments", Info.Arguments);
    out.mapOptional("inputs", Info.Inputs);
    out.mapOptional("outputs", Info.Outputs);
  }
};

class TaskBasedMessage : public CommandBasedMessage {
  int64_t Pid;
public:
  TaskBasedMessage(
      StringRef kind, StringRef name, const JobInfo &info, int64_t pid
  ) : CommandBasedMessage(kind, name), Pid(pid) { }

  void provideMapping(swift::json::Output &out) override {
    CommandBasedMessage::provideMapping(out);
    out.mapRequired("pid", Pid);
  }
};

class BeganMessage : public DetailedCommandBasedMessage {
  int64_t Pid;
  TaskProcessInformation ProcInfo;

public:
  BeganMessage(
      StringRef name, const JobInfo &info, int64_t pid,
      TaskProcessInformation procInfo
  ) : DetailedCommandBasedMessage("began", name, info), Pid(pid),
      ProcInfo(procInfo) {}

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
  TaskOutputMessage(
      StringRef kind, StringRef name, const JobInfo &info, int64_t pid,
      StringRef output, TaskProcessInformation procInfo
  ) : TaskBasedMessage(kind, name, info, pid), Output(output),
      ProcInfo(procInfo) {}

  void provideMapping(swift::json::Output &out) override {
    TaskBasedMessage::provideMapping(out);
    out.mapOptional("output", Output, std::string());
    out.mapRequired("process", ProcInfo);
  }
};

class FinishedMessage : public TaskOutputMessage {
  int ExitStatus;
public:
  FinishedMessage(
      StringRef name, const JobInfo &info, int64_t pid, StringRef output,
      TaskProcessInformation procInfo, int exitStatus
  ) : TaskOutputMessage("finished", name, info, pid, output, procInfo),
      ExitStatus(exitStatus) {}

  void provideMapping(swift::json::Output &out) override {
    TaskOutputMessage::provideMapping(out);
    out.mapRequired("exit-status", ExitStatus);
  }
};

class SignalledMessage : public TaskOutputMessage {
  std::string ErrorMsg;
  Optional<int> Signal;
public:
  SignalledMessage(
      StringRef name, const JobInfo &info, int64_t pid, StringRef output,
      StringRef errorMsg, Optional<int> signal,
      TaskProcessInformation procInfo
  ) : TaskOutputMessage("signalled", name, info, pid, output, procInfo),
      ErrorMsg(errorMsg), Signal(signal) {}

  void provideMapping(swift::json::Output &out) override {
    TaskOutputMessage::provideMapping(out);
    out.mapOptional("error-message", ErrorMsg, std::string());
    out.mapOptional("signal", Signal);
  }
};

class SkippedMessage : public DetailedCommandBasedMessage {
public:
  SkippedMessage(StringRef name, const JobInfo &info) :
      DetailedCommandBasedMessage("skipped", name, info) {}
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

void parseable_output::emitBeganMessage(
    raw_ostream &os, StringRef name, const JobInfo &info, int64_t pid,
    TaskProcessInformation procInfo) {
  BeganMessage msg(name, info, pid, procInfo);
  emitMessage(os, msg);
}

void parseable_output::emitFinishedMessage(
    raw_ostream &os, StringRef name, const JobInfo &info, int64_t pid,
    int exitStatus, StringRef output, TaskProcessInformation procInfo) {
  FinishedMessage msg(name, info, pid, output, procInfo, exitStatus);
  emitMessage(os, msg);
}

void parseable_output::emitSignalledMessage(
    raw_ostream &os, StringRef name, const JobInfo &info, int64_t pid,
    StringRef errorMsg, StringRef output, Optional<int> signal,
    TaskProcessInformation procInfo) {
  SignalledMessage msg(name, info, pid, output, errorMsg, signal, procInfo);
  emitMessage(os, msg);
}

void parseable_output::emitSkippedMessage(
    raw_ostream &os, StringRef name, const JobInfo &info) {
  SkippedMessage msg(name, info);
  emitMessage(os, msg);
}

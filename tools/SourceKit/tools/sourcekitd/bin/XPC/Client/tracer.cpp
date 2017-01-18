//===--- tracer.cpp -------------------------------------------------------===//
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

#include "sourcekitd/Internal-XPC.h"
#include "sourcekitd/XpcTracing.h"

#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/Concurrency.h"

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"

#include <xpc/xpc.h>

#include <algorithm>
#include <chrono>
#include <deque>
#include <functional>
#include <iomanip>
#include <map>
#include <set>

using namespace llvm;
using namespace sourcekitd;



//===----------------------------------------------------------------------===//
// Generic
//===----------------------------------------------------------------------===//

static std::string trace_root_dir;

bool isTracingEnabled() {
  return !trace_root_dir.empty();
}

typedef SmallString<256> path_t;
static llvm::sys::Mutex FileOpMutex;

static void fsEnsureDirExists(const char *Dir) {
  llvm::sys::ScopedLock L(FileOpMutex);
  if (!sys::fs::exists(Dir)) {
    sys::fs::create_directory(Dir);
  }
}

static void fsWriteFile(const char *Path, StringRef Text) {
  llvm::sys::ScopedLock L(FileOpMutex);
  if (!sys::fs::exists(Path)) {
    std::error_code EC;
    raw_fd_ostream Outfile(Path, EC, sys::fs::F_Text);
    Outfile << Text;
    Outfile.close();
  }
}

static void fsInitTraceRoot(path_t &RootDir, uint64_t Id) {
  RootDir = trace_root_dir;
  std::string DirName =
    std::to_string(std::chrono::system_clock::now().time_since_epoch().count());
  std::for_each(DirName.begin(), DirName.end(),
                [] (char &C) { if (!isalnum(C)) C = '-'; });
  DirName += '-';
  DirName += std::to_string(Id);
  sys::path::append(RootDir, DirName);
}

static void fsAddFileWithRevision(path_t &Path,
                                  const std::string &File,
                                  uint64_t Id) {
  sys::path::append(Path,
                    sys::path::stem(File) +
                    "." +
                    std::to_string(Id) +
                    sys::path::extension(File));
}

typedef SourceKit::trace::OperationKind OperationKind;

struct OperationInfo {
  std::chrono::system_clock::time_point StartedAt;
  OperationKind Kind;
  std::string SwiftArgs;
  trace::StringPairs OpArgs;
  trace::StringPairs Files;

  OperationInfo(OperationKind K,
                std::string &&SwiftArgs,
                trace::StringPairs &&Files,
                trace::StringPairs &&OpArgs)
    : StartedAt(std::chrono::system_clock::now()),
      Kind(K), SwiftArgs(SwiftArgs), OpArgs(OpArgs), Files(Files) {}

  OperationInfo() = default;
  OperationInfo(OperationInfo &&) = default;
  OperationInfo &operator=(OperationInfo &&) = default;

  OperationInfo(const OperationInfo &) = delete;
  OperationInfo &operator=(const OperationInfo &) = delete;
};

struct OpRec {
  std::string StartedAt;
  std::string OpName;
  std::string SwiftArgs;
  trace::StringPairs OpArgs;
  trace::StringPairs FileMap;
};

template <typename U>
struct llvm::yaml::SequenceTraits<std::vector<U>> {
  static size_t size(IO &Io, std::vector<U> &Vec) {
    return Vec.size();
  }
  static U &element(IO &Io, std::vector<U> &Vec, size_t Index) {
    return Vec[Index];
  }
};

template <>
struct llvm::yaml::MappingTraits<OpRec> {
  static void mapping(IO &Io, OpRec &Rec) {
    Io.mapRequired("op", Rec.OpName);
    Io.mapRequired("swift-args", Rec.SwiftArgs);
    Io.mapRequired("op-args", Rec.OpArgs);
    Io.mapRequired("file-map", Rec.FileMap);
  }
};

template <typename U, typename V>
struct llvm::yaml::MappingTraits<std::pair<U, V>> {
  static void mapping(IO &Io, std::pair<U, V> &Pair) {
    Io.mapRequired("first", Pair.first);
    Io.mapRequired("second", Pair.second);
  }
};



//===----------------------------------------------------------------------===//
// State
//===----------------------------------------------------------------------===//

class State {
  typedef std::map<uint64_t, OperationInfo> OperationsType;

  static sys::Mutex GlobalMutex;
  sys::Mutex LocalMutex;

  static std::shared_ptr<State> CurrentState;
  static SourceKit::WorkQueue Queue;
  static std::string CompArgsFile;
  static std::string ActiveOpFile;

  const uint64_t Id;
  std::atomic<uint64_t> UniqueId;
  std::string TraceDir;

  std::set<uint64_t> ReportedOps;
  std::set<uint64_t> SeenOps;
  OperationsType Operations;

  State(uint64_t Id) : Id(Id), UniqueId(0) {
  }

  void persist();

  static std::shared_ptr<State> getState(uint64_t Session) {
    llvm::sys::ScopedLock L(GlobalMutex);

    if (!CurrentState) {
      CurrentState.reset(new State(Session));
    } else if (!CurrentState || CurrentState->Id != Session) {
      auto S = CurrentState;
      Queue.dispatch([S] {S->persist();});
      CurrentState.reset(new State(Session));
    }
    return CurrentState;
  }

public:
  State(const State &) = delete;
  State &operator=(const State &) = delete;
  State(State &&) = delete;
  State &operator=(State &&) = delete;

  static void addOperation(uint64_t Session,
                           uint64_t OpId,
                           OperationKind Kind,
                           std::string &&SwiftArgs,
                           trace::StringPairs &&Files,
                           trace::StringPairs &&OpArgs) {
    auto S = getState(Session);
    llvm::sys::ScopedLock L(S->LocalMutex);
    assert(S->SeenOps.find(OpId) == S->SeenOps.end());
    S->Operations[OpId] = OperationInfo(
      Kind, std::move(SwiftArgs), std::move(Files), std::move(OpArgs));
    S->SeenOps.insert(OpId);
  }

  static void removeOperation(uint64_t Session,
                              uint64_t OpId) {
    auto S = getState(Session);
    llvm::sys::ScopedLock L(S->LocalMutex);
    assert(S->Operations.find(OpId) != S->Operations.end());
    S->Operations.erase(OpId);
  }

  static void persistAsync() {
    llvm::sys::ScopedLock L(GlobalMutex);
    auto S = CurrentState;
    Queue.dispatch([S] {S->persist();});
  }
};

std::shared_ptr<State> State::CurrentState;
sys::Mutex State::GlobalMutex;
std::string State::CompArgsFile("swift-args.txt");
std::string State::ActiveOpFile("active-operation.txt");

SourceKit::WorkQueue State::Queue {
  SourceKit::WorkQueue::Dequeuing::Serial, "sourcekit.swift.tracer.state"
};

static std::map<OperationKind, std::string> op_kind_names {
  std::make_pair(OperationKind::SimpleParse, "SimpleParse"),
  std::make_pair(OperationKind::PerformSema, "PerformSema"),
  std::make_pair(OperationKind::AnnotAndDiag, "AnnotAndDiag"),
  std::make_pair(OperationKind::OpenInterface, "OpenInterface"),
  std::make_pair(OperationKind::ReadDiagnostics, "ReadDiagnostics"),
  std::make_pair(OperationKind::ReadSemanticInfo, "ReadSemanticInfo"),
  std::make_pair(OperationKind::ReadSyntaxInfo, "ReadSyntaxInfo"),
  std::make_pair(OperationKind::IndexModule, "IndexModule"),
  std::make_pair(OperationKind::IndexSource, "IndexSource"),
  std::make_pair(OperationKind::CursorInfoForIFaceGen, "CursorInfoForIFaceGen"),
  std::make_pair(OperationKind::CursorInfoForSource, "CursorInfoForSource"),
  std::make_pair(OperationKind::RelatedIdents, "RelatedIdents"),
  std::make_pair(OperationKind::FormatText, "FormatText"),
  std::make_pair(OperationKind::ExpandPlaceholder, "ExpandPlaceholder"),
  std::make_pair(OperationKind::CodeCompletion, "CodeCompletion"),
  std::make_pair(OperationKind::CodeCompletionInit, "CodeCompletionInit"),
};

void State::persist() {
  llvm::sys::ScopedLock L(LocalMutex);

  if (TraceDir.empty()) {
    path_t RootDir;
    fsInitTraceRoot(RootDir, Id);
    TraceDir = RootDir.c_str();
  }
  fsEnsureDirExists(TraceDir.c_str());

  path_t FilePath;

  std::for_each(
    Operations.begin(), Operations.end(),
    [&] (OperationsType::value_type &Pair) {
      if (ReportedOps.find(Pair.first) == ReportedOps.end()) {
        ReportedOps.insert(Pair.first);

        auto &Op = Pair.second;

        OpRec Rec;
        Rec.OpName = op_kind_names[Op.Kind];
        Rec.OpArgs = Op.OpArgs;

        // Dump Swift arguments
        FilePath = TraceDir;
        fsAddFileWithRevision(FilePath, CompArgsFile, ++UniqueId);
        Rec.SwiftArgs = FilePath.c_str();
        fsWriteFile(FilePath.c_str(), Op.SwiftArgs);

        // Dump files
        std::for_each(
          Op.Files.begin(), Op.Files.end(),
          [&] (trace::StringPairs::value_type &Pair) {
            FilePath = TraceDir;
            fsAddFileWithRevision(FilePath, Pair.first, ++UniqueId);
            Rec.FileMap.push_back(std::make_pair(Pair.first, FilePath.c_str()));
            fsWriteFile(FilePath.c_str(), Pair.second);
          });

        // Serialize operation info
        FilePath = TraceDir;
        fsAddFileWithRevision(FilePath, ActiveOpFile, Pair.first);
        std::error_code EC;
        raw_fd_ostream Outfile(FilePath, EC, sys::fs::F_Text);
        llvm::yaml::Output YamlOutput(Outfile);
        YamlOutput << Rec;
      }
    });
}



//===----------------------------------------------------------------------===//
// Init & trace
//===----------------------------------------------------------------------===//

void initializeTracing() {
  const char *EnvOpt = ::getenv("SOURCEKIT_TRACE_ROOT");
  if (EnvOpt) {
    using namespace SourceKit;
    LOG_WARN_FUNC("TRACE: Enabled with root: " << EnvOpt);
    trace_root_dir = EnvOpt;
  }
}

static uint32_t readUint64(xpc_object_t Arr, size_t Index) {
  return xpc_array_get_uint64(Arr, Index);
}

static trace::ActionKind readAction(xpc_object_t Arr, size_t Index) {
  return static_cast<trace::ActionKind>(readUint64(Arr, Index));
}

static trace::OperationKind readOpKind(xpc_object_t Arr, size_t Index) {
  return static_cast<trace::OperationKind>(readUint64(Arr, Index));
}

static llvm::StringRef readString(xpc_object_t Arr, size_t Index) {
  return xpc_array_get_string(Arr, Index);
}

static trace::StringPairs readStringPairs(xpc_object_t Arr, size_t &Index) {
  trace::StringPairs Files;
  auto Count = readUint64(Arr, Index++);
  for (uint64_t I = 0; I < Count; I++) {
    auto FileName = readString(Arr, Index++);
    auto Text = readString(Arr, Index++);
    Files.push_back(std::make_pair(FileName, Text));
  }
  return Files;
}

void handleTraceMessageRequest(uint64_t Session, xpc_object_t Msg) {
  if (isTracingEnabled()) {
    if (xpc_get_type(Msg) == XPC_TYPE_ARRAY) {
      switch (readAction(Msg, 0)) {

      case trace::ActionKind::OperationStarted: {
        size_t Index = 4;
        auto Files = readStringPairs(Msg, Index);
        auto OpArgs = readStringPairs(Msg, Index);
        State::addOperation(Session,
                            readUint64(Msg, 1),
                            readOpKind(Msg, 2),
                            readString(Msg, 3),
                            std::move(Files),
                            std::move(OpArgs));
        break;
      }
        
      case trace::ActionKind::OperationFinished: {
        State::removeOperation(Session, readUint64(Msg, 1));
        break;
      }
          
      }

    } else {
      llvm::report_fatal_error("Unknown trace message");
    }
  }
}

void persistTracingData() {
  if (isTracingEnabled()) {
    State::persistAsync();
  }
}


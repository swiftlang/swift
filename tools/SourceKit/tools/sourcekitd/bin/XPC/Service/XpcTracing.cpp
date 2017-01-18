//===--- XpcTracing.cpp ---------------------------------------------------===//
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

#include "sourcekitd/XpcTracing.h"

#include "swift/Frontend/Frontend.h"

#include "llvm/Support/YAMLTraits.h"

#include <chrono>
#include <xpc/xpc.h>

using namespace sourcekitd;
using namespace sourcekitd::trace;
using namespace llvm;



//===----------------------------------------------------------------------===//
// General
//===----------------------------------------------------------------------===//

static auto tracing_session = std::chrono::system_clock::now();

uint64_t trace::getTracingSession() {
  using namespace std::chrono;
  time_point<system_clock, milliseconds> msec =
    time_point_cast<milliseconds>(tracing_session);
  return msec.time_since_epoch().count();
}

static void append(xpc_object_t Contents, uint64_t Value) {
  xpc_array_set_uint64(Contents, XPC_ARRAY_APPEND, Value);
}

static void append(xpc_object_t Contents, ActionKind Value) {
  append(Contents, static_cast<uint64_t>(Value));
}

static void append(xpc_object_t Contents, OperationKind Value) {
  append(Contents, static_cast<uint64_t>(Value));
}

static void append(xpc_object_t Contents, llvm::StringRef Value) {
  xpc_array_set_string(Contents, XPC_ARRAY_APPEND, Value.data());
}

static void append(xpc_object_t Contents, const StringPairs &Files) {
  append(Contents, Files.size());
  std::for_each(Files.begin(), Files.end(),
                [&] (const std::pair<std::string, std::string> &File) {
                  append(Contents, File.first);
                  append(Contents, File.second);
                });
}

template <typename U>
struct llvm::yaml::SequenceTraits<std::vector<U>> {
  static size_t size(IO &Io, std::vector<U> &Vector) {
    return Vector.size();
  }
  static U &element(IO &Io, std::vector<U> &Vector, size_t Index) {
    return Vector[Index];
  }
};

template <>
struct llvm::yaml::MappingTraits<SwiftArguments> {
  static void mapping(IO &Io, SwiftArguments &Args) {

    Io.mapOptional("PrimaryFile", Args.PrimaryFile, std::string());
    Io.mapRequired("CompilerArgs", Args.Args);
  }
};

static std::string serializeCompilerArguments(const SwiftArguments &Args) {
  // Serialize compiler instance
  std::string OptionsAsYaml;
  llvm::raw_string_ostream OptionsStream(OptionsAsYaml);
  llvm::yaml::Output YamlOutput(OptionsStream);
  YamlOutput << const_cast<SwiftArguments &>(Args);
  OptionsStream.flush();
  return OptionsAsYaml;
}



//===----------------------------------------------------------------------===//
// Trace consumer
//===----------------------------------------------------------------------===//

class XpcTraceConsumer : public SourceKit::trace::TraceConsumer {
public:
  virtual ~XpcTraceConsumer() = default;

  // Operation previously started with startXXX has finished
  virtual void operationFinished(uint64_t OpId) override;

  // Trace start of SourceKit operation
  virtual void operationStarted(uint64_t OpId, OperationKind OpKind,
                                const SwiftInvocation &Inv,
                                const StringPairs &OpArgs) override;
};

// Trace start of SourceKit operation
void XpcTraceConsumer::operationStarted(uint64_t OpId,
                                        OperationKind OpKind,
                                        const SwiftInvocation &Inv,
                                        const StringPairs &OpArgs) {
  xpc_object_t Contents = xpc_array_create(nullptr, 0);
  append(Contents, ActionKind::OperationStarted);
  append(Contents, OpId);
  append(Contents, OpKind);
  append(Contents, serializeCompilerArguments(Inv.Args));
  append(Contents, Inv.Files);
  append(Contents, OpArgs);
  trace::sendTraceMessage(Contents);
}

// Operation previously started with startXXX has finished
void XpcTraceConsumer::operationFinished(uint64_t OpId) {
  xpc_object_t Contents = xpc_array_create(nullptr, 0);
  append(Contents, trace::ActionKind::OperationFinished);
  append(Contents, OpId);
  trace::sendTraceMessage(Contents);
}

static XpcTraceConsumer Instance;

void trace::initialize() {
  SourceKit::trace::registerConsumer(&Instance);
}


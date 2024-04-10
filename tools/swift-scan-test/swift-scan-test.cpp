//===--- swift-scan-test.cpp - Test libSwiftScan Dylib --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A simple program to test libSwiftScan interfaces.
//
//===----------------------------------------------------------------------===//

#include "swift-c/DependencyScan/DependencyScan.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FileTypes.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/StringSaver.h"
#include "llvm/Support/ThreadPool.h"

using namespace llvm;

namespace {
enum Actions {
  compute_cache_key,
  compute_cache_key_from_index,
  cache_query,
  replay_result,
};

llvm::cl::OptionCategory Category("swift-scan-test Options");
llvm::cl::opt<std::string> CASPath("cas-path", llvm::cl::desc("<path>"),
                                   llvm::cl::cat(Category));
llvm::cl::opt<std::string> CASID("id", llvm::cl::desc("<casid>"),
                                 llvm::cl::cat(Category));
llvm::cl::opt<std::string> Input("input", llvm::cl::desc("<file|index>"),
                                 llvm::cl::cat(Category));
llvm::cl::opt<unsigned> Threads("threads",
                                llvm::cl::desc("<number of threads>"),
                                llvm::cl::cat(Category), cl::init(1));
llvm::cl::opt<Actions>
    Action("action", llvm::cl::desc("<action>"),
           llvm::cl::values(clEnumVal(compute_cache_key, "compute cache key"),
                            clEnumVal(compute_cache_key_from_index,
                                      "compute cache key from index"),
                            clEnumVal(cache_query, "cache query"),
                            clEnumVal(replay_result, "replay result")),
           llvm::cl::cat(Category));
llvm::cl::list<std::string>
    SwiftCommands(llvm::cl::Positional, llvm::cl::desc("<swift-frontend args>"),
                  llvm::cl::cat(Category));

} // namespace

static StringRef toString(swiftscan_string_ref_t str) {
  return StringRef((const char *)str.data, str.length);
}

static int printError(swiftscan_string_ref_t err) {
  llvm::errs() << toString(err) << "\n";
  swiftscan_string_dispose(err);
  return EXIT_FAILURE;
}

static int action_compute_cache_key(swiftscan_cas_t cas, StringRef input,
                                    std::vector<const char *> &Args) {
  if (input.empty()) {
    llvm::errs() << "-input is not specified for compute_cache_key\n";
    return EXIT_FAILURE;
  }

  swiftscan_string_ref_t err_msg;
  auto key = swiftscan_cache_compute_key(cas, Args.size(), Args.data(),
                                         input.str().c_str(), &err_msg);
  if (key.length == 0)
    return printError(err_msg);

  llvm::outs() << toString(key) << "\n";
  swiftscan_string_dispose(key);

  return EXIT_SUCCESS;
}

static int
action_compute_cache_key_from_index(swiftscan_cas_t cas, StringRef index,
                                    std::vector<const char *> &Args) {
  unsigned inputIndex = 0;
  if (!to_integer(index, inputIndex)) {
    llvm::errs() << "-input is not a number for compute_cache_key_from_index\n";
    return EXIT_FAILURE;
  }

  swiftscan_string_ref_t err_msg;
  auto key = swiftscan_cache_compute_key_from_input_index(
      cas, Args.size(), Args.data(), inputIndex, &err_msg);
  if (key.length == 0)
    return printError(err_msg);

  llvm::outs() << toString(key) << "\n";
  swiftscan_string_dispose(key);

  return EXIT_SUCCESS;
}

static int print_cached_compilation(swiftscan_cached_compilation_t comp,
                                    const char *key) {
  auto numOutput = swiftscan_cached_compilation_get_num_outputs(comp);
  llvm::outs() << "Cached Compilation for key \"" << key << "\" has "
               << numOutput << " outputs: \n";

  for (unsigned i = 0; i < numOutput; ++i) {
    swiftscan_cached_output_t out =
        swiftscan_cached_compilation_get_output(comp, i);
    swiftscan_string_ref_t id = swiftscan_cached_output_get_casid(out);
    swiftscan_string_ref_t kind = swiftscan_cached_output_get_name(out);
    SWIFT_DEFER { swiftscan_string_dispose(kind); };
    llvm::outs() << toString(kind) << ": " << toString(id) << "\n";
    swiftscan_string_dispose(id);
  }
  llvm::outs() << "\n";
  return EXIT_SUCCESS;
}

static int action_cache_query(swiftscan_cas_t cas, const char *key) {
  swiftscan_string_ref_t err_msg;
  auto comp = swiftscan_cache_query(cas, key, /*globally=*/false, &err_msg);
  if (err_msg.length != 0)
    return printError(err_msg);

  if (!comp) {
    llvm::errs() << "cached output not found for \"" << key << "\"\n";
    return EXIT_FAILURE;
  }

  SWIFT_DEFER { swiftscan_cached_compilation_dispose(comp); };
  return print_cached_compilation(comp, key);
}

static int action_replay_result(swiftscan_cas_t cas, const char *key,
                                std::vector<const char *> &Args) {
  swiftscan_string_ref_t err_msg;
  auto comp = swiftscan_cache_query(cas, key, /*globally=*/false, &err_msg);
  if (!comp)
    return printError(err_msg);

  SWIFT_DEFER { swiftscan_cached_compilation_dispose(comp); };
  auto numOutput = swiftscan_cached_compilation_get_num_outputs(comp);
  for (unsigned i = 0; i < numOutput; ++i) {
    auto output = swiftscan_cached_compilation_get_output(comp, i);
    SWIFT_DEFER { swiftscan_cached_output_dispose(output); };

    if (swiftscan_cached_output_is_materialized(output))
      continue;

    auto load = swiftscan_cached_output_load(output, &err_msg);
    if (err_msg.length != 0)
      return printError(err_msg);

    if (!load) {
      llvm::errs() << "output at index " << i
                   << " cannot be loaded for key: " << key << "\n";
      return EXIT_FAILURE;
    }
  }

  auto instance = swiftscan_cache_replay_instance_create(Args.size(),
                                                         Args.data(), &err_msg);
  if (!instance)
    return printError(err_msg);
  SWIFT_DEFER { swiftscan_cache_replay_instance_dispose(instance); };

  auto result = swiftscan_cache_replay_compilation(instance, comp, &err_msg);
  if (err_msg.length != 0)
    return printError(err_msg);

  SWIFT_DEFER { swiftscan_cache_replay_result_dispose(result); };

  llvm::outs() << toString(swiftscan_cache_replay_result_get_stdout(result));
  llvm::errs() << toString(swiftscan_cache_replay_result_get_stderr(result));
  return EXIT_SUCCESS;
}

static std::vector<const char *> createArgs(ArrayRef<std::string> Cmd,
                                            StringSaver &Saver) {
  if (!Cmd.empty() && StringRef(Cmd.front()).ends_with("swift-frontend"))
    Cmd = Cmd.drop_front();

  std::vector<const char *> Args;
  for (auto A : Cmd) {
    StringRef Arg = Saver.save(A);
    Args.push_back(Arg.data());
  }

  return Args;
}

int main(int argc, char *argv[]) {
  llvm::cl::HideUnrelatedOptions(Category);
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "Test libSwiftScan interfaces\n");

  // Create CAS.
  auto option = swiftscan_cas_options_create();
  SWIFT_DEFER { swiftscan_cas_options_dispose(option); };
  swiftscan_cas_options_set_ondisk_path(option, CASPath.c_str());

  swiftscan_string_ref_t err_msg;
  auto cas = swiftscan_cas_create_from_options(option, &err_msg);
  if (!cas)
    return printError(err_msg);
  SWIFT_DEFER { swiftscan_cas_dispose(cas); };

  // Convert commands.
  llvm::BumpPtrAllocator Alloc;
  llvm::StringSaver Saver(Alloc);
  auto Args = createArgs(SwiftCommands, Saver);

  std::atomic<int> Ret = 0;
  llvm::ThreadPool Pool(llvm::hardware_concurrency(Threads));
  for (unsigned i = 0; i < Threads; ++i) {
    Pool.async([&]() {
      switch (Action) {
      case compute_cache_key:
        Ret += action_compute_cache_key(cas, Input, Args);
        break;
      case compute_cache_key_from_index:
        Ret += action_compute_cache_key_from_index(cas, Input, Args);
        break;
      case cache_query:
        Ret += action_cache_query(cas, CASID.c_str());
        break;
      case replay_result:
        Ret += action_replay_result(cas, CASID.c_str(), Args);
        break;
      }
    });
  }
  Pool.wait();

  return Ret;
}

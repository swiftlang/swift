//===--- swift_parse_test_main.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A utility tool to measure the parser performance.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Bridging/ASTGen.h"
#include "swift/Parse/Parser.h"
#include "llvm/Support/CommandLine.h"

#include <chrono>
#include <ctime>

using namespace swift;

namespace {

enum class Executor {
  SwiftParser,
  LibParse,
};

enum class ExecuteOptionFlag {
  /// Enable body skipping
  SkipBodies = 1 << 0,
};
using ExecuteOptions = OptionSet<ExecuteOptionFlag>;

struct SwiftParseTestOptions {
  llvm::cl::list<Executor> Executors = llvm::cl::list<Executor>(
      llvm::cl::desc("Available parsers"),
      llvm::cl::values(
          clEnumValN(Executor::SwiftParser, "swift-parser", "SwiftParser"),
          clEnumValN(Executor::LibParse, "lib-parse", "libParse")));

  llvm::cl::opt<unsigned int> Iteration = llvm::cl::opt<unsigned int>(
      "n", llvm::cl::desc("iteration"), llvm::cl::init(1));

  llvm::cl::opt<bool> SkipBodies = llvm::cl::opt<bool>(
      "skip-bodies",
      llvm::cl::desc("skip function bodies and type members if possible"));

  llvm::cl::list<std::string> InputPaths = llvm::cl::list<std::string>(
      llvm::cl::Positional, llvm::cl::desc("input paths"));
};

struct LibParseExecutor {
  constexpr static StringRef name = "libParse";

  static void performParse(llvm::MemoryBufferRef buffer, ExecuteOptions opts) {
    SourceManager SM;
    unsigned bufferID =
        SM.addNewSourceBuffer(llvm::MemoryBuffer::getMemBuffer(buffer));
    DiagnosticEngine diagEngine(SM);
    LangOptions langOpts;
    TypeCheckerOptions typeckOpts;
    SILOptions silOpts;
    SearchPathOptions searchPathOpts;
    ClangImporterOptions clangOpts;
    symbolgraphgen::SymbolGraphOptions symbolOpts;
    std::unique_ptr<ASTContext> ctx(
        ASTContext::get(langOpts, typeckOpts, silOpts, searchPathOpts,
                        clangOpts, symbolOpts, SM, diagEngine));

    SourceFile::ParsingOptions parseOpts;
    parseOpts |= SourceFile::ParsingFlags::DisablePoundIfEvaluation;
    if (!opts.contains(ExecuteOptionFlag::SkipBodies))
      parseOpts |= SourceFile::ParsingFlags::DisableDelayedBodies;

    ModuleDecl *M = ModuleDecl::create(Identifier(), *ctx);
    SourceFile *SF =
        new (*ctx) SourceFile(*M, SourceFileKind::Library, bufferID, parseOpts);

    Parser parser(bufferID, *SF, /*SILParserState=*/nullptr);
    SmallVector<ASTNode> items;
    parser.parseTopLevelItems(items);
  }
};

struct SwiftParserExecutor {
  constexpr static StringRef name = "SwiftParser";

  static void performParse(llvm::MemoryBufferRef buffer, ExecuteOptions opts) {
#if SWIFT_BUILD_SWIFT_SYNTAX
    // TODO: Implement 'ExecuteOptionFlag::SkipBodies'
    auto sourceFile = swift_ASTGen_parseSourceFile(
        buffer.getBufferStart(), buffer.getBufferSize(), /*moduleName=*/"",
        buffer.getBufferIdentifier().data(), /*ASTContext=*/nullptr);
    swift_ASTGen_destroySourceFile(sourceFile);
#endif
  }
};

static void _loadSwiftFilesRecursively(
    StringRef path,
    SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>> &buffers) {
  if (llvm::sys::fs::is_directory(path)) {
    std::error_code err;
    for (auto I = llvm::sys::fs::directory_iterator(path, err),
              E = llvm::sys::fs::directory_iterator();
         I != E; I.increment(err)) {
      _loadSwiftFilesRecursively(I->path(), buffers);
    }
  } else if (path.endswith(".swift")) {
    if (auto buffer = llvm::MemoryBuffer::getFile(path)) {
      buffers.push_back(std::move(*buffer));
    }
  }
}

/// Load all '.swift' files in the specified \p filePaths into \p buffers.
/// If the path is a directory, this recursively collects the files in it.
static void
loadSources(ArrayRef<std::string> filePaths,
            SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>> &buffers) {
  for (auto path : filePaths) {
    _loadSwiftFilesRecursively(path, buffers);
  }
}

/// Measure the duration of \p body execution.
template <typename Duration>
static std::pair<Duration, Duration> measure(llvm::function_ref<void()> body) {
  auto cStart = std::clock();
  auto tStart = std::chrono::steady_clock::now();
  body();
  auto cEnd = std::clock();
  auto tEnd = std::chrono::steady_clock::now();

  auto clockMultiply =
      Duration::period::den / CLOCKS_PER_SEC / Duration::period::num;

  Duration cDuration((cEnd - cStart) * clockMultiply);
  return {std::chrono::duration_cast<Duration>(tEnd - tStart),
          std::chrono::duration_cast<Duration>(cDuration)};
}

/// Perform the performance measurement using \c Executor .
/// Parse all \p buffers using \c Executor , \p iteration times, and print out
/// the measurement to the \c stdout.
template <typename Executor>
static void
perform(const SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>> &buffers,
        ExecuteOptions opts, unsigned iteration, uintmax_t totalBytes,
        uintmax_t totalLines) {

  llvm::outs() << "----\n";
  llvm::outs() << "parser: " << Executor::name << "\n";

  using duration_t = std::chrono::nanoseconds;
  auto tDuration = duration_t::zero();
  auto cDuration = duration_t::zero();

  for (unsigned i = 0; i < iteration; i++) {
    for (auto &buffer : buffers) {
      std::pair<duration_t, duration_t> elapsed = measure<duration_t>(
          [&] { Executor::performParse(buffer->getMemBufferRef(), opts); });
      tDuration += elapsed.first;
      cDuration += elapsed.second;
    }
  }

  auto tDisplay =
      std::chrono::duration_cast<std::chrono::milliseconds>(tDuration).count();
  auto cDisplay =
      std::chrono::duration_cast<std::chrono::milliseconds>(cDuration).count();

  auto byteTPS = totalBytes * duration_t::period::den / cDuration.count();
  auto lineTPS = totalLines * duration_t::period::den / cDuration.count();

  llvm::outs() << llvm::format("wall clock time (ms): %8d\n", tDisplay)
               << llvm::format("cpu time (ms):        %8d\n", cDisplay)
               << llvm::format("throughput (byte/s):  %8d\n", byteTPS)
               << llvm::format("throughput (line/s):  %8d\n", lineTPS);
}

} // namespace

int swift_parse_test_main(ArrayRef<const char *> args, const char *argv0,
                          void *mainAddr) {
  SwiftParseTestOptions options;
  llvm::cl::ParseCommandLineOptions(args.size(), args.data(),
                                    "Swift parse test\n");

  unsigned iteration = options.Iteration;
  ExecuteOptions execOptions;
  if (options.SkipBodies)
    execOptions |= ExecuteOptionFlag::SkipBodies;

  SmallVector<std::unique_ptr<llvm::MemoryBuffer>> buffers;
  loadSources(options.InputPaths, buffers);
  unsigned int byteCount = 0;
  unsigned int lineCount = 0;
  for (auto &buffer : buffers) {
    byteCount += buffer->getBufferSize();
    lineCount += buffer->getBuffer().count('\n');
  }

  llvm::outs() << llvm::format("file count:  %8d\n", buffers.size())
               << llvm::format("total bytes: %8d\n", byteCount)
               << llvm::format("total lines: %8d\n", lineCount)
               << llvm::format("iterations:  %8d\n", iteration);
  for (auto mode : options.Executors) {
    switch (mode) {
    case Executor::SwiftParser:
#if SWIFT_BUILD_SWIFT_SYNTAX
      perform<SwiftParserExecutor>(buffers, execOptions, iteration, byteCount,
                                   lineCount);
      break;
#else
      llvm::errs() << "error: SwiftParser is not enabled\n";
      return 1;
#endif
    case Executor::LibParse:
      perform<LibParseExecutor>(buffers, execOptions, iteration, byteCount,
                                lineCount);
      break;
    }
  }

  return 0;
}

//===--------- ProfileData.cpp - Tools for loading llvm profdata -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ProfileData.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/FormattedStream.h"
#include "swift/Basic/Demangle.h"
#include <cxxabi.h>

namespace llvm {
namespace cov2json {

/// Runs the provided function with a color set on ferrs().
void withColor(raw_ostream::Colors color, bool bold, bool bg,
               std::function<void()> f) {
  bool colored = sys::Process::StandardErrHasColors();
  if (colored)
    ferrs().changeColor(color, bold, bg);
  f();
  if (colored)
    // FIXME: Properly reset state to the previous state,
    //         instead of resetting the color entirely.
    ferrs().resetColor();
}

/// Prints an error message and exits with the error's value.
void exitWithErrorCode(std::error_code error, StringRef whence) {
  withColor(raw_ostream::RED, /* bold = */ true, /* bg = */ false,
            [] { ferrs() << "error: "; });
  ferrs() << whence << ": " << error.message() << "\n";
  exit(error.value());
}

/// Grabs the symbol from a string that's 'File.cpp:symbol',
/// or just returns the symbol if there's no ':'.
StringRef extractSymbol(StringRef name) {
  auto pair = name.split(':');
  if (pair.second == "") {
    return pair.first;
  } else {
    return pair.second;
  }
}

/// Returns the demangled (Swift or C++) version of `symbol`.
std::string demangle(StringRef symbol) {
  auto prefix = symbol.substr(0, 2);
  if (prefix == "_Z") {
    auto demangled = abi::__cxa_demangle(symbol.data(), 0, 0, NULL);
    if (demangled) {
      std::string s(demangled);
      free(demangled);
      return s;
    }
  } else if (prefix == "_T") {
    return swift::Demangle::demangleSymbolAsString(symbol);;
  }
  return symbol;
}

/// Opens a stream to a file, or stdout if "" is provided.
std::unique_ptr<raw_fd_ostream> openStream(StringRef file) {
  if (file.size()) {
    std::error_code error;
    auto os = make_unique<raw_fd_ostream>(file, error, sys::fs::F_RW);
    if (error) {
      auto whence = "opening output stream for '" + file + "'";
      exitWithErrorCode(error, whence.str());
    }
    return os;
  } else {
    return make_unique<raw_fd_ostream>(fileno(stdout),
                                       /* shouldClose = */ false);
  }
}

/// Creates a Function struct off of a FunctionRecord.
Function::Function(const coverage::FunctionRecord &record) {
  this->symbol = extractSymbol(record.Name);
  this->name = demangle(symbol);
  for (auto &region : record.CountedRegions) {
    if (region.FileID != region.ExpandedFileID)
      continue;
    if (region.Kind !=
        llvm::coverage::CounterMappingRegion::RegionKind::CodeRegion)
      continue;
    Region r(region.ColumnStart, region.ColumnEnd, region.LineStart,
             region.LineEnd, region.ExecutionCount);
    regions.emplace_back(r);
  }
  executionCount = record.ExecutionCount;
}

Optional<unsigned> Function::firstLine() {
  if (regions.size()) {
    return regions[0].lineStart;
  }
  return None;
}

uint64_t Function::regionCount() {
  return regions.size();
}

uint64_t Function::executedRegionCount() {
  uint64_t count = 0;
  for (auto &region : regions) {
    if (region.executionCount > 0) {
      count += 1;
    }
  }
  return count;
}
  
uint64_t File::regionCount() {
  uint64_t counter = 0;
  for (auto &function : functions) {
    counter += function.regionCount();
  }
  return counter;
}

uint64_t File::executedRegionCount() {
  uint64_t counter = 0;
  for (auto &function : functions) {
    counter += function.executedRegionCount();
  }
  return counter;
}

uint64_t Project::regionCount() {
  uint64_t counter = 0;
  for (auto &file : files) {
    counter += file.regionCount();
  }
  return counter;
}

uint64_t Project::executedRegionCount() {
  uint64_t counter = 0;
  for (auto &file : files) {
    counter += file.executedRegionCount();
  }
  return counter;
}

/// Loads a CoverageMapping for a CoverageFilePair
std::unique_ptr<llvm::coverage::CoverageMapping>
CoverageFilePair::coverageMapping() {
  auto map = llvm::coverage::CoverageMapping::load(binary, filename);
  
  if (auto error = map.getError()) {
    auto whence = "loading coverage map for '" + filename +
                  "' with binary '" + binary + "'";
    exitWithErrorCode(error, whence.str());
  }
  
  return move(map.get());
}

/// Loads a list of files that are within the provided coveredDir.
void CoverageFilePair::loadFileMap(Project &project,
                                   StringRef coveredDir,
                                   std::vector<std::string> &coveredFiles) {
  auto mapping = coverageMapping();
  for (auto &filename : mapping->getUniqueSourceFiles()) {
    StringRef truncatedFilename = filename;
    if (coveredDir != "") {
      if (!filename.startswith(coveredDir)) {
        continue;
      }
      StringRef parentPath = sys::path::parent_path(coveredDir);
      truncatedFilename = filename.substr(parentPath.size(),
                                          filename.size() - parentPath.size());
      truncatedFilename = sys::path::relative_path(truncatedFilename);
    }
    if (!coveredFiles.empty()) {
      bool found = false;
      for (auto &file : coveredFiles) {
        if (truncatedFilename.endswith(file)) {
          found = true;
          break;
        }
      }
      if (!found) continue;
    }
    std::vector<Function> functions;
    for (auto &func : mapping->getCoveredFunctions(filename)) {
      functions.emplace_back(func);
    }
    project.files.emplace_back(File(truncatedFilename, functions));
  }
}
  
}
}

//===----- ProfileData.h - Data structures for serializing profdata -----===//
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

#ifndef ProfileData_h
#define ProfileData_h

#include <stdio.h>
#include <iostream>
#include "llvm/ProfileData/InstrProfReader.h"
#include "llvm/ProfileData/CoverageMapping.h"
#include "llvm/ProfileData/CoverageMappingReader.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Path.h"
#include <map>
#include <set>

namespace llvm {
namespace cov2json {

struct Region {
public:
  unsigned columnStart, columnEnd, lineStart, lineEnd;
  uint64_t executionCount;

  Region(unsigned columnStart, unsigned columnEnd, unsigned lineStart,
         unsigned lineEnd, uint64_t executionCount)
      : columnStart(columnStart), columnEnd(columnEnd), lineStart(lineStart),
        lineEnd(lineEnd), executionCount(executionCount) {}
  Region(llvm::coverage::CountedRegion &region)
      : Region(region.ColumnStart, region.ColumnEnd, region.LineStart,
               region.LineEnd, region.ExecutionCount) {}
  Region() {}
};

struct Function {
public:
  std::string name;
  std::string symbol;
  std::vector<Region> regions;
  uint64_t executionCount;
  Function(StringRef name, std::vector<Region> regions,
           uint64_t executionCount)
      : name(name), regions(regions), executionCount(executionCount) {}

  uint64_t regionCount();
  uint64_t executedRegionCount();
  Optional<unsigned> firstLine();
  
  Function(const llvm::coverage::FunctionRecord &record);
  Function() {}
};

/// A struct that stores all functions associated with a given source file.
struct File {
public:
  std::string name;
  std::vector<Function> functions;
  
  uint64_t regionCount();
  uint64_t executedRegionCount();

  File(StringRef name, std::vector<Function> functions)
      : name(name), functions(functions) {}
  File() {}
};
  
struct Project {
  std::vector<File> files;
  uint64_t regionCount();
  uint64_t executedRegionCount();
  
  Project(std::vector<File> files): files(files) {}
  Project() {}
};


/// A struct that represents a pair of binary file and profdata file,
/// which reads and digests the contents of those files.
struct CoverageFilePair {
public:
  /// The .profdata file path.
  StringRef filename;

  /// The binary file path that generated the .profdata.
  StringRef binary;

  /// \returns A CoverageMapping object corresponding
  /// to the binary and profdata.
  std::unique_ptr<llvm::coverage::CoverageMapping> coverageMapping();

  /// Loads a vector of File objects that are covered in this profdata.
  void loadFileMap(Project &project, StringRef coveredDir,
                   std::vector<std::string> &coveredFiles);

  CoverageFilePair(StringRef filename, StringRef binary)
      : filename(filename), binary(binary) {}
};

std::unique_ptr<raw_fd_ostream> openStream(StringRef file);

} // namespace cov2json;
} // namespace llvm;

#endif /* ProfileData_h */

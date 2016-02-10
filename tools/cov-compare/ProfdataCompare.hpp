//===------ ProfdataCompare.hpp - Tools for analyzing llvm profdata -------===//
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

#ifndef ProfdataCompare_hpp
#define ProfdataCompare_hpp

#include <stdio.h>
#include <iostream>
#include "llvm/ProfileData/InstrProfReader.h"
#include "llvm/ProfileData/CoverageMappingReader.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Path.h"
#include "Utils.hpp"
#include <map>

using namespace llvm;
using namespace coverage;

namespace covcompare {
  
  /// A struct that stores output options.
  struct Options {
  public:
    /// The output formats supported by this tool.
    typedef enum {
      HTML,
      Markdown
    } Format;
    
    /// The files that this program should consider when comparing coverage.
    std::vector<std::string> coveredFiles;
    
    /// The output format.
    Format output;
    
    /// The filename (or directory, if the output format is HTML) to output.
    std::string outputFilename;
    
    Options(std::vector<std::string> coveredFiles, Format output,
            std::string outputFilename)
    : coveredFiles(coveredFiles), output(output),
    outputFilename(outputFilename) {}
  };
  
  struct Region {
  public:
    unsigned columnStart, columnEnd, lineStart, lineEnd;
    uint64_t executionCount;
    Region(unsigned columnStart, unsigned columnEnd,
           unsigned lineStart, unsigned lineEnd, uint64_t executionCount)
    : columnStart(columnStart), columnEnd(columnEnd), lineStart(lineStart),
      lineEnd(lineEnd), executionCount(executionCount) {}
    Region(llvm::coverage::CountedRegion &region)
    : Region(region.ColumnStart, region.ColumnEnd,
             region.LineStart, region.LineEnd, region.ExecutionCount) {}
    Region() {}
  };
  
  struct Function {
  public:
    std::string name;
    std::vector<Region> regions;
    uint64_t executionCount;
    double coveragePercentage() {
      int counted = 0;
      for (auto &region : regions) {
        if (region.executionCount > 0) {
          counted++;
        }
      }
      return ((double)counted / (double)regions.size()) * 100;
    }
    Function(std::string name, std::vector<Region> regions,
             uint64_t executionCount)
    : name(name), regions(regions), executionCount(executionCount) {}
    
    Function(llvm::coverage::FunctionRecord record) {
      this->name = extractSymbol(record.Name);
      for (auto &region : record.CountedRegions) {
        this->regions.push_back(region);
      }
      this->executionCount = record.ExecutionCount;
    }
    
    Function(const Function &copy): name(copy.name), regions(copy.regions),
    executionCount(copy.executionCount) {}
    
    Function(Function &copy): name(copy.name), regions(copy.regions),
    executionCount(copy.executionCount) {}
    
    Function() {}
  };
  
  /// A struct that stores all functions associated with a given source file.
  struct File {
    
    std::shared_ptr<std::map<std::string, std::shared_ptr<Function>>>
    _functionMap;
  public:
    std::string name;
    std::vector<Function> functions;
    
    /// \returns The percentage of functions with a non-zero execution count.
    double coveragePercentage();
    
    /// \returns A map of function symbols to the
    /// corresponding Functions.
    std::shared_ptr<std::map<std::string, std::shared_ptr<Function>>>
    functionMap();
    
    File(std::string name,
         std::vector<Function> functions)
    : name(sys::path::filename(name)), functions(functions) {
    }
    
    File(File &copy)
    : _functionMap(copy._functionMap), name(copy.name),
      functions(copy.functions) {}
    
    File(const File &copy)
    : _functionMap(copy._functionMap), name(copy.name),
      functions(copy.functions) {}
    
    File() {}
  };
  
  /// A class that representd a comparison between the old and new versions of
  /// a file in the two coverage profiles.
  template<typename Compared>
  class Comparison {
  public:
    /// The old and new files.
    std::shared_ptr<Compared> oldItem, newItem;
    
    /// \returns The difference in coverage between these two files.
    double coverageDifference() {
      double oldPercentage = oldItem ? oldItem->coveragePercentage()
      : 0.0;
      double diff = newItem->coveragePercentage() - oldPercentage;
      return fabs(diff) < 0.01 ? 0 : diff;
    }
    
    /// \returns A percent-formatted string representing the coverage difference
    /// between the two items in this comparison.
    std::string formattedCoverageDifference() {
      double diff = coverageDifference();
      return (diff > 0 ? "+" : "") + formattedDouble(diff);
    }
    
    Comparison(std::shared_ptr<Compared> oldItem,
               std::shared_ptr<Compared> newItem)
    : oldItem(oldItem), newItem(newItem) {
      assert(newItem && "New item must not be null");
      if (oldItem) {
        assert(oldItem->name == newItem->name
               && "Names must match.");
      }
    }
  };
  
  /// A class that represents a comparison between the old and new versions of
  /// a function in the two coverage profiles.
  class FunctionComparison: public Comparison<Function> {
  public:
    /// \returns The attempted-demangled symbol name for this function.
    std::string functionName();
    
    FunctionComparison(std::shared_ptr<Function> oldFunction,
                       std::shared_ptr<Function> newFunction)
    : Comparison(oldFunction, newFunction) {}
  };
  
  /// A class that represents a comparison between the old and new versions of
  /// a file in the two coverage profiles.
  class FileComparison: public Comparison<File> {
  public:
    /// \returns A list of function comparisons between each of the functions
    /// in the new file (the old file's functions are ignored, as they are no
    /// longer relevant from a coverage perspective.)
    std::vector<FunctionComparison> functionComparisons();
    
    FileComparison(std::shared_ptr<File> oldFile,
                   std::shared_ptr<File> newFile)
    : Comparison(oldFile, newFile) {}
  };
  
  /// A struct that represents a pair of binary file and profdata file,
  /// which reads and digests the contents of those files.
  struct CoverageFilePair {
  public:
    /// The .profdata file path.
    std::string filename;
    
    /// The binary file path that generated the .profdata.
    std::string binary;
    
    /// \returns A CoverageMapping object corresponding
    /// to the binary and profdata.
    std::unique_ptr<CoverageMapping> coverageMapping();
    
    /// \returns A map of filenames to File
    /// objects that are covered in this profdata.
    std::map<std::string, std::shared_ptr<File>> fileMap();
    
    CoverageFilePair(std::string filename, std::string binary) :
    filename(filename), binary(binary) {}
  };
  
  /// A class that handles comparing two coverage profiles and outputting an
  /// analysis of the difference.
  class ProfdataCompare {
    /// The options passed-into this object.
    Options options;
    
    /// \returns A list of file comparisons based on all the files inside the
    /// new coverage data (functions that exist in the old, but not the new,
    /// are ignored as they are no longer relevant.)
    std::vector<std::shared_ptr<FileComparison>> genComparisons();
    
  public:
    /// The old and new YAML files for coverage data.
    std::string oldFile, newFile;
    
    /// The output stream that will be used to output Markdown if Markdown
    /// is the selected output format.
    std::unique_ptr<llvm::raw_ostream> os;
    
    /// A cached list of comparisons between all files in the
    /// new coverage profile.
    std::vector<std::shared_ptr<FileComparison>> comparisons;
    
    ProfdataCompare(std::string oldFile, std::string newFile, Options options)
    : options(options), oldFile(oldFile), newFile(newFile) {
      comparisons = genComparisons();
      if (options.output != Options::HTML) {
        os = streamForFile(options.outputFilename);
      }
    }
    
    void compare();
  };
  
  int compareMain(int argc, const char *argv[]);
  int yamlMain(int argc, const char *argv[]);
  
} // namespace covcompare;

#endif /* ProfdataCompare_hpp */

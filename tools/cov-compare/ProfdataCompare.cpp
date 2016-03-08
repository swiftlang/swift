//===------ ProfdataCompare.cpp - Tools for comparing llvm profdata -------===//
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

#include "ProfdataCompare.hpp"
#include "llvm/Support/CommandLine.h"
#include "HTMLWriter.hpp"
#include "MarkdownWriter.hpp"
#include "YAMLWriter.hpp"

using namespace std;
using namespace llvm;
using namespace coverage;

namespace covcompare {
  
  double File::coveragePercentage() {
    if (functions.empty()) return 100.0;
    double totalPercentagePoints = 0;
    for (auto &func : functions) {
      totalPercentagePoints += func.coveragePercentage();
    }
    return (totalPercentagePoints / (double)functions.size());
  }
  
  vector<FunctionComparison> FileComparison::functionComparisons() {
    vector<FunctionComparison> funcComparisons;
    for (auto &function : newItem->functions) {
      shared_ptr<Function> oldFunction;
      if (oldItem) {
        auto pair = oldItem->functionMap()->find(function.name);
        if (pair != oldItem->functionMap()->end()) {
          oldFunction = pair->second;
        }
      }
      auto func = FunctionComparison(oldFunction,
               make_shared<Function>(function));
      funcComparisons.push_back(func);
    }
    return funcComparisons;
  }
  
  unique_ptr<CoverageMapping> CoverageFilePair::coverageMapping() {
    auto map = CoverageMapping::load(binary, filename);
    
    if (auto error = map.getError()) {
      exitWithErrorCode(error);
    }
    
    return move(map.get());
  }
  
  shared_ptr<map<string, shared_ptr<Function>>> File::functionMap() {
    if (_functionMap) return _functionMap;
    _functionMap = make_shared<map<string, shared_ptr<Function>>>();
    for (auto &function : functions) {
      (*_functionMap)[function.name] = make_shared<Function>(function);
    }
    return _functionMap;
  }

  map<string, shared_ptr<File>> CoverageFilePair::fileMap() {
    auto mapping = coverageMapping();
    map<string, shared_ptr<File>> files;
    for (auto &filename : mapping->getUniqueSourceFiles()) {
      vector<Function> functions;
      for (auto &func : mapping->getCoveredFunctions(filename)) {
        functions.push_back(func);
      }
      files[filename] = make_shared<File>(filename, functions);
    }
    return files;
  }
  
  map<string, File> fileMapFromYAML(string yamlFile) {
    auto buffer = MemoryBuffer::getFile(yamlFile);
    if (auto error = buffer.getError()) {
      exitWithErrorCode(error);
    }
    yaml::Input yin(buffer.get()->getBuffer());
    vector<File> files;
    yin >> files;
    if (auto error = yin.error()) {
      exitWithErrorCode(error);
    }
    map<string, File> fileMap;
    for (auto &file : files) {
      fileMap[file.name] = file;
    }
    return fileMap;
  }
  
  string FunctionComparison::functionName() {
    auto name = extractSymbol(newItem->name);
    return demangled(name);
  }

  vector<shared_ptr<FileComparison>>
  ProfdataCompare::genComparisons() {
    vector<shared_ptr<FileComparison>> comparisons;
    auto oldFiles = fileMapFromYAML(oldFile);
    auto newFiles = fileMapFromYAML(newFile);
    for (auto &iter : newFiles) {
      auto idx = find(options.coveredFiles.begin(),
                      options.coveredFiles.end(),
                      iter.first);
      if (options.coveredFiles.size() && idx == options.coveredFiles.end()) {
        continue;
      }
      shared_ptr<File> oldFile;
      auto old = oldFiles.find(iter.first);
      if (old != oldFiles.end()) {
        oldFile = make_shared<File>(old->second);
      }
      auto c = make_shared<FileComparison>(oldFile, make_shared<File>(iter.second));
      comparisons.push_back(c);
    }

    sort(comparisons.begin(), comparisons.end(),
         [](shared_ptr<FileComparison> a, shared_ptr<FileComparison> b) {
           if (!a) return true;
           if (!b) return false;
           return a->coverageDifference() < b->coverageDifference();
         });
    return comparisons;
  }

  void ProfdataCompare::compare() {
    auto output = options.output;
    
    if (output == Options::HTML) {
      HTMLWriter(options.outputFilename).write(*this);
      return;
    }
    
    Column fnCol("Filename");
    Column prevCol("Previous Coverage", Column::Alignment::Center);
    Column currCol("Current Coverage", Column::Alignment::Center);
    Column diffCol("Coverage Difference", Column::Alignment::Center);
    for (auto &cmp : comparisons) {
      string oldPercentage = cmp->oldItem ?
      formattedDouble(cmp->oldItem->coveragePercentage()) : "N/A";
      string newPercentage =
      formattedDouble(cmp->newItem->coveragePercentage());
      fnCol.add(cmp->newItem->name);
      prevCol.add(oldPercentage);
      currCol.add(newPercentage);
      diffCol.add(cmp->formattedCoverageDifference());
    }
    
    MarkdownWriter({ fnCol, prevCol, currCol, diffCol }).write(*os);
  }
  
  int compareMain(int argc, const char *argv[]) {
    cl::opt<std::string> output("o",
                                cl::desc("<output filename>"),
                                cl::value_desc("The output file to write. "
                                               "If targeting HTML, this will "
                                               "be the name of the output "
                                               "directory."));
    cl::opt<std::string> oldFile(cl::Positional,
                                 cl::desc("<old yaml file>"), cl::Required);
    cl::opt<std::string> newFile(cl::Positional,
                                 cl::desc("<new yaml file>"), cl::Required);
    cl::list<std::string> coveredFiles("c", cl::Positional,
                                       cl::PositionalEatsArgs,
                                       cl::desc("Restrict output to a certain "
                                                "set of covered files"),
                                       cl::ZeroOrMore);
    cl::opt<Options::Format> format("f",
                                    cl::desc("Format to output comparison"),
                                    cl::values(clEnumValN(Options::HTML,
                                                          "html",
                                                          "HTML output"),
                                               clEnumValN(Options::Markdown,
                                                          "markdown",
                                                          "Markdown table output"),
                                               clEnumValEnd),
                                    cl::init(Options::Markdown));
    cl::ParseCommandLineOptions(argc, argv);
    
    Options options(coveredFiles,
                    format.getValue(),
                    output);
    
    ProfdataCompare comparer(oldFile, newFile, options);
    
    comparer.compare();
    return 0;
  }
  
  int yamlMain(int argc, const char *argv[]) {
    cl::opt<std::string> output("o",
                                cl::desc("<output filename>"),
                                cl::value_desc("The output file to write. "
                                               "If targeting HTML, this will "
                                               "be the name of the output "
                                               "directory."),
                                cl::init(""));
    cl::opt<std::string> file(cl::Positional,
                              cl::desc("<profdata file>"), cl::Required);
    cl::opt<std::string> binary(cl::Positional,
                                cl::desc("<binary file>"), cl::Required);
    cl::ParseCommandLineOptions(argc, argv);
    
    CoverageFilePair filePair(file, binary);
    auto map = filePair.fileMap();
    vector<File> files;
    for (auto &pair : map) {
      files.push_back(*pair.second);
    }
    
    unique_ptr<raw_ostream> os = streamForFile(output);
    
    yaml::Output yout(*os);
    yout << files;
    
    return 0;
  }
}

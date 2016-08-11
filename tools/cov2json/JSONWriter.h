//===----------- JSONWriter.h - Serializes structures to JSON -----------===//
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

#ifndef JSONWriter_h
#define JSONWriter_h

#include "swift/Basic/JSONSerialization.h"
#include "ProfileData.h"

namespace swift {
namespace json {

template <typename U> struct ArrayTraits<std::vector<U>> {
  static size_t size(Output &out, std::vector<U> &seq) { return seq.size(); }
  static U &element(Output &out, std::vector<U> &seq, size_t index) {
    if (seq.size() <= index) {
      seq.resize(index + 1);
    }
    return seq[index];
  }
};

template <> struct ObjectTraits<llvm::cov2json::Function> {
  static void mapping(Output &out, llvm::cov2json::Function &function) {
    uint64_t regionCount = function.regionCount();
    uint64_t executedCount = function.executedRegionCount();
    if (auto firstLine = function.firstLine()) {
      out.mapOptional("start_line", firstLine);
    }
    out.mapRequired("name", function.name);
    out.mapRequired("symbol", function.symbol);
    out.mapRequired("regions", regionCount);
    out.mapRequired("executed_regions", executedCount);
  }
};

template <> struct ObjectTraits<llvm::cov2json::File> {
  static void mapping(Output &out, llvm::cov2json::File &file) {
    uint64_t regionCount = file.regionCount();
    uint64_t executedCount = file.executedRegionCount();
    out.mapRequired("filename", file.name);
    out.mapRequired("functions", file.functions);
    out.mapRequired("regions", regionCount);
    out.mapRequired("executed_regions", executedCount);
  }
};

template <> struct ObjectTraits<llvm::cov2json::Project> {
  static void mapping(Output &out, llvm::cov2json::Project &project) {
    uint64_t regionCount = project.regionCount();
    uint64_t executedCount = project.executedRegionCount();
    out.mapRequired("files", project.files);
    out.mapRequired("regions", regionCount);
    out.mapRequired("executed_regions", executedCount);
  }
};
}
}

#endif /* JSONWriter_h */

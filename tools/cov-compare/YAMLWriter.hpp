//===--------- YAMLWriter.hpp - Tools for analyzing llvm profdata ---------===//
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

#ifndef YAMLWriter_hpp
#define YAMLWriter_hpp

#include <stdio.h>
#include "llvm/Support/YAMLTraits.h"
#include "ProfdataCompare.hpp"

template<typename U>
struct llvm::yaml::SequenceTraits<std::vector<U>> {
    static size_t size(IO &io, std::vector<U> &seq) {
        return seq.size();
    }
    static U &element(IO &, std::vector<U> &seq, size_t index) {
        if (seq.size() <= index) {
            seq.resize(index + 1);
        }
        return seq[index];
    }
};

template <>
struct llvm::yaml::MappingTraits<covcompare::Region> {
    static void mapping(IO &io, covcompare::Region &region) {
        io.mapRequired("column-start", region.columnStart);
        io.mapRequired("column-end", region.columnEnd);
        io.mapRequired("line-start", region.lineStart);
        io.mapRequired("line-end", region.lineEnd);
        io.mapRequired("count", region.executionCount);
    }
    static const bool flow = true;
};

template <>
struct llvm::yaml::MappingTraits<covcompare::Function> {
    static void mapping(IO &io, covcompare::Function &function) {
        io.mapRequired("name", function.name);
        io.mapRequired("regions", function.regions);
        io.mapRequired("count", function.executionCount);
    }
    static const bool flow = true;
};

template <>
struct llvm::yaml::MappingTraits<covcompare::File> {
    static void mapping(IO &io, covcompare::File &file) {
        io.mapRequired("filename", file.name);
        io.mapRequired("functions", file.functions);
    }
};

#endif /* YAMLWriter_hpp */
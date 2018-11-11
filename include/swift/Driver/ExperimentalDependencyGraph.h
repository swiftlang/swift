//===--- ExperimentalDependencyGraph.h - Track intra-module dependencies -*- C++ -*-===//
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


#ifndef ExperimentalDependencyGraph_h
#define ExperimentalDependencyGraph_h

#include "swift/Basic/ExperimentalDependencies.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Driver/Job.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <string>
#include <vector>
#include <unordered_map>

namespace swift {
  namespace driver {
    namespace experimental_dependencies {
      class DependencyGraph: public swift::experimental_dependencies::Graph {
        using Node = swift::experimental_dependencies::Node;
        using Arc = swift::experimental_dependencies::Arc;

        std::unordered_multimap<std::string, Node*> nodesByNameForDependencies{};
        
      public:
        DependencyGraph();
        
        void registerCmdForReevaluation(const Job* Cmd);
        static Job::Condition loadFromFile(const Job* Cmd, StringRef filename);
        
        void addNode(Node*);
//        void addArc(Arc*);
        
      private:
        static std::string depsFileForCmd(const Job* Cmd);
        void registerDepsFileForReevaluation(std::string depsFile); // XXX
      };
    } // experimental_dependencies
  } // driver
} // swift

#endif /* ExperimentalDependencyGraph_h */

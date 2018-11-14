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
#include "swift/Driver/DependencyGraph.h"
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
      using Node = swift::experimental_dependencies::Node;
      using FrontendNode = swift::experimental_dependencies::FrontendNode;
      using FrontendGraph = swift::experimental_dependencies::FrontendGraph;
      using NodeDependencyKey = swift::experimental_dependencies::NodeDependencyKey;

      class DriverNode: public Node {
        std::string swiftDepsFile;
        std::set<NodeDependencyKey> dependers;
      };
      
      
      class DependencyGraphImpl {
      public:
        using LoadResult = typename swift::DependencyGraphImpl::LoadResult;
      };

      class DriverGraph {
        std::unordered_map<std::string, std::vector<DriverNode*>> nodesBySwiftDepsFile;
        std::vector<DriverNode*> nodesInNoSwiftDepsFile;
        std::unordered_multimap<NodeDependencyKey, DriverNode*, typename NodeDependencyKey::hash> nodesByDependency;

        
      public:
        DriverGraph() = default;
        
        DependencyGraphImpl::LoadResult loadFromPath(const Job* Cmd, StringRef path);
        
        bool isMarked(const Job* Cmd) const;//XXX
        
        template <unsigned N>
        void markTransitive(SmallVector<const Job*, N> &visited, const Job* node,
                            DependencyGraph<const Job*>::MarkTracer *tracer = nullptr);
        bool markIntransitive(const Job* node);
        void addIndependentNode(const Job* node);
        std::vector<std::string> getExternalDependencies() const;
        void markExternal(SmallVectorImpl<const Job *> &visited,
                          StringRef externalDependency);
        
      private:
        DependencyGraphImpl::LoadResult loadFromBuffer(const void *node,
                                                       llvm::MemoryBuffer &buffer);
      
       
        DependencyGraphImpl::LoadResult integrate(const FrontendGraph &);
        DependencyGraphImpl::LoadResult integrateNew(const FrontendGraph &);
        DependencyGraphImpl::LoadResult integrateExisting(const FrontendGraph &, std::vector<DriverNode*>&);
      };
    } // experimental_dependencies
  } // driver
} // swift

#endif /* ExperimentalDependencyGraph_h */

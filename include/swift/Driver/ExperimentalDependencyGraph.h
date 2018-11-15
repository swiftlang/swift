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
#include <unordered_set>

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
        
      public:
        DriverNode(const NodeDependencyKey &key, StringRef fingerprint)
        : Node(key, fingerprint) {}
      };
      
      
      class DependencyGraphImpl {
      public:
        using LoadResult = typename swift::DependencyGraphImpl::LoadResult;
      };

      class DriverGraph {
        using NodesByKey = std::unordered_map<NodeDependencyKey, DriverNode*>;
        // empty string for no file
        std::unordered_map<std::string, NodesByKey> nodesBySwiftDepsFile;
        
        std::unordered_map<NodeDependencyKey, std::unordered_map<std::string, DriverNode*>> nodesByDependencyKey;
        
        std::unordered_map<NodeDependencyKey, std::unordered_set<NodeDependencyKey>> dependersByDependee;

        template <typename Key1, typename Key2>
          DriverNode* findNodeInTwoStageMap(std::unordered_map<Key1,
                               std::unordered_map<Key2,
                               DriverNode*>>
                               &mapmap,
                               const Key1 &key1, const Key2 &key2) {
            auto iter = mapmap.find(key1);
            return iter == mapmap.end() ? nullptr : findPointer(iter->second, key2);
          }

        // For a keyed container holding pointers, lookup and return nullptr if absent
        template <typename Container>
        static typename Container::value_type::second_type
        findPointer(Container &c, const typename Container::key_type  &k) {
          auto iter = c.find(k);
          return iter == c.end() ? nullptr : iter->second;
        }
        
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
       
        void integrateHereNode(const FrontendNode *integrand, const std::string &depsFilename, NodesByKey &nodesInFile, NodesByKey &nodesToRemove);
        void integrateElsewhereNode(const FrontendNode *integrand);
        void rememberToPropagateChangesFrom(DriverNode* n);
        
        void updateDependersByDependeesFor(const FrontendNode* n, const FrontendGraph& g);
        
        void addNode(StringRef swiftDeps, DriverNode *n);
        void removeNode(StringRef swiftDeps, DriverNode *n);
      };
    } // experimental_dependencies
  } // driver
} // swift

#endif /* ExperimentalDependencyGraph_h */

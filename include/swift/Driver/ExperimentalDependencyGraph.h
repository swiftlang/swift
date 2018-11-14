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
      class DependencyGraphImpl {
      public:
        using LoadResult = typename swift::DependencyGraphImpl::LoadResult;
      };
      // TODO: not FrontendGraph
      class ExpDependencyGraph: public swift::experimental_dependencies::FrontendGraph {
        using Node = swift::experimental_dependencies::Node;
        using FrontendNode = swift::experimental_dependencies::FrontendNode;

        /// When rereading a dependencies file, must be able to find the old nodes for that file.
//        std::unordered_map<std::string,
//        FrontendNode::Cache
//        > nodesByDepsFile;
        
        /// When reading a dependencies file, there are nodes that are depended-upon but
        /// (do not yet) correspond to any file.
//        FrontendNode::Cache orphans;
        
      public:
        ExpDependencyGraph() = default;
        ExpDependencyGraph(ExpDependencyGraph &&other) = default;
        
        DependencyGraphImpl::LoadResult loadFromPath(const Job* Cmd, StringRef path);
        
        bool isMarked(const Job* Cmd) const;//XXX
        
        /// returns address of node IF it has changed or is new
        FrontendNode* addNodeForFile(StringRef depsFile,
//                                     , FrontendNode::Cache&,
                                     FrontendNode&);
//        void addArc(Arc*);
        
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
        using NodeCallbackTy = void(FrontendNode *);
        using ErrorCallbackTy = void();
        void
        parseDependencyFile(llvm::MemoryBuffer &buffer,
                            llvm::function_ref<NodeCallbackTy> nodeCallback,
                            llvm::function_ref<ErrorCallbackTy> errorCallback);
        static void parseNode(llvm::yaml::SequenceNode *,
                              llvm::function_ref<NodeCallbackTy> nodeCallback,
                              llvm::function_ref<ErrorCallbackTy> errorCallback);
        
        DependencyGraphImpl::LoadResult integrate(FrontendGraph &&);
        
//        FrontendNode::Cache &getMemoizedNodesForFile(StringRef depsFileName) {
//          auto iter = nodesByDepsFile.find(depsFileName);
//          if (iter != nodesByDepsFile.end())
//            return iter->second;
//          nodesByDepsFile.insert(std::make_pair(depsFileName, FrontendNode::Cache()));
//          return getMemoizedNodesForFile(depsFileName);
//        }
      };
    } // experimental_dependencies
  } // driver
} // swift

#endif /* ExperimentalDependencyGraph_h */

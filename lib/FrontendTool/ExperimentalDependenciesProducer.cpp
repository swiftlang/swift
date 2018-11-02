//===--- ExperimentalDependenciesProducer.cpp - Generates swiftdeps files -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <stdio.h>

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/ExperimentalDependencies.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/ReferenceDependencyKeys.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;
using namespace experimental_dependencies;

namespace {
enum class ReferendeDependenciesKind { StatusQuo };
/// Emits the reference dependencies from the frontend so that the driver
/// can compute a dependency graph for the whole module, and use it to decide
/// which files need to be recompiled when doing incremental compilation.
template <ReferendeDependenciesKind> class ReferenceDependenciesEmitter {
protected:
  SourceFile *const SF;
  const DependencyTracker &depTracker;
  llvm::raw_ostream &out;

public:
  ReferenceDependenciesEmitter(SourceFile *const SF,
                               const DependencyTracker &depTracker,
                               llvm::raw_ostream &out)
      : SF(SF), depTracker(depTracker), out(out) {}
  void emit();
};
} // end namespace

template <>
void ReferenceDependenciesEmitter<ReferendeDependenciesKind::StatusQuo>::emit();

bool swift::experimental_dependencies::emitReferenceDependencies(
    DiagnosticEngine &diags, SourceFile *SF,
    const DependencyTracker &depTracker, StringRef outputPath) {

  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
    ReferenceDependenciesEmitter<ReferendeDependenciesKind::StatusQuo>(
        SF, depTracker, out)
        .emit();
    return false;
  });
}



// shorthands

using StringVec = std::vector<std::string>;
template <typename T> using CPVec = std::vector<const T*>;
template <typename T1 = std::string, typename T2 = std::string> using PairVec = std::vector<std::pair<T1, T2>>;
template <typename T1, typename T2> using CPPairVec = std::vector<std::pair<const T1*, const T2*>>;

namespace SQ {
  class YAMLEmitter {
  private:
    llvm::raw_ostream &out;
    
  public:
    YAMLEmitter(llvm::raw_ostream &out) : out(out) {}
    
    void emitSectionStart(StringRef section) const {
      out << reference_dependency_keys::providesMember << ":\n";
    }
    void emitName(StringRef name) const {
      out << "- \"" << llvm::yaml::escape(name) << "\"\n";
    }
    void emitSingleValueSection(StringRef section, StringRef value) const {
      out << section << ": \"" << llvm::yaml::escape(value) << "\"\n";
    }
    void emitDoubleNameLine(StringRef name1, StringRef name2) const {
      out << "- [\"" << llvm::yaml::escape(name1) << "\", \""
      << (name2.empty() ? std::string() : llvm::yaml::escape(name2))
      << "\"]\n";
    }
    
    template <typename NodeT>
    void emitNodes(ArrayRef<NodeT> nodes) {
      for (auto &n: nodes)
        n.emit(this);
    }
  };
  
  class ProvideNode {
    
  };
  class SingleDeclNode {
    
  };
  
 
  
//  template <DeclKind K>
//  bool match(const DeclKind k) { return k == K; }

  template <typename> // need a dummy template arg for this to work
  bool match(const DeclKind k) { return false; }
  
  template <typename, DeclKind F, DeclKind... Kinds>
  bool match(const DeclKind k) {
    return k == F || match<int, Kinds...>(k);
  }
  
  template <typename DeclT, DeclKind ... Kinds>
  void getSFDecls(const SourceFile *const SF, std::vector<const DeclT*> &dest) {
    for (const Decl *const D: SF->Decls) {
     if (match<int, Kinds ...>(D->getKind()))
       dest.push_back(cast<DeclT>(D));
    }
  }
  
  
  
  class SimpleNode {
 
  };
  
  class Node {
    std::string name;
  public:
    const Decl *const D;
    Node(std::string name, const Decl *const D) : name(name), D(D) {}
  };
  
  template<typename DeclT, DeclKind ...Kinds>
  class SimpleTopLevelProvide: Node {
    static void
    getDecls(const SourceFile *const SF, std::vector<const DeclT*> &dest) {
      getSFDecls<DeclT, Kinds...>(SF, dest);
    }
    static Identifier identifier(const DeclT* D) {return D->getName(); }
    static std::string name(const DeclT *D) {
      return DeclBaseName(identifier(D)).userFacingName();
    }
    SimpleTopLevelProvide(const DeclT *D) : Node(name(D), D) {}
  public:
    static void getNodes(const SourceFile *SF, std::vector<const Node> &dest) {
      CPVec<DeclT> decls;
      getDecls(SF, decls);
      for (const auto *D: decls)
        dest.push_back(SimpleTopLevelProvide(D));
    }
    void emit(YAMLEmitter &emitter) {emitter.emitName(name);}
  };
  using TopLevelOperatorProvide = SimpleTopLevelProvide<OperatorDecl, DeclKind::InfixOperator, DeclKind::PrefixOperator, DeclKind::PostfixOperator>;
  using TopLevelPrecedenceGroup = SimpleTopLevelProvide<PrecedenceGroupDecl, DeclKind::PrecedenceGroup>;
  
  class NodeConstructor {
 
  public:
    std::vector<const Node> topLevelProvides;
    
//    std::vector<const NominalProvide> nominalProvides;
//    std::vector<const MemberProvide> memberProvides;
//    std::vector<const DynamicLookupProvide> dynamicLookupProvides;
//    std::vector<const TopLevelDepend> topLevelDepends;
//    std::vector<const NominalDepend> nominalDepends;
//    std::vector<const MemberDepend> memberDepends;
//    std::vector<const DynamicLookupDepend> dynamicLookupDepends;
//    std::vector<const ExternalDepend> externalDepends;
    
    
  public:
    NodeConstructor(SourceFile *const SF,
                                 const DependencyTracker &depTracker)
    {
      TopLevelOperatorProvide::getNodes(SF, topLevelProvides);
      TopLevelPrecedenceGroup::getNodes(SF, topLevelProvides);
    }
  
  };
  
  
  
  void writeTheFile(SourceFile *const SF, const DependencyTracker &depTracker, YAMLEmitter &emitter) {
    NodeConstructor nodeConstructor{SF, depTracker};
    emitter.emitSectionStart(reference_dependency_keys::providesTopLevel);
    emitter.emitNodes(nodeConstructor.topLevelProvides);
  }
}




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

#include <unordered_map>

using namespace swift;
using namespace experimental_dependencies;





// shorthands

using StringVec = std::vector<std::string>;
template <typename T> using CPVec = std::vector<const T*>;
template <typename T1 = std::string, typename T2 = std::string> using PairVec = std::vector<std::pair<T1, T2>>;
template <typename T1, typename T2> using CPPairVec = std::vector<std::pair<const T1*, const T2*>>;



namespace {
  /// Takes all the Decls in a SourceFile, and collects them into buckets by groups of DeclKinds.
  /// Also casts them to more specific types.
  
  class SourceFileDeclDemux {
  private:
    template <typename SpecificDeclType, DeclKind f, DeclKind ...r>
    bool take(const Decl *const D, CPVec<SpecificDeclType> &decls) {
      if (D->getKind() != f)
        return take<SpecificDeclType, r...>(D, decls);
      decls.push_back(cast<SpecificDeclType>(D));
      return true;
    }
    template <typename SpecificDeclType>
    bool take(const Decl *const D, CPVec<SpecificDeclType> &decls) {
      return false;
    }
  public:
    CPVec<ExtensionDecl> extensions;
    CPVec<OperatorDecl> operators;
    CPVec<PrecedenceGroupDecl> precedenceGroups;
    CPVec<NominalTypeDecl> topNominals;
    CPVec<ValueDecl> topValues;
    CPVec<NominalTypeDecl> allNominals;
    CPVec<FuncDecl> memberOperatorDecls;
    CPVec<ValueDecl> valuesInExtensions;
    CPVec<ValueDecl> classMembers;
    
    SourceFileDeclDemux(const SourceFile *const SF) {
      for (const Decl *const D: SF->Decls) {
        take<ExtensionDecl, DeclKind::Extension>(D, extensions)
        || take<OperatorDecl, DeclKind::InfixOperator, DeclKind::PrefixOperator, DeclKind::PostfixOperator>(D, operators)
        || take<PrecedenceGroupDecl, DeclKind::PrecedenceGroup> (D, precedenceGroups)
        || take<NominalTypeDecl, DeclKind::Enum, DeclKind::Struct, DeclKind::Class, DeclKind::Protocol>(D, topNominals)
        || take<ValueDecl, DeclKind::TypeAlias, DeclKind::Var, DeclKind::Func, DeclKind::Accessor>(D, topValues);
      }
      findNominalsFromExtensions();
      findNominalsInTopNominals();
      findValuesInExtensions();
      findClassMembers(SF);
    }
  private:
    void findNominalsFromExtensions() {
      for (auto *ED: extensions)
        findNominalsAndOperatorsIn(ED->getExtendedNominal());
    }
    void findNominalsInTopNominals() {
      for (const auto *const NTD: topNominals)
        findNominalsAndOperatorsIn(NTD);
    }
    void findNominalsAndOperatorsIn(const NominalTypeDecl *const NTD) {
      allNominals.push_back(NTD);
      findNominalsAndOperatorsInMembers(NTD->getMembers());
    }
    void findNominalsAndOperatorsInMembers(const DeclRange members) {
      for (const Decl *const D: members) {
        if (dyn_cast<ValueDecl>(D)->getFullName().isOperator())
          memberOperatorDecls.push_back(cast<FuncDecl>(D));
        else if (const auto *const NTD = dyn_cast<NominalTypeDecl>(D))
          findNominalsAndOperatorsIn(NTD);
      }
    }
    void findValuesInExtensions() {
      for (const auto* ED: extensions) {
        for (const auto *member: ED->getMembers())
          if (const auto *VD = dyn_cast<ValueDecl>(member))
            if (VD->hasName())
              valuesInExtensions.push_back(VD);
      }
    }
    void findClassMembers(const SourceFile *const SF) {
      struct Collector: public VisibleDeclConsumer {
        CPVec<ValueDecl> &classMembers;
        Collector(CPVec<ValueDecl> &classMembers) : classMembers(classMembers) {}
        void foundDecl(ValueDecl *VD, DeclVisibilityKind) override {
          classMembers.push_back(VD);
        }
      } collector {classMembers};
      SF->lookupClassMembers({}, collector);
    }
  };
}




////////////



namespace {
  class GraphConstructor {
    SourceFile *SF;
    const DependencyTracker &depTracker;
    StringRef outputPath;
    FrontendNode *sourceFileNode;
 
  public:
     GraphConstructor(
                     SourceFile *SF,
                     const DependencyTracker &depTracker,
                     StringRef outputPath) : SF(SF), depTracker(depTracker), outputPath(outputPath) {}
  private:
    FrontendGraph g;
    
  public:
    FrontendGraph construct() {
      //TODO storage mgmt
      sourceFileNode = g.addNode(
                                 NodeDependencyKey(NodeKind::sourceFileProvide, outputPath, ""), getInterfaceHash(), FrontendNode::Location::Here);
      
      addProviderNodesToGraph(); // must preceed dependencies for cascades
      addDependencyArcsToGraph();
      
      return std::move(g);
    }
    
  private:
    std::string getInterfaceHash() const {
      llvm::SmallString<32> interfaceHash;
      SF->getInterfaceHash(interfaceHash);
      return interfaceHash.str().str();
    }
    
    void addProviderNodesToGraph();
    void addDependencyArcsToGraph();
    
    template <typename DeclT>
    static std::string computeContextNameOfMember(const DeclT *member) {
      auto *context = member->getDeclContext();
      auto *containingDecl = context ? context->getAsDecl() : nullptr;
      const auto * NTD = dyn_cast<NominalTypeDecl>(containingDecl);
      return mangleTypeAsContext(NTD);
    }
    
    template <typename DeclT>
    void addOneTypeOfProviderNodesToGraph(CPVec<DeclT> &decls, NodeKind kind, std::string(*nameFn)(const DeclT *)) {
      for (const auto* D: decls) {
        std::string nameForHolderOfMember{};
        // native nodes have non-empty container names
        // TODO: centralize this invariant
        g.addNode(NodeDependencyKey(
                                    kind,
                                    (*nameFn)(D),
                                    kind == NodeKind::member ? computeContextNameOfMember(D) : sourceFileNode->getNameForDependencies()
                                    ),
                  "",
                  FrontendNode::Location::Here);
      }
    }
    /// name converters
    template <typename DeclT>
    static std::string getBaseName(const DeclT *decl) { return decl->getBaseName().userFacingName(); }
    
    template <typename DeclT>
    static std::string getName(const DeclT *decl) { return DeclBaseName(decl->getName()).userFacingName(); }
    
    static std::string mangleTypeAsContext(const NominalTypeDecl * NTD) {
      Mangle::ASTMangler Mangler;
      return Mangler.mangleTypeAsContextUSR(NTD);
    }
    
    template<NodeKind kind>
    void addOneTypeOfDependencyToGraph(const llvm::DenseMap<DeclBaseName, bool>& map) {
      for (const auto &p: map)
        addToGraphThatThisWholeFileDependsUpon(kind, "", p.first.userFacingName(), p.second);
    }
    
    void addOneTypeOfDependencyToGraph(
                                       const llvm::DenseMap<
                                       std::pair<const NominalTypeDecl *, DeclBaseName>,
                                       bool> &);
    
    void addOneTypeOfDependencyToGraph(ArrayRef<std::string> externals) {
      for (const auto &s: externals)
        addToGraphThatThisWholeFileDependsUpon(NodeKind::externalDepend, "", s, true);
    }
    
    void addToGraphThatThisWholeFileDependsUpon(NodeKind,
                                                const std::string &nameForHolderOfMember,
                                                const std::string &dependeeNameIfNotEmpty,
                                                bool cascades);
  };
}

void GraphConstructor::addProviderNodesToGraph() {
  SourceFileDeclDemux demux(SF);
   // TODO: express the multiple provides and depends streams with variadic templates
  addOneTypeOfProviderNodesToGraph(demux.precedenceGroups, NodeKind::topLevel, getName);
  addOneTypeOfProviderNodesToGraph(demux.memberOperatorDecls, NodeKind::topLevel, getName);
  addOneTypeOfProviderNodesToGraph(demux.operators, NodeKind::topLevel, getName);
  addOneTypeOfProviderNodesToGraph(demux.topNominals, NodeKind::topLevel, getName);
  addOneTypeOfProviderNodesToGraph(demux.topValues, NodeKind::topLevel, getBaseName);
  
  addOneTypeOfProviderNodesToGraph(demux.allNominals, NodeKind::nominals, mangleTypeAsContext);
  addOneTypeOfProviderNodesToGraph(demux.allNominals, NodeKind::blankMembers, mangleTypeAsContext); // TODO: fix someday
  
  addOneTypeOfProviderNodesToGraph(demux.valuesInExtensions, NodeKind::member, getBaseName);
  
  // could optimize by uniqueing by name, but then what of container?
  addOneTypeOfProviderNodesToGraph(demux.classMembers, NodeKind::dynamicLookup, getBaseName);
}

void GraphConstructor::addOneTypeOfDependencyToGraph(
                                                     const llvm::DenseMap<std::pair<const NominalTypeDecl *, DeclBaseName>,
                                                     bool> &map) {
  std::unordered_set<const NominalTypeDecl*> holdersOfCascadingMembers;
  for (auto &entry: map)
    if (entry.second)
      holdersOfCascadingMembers.insert(entry.first.first);
  for (auto &entry: map) {
    const std::string mangledTypeAsContext = mangleTypeAsContext(entry.first.first);
    addToGraphThatThisWholeFileDependsUpon(NodeKind::nominals,
                                           "", // nominal name IS the holder
                                           mangledTypeAsContext,
                                           holdersOfCascadingMembers.count(entry.first.first) != 0);
    const bool isMemberBlank = entry.first.second.empty();
    addToGraphThatThisWholeFileDependsUpon(isMemberBlank ? NodeKind::blankMembers : NodeKind::member,
                  mangledTypeAsContext,
                  isMemberBlank ? "" : entry.first.second.userFacingName(),
                  entry.second);
  }
}


  // TODO: express the multiple provides and depends streams with variadic templates

void GraphConstructor::addDependencyArcsToGraph() {
  addOneTypeOfDependencyToGraph<NodeKind::topLevel>(SF->getReferencedNameTracker()->getTopLevelNames());
  addOneTypeOfDependencyToGraph(SF->getReferencedNameTracker()->getUsedMembers());
  addOneTypeOfDependencyToGraph<NodeKind::dynamicLookup>(SF->getReferencedNameTracker()->getDynamicLookupNames());
  addOneTypeOfDependencyToGraph(depTracker.getDependencies());
}

void GraphConstructor::addToGraphThatThisWholeFileDependsUpon(NodeKind kind,
                                                              const std::string &nameForHolderOfMember,
                                                              const std::string &dependeeNameIfNotEmpty,
                                                              bool cascades) {
  // foreign nodes have empty container names
  // TODO: centralize this invarient
  FrontendNode *dependee = g.addNode(
                                     NodeDependencyKey(kind,
                                                       dependeeNameIfNotEmpty,
                                                       nameForHolderOfMember),
                                     "",
                                     FrontendNode::Location::Elsewhere);
  if (!cascades)
    g.addArc(sourceFileNode, dependee);
  else
    g.addArcFromEveryNodeHereTo(dependee);
}


  
  
  

class YAMLEmitter {
private:
  llvm::raw_ostream &out;
  
public:
  YAMLEmitter(llvm::raw_ostream &out) : out(out) {}
  
  void newNode() const { out << "-\n"; }
  
  void entry(const std::string &s) const {
    out << " - \"" << llvm::yaml::escape(s) << "\"\n";
  }
  void entry(size_t n) const {
    out << " - " << n << "\n";
  }
  void entry(std::vector<size_t> &numbers) const {
    if (numbers.empty()) {
      out << " - []\n";
      return;
    }
    out << " - \n";
    for (auto i: numbers)
      out << "  - " << i << "\n";
  }
};



//////////////////////////
namespace {

  template <typename Emitter>
  class GraphEmitter {
  private:
    const FrontendGraph &g;
    Emitter emitter;
  public:
    GraphEmitter(const FrontendGraph& g, llvm::raw_ostream &out) : g(g), emitter(Emitter(out)) {}
  public:
    void emit() const {
      // FIXME: emits info for each arc twice
      std::for_each(g.nodesBegin(), g.nodesEnd(), [&](const FrontendNode* n) {emitNode(n); });
    }
    void emitNode(const FrontendNode*) const;
  };
}
template <>
void GraphEmitter<YAMLEmitter>::emitNode(const FrontendNode* n) const {
  emitter.newNode();
  auto nn = const_cast<FrontendNode*>(n);
  nn->serialize(
               [&](size_t s) {emitter.entry(s);},
               [&](std::string &s) {emitter.entry(s);},
               [&](std::vector<size_t> &s) {emitter.entry(s);});
}




/// Entry point to this whole file:

bool swift::experimental_dependencies::emitReferenceDependencies(
                                                                 DiagnosticEngine &diags, SourceFile *const SF,
                                                                 const DependencyTracker &depTracker, StringRef outputPath) {
  
  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out)  {
    GraphConstructor gc(SF, depTracker, outputPath);
    FrontendGraph g = gc.construct();
    GraphEmitter<YAMLEmitter>(g, out).emit();
    return false;
  });

}

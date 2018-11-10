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

using MemoizedNodeKey = std::tuple<std::string, std::string, Node::Kind>;
template <>
struct std::hash<Node::Kind> : public unary_function<Node::Kind, size_t> {
  size_t operator()(const Node::Kind k) const { return (size_t)(k); }
};
static MemoizedNodeKey createMemoizedKey(Node::Kind kind,
                                         std::string nameForDependencies,
                                         std::string nameForHolderOfMember) {
  return std::make_tuple(nameForHolderOfMember, nameForDependencies, kind);
}

template <>
struct std::hash<MemoizedNodeKey>
: public unary_function<MemoizedNodeKey, size_t>
{
  size_t operator()(const MemoizedNodeKey key) const {
    return std::hash<std::string>()(std::get<0>(key)) ^
    std::hash<std::string>()(std::get<1>(key)) ^
    std::hash<Node::Kind>()(std::get<2>(key));
  }
};



namespace {
  /// Memoize nodes serving as heads of dependency arcs:
  /// Could be a definition in another file that a lookup here depends upon,
  /// or could be definition in this file that a lookup here depends upon.
  
  class MemoizedNode: public Node {
    static std::unordered_map<MemoizedNodeKey, MemoizedNode*> cache;
    
    MemoizedNode(Kind kind,
                 std::string nameForDependencies,
                 std::string nameForHolderOfMember) :
    Node(kind, nameForDependencies, nameForHolderOfMember) {}
    
  public:
    MemoizedNodeKey memoizedKey() const {
      return createMemoizedKey(getKind(), getNameForDependencies(), getNameForHolderOfMember());
    }
    static MemoizedNode *create(Kind kind,
                                std::string nameForDependencies,
                                std::string nameForHolderOfMember) {
      auto key = createMemoizedKey(kind, nameForDependencies, nameForHolderOfMember);
      auto iter = cache.find(key);
      if (iter != cache.end())
        return iter->second;
      auto node = new MemoizedNode(kind, nameForDependencies, nameForHolderOfMember);
      cache.insert(std::make_pair(key, node));
      return node;
    }
    static decltype(cache)::const_iterator begin() { return cache.cbegin(); }
    static decltype(cache)::const_iterator end()   { return cache.cend(); }
  };
}

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
    MemoizedNode *sourceFileNode;
    
    GraphConstructor(
                     SourceFile *SF,
                     const DependencyTracker &depTracker,
                     StringRef outputPath) : SF(SF), depTracker(depTracker), outputPath(outputPath) {}
    
    Graph g;
    void construct() {
      //TODO storage mgmt
      sourceFileNode = MemoizedNode::create(Node::Kind::sourceFileProvide, outputPath, "");
      g.addNode(sourceFileNode);
      
      addProviderNodesToGraph(); // must preceed dependencies for cascades
      addDependencyArcsToGraph();
    }
    
  private:
    std::string getInterfaceHash() const {
      llvm::SmallString<32> interfaceHash;
      SF->getInterfaceHash(interfaceHash);
      return interfaceHash.str().str();
#error where to add this? source file node fingerprint
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
    void addOneTypeOfProviderNodesToGraph(CPVec<DeclT> &decls, Node::Kind kind, std::string(*nameFn)(const DeclT *)) {
      for (const auto* D: decls) {
        std::string nameForHolderOfMember{};
        MemoizedNode::create(kind, (*nameFn)(D), kind == Node::Kind::member ? computeContextNameOfMember(D) : "");
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
    
    template<Node::Kind kind>
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
        addToGraphThatThisWholeFileDependsUpon(Node::Kind::externalDepend, "", s, true);
    }
    
    void addToGraphThatThisWholeFileDependsUpon(Node::Kind,
                                                const std::string &nameForHolderOfMember,
                                                const std::string &dependedUponNameIfNotEmpty,
                                                bool cascades);
  };
}

void GraphConstructor::addProviderNodesToGraph() {
  SourceFileDeclDemux demux(SF);
  
  addOneTypeOfProviderNodesToGraph<PrecedenceGroupDecl>(demux.precedenceGroups, Node::Kind::topLevel, getName);
  addOneTypeOfProviderNodesToGraph<FuncDecl>(demux.memberOperatorDecls, Node::Kind::topLevel, getName);
  addOneTypeOfProviderNodesToGraph<OperatorDecl>(demux.operators, Node::Kind::topLevel, getName);
  addOneTypeOfProviderNodesToGraph<NominalTypeDecl>(demux.topNominals, Node::Kind::topLevel, getName);
  addOneTypeOfProviderNodesToGraph<ValueDecl>(demux.topValues, Node::Kind::topLevel, getBaseName);
  
  addOneTypeOfProviderNodesToGraph<NominalTypeDecl>(demux.allNominals, Node::Kind::nominals, mangleTypeAsContext);
  addOneTypeOfProviderNodesToGraph<NominalTypeDecl>(demux.allNominals, Node::Kind::blankMembers, mangleTypeAsContext); // TODO: fix someday
  
  addOneTypeOfProviderNodesToGraph<ValueDecl>(demux.valuesInExtensions, Node::Kind::member, getBaseName);
  
  // could optimize by uniqueing by name, but then what of container?
  addOneTypeOfProviderNodesToGraph<ValueDecl>(demux.classMembers, Node::Kind::dynamicLookup, getBaseName);
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
    addToGraphThatThisWholeFileDependsUpon(Node::Kind::nominals,
                                           "", // nominal name IS the holder
                                           mangledTypeAsContext,
                                           holdersOfCascadingMembers.count(entry.first.first) != 0);
    const bool isMemberBlank = entry.first.second.empty();
    addToGraphThatThisWholeFileDependsUpon(isMemberBlank ? Node::Kind::blankMembers : Node::Kind::member,
                  mangledTypeAsContext,
                  isMemberBlank ? "" : entry.first.second.userFacingName(),
                  entry.second);
  }
}


  // TODO: express the multiple provides and depends streams with variadic templates

void GraphConstructor::addDependencyArcsToGraph() {
  addOneTypeOfDependencyToGraph<Node::Kind::topLevel>(SF->getReferencedNameTracker()->getTopLevelNames());
  addOneTypeOfDependencyToGraph(SF->getReferencedNameTracker()->getUsedMembers());
  addOneTypeOfDependencyToGraph<Node::Kind::dynamicLookup>(SF->getReferencedNameTracker()->getDynamicLookupNames());
  addOneTypeOfDependencyToGraph(depTracker.getDependencies());
}

void GraphConstructor::addToGraphThatThisWholeFileDependsUpon(Node::Kind kind,
                                                              const std::string &nameForHolderOfMember,
                                                              const std::string &dependedUponNameIfNotEmpty,
                                                              bool cascades) {
  MemoizedNode *whatIsDependedUpon = MemoizedNode::create(kind, dependedUponNameIfNotEmpty, nameForHolderOfMember);
  if (!cascades)
    g.addArc(Arc{sourceFileNode, whatIsDependedUpon});
  else
    std::for_each(MemoizedNode::begin(), MemoizedNode::end(),
                  [&](MemoizedNode *providingNode) {
                    g.addArc(Arc{providingNode, whatIsDependedUpon});
                  });
}


  
  
  

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
  


  



// move filter etc into wherever
// do depend decls
// do output


////////////////////////


namespace {
  class FileRenamerAndWriter {
    DiagnosticEngine &diags;
    std::string outputPath;
    
  public:
    FileRenamerAndWriter(DiagnosticEngine &diags, StringRef outputPath) :diags(diags), outputPath(outputPath.str())
     {}
    
    /// Returns true on error
    bool operator() (ArrayRef<std::string> strings) {
      // Before writing to the dependencies file path, preserve any previous file
      // that may have been there. No error handling -- this is just a nicety, it
      // doesn't matter if it fails.
      llvm::sys::fs::rename(outputPath, outputPath + "~");
      return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
        for (auto s: strings) out << s;
        return false;
      } );
    }
    
  };
}


//////////////////////////







/// Entry point to this whole file:

bool swift::experimental_dependencies::emitReferenceDependencies(
                                                                 DiagnosticEngine &diags, SourceFile *const SF,
                                                                 const DependencyTracker &depTracker, StringRef outputPath) {
  
  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out)  {
    abort();
    return false;
  });

}

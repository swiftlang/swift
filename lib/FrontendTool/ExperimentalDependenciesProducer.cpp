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

static std::string mangleTypeAsContext(const NominalTypeDecl * NTD) {
  Mangle::ASTMangler Mangler;
  return Mangler.mangleTypeAsContextUSR(NTD);
}


////////////




//////////////
  



////////////////////////////


using MemberTableEntryTy = std::pair<ReferencedNameTracker::MemberPair, bool>;

namespace {

  struct DependProtoNode {
    /// nil means either no container for the file, or topLevel
    std::string holderName;
    std::string memberName;
    bool cascades;
    Node::Kind kind;

    DependProtoNode(const NominalTypeDecl *holder,
                         DeclBaseName member,
                         const bool cascades,
                         const Node::Kind kind) :
    DependProtoNode(holder ? mangleTypeAsContext(holder) : std::string(), member.userFacingName(), cascades, kind) {}
    
    DependProtoNode(const std::string holderName,
                    const std::string memberName,
                    bool cascades,
                    const Node::Kind kind) :
    holderName(holderName), memberName(memberName), cascades(cascades), kind(kind) {}
  };
}


namespace {
  class DependNames {
  private:
    const ReferencedNameTracker *const tracker;

    std::vector<DependProtoNode> _members;
    std::unordered_set<std::string>cascadingHolders;
    std::vector<DependProtoNode> holdersWithMergedCascades;
    std::vector<DependProtoNode> sortedDynamicLookupNames;
    std::vector<DependProtoNode> externalNames;



    void convertSetOfDeclBaseNames(const llvm::DenseMap<DeclBaseName, bool> &map, Node::Kind);
    static std::unordered_set<std::string>
    findCascadingHolders(
                         std::vector<DependProtoNode>::const_iterator,
                         std::vector<DependProtoNode>::const_iterator);
    void mergeCascades(std::vector<DependProtoNode>::const_iterator,
                       std::vector<DependProtoNode>::const_iterator,
                       const std::unordered_set<std::string> &cascades);
    void convertArrayOfFilenames(const ArrayRef<std::string> in);
    void convertSetOfMemberPairs(const llvm::DenseMap<std::pair<const NominalTypeDecl *, DeclBaseName>, bool>&);

  public:
    DependNames(const SourceFile *const SF, const DependencyTracker &depTracker) :
    tracker(SF->getReferencedNameTracker())
    {
      convertSetOfDeclBaseNames(tracker->getTopLevelNames(), Node::Kind::topLevel);
      auto beginMembers = names.cend();
convertSetOfMemberPairs(tracker->getUsedMembers());
      auto endMembers = names.cend();
      auto cascadingHolders = findCascadingHolders(beginMembers, endMembers);
      mergeCascades(beginMembers, endMembers, cascadingHolders);
      convertSetOfDeclBaseNames(tracker->getDynamicLookupNames(), Node::Kind::dynamicLookup); // should sort/uniq for efficiency
      convertArrayOfFilenames(depTracker.getDependencies());
    }
 
    std::vector<DependProtoNode> names;
  };
}

//////////////////////

void DependNames::convertSetOfDeclBaseNames(const llvm::DenseMap<DeclBaseName, bool> &map, const Node::Kind kind) {
  for (const auto &p: map) {
    names.push_back(DependProtoNode{nullptr, p.getFirst(), p.getSecond(), kind}); // context container??
  }
}

void
DependNames::convertSetOfMemberPairs(const llvm::DenseMap<
                                     std::pair<const NominalTypeDecl *, DeclBaseName
                                     >,
                                     bool>& memberMap) {
  for (auto &entry: memberMap) {
    names.push_back(DependProtoNode(entry.first.first, entry.first.second, entry.second, Node::Kind::member));
  }
}





std::unordered_set<std::string>
DependNames::findCascadingHolders(
                                  std::vector<DependProtoNode>::const_iterator beginMembers,
                                  std::vector<DependProtoNode>::const_iterator endMembers) {
  std::unordered_set<std::string> out;
  std::for_each(beginMembers, endMembers,
                [&] (const DependProtoNode &n) {
                  if (n.cascades && !n.holderName.empty())
                    out.insert(n.holderName);
                });
  return out;
}

void
DependNames::mergeCascades(
                           std::vector<DependProtoNode>::const_iterator beginMembers,
                           std::vector<DependProtoNode>::const_iterator endMembers,
                           const std::unordered_set<std::string> &cascades) {
  std::transform(beginMembers, endMembers, std::back_inserter(names),
                 [&] (const DependProtoNode &member)->DependProtoNode {
                   const bool cascade = cascades.count(member.holderName) != 0 ? true : false;
                   // TODO: COuld have holder here, since had NTD originally
                   return DependProtoNode(nullptr, member.holderName, cascade, Node::Kind::nominals);
                 });
}

void DependNames::convertArrayOfFilenames(const ArrayRef<std::string> elts) {
  std::vector<std::string> tmp(elts.begin(), elts.end());
  std::sort(tmp.begin(), tmp.end(), [](const std::string &a,
                                       const std::string &b) -> bool {
    return std::lexicographical_compare(a.rbegin(), a.rend(),
                                        b.rbegin(), b.rend());
  });
  for (auto s: tmp)
    names.push_back(DependProtoNode(nullptr, s, true, Node::Kind::externalDepend));
}


class GraphConstructor {
  SourceFile *SF;
  const DependencyTracker &depTracker;
  StringRef outputPath;
  DeclNode *sourceFileNode;
  
  GraphConstructor(
                   SourceFile *SF,
                   const DependencyTracker &depTracker,
                   StringRef outputPath) : SF(SF), depTracker(depTracker), outputPath(outputPath) {}
  
  Graph g;
  void construct() {
    //TODO storage mgmt
    sourceFileNode = new DeclNode(DeclNode::Kind::sourceFileProvide, SF->getAsDecl(), nullptr, getInterfaceHash(), outputPath);
    g.addNode(sourceFileNode);
    
    constructProvidesNodes();
    constructDependArcs();
  }
  
private:
  std::unordered_map<const Decl*, Node*> nodesByDecl;
  std::array<llvm::StringMap<Node*>, uint(Node::Kind::end)> nodesByKindAndName{};
  
  std::string getInterfaceHash() const {
    llvm::SmallString<32> interfaceHash;
    SF->getInterfaceHash(interfaceHash);
    return interfaceHash.str().str();
  }
  
  void constructProvidesNodes();
  void constructDependArcs();
  void constructDependArcs(const std::vector<DependProtoNode> &);
  
  
  
  /// name converters
  template <typename DeclT>
  static std::string getBaseName(DeclT const* D) { return D->getBaseName().userFacingName(); }
  
  template <typename DeclT>
  static std::string getName(DeclT const* D) { return DeclBaseName(D->getName()).userFacingName(); }
  
  
  
  template <typename DeclT>
  void createNodes(CPVec<DeclT> &decls, DeclNode::Kind kind, std::string(*nameFn)(const DeclT *)) {
    for (const auto* D: decls) {
      auto *context = D->getDeclContext();
      auto *containingDecl = context ? context->getAsDecl() : nullptr;
      auto iter = nodesByDecl.find(containingDecl);
      assert(iter != nodesByDecl.end() && "missing container");
      Node *containingNode = iter->second;
      addDeclNode(D, new DeclNode(kind, D, containingNode, "", (*nameFn)(D)));
    }
  }
  
  void addDeclNode(const Decl* D, DeclNode *node) {
    bool inserted = nodesByDecl.insert(std::make_pair(D, node)).second;
    assert(inserted && "dup node?");
    inserted = nodesByKindAndName[uint(node->kind)].insert(std::make_pair(node->nameForDependencies, node)).second;
    assert(inserted && "dup node??");
    g.addNode(node);
  }
  
  Node* getOrCreateDependencyHeadNode(const DependProtoNode & protoNode) {
    if (protoNode.holderName.empty())
      return sourceFileNode; // no better info at present
    auto &map = nodesByKindAndName[uint(protoNode.kind)];
    auto iter = map.find(protoNode.holderName);
    return iter == map.end() ? sourceFileNode : iter->second;
  }
};

void GraphConstructor::constructProvidesNodes() {
  SourceFileDeclDemux demux(SF);
  
  createNodes<PrecedenceGroupDecl>(demux.precedenceGroups, DeclNode::Kind::topLevel, getName);
  createNodes<FuncDecl>(demux.memberOperatorDecls, DeclNode::Kind::topLevel, getName);
  createNodes<OperatorDecl>(demux.operators, DeclNode::Kind::topLevel, getName);
  createNodes<NominalTypeDecl>(demux.topNominals, DeclNode::Kind::topLevel, getName);
  createNodes<ValueDecl>(demux.topValues, DeclNode::Kind::topLevel, getBaseName);
  
  createNodes<NominalTypeDecl>(demux.allNominals, DeclNode::Kind::nominals, mangleTypeAsContext);
  createNodes<NominalTypeDecl>(demux.allNominals, DeclNode::Kind::blankMembers, mangleTypeAsContext); // TODO: fix someday

  createNodes<ValueDecl>(demux.valuesInExtensions, DeclNode::Kind::member, getBaseName);
  
  // could optimize by uniqueing by name, but then what of container?
  createNodes<ValueDecl>(demux.classMembers, DeclNode::Kind::dynamicLookup, getBaseName);
}
void GraphConstructor::constructDependArcs() {
  DependNames dn(SF, depTracker);
  constructDependArcs(dn.names);
}

void GraphConstructor::constructDependArcs(const std::vector<DependProtoNode> & protoNodes) {
  for (auto &protoNode: protoNodes) {
    Node* def = getOrCreateDependencyHeadNode(protoNode);
    Node* use = sourceFileNode;
    g.addArc(Arc{use, def});
  }
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

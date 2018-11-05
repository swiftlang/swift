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






// shorthands

using StringVec = std::vector<std::string>;
template <typename T> using CPVec = std::vector<const T*>;
template <typename T1 = std::string, typename T2 = std::string> using PairVec = std::vector<std::pair<T1, T2>>;
template <typename T1, typename T2> using CPPairVec = std::vector<std::pair<const T1*, const T2*>>;

//TODO: elim virtuals with templating>

namespace {
  template <typename ElementT>
  class Source {
  public:
    typedef ElementT Element;
    virtual Optional<Element> next() = 0;
    virtual ~Source() = default;
  };
  
  template <typename FeederT>
  class Sink {
    FeederT &feeder;
  public:
    Sink(FeederT &feeder) : feeder(feeder) {}
    void run() {
      for (;;) {
        auto in = feeder.next();
        if (!in)
          return;
        process(in);
      }
    }
    virtual void process(typename FeederT::Element &in) = 0;
  };
  
  template <typename ElementT, typename FeederT>
  class Transducer: public Source<ElementT> {
    FeederT &feeder;
  public:
    Transducer(FeederT &feeder) : feeder(feeder) {}
    Optional<ElementT> next() override {
      auto in = feeder.next();
      if (!in) return None;
      return process(in);
    }
    virtual Optional<ElementT> process(typename FeederT::Element &in) = 0;
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
    CPVec<NominalTypeDecl> nominals;
    CPVec<ValueDecl> values;
    
    SourceFileDeclDemux(const SourceFile *const SF) {
      for (const Decl *const D: SF->Decls) {
        take<ExtensionDecl, DeclKind::Extension>(D, extensions)
        || take<OperatorDecl, DeclKind::InfixOperator, DeclKind::PrefixOperator, DeclKind::PostfixOperator>(D, operators)
        || take<PrecedenceGroupDecl, DeclKind::PrecedenceGroup> (D, precedenceGroups)
        || take<NominalTypeDecl, DeclKind::Enum, DeclKind::Struct, DeclKind::Class, DeclKind::Protocol>(D, nominals)
        || take<ValueDecl, DeclKind::TypeAlias, DeclKind::Var, DeclKind::Func, DeclKind::Accessor>(D, values);
      }
    }
  };
  
  template <typename ContainerT>
  class ContainerSource: public Source<typename ContainerT::value_type> {
    typename ContainerT::const_iterator _next, end;
  public:
    typedef typename ContainerT::value_type Element;
    ContainerSource(ContainerT container) :
    _next(container.cbegin()),
    end(container.cend()) {}
    Optional<Element> next() override {
      return _next == end ? Optional<Element>(None) : Optional<Element>(*_next++);
    }
    ContainerSource() = default;
  };
}
    




namespace {
  class CommonDeclHelpers {
  private:
    static bool protoIsVisible(ProtocolType *proto) {
      return memberIsVisible(proto->getDecl());
    }
    static bool conformedProtocolIsVisible(TypeLoc inheritedType) {
      auto type = inheritedType.getType();
      if (!type)
        return false;
      if (!type->isExistentialType()) {
        // Be conservative. We don't know how to deal with other extended types.
        return true;
      }
      auto layout = type->getExistentialLayout();
      assert(!layout.explicitSuperclass && "Should not have a subclass existential "
             "in the inheritance clause of an extension");
      return std::any_of(layout.getProtocols().begin(), layout.getProtocols().end(),protoIsVisible);
    }

  protected:
    template <typename DeclT>
    static bool isVisible(const DeclT *const D) {
      return D  &&  D->getFormalAccess() > AccessLevel::FilePrivate;
    }
    static bool memberIsVisible(const Decl *member) {
      if (auto *VD = dyn_cast<ValueDecl>(member))
        return isVisible(VD);
      
      switch (member->getKind()) {
        case DeclKind::Import:
        case DeclKind::PatternBinding:
        case DeclKind::EnumCase:
        case DeclKind::TopLevelCode:
        case DeclKind::IfConfig:
        case DeclKind::PoundDiagnostic:
          return false;
          
        case DeclKind::Extension:
        case DeclKind::InfixOperator:
        case DeclKind::PrefixOperator:
        case DeclKind::PostfixOperator:
          return true;
          
        default:
          llvm_unreachable("everything else is a ValueDecl");
      }
    }
    static bool introducesConformanceToVisibleProtocol(const ExtensionDecl *const ED) {
      return std::any_of(ED->getInherited().begin(), ED->getInherited().end(),
                         conformedProtocolIsVisible);
    }
  };
}


    
 
   /////////////////////////
    
  
        
namespace {
  struct InnerDeclCollector: CommonDeclHelpers {
    // Records every nominal declaration, and whether or not the declaration
    /// changes the externally-observable shape of the type.
    llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
    
    /// Records operator declarations so they can be included as top-level
    /// declarations.
    CPVec<FuncDecl> memberOperatorDecls;
    
    /// Records extension declarations which are not introducing a conformance
    /// to a public protocol and add a public member.
    CPVec<ExtensionDecl> extensionsWithJustMembers;
    
    InnerDeclCollector(const CPVec<ExtensionDecl> &filteredExtensions, const CPVec<NominalTypeDecl> &filteredTopNominals);
    
    /// Recursively computes the transitive closure over members
    /// adding memberOperatorDecls and extendedNominals to the receiver.
    void findNominalsAndOperators(const DeclRange members);
  };
}


InnerDeclCollector::InnerDeclCollector(const CPVec<ExtensionDecl> &filteredExtensions, const CPVec<NominalTypeDecl> &filteredTopNominals) {
  for (auto *ED: filteredExtensions) {
    if (!introducesConformanceToVisibleProtocol(ED))
      extensionsWithJustMembers.push_back(ED);
    extendedNominals[ED->getExtendedNominal()] |= introducesConformanceToVisibleProtocol(ED);
    findNominalsAndOperators(ED->getMembers());
  }
  for (auto *NTD: filteredTopNominals) {
    extendedNominals[NTD] |= true;
    findNominalsAndOperators(NTD->getMembers());
  }
}

/// Recursively computes the transitive closure over members
/// adding memberOperatorDecls and extendedNominals to the receiver.
void InnerDeclCollector::findNominalsAndOperators(const DeclRange members) {
  CPVec<Decl> visibleValues;
  std::copy_if(members.begin(), members.end(), std::back_inserter(visibleValues),
               [](const Decl *const D) -> bool { return isVisible(dyn_cast<ValueDecl>(D)); } );
  for (auto *D: visibleValues) {
    if (dyn_cast<ValueDecl>(D)->getFullName().isOperator())
      memberOperatorDecls.push_back(cast<FuncDecl>(D));
    else if (auto nominal = dyn_cast<NominalTypeDecl>(D)) {
      extendedNominals[nominal] |= true;
      findNominalsAndOperators(nominal->getMembers());
    }
  }
}

  /////////////////////////
  
namespace  {

  class ProviderDecls: CommonDeclHelpers {
  private:
    SourceFileDeclDemux tops;
    CPVec<ExtensionDecl> filteredExtensions;
    CPVec<NominalTypeDecl> filteredTopNominals;
    CPVec<ValueDecl> filteredTopValues;
    InnerDeclCollector innerDeclCollector;
    CPVec<NominalTypeDecl> _extendedNominalsThatCanChangeExernallyObservableShape;
    CPVec<NominalTypeDecl> allExtendedNominals;
    std::vector<std::pair<const NominalTypeDecl*, const ValueDecl*>> _holdersAndMembers;
    CPVec<ValueDecl> _classMembers;
    
    
    static CPVec<NominalTypeDecl> filterMap(llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals);

    // helpers
    template <typename DeclT>
    static CPVec<DeclT> filter(const CPVec<DeclT> &in);
    
    static bool hasVisibleMember(const ExtensionDecl *const ED) {
      return std::any_of(ED->getMembers().begin(), ED->getMembers().end(), memberIsVisible);
    }

    static CPVec<ExtensionDecl>  filterExtensions(const CPVec<ExtensionDecl> &in);
    static CPVec<NominalTypeDecl> getKeysOf(const llvm::MapVector<const NominalTypeDecl *, bool>& extendedNominals);
    static std::vector<std::pair<const NominalTypeDecl*, const ValueDecl*>>
    constructHoldersAndMembers(const CPVec<ExtensionDecl> &in );
    
    static CPVec<ValueDecl> findClassMembers(const SourceFile* SF);
    
    template <typename DeclT>
    static bool hasVisibleName(const DeclT *const D) {
      return D->hasName() && isVisible(D);
    }
    static bool extendsVisibleNominal(const ExtensionDecl *const ED) {
      return isVisible(ED->getExtendedNominal());
    }

    
  public:
    ProviderDecls(const SourceFile *SF) :
    tops(SF),
    filteredExtensions(filterExtensions(tops.extensions)),
    filteredTopNominals(filter(tops.nominals)),
    filteredTopValues(filter(tops.values)),
    innerDeclCollector(filteredExtensions, filteredTopNominals),
    _extendedNominalsThatCanChangeExernallyObservableShape(filterMap(innerDeclCollector.extendedNominals)),
    allExtendedNominals(getKeysOf(innerDeclCollector.extendedNominals)),
    _holdersAndMembers(constructHoldersAndMembers(innerDeclCollector.extensionsWithJustMembers)),
    _classMembers(findClassMembers(SF))
    { }
    // Tops:
    const CPVec<NominalTypeDecl>& topNominals() const { return filteredTopNominals; }
    const CPVec<PrecedenceGroupDecl>& precedenceGroups() const { return tops.precedenceGroups; }
    const CPVec<OperatorDecl>& topOperators() const {return tops.operators; }
    const CPVec<ValueDecl>& topValues() const {return filteredTopValues; }
    const CPVec<FuncDecl>& operatorFunctions() const { return innerDeclCollector.memberOperatorDecls; }
    
    // Nominals:
    const CPVec<NominalTypeDecl>& extendedNominalsThatCanChangeExernallyObservableShape() const {
      return _extendedNominalsThatCanChangeExernallyObservableShape;
    }
    
    // Members:
    const CPVec<NominalTypeDecl>& extendedNominalsThatCouldAddMembers() const {
      return allExtendedNominals;
    }
    const std::vector<std::pair<const NominalTypeDecl*, const ValueDecl*>>& c() const {
      return _holdersAndMembers;
    }
    
    // Dynamic lookup:
    const CPVec<ValueDecl>& classMembers() const { return _classMembers; }
  };
}


CPVec<NominalTypeDecl> ProviderDecls::filterMap(llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals) {
  CPVec<NominalTypeDecl> out;
  for (const auto entry: extendedNominals)
    if (entry.second)
      out.push_back(entry.first);
  return out;
}

template <typename DeclT>
CPVec<DeclT> ProviderDecls::filter(const CPVec<DeclT> &in) {
  CPVec<DeclT> out;
  std::copy_if(in.begin(), in.end(), std::back_inserter(out), hasVisibleName<DeclT>);
  return out;
}

CPVec<ExtensionDecl>  ProviderDecls::filterExtensions(const CPVec<ExtensionDecl> &in)  {
  CPVec<ExtensionDecl> out;
  std::copy_if(in.begin(), in.end(), std::back_inserter(out),
               [](const ExtensionDecl *const ED) -> bool {
                 return extendsVisibleNominal(ED)  &&  (hasVisibleMember(ED) || introducesConformanceToVisibleProtocol(ED));
               });
  return out;
}

CPVec<NominalTypeDecl> ProviderDecls::getKeysOf(const llvm::MapVector<const NominalTypeDecl *, bool>& extendedNominals) {
  CPVec<NominalTypeDecl> out;
  for (auto &p: extendedNominals)
    out.push_back(p.first);
  return out;
}

std::vector<std::pair<const NominalTypeDecl*, const ValueDecl*>>
ProviderDecls::constructHoldersAndMembers(const CPVec<ExtensionDecl> &in ) {
  std::vector<std::pair<const NominalTypeDecl*, const ValueDecl*>> out;
  for (const auto *const ED: in) {
    for (const auto *const member: ED->getMembers()) {
      const auto *const VD = dyn_cast<ValueDecl>(member);
      if (hasVisibleName(VD))
        out.push_back(std::make_pair(ED->getExtendedNominal(), VD));
    }
  }
  return out;
}

CPVec<ValueDecl> ProviderDecls::findClassMembers(const SourceFile* SF) {
  struct Collector: public VisibleDeclConsumer {
    CPVec<ValueDecl> members;
    void foundDecl(ValueDecl *VD, DeclVisibilityKind) override {
      members.push_back(VD);
    }
  } collector;
  SF->lookupClassMembers({}, collector);
  return collector.members;
}

class CommonNameHelpers {
protected:
  static std::string mangleTypeAsContext(const NominalTypeDecl *type) {
    Mangle::ASTMangler Mangler;
    return Mangler.mangleTypeAsContextUSR(type);
  }
};


///////////////////////
namespace {
  class ProviderNames: CommonNameHelpers {
    const ProviderDecls &pds;
    
  private:
    template <typename DeclT>
    static DeclBaseName getName(const DeclT *const D) { return DeclBaseName(D->getName()); }
    
    static DeclBaseName getBaseName(const ValueDecl *const D) { return D->getBaseName(); }
    
  public:
    ProviderNames(const ProviderDecls &pds) : pds(pds) {}
    

    template <
    typename DeclT,
    const CPVec<DeclT>& (ProviderDecls::*declFunc)() const,
    DeclBaseName (*baseNameFn)(const DeclT*)
    >
    StringVec names() const {
      StringVec out;
      for (const auto *const D: (pds.*declFunc)())
        out.push_back((*baseNameFn)(D).userFacingName());
      return out;
    }

    void topsSomehow() {
      names<NominalTypeDecl, &ProviderDecls::topNominals, getName>(); // TypeDecl
      names<OperatorDecl, &ProviderDecls::topOperators, getName>();
      names<PrecedenceGroupDecl, &ProviderDecls::precedenceGroups, getName>();
      names<ValueDecl, &ProviderDecls::topValues, getBaseName>();// rets DeclBaseName
      names<FuncDecl, &ProviderDecls::operatorFunctions, getName>();
    }
    void nominalsSomehow() {
      names<NominalTypeDecl, &ProviderDecls::extendedNominalsThatCanChangeExernallyObservableShape,getName>();
    }
    void membersSomehow() {
//      xxx<NominalTypeDecl,&ProviderDecls::extendedNominalsThatCouldAddMembers,getMangledTypeAsContext, "">();
//      yyy<<std::pair<const NominalTypeDecl*, const ValueDecl*>, &ProviderDecls::<std::pair<const NominalTypeDecl*, const ValueDecl*>, getMangledTypeAsContext, names>;
    }
    void dynamicLookupsSomehow() {
      names<ValueDecl, &ProviderDecls::classMembers, getName>();
    }

  };
}


////////////////////////////


using MemberTableEntryTy = std::pair<ReferencedNameTracker::MemberPair, bool>;

namespace {
  struct DependName {
    DeclBaseName name;
    bool cascades;
    DependName(const DeclBaseName name, const bool cascades) : name(name), cascades(cascades) {}
    int compare(DependName other) const { return name.compare(other.name); }
    static int compare(const DependName &lhs, const DependName &rhs) {
      return lhs.compare(rhs);
    }
    bool operator< (const DependName &other) const {return compare(other) == -1; }
  };
}

namespace {
  class DependsArcs: CommonNameHelpers, CommonDeclHelpers {
  private:
    const ReferencedNameTracker *const tracker;
    std::vector<DependName> sortedTops;
    std::vector<MemberTableEntryTy> sortedMembers;
    std::vector<MemberTableEntryTy> visibleSortedMembers;
    std::unordered_set<const NominalTypeDecl*>cascadingHolders;
    std::vector<MemberTableEntryTy> sortedMembersWithPropagatedCascades;
    std::vector<DependName> sortedDynamicLookupNames;
    StringVec externalNames;
    
    
    static std::vector<DependName> linearizeAndSort(const llvm::DenseMap<DeclBaseName, bool> &map);
    static std::vector<MemberTableEntryTy> sort(const llvm::DenseMap<ReferencedNameTracker::MemberPair, bool> &members);
    static std::vector<MemberTableEntryTy> filter(const std::vector<MemberTableEntryTy> &in);
    static std::unordered_set<const NominalTypeDecl*> findCascadingHolders(const std::vector<MemberTableEntryTy> &in);
    static std::vector<MemberTableEntryTy> spreadCascades(const std::vector<MemberTableEntryTy> &members,
                                                          const std::unordered_set<const NominalTypeDecl*> &cascades);
    static StringVec reversePathSortedFilenames(const ArrayRef<std::string> in);
 
  public:
    DependsArcs(const SourceFile *const SF, const DependencyTracker &depTracker) :
    tracker(SF->getReferencedNameTracker()),
    sortedTops(linearizeAndSort(tracker->getTopLevelNames())),
    sortedMembers(sort(tracker->getUsedMembers())),
    visibleSortedMembers(filter(sortedMembers)),
    cascadingHolders(findCascadingHolders(sortedMembers)),
    sortedMembersWithPropagatedCascades(spreadCascades(sortedMembers, cascadingHolders)),
    sortedDynamicLookupNames(linearizeAndSort(tracker->getDynamicLookupNames())),
    externalNames(reversePathSortedFilenames(depTracker.getDependencies()))
    {
    }
    const std::vector<DependName> &tops() const { return sortedTops; }
    const std::vector<MemberTableEntryTy> &membersWithPropagatedCascades() const { return sortedMembersWithPropagatedCascades; } // nominals
    const std::vector<MemberTableEntryTy> &members() const { return visibleSortedMembers; } // members
    const std::vector<DependName> &dynamicLookups() const { return sortedDynamicLookupNames; }
    const StringVec &externals() const { return externalNames; }
  };
}

////////////////////////

std::vector<DependName> DependsArcs::linearizeAndSort(const llvm::DenseMap<DeclBaseName, bool> &map) {
  std::vector<DependName> out;
  for (const auto &p: map)
    out.push_back(DependName{p.getFirst(), p.getSecond()});
  
  std::sort(out.begin(), out.end());
  return out;
}




std::vector<MemberTableEntryTy> DependsArcs::sort(const llvm::DenseMap<ReferencedNameTracker::MemberPair, bool> &members) {
  std::vector<MemberTableEntryTy> out{members.begin(), members.end()};
  llvm::array_pod_sort(out.begin(), out.end(),
                       [](const MemberTableEntryTy *lhs,
                          const MemberTableEntryTy *rhs) -> int {
                         if (auto cmp = lhs->first.first->getName().compare(rhs->first.first->getName()))
                           return cmp;
                         
                         if (auto cmp = lhs->first.second.compare(rhs->first.second))
                           return cmp;
                         
                         // We can have two entries with the same member name if one of them
                         // was the special 'init' name and the other is the plain 'init' token.
                         if (lhs->second != rhs->second)
                           return lhs->second ? -1 : 1;
                         
                         // Break type name ties by mangled name.
                         auto lhsMangledName = mangleTypeAsContext(lhs->first.first);
                         auto rhsMangledName = mangleTypeAsContext(rhs->first.first);
                         return lhsMangledName.compare(rhsMangledName);
                       });
  return out;
}

std::vector<MemberTableEntryTy> DependsArcs::filter(const std::vector<MemberTableEntryTy> &in) {
  std::vector<MemberTableEntryTy> out;
  std::copy_if(in.begin(), in.end(), std::back_inserter(out),
               [] (const MemberTableEntryTy &mte) -> bool {
                 assert(mte.first.first != nullptr);
                 return isVisible(mte.first.first);
               });
  return out;
}

std::unordered_set<const NominalTypeDecl*> DependsArcs::findCascadingHolders(const std::vector<MemberTableEntryTy> &in) {
  std::unordered_set<const NominalTypeDecl*> out;
  for (const auto &entry: in)
    if (entry.second)
      out.insert(entry.first.first);
  return out;
}

std::vector<MemberTableEntryTy> DependsArcs::spreadCascades(const std::vector<MemberTableEntryTy> &members,
                                               const std::unordered_set<const NominalTypeDecl*> &cascades) {
  std::vector<MemberTableEntryTy> out;
  std::transform(members.begin(), members.end(), std::back_inserter(out),
                 [&cascades] (MemberTableEntryTy entry) -> MemberTableEntryTy {
                   entry.second = cascades.count(entry.first.first) != 0 ? true : false;
                   return entry;
                 });
  return out;
}

StringVec DependsArcs::reversePathSortedFilenames(const ArrayRef<std::string> in) {
  StringVec out{in.begin(), in.end()};
  std::sort(out.begin(), out.end(),  [](const std::string &a,
                                        const std::string &b) -> bool {
    return std::lexicographical_compare(a.rbegin(), a.rend(),
                                        b.rbegin(), b.rend());
  });
  return out;
}

#if 0
namespace SQ
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
  
  
}
#endif


// move filter etc into wherever
// do depends decls
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
        for (auto s: strings) out << s; } );
    }
    
  };
}


//////////////////////////

template <typename InT, typename OutT>
OutT operator>>(InT in, OutT (*fn)(InT)) { return (*fn)(in); }

template <typename InT, typename OutT>
OutT operator>>(InT in, std::function<OutT(InT)> &fn) { return fn(in); }

template <typename InInT, typename InT, typename OutT>
std::function<OutT(InInT)>
operator>> (std::function<InT(InInT)> lhs, std::function<OutT(InT)> rhs) {
  return [&](InInT in) -> OutT { rhs(lhs(in)); };
}


//template <typename InInT, typename InT, typename OutT>
//std::function<OutT(InInT)>
//operator>> ( (InInT) -> InT  lhs, std::function<OutT(InT)> rhs) {
//  return [&](InInT in) -> OutT { rhs(lhs(in)); };
//}


template <typename T>
using Sum = llvm::SmallVector<T, 32>;

template <typename InT, typename OutT>
Sum<OutT> operator+ (std::function<OutT(InT)> fn1, std::function<OutT(InT)> fn2) {
  return [&](InT in) -> Sum<OutT> { return Sum<OutT> {fn1(in), fn2(in)}; };
}

template <typename InT, typename OutT, size_t N>
std::function< Sum<OutT>(InT) > operator+ (std::function<Sum<OutT>(InT)> lhs, std::function<OutT(InT)> fn2) {
  return [&](InT in) -> Sum<OutT> {
    auto out = lhs(in);
    out.push_back(fn2(in));
    return out;
  };
}




/// Entry point to this whole file:

bool swift::experimental_dependencies::emitReferenceDependencies(
                                                                 DiagnosticEngine &diags, SourceFile *const SF,
                                                                 const DependencyTracker &depTracker, StringRef outputPath) {
  
//  auto x = SourceFileDeclDemux(SF) >> InnerDeclCollector() ;
//
//  auto provides = SF >> SourceFileDeclDemux >> InnerDeclCollector >> (TopLevelProvidesSection + NominalSection + MemberSection + DynamicLookupSection);
  
//  auto depends = DependsFeeder(SF, depTracker) >> (TopLevelDependsSection + NominalDependsSection + MembersDependsSection + dynamicLookupDependsSection + externalDependsSection);
//
//  (provedes + depends + interfaceHash) >> SectionEncoder >> FileRenamerAndWriter(outputPath);
//
//  // Before writing to the dependencies file path, preserve any previous file
//  // that may have been there. No error handling -- this is just a nicety, it
//  // doesn't matter if it fails.
//  llvm::sys::fs::rename(outputPath, outputPath + "~");
//  return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
//    ReferenceDependenciesEmitter<ReferendeDependenciesKind::StatusQuo>(
//                                                                       SF, depTracker, out)
//    .emit();
//    return false;
//  });
  
//  return ([]()->std::vector<std::string> {return StringVec{std::string()};})
//  >> FileRenamerAndWriter(diags, outputPath);
  
  SourceFileDeclDemux demux{SF};
  
  ContainerSource<decltype(demux.precedenceGroups)>(demux.precedenceGroups);
}

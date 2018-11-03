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

namespace {
    template <typename DeclT, DeclKind ...kinds>
    class Sink {
    public:
        CPVec<DeclT> decls;

        template <DeclKind f, DeclKind ...r>
        bool didSink(const Decl *const D) {
            if (D->getKind() == f) {
                decls.push_back(cast<DeclT>(D));
                return true;
            }
            return didSink<r...>(D);
        }
        bool didSink(const Decl *const D) {
            return false;
        }
    };
        
        

    
    class SourceFileDeclDemux {
    public:
        Sink<ExtensionDecl, DeclKind::Extension> extensions;
        Sink<OperatorDecl, DeclKind::InfixOperator, DeclKind::PrefixOperator, DeclKind::PostfixOperator> operators;
        Sink<PrecedenceGroupDecl, DeclKind::PrecedenceGroup> precedenceGroups;
        Sink<NominalTypeDecl, DeclKind::Enum, DeclKind::Struct, DeclKind::Class, DeclKind::Protocol> nominals;
        Sink<ValueDecl, DeclKind::TypeAlias, DeclKind::Var, DeclKind::Func, DeclKind::Accessor> values;

        SourceFileDeclDemux(const SourceFile *const SF) {
            for (const Decl *const D: SF->Decls) {
                extensions.didSink(D)
                || operators.didSink(D)
                || precedenceGroups.didSink(D)
                || nominals.didSink(D)
                ||   values.didSink(D);
            }
        }
    };
    
    template <typename DeclT>
    bool isVisible(const DeclT *const D) {
        return D  &&  D->getFormalAccess() > AccessLevel::FilePrivate;
    }
    
    bool memberIsVisible(const Decl *member) {
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

    bool protoIsVisible(ProtocolType *proto) {
        return memberIsVisible(proto->getDecl());
    }
    
    bool conformedProtocolIsVisible(TypeLoc inheritedType) {
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
    
    bool extendsVisibleNominal(const ExtensionDecl *const ED) {
        return isVisible(ED->getExtendedNominal());
    }
    bool introducesConformanceToVisibleProtocol(const ExtensionDecl *const ED) {
        return std::any_of(ED->getInherited().begin(), ED->getInherited().end(),
                           conformedProtocolIsVisible);
    }
    bool hasVisibleMember(const ExtensionDecl *const ED) {
        return std::any_of(ED->getMembers().begin(), ED->getMembers().end(), memberIsVisible);
    }

    template <typename DeclT>
    bool hasVisibleName(const DeclT *const D) {
        return D->hasName() && isVisible(D);
    }
    
 
    
    template <typename DeclT>
    CPVec<DeclT> filter(const CPVec<DeclT> &in) {
        CPVec<DeclT> out;
        std::copy_if(in.begin(), in.end(), std::back_inserter(out), hasVisibleName<DeclT>);
        return out;
    }
    template <>
    CPVec<ExtensionDecl>  filter<ExtensionDecl>(const CPVec<ExtensionDecl> &in)  {
        CPVec<ExtensionDecl> out;
        std::copy_if(in.begin(), in.end(), std::back_inserter(out),
                     [](const ExtensionDecl *const ED) -> bool {
                         return extendsVisibleNominal(ED)  &&  (hasVisibleMember(ED) || introducesConformanceToVisibleProtocol(ED));
                     });
        return out;
    }
    
    CPVec<NominalTypeDecl> getKeysOf(const llvm::MapVector<const NominalTypeDecl *, bool>& extendedNominals) {
        CPVec<NominalTypeDecl> out;
        for (auto &p: extendedNominals)
            out.push_back(p.first);
        return out;
    }
    
    std::vector<std::pair<const NominalTypeDecl*, const ValueDecl*>>
    constructHoldersAndMembers(const CPVec<ExtensionDecl> &in ) {
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
        
    
    struct InnerDeclCollector {
        // Records every nominal declaration, and whether or not the declaration
        /// changes the externally-observable shape of the type.
        llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals;
        
        /// Records operator declarations so they can be included as top-level
        /// declarations.
        CPVec<FuncDecl> memberOperatorDecls;
        
        /// Records extension declarations which are not introducing a conformance
        /// to a public protocol and add a public member.
        CPVec<ExtensionDecl> extensionsWithJustMembers;
        
        InnerDeclCollector(const CPVec<ExtensionDecl> &filteredExtensions, const CPVec<NominalTypeDecl> &filteredTopNominals) {
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
        void findNominalsAndOperators(const DeclRange members) {
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
    };
    
    CPVec<NominalTypeDecl> filter(llvm::MapVector<const NominalTypeDecl *, bool> extendedNominals) {
        CPVec<NominalTypeDecl> out;
        for (const auto entry: extendedNominals)
            if (entry.second)
                out.push_back(entry.first);
        return out;
    }
    
    class ProviderDecls {
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
        
        CPVec<ValueDecl> findClassMembers(const SourceFile* SF) {
            struct Collector: public VisibleDeclConsumer {
                CPVec<ValueDecl> members;
                void foundDecl(ValueDecl *VD, DeclVisibilityKind) override {
                    members.push_back(VD);
                }
            } collector;
            SF->lookupClassMembers({}, collector);
            return collector.members;
        }
        
    public:
        ProviderDecls(const SourceFile *SF) :
        tops(SF),
        filteredExtensions(filter(tops.extensions.decls)),
        filteredTopNominals(filter(tops.nominals.decls)),
        filteredTopValues(filter(tops.values.decls)),
        innerDeclCollector(filteredExtensions, filteredTopNominals),
        _extendedNominalsThatCanChangeExernallyObservableShape(filter(innerDeclCollector.extendedNominals)),
        allExtendedNominals(getKeysOf(innerDeclCollector.extendedNominals)),
        _holdersAndMembers(constructHoldersAndMembers(innerDeclCollector.extensionsWithJustMembers)),
        _classMembers(findClassMembers(SF))
        { }
        // Tops:
        const CPVec<FuncDecl>& operatorFunctions() const { return innerDeclCollector.memberOperatorDecls; }
        const CPVec<NominalTypeDecl>& topNominals() const { return filteredTopNominals; }
        const CPVec<PrecedenceGroupDecl>& precedenceGroups() const { return tops.precedenceGroups.decls; }
        const CPVec<OperatorDecl>& topOperators() const {return tops.operators.decls; }
        const CPVec<ValueDecl>& topValues() const {return filteredTopValues; }
        
        // Nominals:
        const CPVec<NominalTypeDecl>& extendedNominalsThatCanChangeExernallyObservableShape() const {
            return _extendedNominalsThatCanChangeExernallyObservableShape;
        }
        
        // Members:
        const CPVec<NominalTypeDecl>& extendedNominalsThatCouldAddMembers() const {
            return allExtendedNominals;
        }
        const std::vector<std::pair<const NominalTypeDecl*, const ValueDecl*>>& holdersAndMembers() const {
            return _holdersAndMembers;
        }
        
        // Dynamic lookup:
        const CPVec<ValueDecl> classMembers() const { return _classMembers; }
        
    };
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

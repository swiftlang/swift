//===--- swift-module-summary-test.cpp - Test util for C parser library ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Testing utility for the C API of the parser library.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Serialization/ModuleSummary.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"

using namespace swift;
using namespace modulesummary;

enum class ActionType : unsigned {
  None,
  BinaryToYAML,
  YAMLToBinary,
  BinaryToDot,
};

namespace options {

static llvm::cl::OptionCategory Category("swift-module-summary-test Options");

static llvm::cl::opt<std::string>
    InputFilename(llvm::cl::Positional, llvm::cl::desc("<input file>"),
                  llvm::cl::init("-"), llvm::cl::value_desc("filename"));

static llvm::cl::opt<std::string>
    OutputFilename("o", llvm::cl::desc("Override output filename"),
                   llvm::cl::value_desc("filename"));

static llvm::cl::opt<ActionType>
    Action(llvm::cl::desc("Mode:"), llvm::cl::init(ActionType::None),
           llvm::cl::cat(Category),
           llvm::cl::values(
               clEnumValN(ActionType::BinaryToYAML, "to-yaml",
                          "Convert new binary .swiftmodule.summary format to YAML"),
               clEnumValN(ActionType::YAMLToBinary, "from-yaml",
                          "Convert YAML to new binary .swiftmodule.summary format"),
               clEnumValN(ActionType::BinaryToDot, "binary-to-dot",
                          "Convert new binary .swiftmodule.summary format to Dot")));

} // namespace options

LLVM_YAML_DECLARE_MAPPING_TRAITS(modulesummary::ModuleSummaryIndex)
LLVM_YAML_DECLARE_MAPPING_TRAITS(modulesummary::FunctionSummary)
LLVM_YAML_DECLARE_MAPPING_TRAITS(modulesummary::FunctionSummary::Call)
LLVM_YAML_DECLARE_MAPPING_TRAITS(modulesummary::FunctionSummary::TypeRef)
LLVM_YAML_DECLARE_MAPPING_TRAITS(modulesummary::VFuncImpl)
LLVM_YAML_IS_SEQUENCE_VECTOR(modulesummary::FunctionSummary::Call)
LLVM_YAML_IS_SEQUENCE_VECTOR(modulesummary::FunctionSummary::TypeRef)
LLVM_YAML_IS_SEQUENCE_VECTOR(modulesummary::VFuncImpl)
LLVM_YAML_DECLARE_ENUM_TRAITS(modulesummary::FunctionSummary::Call::KindTy)

namespace llvm {
namespace yaml {

template <> struct MappingTraits<std::unique_ptr<FunctionSummary>> {
  static void mapping(IO &io, std::unique_ptr<FunctionSummary> &Ptr) {
    if (!Ptr) {
      Ptr.reset(new FunctionSummary());
    }
    MappingTraits<FunctionSummary>::mapping(io, *Ptr.get());
  }
};

void ScalarEnumerationTraits<FunctionSummary::Call::KindTy>::enumeration(
    IO &io, FunctionSummary::Call::KindTy &V) {
  using Kind = FunctionSummary::Call::KindTy;
  io.enumCase(V, "direct", Kind::Direct);
  io.enumCase(V, "vtable", Kind::VTable);
  io.enumCase(V, "witness", Kind::Witness);
}

void MappingTraits<FunctionSummary::Call>::mapping(IO &io,
                                                   FunctionSummary::Call &V) {
  io.mapRequired("callee_name", V.Name);
  io.mapRequired("callee_guid", V.Callee);
  io.mapRequired("kind", V.Kind);
}

void MappingTraits<FunctionSummary::TypeRef>::mapping(IO &io,
                                                      FunctionSummary::TypeRef &V) {
  io.mapRequired("name", V.Name);
  io.mapRequired("guid", V.Guid);
}
void MappingTraits<FunctionSummary>::mapping(IO &io, FunctionSummary &V) {
  io.mapRequired("name", V.Name);
  io.mapRequired("guid", V.Guid);
  io.mapRequired("live", V.Flags.Live);
  io.mapRequired("preserved", V.Flags.Preserved);
  io.mapRequired("calls", V.CallGraphEdgeList);
  io.mapRequired("type_refs", V.TypeRefList);
}

template <>
struct CustomMappingTraits<FunctionSummaryMapTy> {
  static void inputOne(IO &io, StringRef Key,
                       FunctionSummaryMapTy &V) {
    GUID KeyInt;
    if (Key.getAsInteger(0, KeyInt)) {
      io.setError("key not an integer");
      return;
    }
    io.mapRequired(Key.str().c_str(), V[KeyInt]);
  }
  static void output(IO &io, FunctionSummaryMapTy &V) {
    for (auto &P : V)
      io.mapRequired(llvm::utostr(P.first).c_str(), P.second);
  }
};

void MappingTraits<VFuncImpl>::mapping(IO &io, VFuncImpl &V) {
  io.mapRequired("guid", V.Guid);
  io.mapRequired("type_guid", V.TypeGuid);
}

template <>
struct CustomMappingTraits<VFuncToImplsMapTy> {
  static void inputOne(IO &io, StringRef Key, VFuncToImplsMapTy &V) {
    GUID KeyInt;
    if (Key.getAsInteger(0, KeyInt)) {
      io.setError("key not an integer");
      return;
    }
    io.mapRequired(Key.str().c_str(), V[KeyInt]);
  }
  static void output(IO &io, VFuncToImplsMapTy &V) {
    for (auto &P : V)
      io.mapRequired(llvm::utostr(P.first).c_str(), P.second);
  }
};

void MappingTraits<ModuleSummaryIndex>::mapping(IO &io, ModuleSummaryIndex &V) {
  io.mapRequired("module_name", V.Name);
  io.mapRequired("functions", V.FunctionSummaryMap);
  io.mapRequired("witness_tables", V.WitnessTableMethodMap);
  io.mapRequired("vtables", V.VTableMethodMap);
  io.mapRequired("used_types", V.UsedTypeList);
}
} // namespace yaml
} // namespace llvm

VFuncSlot createVFuncSlot(FunctionSummary::Call call) {
  VFuncSlot::KindTy slotKind;
  switch (call.getKind()) {
    case FunctionSummary::Call::Witness: {
      slotKind = VFuncSlot::Witness;
      break;
    }
    case FunctionSummary::Call::VTable: {
      slotKind = VFuncSlot::VTable;
      break;
    }
    case FunctionSummary::Call::Direct: {
      llvm_unreachable("Can't get slot for static call");
    }
    case FunctionSummary::Call::kindCount: {
      llvm_unreachable("impossible");
    }
  }
  return VFuncSlot(slotKind, call.getCallee());
}

struct CallGraph {
  struct Node;

  struct Edge {
    FunctionSummary::Call Call;
    GUID Target;
    Node *Child;
  };

  struct Node {
    FunctionSummary *FS;
    SmallVector<Edge, 8> Children;
  };
  
  struct child_iterator
      : public std::iterator<std::random_access_iterator_tag, Node *,
                             ptrdiff_t> {
    SmallVectorImpl<Edge>::iterator baseIter;

    child_iterator(SmallVectorImpl<Edge>::iterator baseIter) :
    baseIter(baseIter)
    { }

    child_iterator &operator++() { baseIter++; return *this; }
    child_iterator operator++(int) {
      auto tmp = *this;
      ++baseIter;
      return tmp;
    }
    Node *operator*() const { return baseIter->Child; }
    bool operator==(const child_iterator &RHS) const {
      return baseIter == RHS.baseIter;
    }
    bool operator!=(const child_iterator &RHS) const {
      return baseIter != RHS.baseIter;
    }
    difference_type operator-(const child_iterator &RHS) const {
      return baseIter - RHS.baseIter;
    }
  };

  std::vector<Node> Nodes;
  using iterator = std::vector<Node>::iterator;
  CallGraph(ModuleSummaryIndex *);
};

CallGraph::CallGraph(ModuleSummaryIndex *Summary) {
  Nodes.resize(Summary->functions_size());
  llvm::DenseMap<GUID, Node *> NodeMap;
  int idx = 0;
  for (auto FI = Summary->functions_begin(), FE = Summary->functions_end();
       FI != FE; ++FI) {
    Node &node = Nodes[idx++];
    node.FS = FI->second.get();
    NodeMap[FI->first] = &node;
  }

  for (Node &node : Nodes) {
    for (FunctionSummary::Call call : node.FS->calls()) {
      switch (call.getKind()) {
      case FunctionSummary::Call::Witness:
      case FunctionSummary::Call::VTable: {
        VFuncSlot slot = createVFuncSlot(call);
        for (auto Impl : Summary->getImplementations(slot)) {
          Node *CalleeNode = NodeMap[Impl.Guid];
          node.Children.push_back({ call, Impl.Guid, CalleeNode });
        }
        break;
      }
      case FunctionSummary::Call::Direct: {
        Node *CalleeNode = NodeMap[call.getCallee()];
        node.Children.push_back({ call, call.getCallee(), CalleeNode });
        break;
      }
      case FunctionSummary::Call::kindCount:
        llvm_unreachable("impossible");
      }
    }
  }
}


namespace llvm {

  template <> struct GraphTraits<CallGraph::Node *> {
    typedef CallGraph::child_iterator ChildIteratorType;
    typedef CallGraph::Node *NodeRef;
  
    static NodeRef getEntryNode(NodeRef N) { return N; }
    static inline ChildIteratorType child_begin(NodeRef N) {
      return N->Children.begin();
    }
    static inline ChildIteratorType child_end(NodeRef N) {
      return N->Children.end();
    }
  };

  template <> struct GraphTraits<CallGraph *>
  : public GraphTraits<CallGraph::Node *> {
    typedef CallGraph *GraphType;
    typedef CallGraph::Node *NodeRef;
  
    static NodeRef getEntryNode(GraphType F) { return nullptr; }
  
    typedef pointer_iterator<CallGraph::iterator> nodes_iterator;
    static nodes_iterator nodes_begin(GraphType CG) {
      return nodes_iterator(CG->Nodes.begin());
    }
    static nodes_iterator nodes_end(GraphType CG) {
      return nodes_iterator(CG->Nodes.end());
    }
    static unsigned size(GraphType CG) { return CG->Nodes.size(); }
  };
  
  /// This is everything the llvm::GraphWriter needs to write the call graph in
  /// a dot file.
  template <>
  struct DOTGraphTraits<CallGraph *> : public DefaultDOTGraphTraits {
  
    DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}
  
    std::string getNodeLabel(const CallGraph::Node *Node,
                             const CallGraph *Graph) {
      Demangle::Context DCtx;
      return DCtx.demangleSymbolAsString(Node->FS->getName());
    }

    static std::string getEdgeSourceLabel(const CallGraph::Node *Node,
                                          CallGraph::child_iterator I) {
      std::string Label;
      raw_string_ostream O(Label);
      FunctionSummary::Call call = I.baseIter->Call;
      Demangle::Context DCtx;
      O << DCtx.demangleSymbolAsString(call.getName());
      O << " (";
      switch (call.getKind()) {
      case FunctionSummary::Call::Witness: {
        O << "W";
        break;
      }
      case FunctionSummary::Call::VTable: {
        O << "V";
        break;
      }
      case FunctionSummary::Call::Direct: {
        O << "D";
        break;
      }
      case FunctionSummary::Call::kindCount:
        llvm_unreachable("impossible");
      }
      O << ")";
      return Label;
    }
  };
} // namespace llvm

int main(int argc, char *argv[]) {
  PROGRAM_START(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv, "Swift Module Summary Test\n");

  StringRef fname = options::InputFilename;
  SourceManager sourceMgr;
  DiagnosticEngine diags(sourceMgr);

  auto fileBufOrErr = llvm::MemoryBuffer::getFile(fname);
  if (!fileBufOrErr) {
    llvm::errs() << "error opening file '" << fname
                 << "': " << fileBufOrErr.getError().message();
    return 1;
  }

  switch (options::Action) {
  case ActionType::None: {
    llvm::errs() << "action required\n";
    llvm::cl::PrintHelpMessage();
    return 1;
  }
  case ActionType::BinaryToYAML: {
    modulesummary::ModuleSummaryIndex summary;
    modulesummary::loadModuleSummaryIndex(fileBufOrErr.get()->getMemBufferRef(),
                                          summary);

    bool hadError = withOutputFile(diags, options::OutputFilename,
                                   [&](llvm::raw_pwrite_stream &out) {
                                     out << "# Module-summary v0\n";
                                     llvm::yaml::Output yamlWriter(out);
                                     yamlWriter << summary;
                                     return false;
                                   });

    if (hadError) {
      llvm::errs() << "Failed to write YAML swiftdeps\n";
    }
    break;
  }
  case ActionType::YAMLToBinary: {
    ModuleSummaryIndex summary;
    llvm::yaml::Input yamlReader(fileBufOrErr.get()->getMemBufferRef(),
                                 nullptr);
    yamlReader >> summary;
    if (yamlReader.error()) {
      llvm::errs() << "Failed to parse YAML swiftdeps\n";
      return 1;
    }

    if (writeModuleSummaryIndex(summary, diags, options::OutputFilename)) {
      llvm::errs() << "Failed to write binary module summary\n";
      return 1;
    }
    break;
  }
  case ActionType::BinaryToDot: {
    modulesummary::ModuleSummaryIndex summary;
    modulesummary::loadModuleSummaryIndex(fileBufOrErr.get()->getMemBufferRef(),
                                          summary);
    CallGraph CG(&summary);
    withOutputFile(diags, options::OutputFilename, [&](raw_ostream &out) {
      llvm::WriteGraph(out, &CG);
      return false;
    });
    break;
  }
  }
  return 0;
}

#ifndef SWIFT_SIL_MODULE_SUMMARY_H
#define SWIFT_SIL_MODULE_SUMMARY_H

#include "swift/AST/Decl.h"
#include "swift/SIL/SILDeclRef.h"

// FIXME: Move this into another module to avoid circular dependencies.
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"

namespace swift {

using GUID = uint64_t;
static GUID getGUID(llvm::StringRef Str) { return llvm::MD5Hash(Str); }

class FunctionSummary {
public:
  class EdgeTy {
  public:
    GUID CalleeFnOrTable;
    enum class Kind {
      Static,
      Witness,
      VTable,
      kindCount,
    };

    Kind kind;

    EdgeTy(SILDeclRef CalleeFn, Kind kind) : kind(kind) {
      // FIXME
      auto name = CalleeFn.getDecl()->getBaseName().getIdentifier().str();
      this->CalleeFnOrTable = getGUID(name);
    }

  public:
    Kind getKind() const { return kind; }
    GUID getCallee() const { return CalleeFnOrTable; }

    EdgeTy(GUID callee, Kind kind) : CalleeFnOrTable(callee), kind(kind) {}

    static EdgeTy staticCall(GUID Callee) {
      return EdgeTy(Callee, Kind::Static);
    }
    static EdgeTy witnessCall(GUID Callee) {
      return EdgeTy(Callee, Kind::Witness);
    }
    static EdgeTy vtableCall(GUID Callee) {
      return EdgeTy(Callee, Kind::VTable);
    }
    static EdgeTy witnessCall(SILDeclRef Callee) {
      return EdgeTy(Callee, Kind::Witness);
    }
    static EdgeTy vtableCall(SILDeclRef Callee) {
      return EdgeTy(Callee, Kind::VTable);
    }
  };

  using CallGraphEdgeListTy = std::vector<EdgeTy>;

private:
  CallGraphEdgeListTy CallGraphEdgeList;

public:
  FunctionSummary(std::vector<EdgeTy> CGEdges)
      : CallGraphEdgeList(std::move(CGEdges)) {}
  FunctionSummary() = default;

  void addCall(GUID targetGUID, EdgeTy::Kind kind) {
    CallGraphEdgeList.emplace_back(targetGUID, kind);
  }

  ArrayRef<EdgeTy> calls() const { return CallGraphEdgeList; }
};

struct FunctionSummaryInfo {
  std::string Name;
  std::unique_ptr<FunctionSummary> TheSummary;
};

class ModuleSummaryIndex {
  using FunctionSummaryInfoMapTy = std::map<GUID, FunctionSummaryInfo>;
  FunctionSummaryInfoMapTy FunctionSummaryInfoMap;

  std::string ModuleName; // Only for debug purpose

public:
  ModuleSummaryIndex() = default;

  std::string getModuleName() const { return this->ModuleName; }
  void setModuleName(std::string name) {
    this->ModuleName = name;
  }

  void addFunctionSummary(std::string name,
                          std::unique_ptr<FunctionSummary> summary) {
    auto guid = getGUID(name);
    FunctionSummaryInfoMap.insert(
        std::make_pair(guid, FunctionSummaryInfo{name, std::move(summary)}));
  }

  const llvm::Optional<std::pair<FunctionSummary *, StringRef>>
  getFunctionInfo(GUID guid) const {
    auto found = FunctionSummaryInfoMap.find(guid);
    if (found == FunctionSummaryInfoMap.end()) {
      return None;
    }
    auto &entry = found->second;
    return std::make_pair(entry.TheSummary.get(), StringRef(entry.Name));
  }

  FunctionSummaryInfoMapTy::const_iterator begin() const {
    return FunctionSummaryInfoMap.begin();
  }
  FunctionSummaryInfoMapTy::const_iterator end() const {
    return FunctionSummaryInfoMap.end();
  }
};

ModuleSummaryIndex buildModuleSummaryIndex(SILModule &M,
                                           BasicCalleeAnalysis &BCA);

}; // namespace swift

#endif

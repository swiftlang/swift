#include "swift/AST/Decl.h"
#include "swift/SIL/SILDeclRef.h"

namespace swift {

using GUID = uint64_t;
GUID getGUID(llvm::StringRef Str) { return llvm::MD5Hash(Str); }

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
  StringRef Name;
  std::unique_ptr<FunctionSummary> TheSummary;
};

class ModuleSummaryIndex {
  using FunctionSummaryMapTy = std::map<GUID, FunctionSummaryInfo>;
  FunctionSummaryMapTy FunctionSummaryMap;

public:
  ModuleSummaryIndex() = default;

  void addFunctionSummary(StringRef name,
                          std::unique_ptr<FunctionSummary> summary) {
    auto guid = getGUID(name);
    FunctionSummaryMap.insert(
        std::make_pair(guid, FunctionSummaryInfo{name, std::move(summary)}));
  }

  FunctionSummaryMapTy::const_iterator begin() const {
    return FunctionSummaryMap.begin();
  }
  FunctionSummaryMapTy::const_iterator end() const {
    return FunctionSummaryMap.end();
  }
};

}; // namespace swift

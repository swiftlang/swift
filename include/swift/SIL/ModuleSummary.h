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

private:
  std::vector<EdgeTy> CallGraphEdgeList;

public:
  FunctionSummary(std::vector<EdgeTy> CGEdges)
      : CallGraphEdgeList(std::move(CGEdges)) {}
  FunctionSummary() = default;

  void addCall(GUID targetGUID, EdgeTy::Kind kind) {
    CallGraphEdgeList.emplace_back(targetGUID, kind);
  }
};

class ModuleSummaryIndex {
  std::map<GUID, std::unique_ptr<FunctionSummary>> FunctionSummaryMap;

public:
  ModuleSummaryIndex() = default;

  void addFunctionSummary(GUID guid, std::unique_ptr<FunctionSummary> summary) {
    FunctionSummaryMap.insert(std::make_pair(guid, std::move(summary)));
  }
};

}; // namespace swift

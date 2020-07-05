#ifndef SWIFT_SIL_MODULE_SUMMARY_H
#define SWIFT_SIL_MODULE_SUMMARY_H

#include "swift/AST/Decl.h"
#include "swift/SIL/SILDeclRef.h"

// FIXME: Move this into another module to avoid circular dependencies.
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"

namespace swift {

using GUID = uint64_t;
static GUID getGUID(llvm::StringRef Str) { return llvm::MD5Hash(Str); }

struct VirtualMethodSlot {
  enum class KindTy {
    Witness, VTable,
  };

  KindTy Kind;
  GUID VirtualFuncID;
  GUID TableID;
  
  VirtualMethodSlot(FuncDecl &VirtualFunc, TypeDecl &Context, KindTy kind) : Kind(kind) {
    switch (Kind) {
      case KindTy::Witness: {
        assert(isa<ProtocolDecl>(Context));
        break;
      }
      case KindTy::VTable: {
        assert(isa<ClassDecl>(Context));
        break;
      }
    }
    VirtualFuncID = getGUID(VirtualFunc.getBaseIdentifier().str());
    TableID = getGUID(Context.getNameStr());
  }
  
  VirtualMethodSlot(FuncDecl &VirtualFunc, ProtocolDecl &Context)
  : VirtualMethodSlot(VirtualFunc, Context, KindTy::Witness) { }
  
  VirtualMethodSlot(FuncDecl &VirtualFunc, ClassDecl &Context)
  : VirtualMethodSlot(VirtualFunc, Context, KindTy::VTable) { }
  
  bool operator<(const VirtualMethodSlot &rhs)  const {
    if (Kind > rhs.Kind)
      return false;
    if (Kind < rhs.Kind)
      return true;
    if (TableID > rhs.TableID)
      return false;
    if (TableID < rhs.TableID)
      return true;

    return VirtualFuncID < rhs.VirtualFuncID;
  }
};

class FunctionSummary {
public:
  class EdgeTy {
    GUID CalleeFn;
    GUID Table;
  public:

    enum class Kind {
      Static,
      Witness,
      VTable,
      kindCount,
    };

    Kind kind;

    EdgeTy(FuncDecl &CalleeFn, TypeDecl &Context, Kind kind) : kind(kind) {
      // FIXME: This is really fragile
      this->CalleeFn = getGUID(CalleeFn.getBaseIdentifier().str());
      this->Table = getGUID(Context.getNameStr());
    }

  public:
    Kind getKind() const { return kind; }
    GUID getCallee() const { return CalleeFn; }
    GUID getTable() const {
      // If kind is static, Table guid should be 0
      assert(kind != Kind::Static || Table == 0);
      return Table;
    }

    EdgeTy(GUID callee, GUID table, Kind kind)
      : CalleeFn(callee), Table(table), kind(kind) {}

    EdgeTy(GUID callee, Kind kind)
      : CalleeFn(callee), Table(0), kind(kind) {}

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
      auto &Fn = *Callee.getFuncDecl();
      auto Context = dyn_cast<ProtocolDecl>(Fn.getDeclContext());
      return EdgeTy(Fn, *Context, Kind::Witness);
    }
    static EdgeTy vtableCall(SILDeclRef Callee) {
      auto &Fn = *Callee.getFuncDecl();
      auto Context = dyn_cast<ClassDecl>(Fn.getDeclContext());
      return EdgeTy(Fn, *Context, Kind::VTable);
    }
  };

  
  struct FlagsTy {
    unsigned Live : 1;
  };
  
  using CallGraphEdgeListTy = std::vector<EdgeTy>;

private:
  FlagsTy Flags;
  CallGraphEdgeListTy CallGraphEdgeList;

public:
  FunctionSummary(std::vector<EdgeTy> CGEdges)
      : CallGraphEdgeList(std::move(CGEdges)) {}
  FunctionSummary() = default;

  void addCall(GUID targetGUID, GUID tableGUID, EdgeTy::Kind kind) {
    // If kind is static, Table guid should be 0
    assert(kind != EdgeTy::Kind::Static || tableGUID == 0);
    CallGraphEdgeList.emplace_back(targetGUID, tableGUID, kind);
  }

  ArrayRef<EdgeTy> calls() const { return CallGraphEdgeList; }
  
  bool isLive() const { return Flags.Live; }
  void setLive(bool Live) { Flags.Live = Live; }
};

struct FunctionSummaryInfo {
  std::string Name;
  std::unique_ptr<FunctionSummary> TheSummary;
};

class ModuleSummaryIndex {
  using FunctionSummaryInfoMapTy = std::map<GUID, FunctionSummaryInfo>;
  using VirtualMethodInfoMapTy = std::map<VirtualMethodSlot, std::vector<GUID>>;

  FunctionSummaryInfoMapTy FunctionSummaryInfoMap;
  VirtualMethodInfoMapTy VirtualMethodInfoMap;

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
  
  void addImplementation(VirtualMethodSlot slot, GUID funcGUID) {
    auto found = VirtualMethodInfoMap.find(slot);
    if (found == VirtualMethodInfoMap.end()) {
      VirtualMethodInfoMap.insert(std::make_pair(slot, std::vector<GUID>{ funcGUID }));
      return;
    }
    found->second.push_back(funcGUID);
  }
  
  llvm::Optional<ArrayRef<GUID>>
  getImplementations(VirtualMethodSlot slot) const {
    auto found = VirtualMethodInfoMap.find(slot);
    if (found == VirtualMethodInfoMap.end()) {
      return None;
    }
    return ArrayRef<GUID>(found->second);
  }

  const VirtualMethodInfoMapTy &virtualMethods() const {
    return VirtualMethodInfoMap;
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

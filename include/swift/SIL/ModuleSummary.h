#ifndef SWIFT_SIL_MODULE_SUMMARY_H
#define SWIFT_SIL_MODULE_SUMMARY_H

#include "swift/AST/Decl.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/AST/ASTMangler.h"

// FIXME: Move this into another module to avoid circular dependencies.
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"

namespace swift {

using GUID = uint64_t;
static GUID getGUID(llvm::StringRef Str) { return llvm::MD5Hash(Str); }

struct VirtualMethodSlot {
  enum class KindTy {
    Witness, VTable,
    kindCount,
  };

  KindTy Kind;
  GUID VirtualFuncID;
  VirtualMethodSlot(KindTy kind, GUID virtualFuncID)
    : Kind(kind), VirtualFuncID(virtualFuncID) { }
  VirtualMethodSlot(SILDeclRef VirtualFuncRef, KindTy kind) : Kind(kind) {
    VirtualFuncID = getGUID(VirtualFuncRef.mangle());
  }

  bool operator<(const VirtualMethodSlot &rhs)  const {
    if (Kind > rhs.Kind)
      return false;
    if (Kind < rhs.Kind)
      return true;
    return VirtualFuncID < rhs.VirtualFuncID;
  }
};

class FunctionSummary {
public:
  class EdgeTy {
    GUID CalleeFn;
  public:

    enum class Kind {
      Static,
      Witness,
      VTable,
      kindCount,
    };

    Kind kind;

    EdgeTy(SILDeclRef &CalleeFn, Kind kind) : kind(kind) {
      this->CalleeFn = getGUID(CalleeFn.mangle());
    }

  public:
    Kind getKind() const { return kind; }
    GUID getCallee() const { return CalleeFn; }
    
    VirtualMethodSlot slot() const {
      VirtualMethodSlot::KindTy slotKind;
      switch (kind) {
        case Kind::Witness: {
        slotKind = VirtualMethodSlot::KindTy::Witness;
        break;
        }
        case Kind::VTable: {
        slotKind = VirtualMethodSlot::KindTy::VTable;
        break;
        }
        case Kind::Static: {
          llvm_unreachable("Can't get slot for static call");
        }
        case Kind::kindCount: {
          llvm_unreachable("impossible");
        }
      }
      return VirtualMethodSlot(slotKind, CalleeFn);
    }

    EdgeTy(GUID callee, Kind kind)
      : CalleeFn(callee), kind(kind) {}

    static EdgeTy staticCall(GUID Callee) {
      return EdgeTy(Callee, Kind::Static);
    }

    static EdgeTy witnessCall(SILDeclRef Callee) {
      return EdgeTy(Callee, Kind::Witness);
    }
    static EdgeTy vtableCall(SILDeclRef Callee) {
      return EdgeTy(Callee, Kind::VTable);
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

  void addCall(GUID targetGUID, EdgeTy::Kind kind) {
    CallGraphEdgeList.emplace_back(targetGUID, kind);
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

#ifndef SWIFTC_SIL_SILINSTRUCTION_H
#define SWIFTC_SIL_SILINSTRUCTION_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/SourceLoc.h"
#include <memory>
#include <vector>

namespace swiftc {

class SILValue;
class SILType;

enum class SILInstructionKind {
  AllocStack,
  AllocBox,
  DeallocStack,
  DeallocBox,
  Load,
  Store,
  Copy,
  Destroy,
  RetainValue,
  ReleaseValue,
  FunctionRef,
  Apply,
  Return,
  Branch,
  CondBranch,
  IntegerLiteral,
  FloatLiteral,
  StringLiteral,
  Tuple,
  TupleExtract,
  Struct,
  StructExtract,
  Enum,
  UncheckedEnumData,
  
  NUM_SIL_INSTRUCTIONS
};

/// Base class for all SIL instructions.
class SILInstruction {
  SILInstructionKind Kind;
  SourceLoc Location;

protected:
  SILInstruction(SILInstructionKind kind, SourceLoc loc)
      : Kind(kind), Location(loc) {}

public:
  virtual ~SILInstruction() = default;

  SILInstructionKind getKind() const { return Kind; }
  SourceLoc getLocation() const { return Location; }

  template<typename T>
  bool isa() const {
    return T::classof(this);
  }

  template<typename T>
  T* dyn_cast() {
    return T::classof(this) ? static_cast<T*>(this) : nullptr;
  }
};

/// SIL value representing the result of an instruction or a parameter.
class SILValue {
  SILInstruction* DefiningInstruction;
  unsigned ResultIndex;

public:
  SILValue() : DefiningInstruction(nullptr), ResultIndex(0) {}
  SILValue(SILInstruction* inst, unsigned index = 0)
      : DefiningInstruction(inst), ResultIndex(index) {}

  SILInstruction* getDefiningInstruction() const { return DefiningInstruction; }
  unsigned getResultIndex() const { return ResultIndex; }
  bool isValid() const { return DefiningInstruction != nullptr; }
};

/// Allocation instruction.
class AllocStackInst : public SILInstruction {
  std::unique_ptr<SILType> AllocatedType;

public:
  AllocStackInst(SourceLoc loc, std::unique_ptr<SILType> type)
      : SILInstruction(SILInstructionKind::AllocStack, loc),
        AllocatedType(std::move(type)) {}

  SILType* getAllocatedType() const { return AllocatedType.get(); }

  static bool classof(const SILInstruction* inst) {
    return inst->getKind() == SILInstructionKind::AllocStack;
  }
};

/// Load instruction.
class LoadInst : public SILInstruction {
  SILValue Address;

public:
  LoadInst(SourceLoc loc, SILValue address)
      : SILInstruction(SILInstructionKind::Load, loc), Address(address) {}

  SILValue getAddress() const { return Address; }

  static bool classof(const SILInstruction* inst) {
    return inst->getKind() == SILInstructionKind::Load;
  }
};

/// Store instruction.
class StoreInst : public SILInstruction {
  SILValue Value;
  SILValue Address;

public:
  StoreInst(SourceLoc loc, SILValue value, SILValue address)
      : SILInstruction(SILInstructionKind::Store, loc),
        Value(value), Address(address) {}

  SILValue getValue() const { return Value; }
  SILValue getAddress() const { return Address; }

  static bool classof(const SILInstruction* inst) {
    return inst->getKind() == SILInstructionKind::Store;
  }
};

/// Return instruction.
class ReturnInst : public SILInstruction {
  SILValue ReturnValue;

public:
  ReturnInst(SourceLoc loc, SILValue returnValue)
      : SILInstruction(SILInstructionKind::Return, loc),
        ReturnValue(returnValue) {}

  SILValue getReturnValue() const { return ReturnValue; }

  static bool classof(const SILInstruction* inst) {
    return inst->getKind() == SILInstructionKind::Return;
  }
};

/// Integer literal instruction.
class IntegerLiteralInst : public SILInstruction {
  int64_t Value;

public:
  IntegerLiteralInst(SourceLoc loc, int64_t value)
      : SILInstruction(SILInstructionKind::IntegerLiteral, loc), Value(value) {}

  int64_t getValue() const { return Value; }

  static bool classof(const SILInstruction* inst) {
    return inst->getKind() == SILInstructionKind::IntegerLiteral;
  }
};

} // namespace swiftc

#endif // SWIFTC_SIL_SILINSTRUCTION_H
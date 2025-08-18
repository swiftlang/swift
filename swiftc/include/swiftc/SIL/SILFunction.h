#ifndef SWIFTC_SIL_SILFUNCTION_H
#define SWIFTC_SIL_SILFUNCTION_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/SIL/SILInstruction.h"
#include <memory>
#include <vector>

namespace swiftc {

class SILType;

/// A basic block in SIL.
class SILBasicBlock {
  std::vector<std::unique_ptr<SILInstruction>> Instructions;
  std::string Label;

public:
  SILBasicBlock(StringRef label = "") : Label(label.str()) {}

  void addInstruction(std::unique_ptr<SILInstruction> inst) {
    Instructions.push_back(std::move(inst));
  }

  const std::vector<std::unique_ptr<SILInstruction>>& getInstructions() const {
    return Instructions;
  }

  StringRef getLabel() const { return Label; }
  void setLabel(StringRef label) { Label = label.str(); }
};

/// A SIL function.
class SILFunction {
  std::string Name;
  std::vector<std::unique_ptr<SILBasicBlock>> BasicBlocks;
  std::vector<std::unique_ptr<SILType>> ParameterTypes;
  std::unique_ptr<SILType> ReturnType;

public:
  SILFunction(StringRef name) : Name(name.str()) {}

  StringRef getName() const { return Name; }

  void addBasicBlock(std::unique_ptr<SILBasicBlock> block) {
    BasicBlocks.push_back(std::move(block));
  }

  const std::vector<std::unique_ptr<SILBasicBlock>>& getBasicBlocks() const {
    return BasicBlocks;
  }

  SILBasicBlock* getEntryBlock() const {
    return BasicBlocks.empty() ? nullptr : BasicBlocks[0].get();
  }

  void setReturnType(std::unique_ptr<SILType> type) {
    ReturnType = std::move(type);
  }

  SILType* getReturnType() const { return ReturnType.get(); }

  void addParameterType(std::unique_ptr<SILType> type) {
    ParameterTypes.push_back(std::move(type));
  }

  const std::vector<std::unique_ptr<SILType>>& getParameterTypes() const {
    return ParameterTypes;
  }
};

/// SIL module containing functions and global data.
class SILModule {
  std::vector<std::unique_ptr<SILFunction>> Functions;

public:
  SILModule() = default;

  void addFunction(std::unique_ptr<SILFunction> function) {
    Functions.push_back(std::move(function));
  }

  const std::vector<std::unique_ptr<SILFunction>>& getFunctions() const {
    return Functions;
  }

  SILFunction* lookupFunction(StringRef name) const {
    for (const auto& func : Functions) {
      if (func->getName() == name) {
        return func.get();
      }
    }
    return nullptr;
  }
};

} // namespace swiftc

#endif // SWIFTC_SIL_SILFUNCTION_H
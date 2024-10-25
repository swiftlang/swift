#ifndef TEST_INTEROP_CXX_CLASS_METHOD_INREG_SRET_H
#define TEST_INTEROP_CXX_CLASS_METHOD_INREG_SRET_H

struct OptionalBridgedBasicBlock {
};

struct BridgedFunction {
  OptionalBridgedBasicBlock getFirstBlock() const { return {}; }
};

#endif // TEST_INTEROP_CXX_CLASS_METHOD_INREG_SRET_H

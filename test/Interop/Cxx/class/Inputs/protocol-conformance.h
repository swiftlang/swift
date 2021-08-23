#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_PROTOCOL_CONFORMANCE_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_PROTOCOL_CONFORMANCE_H

struct ConformsToProtocol {
  int return42() { return 42; }
};

struct DoesNotConformToProtocol {
  int returnFortyTwo() { return 42; }
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_PROTOCOL_CONFORMANCE_H

#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_MEMBERS_TRANSITIVE_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_MEMBERS_TRANSITIVE_H

namespace TransitiveNamespace { }

namespace CommonNamespace {

struct TransitiveStruct {
  bool memberVar;
};

} // namespace CommonNamespace

namespace CommonNamespace::NestedNamespace {

struct TransitiveNestedStruct {
  bool memberVar;
};

} // namespace CommonNamespace::NestedNamespace

struct TopLevelTransitiveStruct {
  bool memberVar;
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_MEMBERS_TRANSITIVE_H

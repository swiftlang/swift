#ifndef TEST_INTEROP_CXX_ENUM_INPUTS_NESTED_ENUMS_H
#define TEST_INTEROP_CXX_ENUM_INPUTS_NESTED_ENUMS_H

namespace ns {

enum EnumInNS {
  kA = 0,
  kB
};

enum class ScopedEnumInNS {
  scopeA,
  scopeB
};

namespace nestedNS {

enum EnumInNestedNS {
  kNestedA = 0,
  kNestedB
};

enum EnumInNS { 
  kA = 0, 
  kB 
};

}

}

namespace nsB {

enum EnumInNS { 
  kA = 0, 
  kB 
};

enum class ScopedEnumInNS { 
  scopeA, 
  scopeB 
};

namespace nestedNS {

enum EnumInNS { 
  kA = 0, 
  kB 
};

}

}

class ClassA {
public:
  enum class EnumInClass { scopeA, scopeB };
};

class ClassB {
public:
  enum class EnumInClass { scopeA, scopeB };
};

#endif // TEST_INTEROP_CXX_ENUM_INPUTS_NESTED_ENUMS_H

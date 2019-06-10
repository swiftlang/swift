#pragma once

namespace ns {

struct Basic {
  int a;
};

}  // namespace ns

ns::Basic makeA();

using ns_Basic = ns::Basic;

namespace nested_struct_problem {

// Because of cycles in the importing process,
// importing NextedStruct directly would get into an infinite loop importing
// WrapperClass which would in turn import NestedStruct.
class WrapperClass {
 public:
  struct NestedStruct {
    int a;
  };
};

}  // namespace nested_struct_problem

using NestedStruct = nested_struct_problem::WrapperClass::NestedStruct;
NestedStruct MakeNestedStruct();

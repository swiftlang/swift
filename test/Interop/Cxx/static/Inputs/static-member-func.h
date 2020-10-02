#ifndef TEST_INTEROP_CXX_STATIC_INPUTS_STATIC_MEMBER_FUNC_H
#define TEST_INTEROP_CXX_STATIC_INPUTS_STATIC_MEMBER_FUNC_H

class WithStaticMemberFunc {
public:
  static int staticMemberFunc();
  typedef int (*Func)();
  static Func getStaticMemberFuncAddress();
};

#endif

#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_EAGER_INSTANTIATION_PROBLEMS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_EAGER_INSTANTIATION_PROBLEMS_H

template<class T>
struct MagicWrapper {
  void callGetInt() const {
    T::getIntDoesNotExist();
  }
};

typedef MagicWrapper<int> BrokenMemberMagicWrapper;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_EAGER_INSTANTIATION_PROBLEMS_H

#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_EAGER_INSTANTIATION_PROBLEMS_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_EAGER_INSTANTIATION_PROBLEMS_H

struct MagicNumber {
  int getInt() const { return 42; }
};

template<class T>
struct MagicWrapper {
  void callGetInt() const {
    T::getIntDoesNotExist();
  }

  template <typename A> int sfinaeGetInt(A a, decltype(&A::getInt)) {
    return a.getInt();
  }
  template <typename A> int sfinaeGetInt(A a, ...) {
    return -42;
  }
};

typedef MagicWrapper<int> BrokenMagicWrapper;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_EAGER_INSTANTIATION_PROBLEMS_H

#ifndef TEST_SIL_OPTIMIZER_CXX_WITH_CUSTOM_DESTRUCTOR_H
#define TEST_SIL_OPTIMIZER_CXX_WITH_CUSTOM_DESTRUCTOR_H

struct HasUserProvidedDestructor {
  int x;
  ~HasUserProvidedDestructor() {}
};

struct Loadable {
  int x;
};

struct HasMemberWithUserProvidedDestructor {
  Loadable y;
  ~HasMemberWithUserProvidedDestructor() {}
};

#endif // TEST_SIL_OPTIMIZER_CXX_WITH_CUSTOM_DESTRUCTOR_H

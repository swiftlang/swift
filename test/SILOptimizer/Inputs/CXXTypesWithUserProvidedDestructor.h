#ifndef TEST_SIL_OPTIMIZER_CXX_WITH_CUSTOM_DESTRUCTOR_H
#define TEST_SIL_OPTIMIZER_CXX_WITH_CUSTOM_DESTRUCTOR_H

struct HasUserProvidedDestructor {
  int x = 0;

  HasUserProvidedDestructor(const HasUserProvidedDestructor &other) : x(other.x) {}
  ~HasUserProvidedDestructor() {}
};

struct Loadable {
  int x = 0;
};

struct HasMemberWithUserProvidedDestructor {
  Loadable y;

  HasMemberWithUserProvidedDestructor(const HasMemberWithUserProvidedDestructor &other) : y(other.y) {}
  ~HasMemberWithUserProvidedDestructor() {}
};

void foo();

struct NonCopyable {
  NonCopyable(int x) : x(x) {}
  NonCopyable(const NonCopyable &) = delete;
  NonCopyable(NonCopyable &&other) : x(other.x) { other.x = -123; }
  ~NonCopyable() { foo(); }

  int x;
};

#endif // TEST_SIL_OPTIMIZER_CXX_WITH_CUSTOM_DESTRUCTOR_H

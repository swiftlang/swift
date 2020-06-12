#ifndef VALIDATION_TEST_SILOPTIMIZER_FOO_H
#define VALIDATION_TEST_SILOPTIMIZER_FOO_H

struct Foo {
  int x;
  ~Foo() {}
};

struct Loadable {
  int x;
};

struct Bar {
  Loadable y;
  ~Bar() {}
};

#endif // VALIDATION_TEST_SILOPTIMIZER_FOO_H

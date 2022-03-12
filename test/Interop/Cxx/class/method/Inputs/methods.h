#ifndef TEST_INTEROP_CXX_CLASS_METHOD_METHODS_H
#define TEST_INTEROP_CXX_CLASS_METHOD_METHODS_H

struct NonTrivialInWrapper {
  int value;

  ~NonTrivialInWrapper() { }
};

struct HasMethods {
  void nonConstMethod() { }
  void constMethod() const { }

  int nonConstPassThrough(int a) { return a; }
  int constPassThrough(int a) const { return a; }

  int nonConstSum(int a, int b) { return a + b; }
  int constSum(int a, int b) const { return a + b; }

  int nonConstSum(NonTrivialInWrapper a, NonTrivialInWrapper b) { return a.value + b.value; }
  int constSum(NonTrivialInWrapper a, NonTrivialInWrapper b) const { return a.value + b.value; }

  NonTrivialInWrapper nonConstSumAsWrapper(NonTrivialInWrapper a, NonTrivialInWrapper b) { return {a.value + b.value}; }
  NonTrivialInWrapper constSumAsWrapper(NonTrivialInWrapper a, NonTrivialInWrapper b) const { return {a.value + b.value}; }

  NonTrivialInWrapper nonConstPassThroughAsWrapper(int a) { return {a}; }
  NonTrivialInWrapper constPassThroughAsWrapper(int a) const { return {a}; }
};

#endif // TEST_INTEROP_CXX_CLASS_METHOD_METHODS_H

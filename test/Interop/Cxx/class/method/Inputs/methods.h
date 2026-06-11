#ifndef TEST_INTEROP_CXX_CLASS_METHOD_METHODS_H
#define TEST_INTEROP_CXX_CLASS_METHOD_METHODS_H

struct __attribute__((swift_attr("import_unsafe"))) NonTrivialInWrapper {
  int value;

  NonTrivialInWrapper(int value) : value(value) {}

  // explicit copy constructor is needed, as on Windows a destructor
  // still makes this a type that's passed in registers.
  NonTrivialInWrapper(const NonTrivialInWrapper &other) : value(other.value) {}
  ~NonTrivialInWrapper() { }
};

struct HasMethods {
  void nonConstMethod() { }
  void nonConstMethod(int) { }
  static void nonConstMethod(float) { } // checking name colisions: rdar://120858502
  void constMethod() const { }
  static void constMethod(float) { } // checking name colisions: rdar://120858502

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

  void nonTrivialTakesConstRef(const NonTrivialInWrapper& w) const {}
  void nonTrivialTakesRef(NonTrivialInWrapper& w) const {}
};

struct ReferenceParams {
  int a;
  int b;
  ReferenceParams(const int &a, const int &b) : a(a), b(b) { }
  static void staticMethod(const int &a, const int &b) {
    ReferenceParams t{a, b};
  }
};

struct HasInitMethods {
  int field;
  int init() const { return field; }
  int init(int n) {
    field = n;
    return field;
  }
};

struct HasInitWithBackticks {
  int x;
  int notInit() const __attribute__((swift_name("`init`()"))) { return x; }
};

struct HasRenamedInitMethods {
  int field;
  int start() const __attribute__((swift_name("init()"))) { return field; }
  // expected-warning@-1 {{ignoring swift_name attribute 'init()'; 'start' cannot be imported as an initializer}} 
  // expected-note@-2 {{use backticks (e.g. 'swift_name("`init`(...)")')}}
  int startWith(int a) const __attribute__((swift_name("init(n:)"))) {
    return field + a;
  }
  // expected-warning@-3 {{ignoring swift_name attribute 'init(n:)'; 'startWith' cannot be imported as an initializer}} 
  // expected-note@-4 {{use backticks (e.g. 'swift_name("`init`(...)")')}}
};

struct HasStaticInitFactoryAndInitMethod {
  // Should be renamed to `init`
  int init() const { return value; }

  // Should be imported as an initializer
  static HasStaticInitFactoryAndInitMethod makeWithValue(int value)
      __attribute__((swift_name("init(value:)"))) {
    HasStaticInitFactoryAndInitMethod result;
    result.value = value + 3;
    return result;
  }

private:
  int value = 0;
};

struct HasNonInitializerStaticInitMethod {
  static int nonInitializer(int value) __attribute__((swift_name("init(n:)"))) {
    return value;
  }
  // expected-warning@-3 {{ignoring swift_name attribute 'init(n:)'; 'nonInitializer' cannot be imported as an initializer}} 
  // expected-note@-4 {{use backticks (e.g. 'swift_name("`init`(...)")')}}
};

struct ConstructorWithRenamedLabel {
  int value;
  __attribute__((swift_name("init(renamed:)")))
  ConstructorWithRenamedLabel(int v) : value(v) {}
};

struct WithFactory {
  int x;
};

inline WithFactory makeWithFactory(int n)
    __attribute__((swift_name("WithFactory.init(n:)"))) {
  return {n * 2};
}
// expected-note@-4 {{'makeWithFactory' declared here}}

inline WithFactory makeWithWrongFactory(int n)
    __attribute__((swift_name("WrongFactory.init(n:)"))) {
  return {n * 3};
}
// expected-warning@-3 {{imported declaration 'makeWithWrongFactory' could not be mapped to 'WrongFactory.init(n:)'}}
// expected-note@-4 {{please report this issue to the owners of 'Methods}}

#endif // TEST_INTEROP_CXX_CLASS_METHOD_METHODS_H

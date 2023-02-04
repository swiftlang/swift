extern "C" void puts(const char *);

inline void testFunctionCollected() {
  puts("test\n");
}

struct Base {
  virtual void foo() = 0;
};

template <class T>
struct Derived : Base {
  inline void foo() override {
    testFunctionCollected();
  }

  void callMe() {
  }
};

using DerivedInt = Derived<int>;

template <class T>
struct Unused : Base {
  inline void foo() override {
  }
};

using UnusedInt = Unused<int>;

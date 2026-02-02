extern "C" void puts(const char *_Null_unspecified);

inline void testFunctionCollected() {
  puts("test\n");
}

struct Base {
  virtual void foo() = 0;
  virtual void virtualRename() const
      __attribute__((swift_name("swiftVirtualRename()")));
};

struct Base2 { virtual int f() = 0; };
struct Base3 { virtual int f() { return 24; } };
struct Derived2 : public Base2 { virtual int f() {  return 42; } };
struct Derived3 : public Base3 { virtual int f() {  return 42; } };
struct Derived4 : public Base3 { };
struct DerivedFromDerived2 : public Derived2 {};

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

struct VirtualNonAbstractBase {
  virtual void nonAbstractMethod() const;
};

struct CallsPureMethod {
  virtual int getPureInt() const = 0;
  int getInt() const { return getPureInt() + 1; }
};

struct DerivedFromCallsPureMethod : CallsPureMethod {
  int getPureInt() const override { return 789; }
};

struct DerivedFromDerivedFromCallsPureMethod : DerivedFromCallsPureMethod {};

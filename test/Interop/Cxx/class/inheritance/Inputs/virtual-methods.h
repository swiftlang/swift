#pragma once

inline void testFunctionCollected() {}

struct Base {
  virtual void foo() = 0;
};

template <class T>
struct Derived : Base {
  inline void foo() override { testFunctionCollected(); }
  void callMe() {}
};

using DerivedInt = Derived<int>;

template <class T>
struct Unused : Base {
  inline void foo() override {}
};

using UnusedInt = Unused<int>;

struct Base2 { virtual int f() = 0; };
struct Derived2 : public Base2 { virtual int f() {  return 999; } };

struct Base3 { virtual int f() { return 111; } };
struct Derived3 : public Base3 { virtual int f() {  return 222; } };
struct Derived4 : public Base3 {};
struct DerivedFromDerived2 : public Derived2 {};

struct VirtualRenamedBase {
  virtual int cxxName() const
      __attribute__((swift_name("swiftName()")))
      { return 101; }
};
struct VirtualRenamedInherited : VirtualRenamedBase {};
struct VirtualRenamedOverridden : VirtualRenamedBase {
  virtual int cxxName() const override { return 303; }
};

struct PureVirtualRenamedBase {
  virtual int cxxName() const
      __attribute__((swift_name("swiftName()"))) = 0;
};
struct PureVirtualRenamedInherited : PureVirtualRenamedBase {};
struct PureVirtualRenamedOverridden : PureVirtualRenamedBase {
  virtual int cxxName() const override { return 404; }
};

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

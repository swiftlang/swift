extern "C" void puts(const char *_Null_unspecified);

inline void testFunctionCollected() {
  puts("test\n");
}

struct Base {
  virtual void foo() = 0;
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

struct HasDestructor {
  ~HasDestructor() {}
};

// MARK: Reference Types:

#define IMMORTAL_FRT                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

struct IMMORTAL_FRT ImmortalBase {
  int value = 0;

  virtual int get42() const { return 42; }
  virtual int getOverridden42() const { return 123; }
  virtual int getIntValue() const { return value; }
};

struct IMMORTAL_FRT Immortal : public ImmortalBase {
  static Immortal *_Nonnull create() { return new Immortal(); }

  virtual int getOverridden42() const override { return 42; }
  virtual void setIntValue(int newValue) { this->value = newValue; }
};

struct IMMORTAL_FRT DerivedFromImmortal : public Immortal {
  static DerivedFromImmortal *_Nonnull create() { return new DerivedFromImmortal(); }
};

struct IMMORTAL_FRT Immortal2 {
public:
  virtual void virtualMethod(HasDestructor) = 0;
};

inline const ImmortalBase *_Nonnull castToImmortalBase(
    const Immortal *_Nonnull immortal) {
  return static_cast<const ImmortalBase *>(immortal);
}

inline const Immortal *_Nonnull castToImmortal(
    const DerivedFromImmortal *_Nonnull immortal) {
  return static_cast<const Immortal *>(immortal);
}

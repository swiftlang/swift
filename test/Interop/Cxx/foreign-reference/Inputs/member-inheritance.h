#pragma once

#define IMMORTAL_FRT                              \
  __attribute__((swift_attr("import_reference"))) \
  __attribute__((swift_attr("retain:immortal")))  \
  __attribute__((swift_attr("release:immortal")))

int &getCopyCounter() {
    static int copyCounter = 0;
    return copyCounter;
}

class CopyTrackedBaseClass {
public:
    CopyTrackedBaseClass(int x) : x(x), field(x + 1) {}
    CopyTrackedBaseClass(const CopyTrackedBaseClass &other) : x(other.x), field(other.field) {
        ++getCopyCounter();
    }

    int getX() const {
        return x;
    }

    int field;

    int operator[](int x) const {
        return x + field;
    }
private:
    int x;
} IMMORTAL_FRT;

class CopyTrackedDerivedClass: public CopyTrackedBaseClass {
public:
    CopyTrackedDerivedClass(int x) : CopyTrackedBaseClass(x) {}

    int getDerivedX() const {
        return getX();
    }
} IMMORTAL_FRT;

CopyTrackedDerivedClass *makeCopyTrackedDerivedClass(int x) {
    return new CopyTrackedDerivedClass(x);
}

class NonEmptyBase {
public:
    int getY() const {
        return y;
    }
private:
    int y = 11;
} IMMORTAL_FRT;

class CopyTrackedDerivedDerivedClass: public NonEmptyBase, public CopyTrackedDerivedClass {
public:
    CopyTrackedDerivedDerivedClass(int x) : CopyTrackedDerivedClass(x) {}
} IMMORTAL_FRT;

CopyTrackedDerivedDerivedClass *makeCopyTrackedDerivedDerivedClass(int x) {
    return new CopyTrackedDerivedDerivedClass(x);
}

class BaseReturningFRTFromSubscript {
public:
    CopyTrackedDerivedClass &operator[](int x) const {
        return *new CopyTrackedDerivedClass(x);
    }
} IMMORTAL_FRT;

BaseReturningFRTFromSubscript *makeBaseReturningFRTFromSubscript() {
    return new BaseReturningFRTFromSubscript();
}

class DerivedFromBaseReturningFRTFromSubscript
    : public BaseReturningFRTFromSubscript{public : } IMMORTAL_FRT;

DerivedFromBaseReturningFRTFromSubscript *makeDerivedFromBaseReturningFRTFromSubscript() {
    return new DerivedFromBaseReturningFRTFromSubscript();
}

class BaseReturningFRTFromSubscriptPointer {
public:
    BaseReturningFRTFromSubscriptPointer(): value(new CopyTrackedDerivedClass(0)) {}

    CopyTrackedDerivedClass *&operator[](int x) {
        return value;
    }

private:
    CopyTrackedDerivedClass *value;
} IMMORTAL_FRT;

class DerivedFromBaseReturningFRTFromSubscriptPointer
    : public BaseReturningFRTFromSubscriptPointer{public : } IMMORTAL_FRT;

DerivedFromBaseReturningFRTFromSubscriptPointer *makeDerivedFromBaseReturningFRTFromSubscriptPointer() {
    return new DerivedFromBaseReturningFRTFromSubscriptPointer();
}

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
  static DerivedFromImmortal *_Nonnull create() {
    return new DerivedFromImmortal();
  }
};

struct HasDestructor {
  ~HasDestructor() {}
};

struct IMMORTAL_FRT Immortal2 {
public:
  virtual void virtualMethod(HasDestructor) = 0;
  virtual void virtualRename() const
      __attribute__((swift_name("swiftVirtualRename()")));
};

inline const ImmortalBase *_Nonnull castToImmortalBase(
    const Immortal *_Nonnull immortal) {
  return static_cast<const ImmortalBase *>(immortal);
}

inline const Immortal *_Nonnull castToImmortal(
    const DerivedFromImmortal *_Nonnull immortal) {
  return static_cast<const Immortal *>(immortal);
}

//       A1
//      /  \
//     B1   B2
//    /  \
//  C1    C2

struct IMMORTAL_FRT A1 {
  virtual int virtualMethod() const { return 111; }

  __attribute__((swift_name("swiftFooRename()"))) 
  virtual int fooRename() const { return 112; }

  __attribute__((swift_name("swiftBarRename()"))) 
  virtual int barRename() const { return 113; }

  __attribute__((swift_name("swiftParamsRename(a1:)"))) 
  virtual int paramsRename(int i) const { return i; }

  static A1 *_Nonnull create() { return new A1(); }
};

struct B1 : A1 {
  __attribute__((swift_name("swiftVirtualMethod()")))
  virtual int virtualMethod() const override { return 211; }

  virtual int fooRename() const override { return 212; }
  
  __attribute__((swift_name("B1BarRename()"))) 
  virtual int barRename() const override { return 213; }

  __attribute__((swift_name("swiftParamsRename(b1:)"))) 
  virtual int paramsRename(int i) const override { return i; }

  static B1 *_Nonnull create() { return new B1(); }
};

struct B2 : A1 {
  int virtualMethod() const { return 221; }

  int fooRename() const { return 222; }

  __attribute__((swift_name("B2BarRename()"))) 
  int barRename() const {
    return 223;
  }

  static B2 *_Nonnull create() { return new B2(); }
};

struct C1 : B1 {
  __attribute__((swift_name("swiftFooRename()")))
  virtual int fooRename() const override { return 312; }

  __attribute__((swift_name("swiftBarRename()")))
  virtual int barRename() const override { return 313; }

  virtual int paramsRename(int i) const override { return i; }

  static C1 *_Nonnull create() { return new C1(); }
};

struct C2 : B1 {
  __attribute__((swift_name("swiftVirtualMethod()")))
  virtual int virtualMethod() const override { return 321; }

  __attribute__((swift_name("C2FooRename()")))
  virtual int fooRename() const override { return 322; }

  __attribute__((swift_name("B1BarRename()")))
  virtual int barRename() const override { return 323; }

  __attribute__((swift_name("swiftParamsRename(b1:)"))) 
  virtual int paramsRename(int i) const override { return i; }

  static C2 *_Nonnull create() { return new C2(); }
};

struct IMMORTAL_FRT A2 {
  __attribute__((swift_name("swiftVirtualMethod()")))
  virtual int virtualMethod() const { return 121; }

  __attribute__((swift_name("swiftFooRename()")))
  virtual int fooRename() const { return 122; }

  __attribute__((swift_name("A2BarRename()"))) 
  virtual int barRename() const { return 123; }

  __attribute__((swift_name("swiftParamsRename(a2:)"))) virtual int
  paramsRename(int i) const {
    return i + 1;
  }

  static A2 *_Nonnull create() { return new A2(); }
};

//  A1    A2   
//   \    /
//     D1

struct D1 : A1, A2 {
  static D1 *_Nonnull create() { return new D1(); }
};

//  A1       A2   
//   \       /
//    B1    / 
//      \  /
//       D2

struct D2 : B1, A2 {
   __attribute__((swift_name("swiftVirtualMethod()")))
  virtual int virtualMethod() const override { return 411; }

  virtual int fooRename() const override { return 412; }
  
  virtual int barRename() const override { return 413; }

  virtual int paramsRename(int i) const override { return i; }
};

//       A1
//      /  \
//     /    \
//    B1    B2
//    |\    /|
//    | \  / |
//    |  D3  |
//    C1     |
//     \     |
//      \   /
//        D4

struct D3 : B1, B2 {};

struct D4 : C1, B2 {};

struct ValueType {
  virtual int virtualMethod() const { return 111; }

  __attribute__((swift_name("swiftRenameMethodBase()")))
  virtual int renameMethodBase() const {
    return 112;
  }

  virtual int renameMethodDerived() const { return 113; }

  virtual int pureVirtualMethod() const = 0;

  __attribute__((swift_name("swiftPureRenameBase()")))
  virtual int pureRenameBase() const = 0;

  virtual int pureRenameDerived() const = 0;
};

struct IMMORTAL_FRT DerivedFRTValueType : ValueType {
  virtual int virtualMethod() const override { return 211; }

  virtual int renameMethodBase() const override { return 212; }

  __attribute__((swift_name("swiftRenameMethodDerived()")))
  virtual int renameMethodDerived() const override {
    return 213;
  }

  virtual int pureVirtualMethod() const override { return 214; }

  virtual int pureRenameBase() const override { return 215; }

  __attribute__((swift_name("swiftPureRenameDerived()"))) 
  virtual int pureRenameDerived() const override {
    return 216;
  }

  static DerivedFRTValueType *_Nonnull create() {
    return new DerivedFRTValueType();
  }
};

struct IMMORTAL_FRT EmptyDerivedFRTValueType : ValueType {};

struct DerivedValueType : ValueType {
  virtual int virtualMethod() const override { return 311; }

  virtual int renameMethodBase() const override { return 312; }

  __attribute__((swift_name("swiftRenameMethodDerived()"))) virtual int
  renameMethodDerived() const override {
    return 313;
  }

  virtual int pureVirtualMethod() const override { return 314; }

  virtual int pureRenameBase() const override { return 315; }

  __attribute__((swift_name("swiftPureRenameDerived()"))) virtual int
  pureRenameDerived() const override {
    return 316;
  }
};

struct IMMORTAL_FRT AbstractFRT {
  virtual int pureVirtualMethod() const = 0;

  __attribute__((swift_name("swiftPureRenameBase()")))
  virtual int pureRenameBase() const = 0;

  virtual int pureRenameDerived() const = 0;
};

struct DerivedAbstractFRT : AbstractFRT {
  virtual int pureVirtualMethod() const override { return 211; }

  virtual int pureRenameBase() const override { return 212; }

  __attribute__((swift_name("swiftPureRenameDerived()"))) 
  virtual int pureRenameDerived() const override {
    return 213;
  }

  static DerivedAbstractFRT *_Nonnull create() {
    return new DerivedAbstractFRT();
  }
};

struct EmptyDerivedAbstractFRT : AbstractFRT {};

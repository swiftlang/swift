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

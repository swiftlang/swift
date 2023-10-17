#pragma once

#define FRT                                       \
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
} FRT;

class CopyTrackedDerivedClass: public CopyTrackedBaseClass {
public:
    CopyTrackedDerivedClass(int x) : CopyTrackedBaseClass(x) {}

    int getDerivedX() const {
        return getX();
    }
} FRT;

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
} FRT;

class CopyTrackedDerivedDerivedClass: public NonEmptyBase, public CopyTrackedDerivedClass {
public:
    CopyTrackedDerivedDerivedClass(int x) : CopyTrackedDerivedClass(x) {}
} FRT;

CopyTrackedDerivedDerivedClass *makeCopyTrackedDerivedDerivedClass(int x) {
    return new CopyTrackedDerivedDerivedClass(x);
}

class BaseReturningFRTFromSubscript {
public:
    CopyTrackedDerivedClass &operator[](int x) const {
        return *new CopyTrackedDerivedClass(x);
    }
} FRT;

BaseReturningFRTFromSubscript *makeBaseReturningFRTFromSubscript() {
    return new BaseReturningFRTFromSubscript();
}

class DerivedFromBaseReturningFRTFromSubscript: public BaseReturningFRTFromSubscript {
public:
} FRT;

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
} FRT;

class DerivedFromBaseReturningFRTFromSubscriptPointer: public BaseReturningFRTFromSubscriptPointer {
public:
} FRT;

DerivedFromBaseReturningFRTFromSubscriptPointer *makeDerivedFromBaseReturningFRTFromSubscriptPointer() {
    return new DerivedFromBaseReturningFRTFromSubscriptPointer();
}

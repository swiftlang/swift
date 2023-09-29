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
    CopyTrackedBaseClass(int x) : x(x) {}
    CopyTrackedBaseClass(const CopyTrackedBaseClass &other) : x(other.x) {
        ++getCopyCounter();
    }

    int getX() const {
        return x;
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

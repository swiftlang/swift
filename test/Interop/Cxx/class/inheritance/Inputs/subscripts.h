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

    int operator [](int y) const {
        return y + x;
    }
private:
    int x;
};

class CopyTrackedDerivedClass: public CopyTrackedBaseClass {
public:
    CopyTrackedDerivedClass(int x) : CopyTrackedBaseClass(x) {}
};

class NonEmptyBase {
public:
    int getY() const {
        return y;
    }
private:
    int y = 11;
};

class CopyTrackedDerivedDerivedClass: public NonEmptyBase, public CopyTrackedDerivedClass {
public:
    CopyTrackedDerivedDerivedClass(int x) : CopyTrackedDerivedClass(x) {}
};

class SubscriptReturnsRef {
public:
    const int &operator [](int y) const {
        return x[y];
    }
    int &operator [](int y) {
        return x[y];
    }

private:
    int x[10] = {0};
};

class DerivedSubscriptReturnsRef: public SubscriptReturnsRef  {
public:
    inline DerivedSubscriptReturnsRef() : SubscriptReturnsRef() {}
};

class NonConstSubscriptReturnsRef {
public:
    int &operator [](int y) {
        return x[y];
    }

private:
    int x[10] = {0};
};

class DerivedNonConstSubscriptReturnsRef: public NonConstSubscriptReturnsRef  {
public:
    inline DerivedNonConstSubscriptReturnsRef() : NonConstSubscriptReturnsRef() {}
};

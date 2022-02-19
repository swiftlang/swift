#ifndef SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H
#define SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H

// TODO: tests for non-const getters.

struct VoidGetter {
    void getX();
    void setX(int);
};

struct VoidGetterNoName {
    void set();
};

struct IllegalIntReturnSetter {
    int setX(int);
};

struct TwoParameterSetter {
    void setX(int, int);
};

struct NoNameSetter {
    void set(int);
};

struct NoNameVoidGetter {
  void get();

};

struct LongNameAllLower {
    int getfoo() const { return 42; }
};

struct LongNameAllUpper {
    int getFOO() const { return 42; }
};

struct LongNameMix {
    int GetFoo() const { return 42; }
};

struct GetterOnly {
    int getFoo() const { return 42; }
};

struct NoNameUpperGetter {
    int Getter();
};

struct NotypeSetter {
    void setX();
};

struct IntGetterSetter {
    int getX() { return 42; }
    void setX(int) {}
};

struct UTF8Str  {
    int getUTF8Str() const { return 42; }
    void setUTF8Str(int) {}
};

// TODO: when setter and getter have different types.

#endif //SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H
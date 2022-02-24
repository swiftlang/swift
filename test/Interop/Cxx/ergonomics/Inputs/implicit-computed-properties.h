#ifndef SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H
#define SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H

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
  int setFOO(int v) const {}
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
  int val;
  int getX() const { return val; }
  void setX(int v) { val = v; }
};

// TODO: Omar please file a radar for this.
struct IntGetterSetterSnakeCaseUpper {
    int val;
    int Get_X() const { return val; }
    void Set_X(int v) { val = v; }
};

// TODO: Omar, please file a radar to deprecate methods when we transform them successfully (telling users to use 
//  the computed properties instead).

// TODO: Omar, please write a markdown file describing how names are imported and what this transformation does.

struct IntGetterSetterSnakeCase {
    int val;
    int get_x() const { return val; }
    void set_x(int v) { val = v; }
};

struct GetterHasArg {
    int getX(int v) const;
    void setX(int v);
};

struct GetterSetterIsUpper {
    int val;
    int GETX() const { return val; }
    void SETX(int v) { val = v; }
};

struct HasXAndY {
    int val;
    int GetXAndY() const { return val; }
    void SetXAndY(int v) { val = v; }
};

struct AllUpper {
    int val;
    int GETFOOANDBAR() const { return val; }
    void SETFOOANDBAR(int v) { val = v; }
};

struct BothUpper {
    int val;
    int getFOOAndBAR() const { return val; }
    void setFOOAndBAR(int v) { val = v; }
};

struct FirstUpper {
    int val;
    int getFOOAndBar() const { return val; }
    void setFOOAndBar(int v) { val = v; }
};

struct NonConstGetter {
    int val;
    int getX() { return val; }
    void setX(int v) { val = v; }
};

struct ConstSetter {
    mutable int val;
    int getX() const { return val; }
    void setX(int v) const { val = v; }
};

struct MultipleArgsSetter {
    int getX();
    void setX(int a, int b);
};

struct NonTrivial {
  int value = 42;
  ~NonTrivial() {}
};

struct PtrGetterSetter {
    int value = 42;
    int *getX() { return &value; }
    void setX(int *v) { value = *v; }
};

struct RefGetterSetter {
  int value = 42;
  const int &getX() { return value; }
  void setX(const int &v) { value = v; }
};

struct NonTrivialGetterSetter {
  NonTrivial value = {42};
  NonTrivial getX() { return value; }
  void setX(NonTrivial v) { value = v; }
};

struct DifferentTypes {
  NonTrivial value = {42};
  NonTrivial getX() { return value; }
  void setX(int v) { value = {v}; }
};

struct UTF8Str {
  int getUTF8Str() const { return 42; }
  void setUTF8Str(int) {}
};

struct MethodWithSameName {
    int value();
    int getValue() const;
    void setValue(int i);
};

struct PropertyWithSameName {
    int value;
    int getValue() const;
    void setValue(int i);
};

class PrivatePropertyWithSameName {
    int value;

public:
    int getValue() const;
    void setValue(int i);
};

#endif // SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H
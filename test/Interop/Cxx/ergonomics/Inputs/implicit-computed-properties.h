#ifndef SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H
#define SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H

//struct VoidGetter {
//    void getX();
//    void setX(int);
//};

//struct VoidGetterNoName {
//    void set();
//};
//
//struct IllegalIntReturnSetter {
//    int setX(int);
//};

//struct TwoParameterSetter {
//    void setX(int, int);
//};

//struct NoNameSetter {
//    void set(int);
//};

//struct NoNameVoidGetter {
//  void get();
//
//};
//
//struct LongNameAllLower {
//    int getfoo();
//};
//
//struct LongNameAllUpper {
//    int getFOO();
//};
//
//struct LongNameMix {
//    int GetFoo();
//};
//
//struct NoNameUpperGetter {
//    int Getter();
//};
//
////struct NotypeSetter {
////    void setX();
////};

struct IntGetterSetter {
//    int getX() const {}
    void setX(int) {}
};

#endif //SWIFT_IMPLICIT_COMPUTED_PROPERTIES_H
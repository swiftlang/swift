@import Foundation;

struct MyRect {
  float x;
  float y;
  float width;
  float height;
};

struct Trio {
  double i;
  double j;
  double k;
};

struct IntPair {
  int a;
  int b;
};

struct NestedInts {
  struct A {
    int value;
  } a;
  struct B {
    int value;
  } b;
};

@interface StructReturns
- (struct MyRect)newRect;
- (struct Trio)newTrio;
- (struct IntPair)newPair;
- (struct NestedInts)newNestedInts;
@end

@interface Gadget : NSObject
- (BOOL) negate:(BOOL) b;
@end

typedef NS_ENUM(unsigned short, ChooseTo) {
  ChooseToTakeIt = 709,
  ChooseToLeaveIt = 1709
};

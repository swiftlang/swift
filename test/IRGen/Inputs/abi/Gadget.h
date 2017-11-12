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

struct BigStruct {
  char a[32];
};

@interface StructReturns : NSObject
- (struct MyRect)newRect;
- (struct Trio)newTrio;
- (struct IntPair)newPair;
- (struct NestedInts)newNestedInts;
- (struct BigStruct)justReturn:(struct BigStruct) s;
@end

@interface Gadget : NSObject
- (BOOL) negate:(BOOL) b;
- (_Bool) invert:(_Bool) b;

- (BOOL) negateThrowing:(BOOL) b error:(NSError **) error;

// This one is not imported as a 'throws' function in Swift
- (_Bool) invertThrowing:(_Bool) b error:(NSError **) error;
@end

@protocol Pasta
-(void) alDente;
@end

typedef NS_ENUM(unsigned short, ChooseTo) {
  ChooseToTakeIt = 709,
  ChooseToLeaveIt = 1709
};

enum RawEnum {
  Intergalactic,
  Planetary
};

typedef struct One {
  float first;
  float second;
} One;

static inline One makeOne(float f, float s) {
  One one;
  one.first = f;
  one.second = s;

  return one;
}

static inline float MyRect_Area(struct MyRect rect) {
  return rect.width * rect.height;
}

// @literals inside static inline function
static inline void* giveMeASelector(void) {
  return @selector(init);
}

static inline NSNumber *giveMeANumber(void) {
  return @42;
}

static inline Class giveMeAMetaclass(void) {
  return [NSString class];
}

typedef struct FiveByteStruct {
    BOOL first;
    BOOL second;
    BOOL third;
    BOOL fourth;
    BOOL fifth;
} FiveByteStruct;


@interface MyClass : NSObject
+ (void)mymethod:(FiveByteStruct)param;
@end

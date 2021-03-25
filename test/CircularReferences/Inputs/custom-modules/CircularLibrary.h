#define MY_ENUM(_name) \
  enum _name _name; \
  enum __attribute__((enum_extensibility(open))) _name

typedef MY_ENUM(MyWholeNumber) {
  MyWholeNumberZero = 0,
  MyWholeNumberOne,
  MyWholeNumberTwo,
  MyWholeNumberThree,
  MyWholeNumberFour,
  MyWholeNumberFive,
  MyWholeNumberSix,
  MyWholeNumberSeven,
  MyWholeNumberEight
};

@interface MyNumberwang
- (_Nonnull instancetype)init;
@property (readonly) MyWholeNumber number;
@end

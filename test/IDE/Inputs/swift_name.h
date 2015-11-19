#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

int SNFoo SWIFT_NAME(Bar);

struct SWIFT_NAME(SomeStruct) SNSomeStruct {
  double X SWIFT_NAME(x);
};

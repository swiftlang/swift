#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

// Renaming global variables.
int SNFoo SWIFT_NAME(Bar);

// Renaming tags and fields.
struct SWIFT_NAME(SomeStruct) SNSomeStruct {
  double X SWIFT_NAME(x);
};

// Renaming C functions
struct SNSomeStruct SNMakeSomeStruct(double X, double Y) SWIFT_NAME(makeSomeStruct(x:y:));

struct SNSomeStruct SNMakeSomeStructForX(double X) SWIFT_NAME(makeSomeStruct(x:));

// swift_private attribute
void SNTransposeInPlace(struct SNSomeStruct *value) __attribute__((swift_private));

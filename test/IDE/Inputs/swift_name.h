#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

#ifndef SWIFT_ENUM_EXTRA
#  define SWIFT_ENUM_EXTRA
#endif

#ifndef SWIFT_ENUM
#  define SWIFT_ENUM(_name)    \
  enum _name _name;            \
  enum __attribute__((enum_extensibility(open))) \
       __attribute__((external_source_symbol(language="Swift", \
                      defined_in="swift_name", generated_declaration))) \
       SWIFT_ENUM_EXTRA _name
#endif

// Renaming global variables.
int SNFoo SWIFT_NAME(Bar);

// Renaming tags and fields.
struct SWIFT_NAME(SomeStruct) SNSomeStruct {
  double X SWIFT_NAME(x);
};

// Renaming C functions
struct SNSomeStruct SNMakeSomeStruct(double X, double Y) SWIFT_NAME(makeSomeStruct(x:y:));

struct SNSomeStruct SNMakeSomeStructForX(double X) SWIFT_NAME(makeSomeStruct(x:));

// Renaming typedefs.
typedef int SNIntegerType SWIFT_NAME(MyInt);

// Renaming enumerations.
SWIFT_ENUM(SNColorChoice) {
  SNColorRed SWIFT_NAME(Rouge),
  SNColorGreen,
  SNColorBlue
};

// swift_private attribute
void SNTransposeInPlace(struct SNSomeStruct *value) __attribute__((swift_private));

typedef struct {
  double x, y, z;
} SNPoint SWIFT_NAME(Point);

// Importing a value into a member.
extern double DefaultXValue __attribute__((swift_name("SomeStruct.defaultX")));

// Importing a function as a method.
struct SNSomeStruct SNAdding(struct SNSomeStruct *value, double x) SWIFT_NAME(SomeStruct.adding(self:x:));

// Importing a function as an initializer.
struct SNSomeStruct SNCreate(double x) SWIFT_NAME(SomeStruct.init(theX:));

// Importing a function as a static property getter.
struct SNSomeStruct SNSomeStructGetDefault(void) SWIFT_NAME(getter:SomeStruct.defaultValue());

// Importing a function as a static property setter.
void SNSomeStructSetDefault(struct SNSomeStruct value) SWIFT_NAME(setter:SomeStruct.defaultValue(_:));

// Importing a function as an instance property getter.
double SNSomeStructGetFoo(struct SNSomeStruct s) SWIFT_NAME(getter:SomeStruct.foo(self:));

// Importing a function as an instance property setter.
void SNSomeStructSetFoo(struct SNSomeStruct s, double value) SWIFT_NAME(setter:SomeStruct.foo(self:_:));

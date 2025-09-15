#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

#if __OBJC__
# define SWIFT_ENUM(_type, _name) \
  enum _name : _type _name; enum __attribute__((enum_extensibility(open))) _name : _type
#else
# define SWIFT_ENUM(_type, _name) \
  enum _name _name; enum __attribute__((enum_extensibility(open))) _name
#endif

void drawString(const char *, int x, int y) SWIFT_NAME(drawString(_:x:y:));

enum SWIFT_NAME(ColorKind) ColorType {
  CT_red,
  CT_green,
  CT_blue,
};

typedef SWIFT_ENUM(int, HomeworkExcuse) {
  HomeworkExcuseDogAteIt,
  HomeworkExcuseOverslept SWIFT_NAME(tired),
  HomeworkExcuseTooHard,
};

typedef struct SWIFT_NAME(Point) {
  int X SWIFT_NAME(x);
  int Y SWIFT_NAME(y);
} PointType;

typedef int my_int_t SWIFT_NAME(MyInt);

void spuriousAPINotedSwiftName(int);
void poorlyNamedFunction(const char *);

PointType readPoint(const char *path, void **errorOut) SWIFT_NAME(Point.init(path:));

struct BoxForConstants {
  int dummy;
};

enum {
  AnonymousEnumConstant SWIFT_NAME(BoxForConstants.anonymousEnumConstant)
};

#if __OBJC__
@interface Foo
- (instancetype)init;
- (instancetype)initWithFoo SWIFT_NAME(initWithFoo()); // expected-warning {{custom Swift name 'initWithFoo()' ignored because it is not valid for initializer; imported as 'init(foo:)' instead}}
@end

void acceptsClosure(id value, void (*fn)(void)) SWIFT_NAME(Foo.accepts(self:closure:)); // expected-note * {{'acceptsClosure' was obsoleted in Swift 3}}
void acceptsClosureStatic(void (*fn)(void)) SWIFT_NAME(Foo.accepts(closure:)); // expected-note * {{'acceptsClosureStatic' was obsoleted in Swift 3}}

enum {
  // Note that there was specifically a crash when renaming onto an ObjC class,
  // not just a struct.
  AnonymousEnumConstantObjC SWIFT_NAME(Foo.anonymousEnumConstant)
};
#endif

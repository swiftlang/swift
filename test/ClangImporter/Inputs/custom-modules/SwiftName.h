#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

#define SWIFT_ENUM(_type, _name) enum _name : _type _name; enum _name : _type

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

@interface Foo
- (instancetype)init;
@end

void acceptsClosure(id value, void (*fn)(void)) SWIFT_NAME(Foo.accepts(self:closure:));
void acceptsClosureStatic(void (*fn)(void)) SWIFT_NAME(Foo.accepts(closure:));

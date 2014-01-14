#import <foo_clang_dep_module.h>

// Types.
enum FooEnum1 { FooEnum1X };
enum FooEnum2 { FooEnum2X, FooEnum2Y };
enum FooEnum3 { FooEnum3X = 10, FooEnum3Y = 20 };

typedef int FooTypedef1;

extern int fooIntVar;

int fooFunc1(int a);
int fooFunc1AnonymousParam(int);
int fooFunc3(int a, float b, double c, int *d);

int redeclaredInMultipleModulesFunc1(int a);

@protocol FooProtocol
- (void)fooProtoFunc;
+ (void)fooProtoClassFunc;
@end

@interface FooClass <FooProtocol> {
  int fooIntIvar;
}

@property int fooProperty;

- (void) fooInstanceFunc0;
- (void) fooInstanceFunc1:(int)a;
- (void) fooInstanceFunc2:(int)a withB:(int)b;

+ (void) fooClassFunc0;
@end

@class BarForwardDeclaredClass;
enum BarforwardDeclaredEnum;

#define FOO_MACRO_1 0
#define FOO_MACRO_2 1
#define FOO_MACRO_3 (-1)
#define FOO_MACRO_4 0xffffffffu
#define FOO_MACRO_5 0xffffffffffffffffull


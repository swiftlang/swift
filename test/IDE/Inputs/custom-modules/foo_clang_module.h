#import <foo_clang_dep_module.h>

// Types.

/// Aaa.  FooEnum1.  Bbb.
enum FooEnum1 {
  /// Aaa.  FooEnum1X.  Bbb.
  FooEnum1X
};
enum FooEnum2 { FooEnum2X, FooEnum2Y };
enum FooEnum3 { FooEnum3X = 10, FooEnum3Y = 20 };

/// Aaa.  FooTypedef1.  Bbb.
typedef int FooTypedef1;

/// Aaa.  fooIntVar.  Bbb.
extern int fooIntVar;

/// Aaa.  fooFunc1.  Bbb.
int fooFunc1(int a);
int fooFunc1AnonymousParam(int);
int fooFunc3(int a, float b, double c, int *d);

/**
 * Aaa.  fooFuncWithComment1.  Bbb.
 * Ccc.
 *
 * Ddd.
 */
void fooFuncWithComment1(void);

/*!
  Aaa.  fooFuncWithComment2.  Bbb.
 */
void fooFuncWithComment2(void);

/**
 * Aaa.  fooFuncWithComment3.  Bbb.
 */
/**
 * Ccc.
 */
void fooFuncWithComment3(void);

/**
 * Aaa.  fooFuncWithComment4.  Bbb.
 */
/// Ddd.
void fooFuncWithComment4(void);

/// Aaa.  fooFuncWithComment5.  Bbb.
/// Ccc.
///
/// Ddd.
void fooFuncWithComment5(void);


/// Aaa.  redeclaredInMultipleModulesFunc1.  Bbb.
int redeclaredInMultipleModulesFunc1(int a);

/// Aaa.  FooProtocol.  Bbb.
@protocol FooProtocol

/// Aaa.  fooProtoFunc.  Bbb.
/// Ccc.
- (void)fooProtoFunc;

  /// Aaa.  fooProtoFuncWithExtraIndentation1.  Bbb.
  /// Ccc.
  - (void)fooProtoFuncWithExtraIndentation1;

  /**
   * Aaa.  fooProtoFuncWithExtraIndentation2.  Bbb.
   * Ccc.
   */
  - (void)fooProtoFuncWithExtraIndentation2;

+ (void)fooProtoClassFunc;

@property int fooProperty1;
@property (readwrite) int fooProperty2;
@property (readonly) int fooProperty3;
@end

@interface FooClass <FooProtocol> {
  int fooIntIvar;
}

@property int fooProperty1;
@property (readwrite) int fooProperty2;
@property (readonly) int fooProperty3;

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

#define FOO_MACRO_UNDEF_1 0
#undef FOO_MACRO_UNDEF_1

#define FOO_MACRO_REDEF_1 0
#undef FOO_MACRO_REDEF_1
#define FOO_MACRO_REDEF_1 1

#define FOO_MACRO_REDEF_2 0
#define FOO_MACRO_REDEF_2 1

void theLastDeclInFooClangModuleH();


#if !defined(__FOO_H__)
#define __FOO_H__ 1

#import <FooSub/FooSub.h>
#import <FooHelper/FooHelper.h>

// Types.

/// Aaa.  FooEnum1.  Bbb.
enum FooEnum1 {
  /// Aaa.  FooEnum1X.  Bbb.
  FooEnum1X
};
enum FooEnum2 { FooEnum2X, FooEnum2Y };
enum FooEnum3 { FooEnum3X = 10, FooEnum3Y = 20 };

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

/// Aaa.  FooComparisonResult.  Bbb.
typedef NS_ENUM(long, FooComparisonResult) {
  FooOrderedAscending = -1L,
  FooOrderedSame,
  FooOrderedDescending
};

/// Aaa.  FooRuncingOptions.  Bbb.
typedef NS_OPTIONS(long, FooRuncingOptions) {
  FooRuncingEnableMince = 1,
  FooRuncingEnableQuince = 2,
};

struct FooStruct1 {
  int x;
  double y;
};

typedef struct FooStruct2 {
  int x;
  double y;
} FooStructTypedef1;

typedef struct {
  int x;
  double y;
} FooStructTypedef2;

/// Aaa.  FooTypedef1.  Bbb.
typedef int FooTypedef1;

/// Aaa.  fooIntVar.  Bbb.
extern int fooIntVar;

/// Aaa.  fooFunc1.  Bbb.
int fooFunc1(int a);
int fooFunc1AnonymousParam(int);
int fooFunc3(int a, float b, double c, int *d);
void fooFuncWithBlock(int (^blk)(float x));

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

/// Aaa.  FooProtocolBase.  Bbb.
@protocol FooProtocolBase

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

@protocol FooProtocolDerived<FooProtocolBase>
@end

@interface FooClassBase
- (void) fooBaseInstanceFunc0;
- (FooClassBase *) fooBaseInstanceFunc1:(id)anObject;

- (void) fooBaseInstanceFuncOverridden;

+ (void) fooBaseClassFunc0;
@end

/// Aaa.  FooClassDerived.  Bbb.
@interface FooClassDerived : FooClassBase<FooProtocolDerived> {
  int fooIntIvar;
}

@property int fooProperty1;
@property (readwrite) int fooProperty2;
@property (readonly) int fooProperty3;

- (void) fooInstanceFunc0;
- (void) fooInstanceFunc1:(int)a;
- (void) fooInstanceFunc2:(int)a withB:(int)b;

- (void) fooBaseInstanceFuncOverridden;

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

#define FOO_MACRO_NOT_IMPORTED_1 10_9

void theLastDeclInFoo();

#endif /* ! __FOO_H__ */

@import Foundation;

#pragma clang assume_nonnull begin

// stdbool.h uses #define, so this test does as well.
#ifndef bool
# define bool _Bool
#endif

bool testCBool(bool);
BOOL testObjCBool(BOOL);
Boolean testDarwinBoolean(Boolean);

typedef bool (*CBoolFn)(bool);
typedef BOOL (*ObjCBoolFn)(BOOL);
typedef Boolean (*DarwinBooleanFn)(Boolean);

typedef bool (^CBoolBlock)(bool);
typedef BOOL (^ObjCBoolBlock)(BOOL);
typedef Boolean (^DarwinBooleanBlock)(Boolean);

__typeof(bool (^)(bool)) testCBoolFnToBlock(bool (*)(bool));
__typeof(BOOL (^)(BOOL)) testObjCBoolFnToBlock(BOOL (*)(BOOL));
__typeof(Boolean (^)(Boolean)) testDarwinBooleanFnToBlock(Boolean (*)(Boolean));

@interface Test : NSObject
@property bool propCBool __attribute__((swift_name("propCBool")));
@property BOOL propObjCBool __attribute__((swift_name("propObjCBool")));
@property Boolean propDarwinBoolean;

- (bool)testCBool:(bool)b;
- (BOOL)testObjCBool:(BOOL)b;
- (Boolean)testDarwinBoolean:(Boolean)b;

@property bool (^propCBoolBlock)(bool);
@property BOOL (^propObjCBoolBlock)(BOOL);
@property Boolean (^propDarwinBooleanBlock)(Boolean);

- (bool (^)(bool))testCBoolFnToBlock:(bool (*)(bool))fp;
- (BOOL (^)(BOOL))testObjCBoolFnToBlock:(BOOL (*)(BOOL))fp;
- (Boolean (^)(Boolean))testDarwinBooleanFnToBlock:(Boolean (*)(Boolean))fp;

- (instancetype)init;
@end

#pragma clang assume_nonnull end

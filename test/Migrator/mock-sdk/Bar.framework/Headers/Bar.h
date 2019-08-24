int barFunc1(int a);

int redeclaredInMultipleModulesFunc1(int a);

int barGlobalFunc(int a);

extern int barGlobalVariable;

extern int barGlobalVariableOldEnumElement;

int barGlobalFuncOldName(int a);

int barGlobalHoistedFuncOldName(int a, int b, int c);

@interface BarForwardDeclaredClass
- (id _Nonnull)initWithOldLabel0:(int)frame;
- (void) barInstanceFunc0;
- (void) barInstanceFunc1:(int)info anotherValue:(int)info1 anotherValue1:(int)info2 anotherValue2:(int)info3;
- (void) barInstanceFunc2:(int)info toRemove:(int)info1 toRemove1:(int)info2 toRemove2:(int)info3;
@end

enum BarForwardDeclaredEnum {
  BarForwardDeclaredEnumValue = 42
};

@interface PropertyUserInterface
- (int) field;
- (int * _Nullable) field2;
- (void) setField:(int)info;
- (void) setURL:(int)url;
+ (int) fieldPlus;
+ (void) methodPlus:(int)info;
+ (void) methodPlus;
@end

#define BAR_MACRO_1 0

typedef struct {
  int count;
  int theSimpleOldName;
  int theSimpleOldNameNotToRename;
} SomeItemSet;

typedef SomeItemSet SomeEnvironment;

@protocol WillOverrideWithTypeChange
- (SomeItemSet)doThing:(SomeItemSet)thing;
@end

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)

typedef NS_ENUM(long, FooComparisonResult) {
  FooOrderedAscending = -1L,
  FooOrderedSame,
  FooOrderedDescending,
  FooOrderedMemberSame,
  FooOrderedMovedToGlobal,
};

@interface BarBase
@end
@interface BarBaseNested
@end

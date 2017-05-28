int barFunc1(int a);

int redeclaredInMultipleModulesFunc1(int a);

int barGlobalFunc(int a);

int barGlobalVariable = 1;

int barGlobalVariableOldEnumElement = 1;

int barGlobalFuncOldName(int a);

@interface BarForwardDeclaredClass
- (id)initWithOldLabel0:(int)frame;
- (void) barInstanceFunc0;
- (void) barInstanceFunc1:(int)info anotherValue:(int)info1 anotherValue1:(int)info2 anotherValue2:(int)info3;
- (void) barInstanceFunc2:(int)info toRemove:(int)info1 toRemove1:(int)info2 toRemove2:(int)info3;
@end

enum BarForwardDeclaredEnum {
  BarForwardDeclaredEnumValue = 42
};

@interface PropertyUserInterface
- (int) field;
- (void) setField:(int)info;
@end

#define BAR_MACRO_1 0

typedef struct {
  int count;
  int theSimpleOldName;
} SomeItemSet;

typedef SomeItemSet SomeEnvironment;

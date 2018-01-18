extern int ANTGlobalValue;

@interface NewType
- (instancetype)init;
@end
@interface OldType
@end

@protocol TypeWithMethod
  -(void) minusPrint;
  +(void) plusPrint;
  @property int PropertyA;
@end

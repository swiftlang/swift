extern int ANTGlobalValue;

@interface NewType
- (instancetype)init;
@end
@interface OldType
- (instancetype)init;
@end

@protocol TypeWithMethod
  -(void) minusPrint;
  +(void) plusPrint;
  -(int) getPropertyA;
@end

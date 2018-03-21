extern int ANTGlobalValue;

@interface NewType
@end
@interface OldType
@end

@protocol TypeWithMethod
  -(void) minusPrint;
  +(void) plusPrint;
  -(int) getPropertyA;
@end

@protocol ObjcProt
  -(void) ProtMemberFunc;
  -(void) ProtMemberFunc2;
  -(void) ProtMemberFunc3;
@end

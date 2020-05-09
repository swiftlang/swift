@import ObjectiveC;

@protocol ObjcProt
  -(void) someFunctionFromProt;
@end

@protocol AnotherObjcProt
  -(void) anotherFunctionFromProt;
@end

@interface ClangInterface: NSObject <ObjcProt>
- (void)someFunction;
@end

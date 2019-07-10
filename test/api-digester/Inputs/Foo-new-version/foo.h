#import <Foundation.h>

@protocol ObjcProt
  -(void) someFunctionFromProt;

  -(void) someFunctionFromProt2;

@optional
  -(void) someOptionalFunctionFromProt;
@end

@protocol AnotherObjcProt
  -(void) anotherFunctionFromProt;
  -(void) anotherFunctionFromProt2;
@end

@interface ClangInterface: NSObject <ObjcProt>
- (void)someFunction;
@end

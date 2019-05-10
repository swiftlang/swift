#import <Foundation.h>

@protocol ObjcProt
  -(void) someFunctionFromProt;
@end

@protocol AnotherObjcProt
  -(void) anotherFunctionFromProt;
@end

@interface ClangInterface: NSObject <ObjcProt>
- (void)someFunction;
@end

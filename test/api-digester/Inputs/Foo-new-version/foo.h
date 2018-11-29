#import <Foundation.h>

@protocol ObjcProt
  -(void) someFunctionFromProt;

  -(void) someFunctionFromProt2;

@optional
  -(void) someOptionalFunctionFromProt;
@end

@interface ClangInterface: NSObject <ObjcProt>
- (void)someFunction;
@end

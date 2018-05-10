#import <Foundation.h>

@protocol ObjcProt
  -(void) someFunctionFromProt;
@end

@interface ClangInterface: NSObject <ObjcProt>
- (void)someFunction;
@end

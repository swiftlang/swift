@interface ObjCClass
- (void)methodFromHeader1:(int)param;
- (void)methodFromHeader2:(int)param;
- (void)methodFromHeader3:(int)param;
- (void)methodFromHeader4:(int)param;

// FIXME: test case involving swift_name
@end

@interface ObjCClass (PresentAdditions)
- (void)categoryMethodFromHeader1:(int)param;
- (void)categoryMethodFromHeader2:(int)param;
- (void)categoryMethodFromHeader3:(int)param;
- (void)categoryMethodFromHeader4:(int)param;

// FIXME: test case involving swift_name
@end

struct ObjCStruct {
  int foo;
};

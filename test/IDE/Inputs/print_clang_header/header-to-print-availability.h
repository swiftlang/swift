@interface MaybeAvailable
-(void)method1 __attribute__((availability(macosx, introduced=10.1)));
-(void)method2 __attribute__((availability(macosx, introduced=10_1)));
-(void)method3 __attribute__((availability(macosx, deprecated=10_10)));
-(void)method4 __attribute__((availability(macosx, introduced=10_1, deprecated=10_10, obsoleted=10_11)));
@end

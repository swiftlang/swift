@import ObjectiveC;

#ifdef MY_MACRO
@interface Hidden : NSObject
@end
#endif

@interface NotHidden : NSObject
@end

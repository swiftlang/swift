@import ObjectiveC;

#ifdef MY_MACRO
@interface Hidden : NSObject
@end
#endif

@interface Exposed : NSObject
@end

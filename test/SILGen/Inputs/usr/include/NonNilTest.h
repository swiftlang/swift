@import ObjectiveC;

@interface NonNilTest: NSObject
- (NonNilTest *)nonNilObject;
+ (NonNilTest *)nonNilObject;
@property NonNilTest *nonNilObjectProperty;
@property (unsafe_unretained) NonNilTest *unownedNonNilObjectProperty;
@end

@interface NSObject
+ alloc;
- init;
@end

@interface A : NSObject
- (int)method:(int)arg withDouble:(double)d;
+ (int)classMethod;
+ (int)classMethod:(int)arg;
- (int)counter;
@property int counter;
- (void)setCounter:(int)value;

- (int)informalProp;

- (int)informalMadeFormal;

@property int overriddenProp;
@end

@interface B : A
- (int)method:(int)arg withFloat:(float)f;
+ (int)classMethod:(int)arg withInt:(int)i;

@property (readonly) int readCounter;

@property int informalMadeFormal;

@property int overriddenProp;

- init;
- initWithInt:(int)i;
- initWithInt:(int)i andDouble:(double)d;
@end

@interface NSDate : NSObject
- (signed char)isEqualToDate:(NSDate *)anotherDate;
@end

NSDate *swift_createDate(void);

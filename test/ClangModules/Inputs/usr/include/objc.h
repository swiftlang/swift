@interface A
- (int)method:(int)arg withDouble:(double)d;
+ (int)classMethod;
+ (int)classMethod:(int)arg;
@end

@interface B : A
- (int)method:(int)arg withFloat:(float)f;
+ (int)classMethod:(int)arg withInt:(int)i;
@end


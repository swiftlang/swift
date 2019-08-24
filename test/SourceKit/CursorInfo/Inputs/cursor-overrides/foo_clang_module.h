@protocol P1
-(void)meth;
@end

@interface B1<P1>
-(void)meth;
@end

@interface S1 : B1
-(void)meth;
@end

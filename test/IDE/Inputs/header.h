void doSomethingInHead(int arg);

@interface BaseInHead
- (void)doIt:(int)arg;
@end

/// Awesome name.
@interface SameName
@end
@protocol SameName
@end

@interface BaseInHead(SomeCategory)
-(void)doItInCategory;
@end

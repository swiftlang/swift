@protocol Context
- (void) operate;
@end

@protocol A
- (void) use: (nonnull void(^)(__nonnull id)) callback;
@end

@protocol B<A>
@end

@protocol C<A>
- (void) use: (nonnull void(^)(__nonnull id<Context>)) callback;
@end

@protocol D<C, B>
@end

@interface NSObject
@end

@interface Widget : NSObject<D>
@end

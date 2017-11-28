@protocol Context
- (void) operate;
@end

@protocol A
- (void)use:(nonnull void (^)(_Nonnull id))callback;
@end

@protocol B<A>
@end

@protocol C<A>
- (void)use:(nonnull void (^)(_Nonnull id<Context>))callback;
@end

@protocol D<B, C>
@end

@interface NSObject
@end

@interface Widget : NSObject<D>
@end

@protocol ClassAndInstance
+ (void)doClassAndInstanceThing __attribute__((swift_name("doClassAndInstanceThing()")));
- (void)doClassAndInstanceThing __attribute__((swift_name("doClassAndInstanceThing()")));

@property (class, readonly, nonnull) id classAndInstanceProp;
@property (readonly, nonnull) id classAndInstanceProp;
@end

@interface Widget (ClassAndInstance) <ClassAndInstance>
@end

__attribute__((objc_root_class))
@interface Base
- (instancetype)init;
@end

@interface IncompleteDesignatedInitializers : Base
- (instancetype)initFirst:(long)x __attribute__((objc_designated_initializer));
- (instancetype)initSecond:(long)x __attribute__((objc_designated_initializer));
- (instancetype)initMissing:(long)x, ... __attribute__((objc_designated_initializer));
- (instancetype)initConveniently:(long)x;
@end
@interface IncompleteDesignatedInitializers (CategoryConvenience)
- (instancetype)initCategory:(long)x;
@end

@interface IncompleteConvenienceInitializers : Base
- (instancetype)initFirst:(long)x __attribute__((objc_designated_initializer));
- (instancetype)initSecond:(long)x __attribute__((objc_designated_initializer));
- (instancetype)initMissing:(long)x, ...;
- (instancetype)initConveniently:(long)x;
@end
@interface IncompleteConvenienceInitializers (CategoryConvenience)
- (instancetype)initCategory:(long)x;
@end

@interface IncompleteUnknownInitializers : Base
- (instancetype)initFirst:(long)x;
- (instancetype)initSecond:(long)x;
- (instancetype)initMissing:(long)x, ...;
- (instancetype)initConveniently:(long)x;
@end
@interface IncompleteUnknownInitializers (CategoryConvenience)
- (instancetype)initCategory:(long)x;
@end

@interface IncompleteDesignatedInitializersWithCategory : Base
- (instancetype)initFirst:(long)x __attribute__((objc_designated_initializer));
- (instancetype)initMissing:(long)x, ... __attribute__((objc_designated_initializer));
- (instancetype)initConveniently:(long)x;
@end
@interface IncompleteDesignatedInitializersWithCategory (/*class extension*/)
- (instancetype)initSecond:(long)x __attribute__((objc_designated_initializer));
- (instancetype)initCategory:(long)x;
@end

@interface DesignatedInitializerInAnotherModule : Base
- (instancetype)initFirst:(long)x __attribute__((objc_designated_initializer));
- (instancetype)initSecond:(long)x __attribute__((objc_designated_initializer));
- (instancetype)initMissing:(long)x, ... __attribute__((objc_designated_initializer));
- (instancetype)initConveniently:(long)x;
@end
@interface DesignatedInitializerInAnotherModule (CategoryConvenience)
- (instancetype)initCategory:(long)x;
@end

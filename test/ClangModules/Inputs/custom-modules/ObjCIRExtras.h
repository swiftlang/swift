@import Foundation;

#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

@interface PointerWrapper
@property void * __null_unspecified voidPtr;
@property int * __null_unspecified intPtr;
@property __null_unspecified id __autoreleasing * __null_unspecified idPtr;
@end

#pragma clang assume_nonnull begin
@interface SwiftNameTest : NSObject

// "Factory methods" that we'd rather have as initializers.
+ (instancetype)a SWIFT_NAME(init());
+ (instancetype)b SWIFT_NAME(init(dummyParam:));
+ (instancetype)c:(nullable id)x SWIFT_NAME(init(cc:));

// Would-be initializers.
+ (instancetype)testZ SWIFT_NAME(zz());
+ (instancetype)testY:(nullable id)x SWIFT_NAME(yy(aa:));
+ (instancetype)testX:(nullable id)x xx:(nullable id)xx SWIFT_NAME(xx(_:bb:));
@end

@interface SwiftNameTestError : NSObject
// Factory methods with NSError.
+ (nullable instancetype)err1:(NSError **)err SWIFT_NAME(init(error:));
+ (nullable instancetype)err2:(nullable id)x error:(NSError **)err SWIFT_NAME(init(aa:error:));
+ (nullable instancetype)err3:(nullable id)x error:(NSError **)err callback:(void(^)(void))block SWIFT_NAME(init(aa:error:block:));
+ (nullable instancetype)err4:(NSError **)err callback:(void(^)(void))block SWIFT_NAME(init(error:block:));

// Would-be initializers.
+ (nullable instancetype)testW:(nullable id)x error:(NSError **)err SWIFT_NAME(ww(_:error:));
+ (nullable instancetype)testV:(NSError **)err SWIFT_NAME(vv(v:));
@end

@interface SwiftNameTestSub : SwiftNameTest
@end

@interface SwiftNameTestErrorSub : SwiftNameTestError
@end
#pragma clang assume_nonnull end

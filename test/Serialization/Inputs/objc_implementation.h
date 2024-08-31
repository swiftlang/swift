#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface ObjCImpl : NSObject

- (instancetype)initWithNonSerialized:(int)value NS_SWIFT_NAME(init(nonSerialized:));
- (void)nonSerializedMethod;
@property (assign) int nonSerializedProperty;

@end

@interface ObjCImpl (CategoryMembers)

- (void)nonSerializedCategoryMethod;
@property (assign) int nonSerializedCategoryProperty;

@end

NS_ASSUME_NONNULL_END

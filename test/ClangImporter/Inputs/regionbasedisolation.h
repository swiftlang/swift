
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface ObjCObject : NSObject

- (void)loadObjectsWithCompletionHandler:
    (void (^NS_SWIFT_SENDABLE)(NSArray<NSObject *> *_Nullable,
                               NSError *_Nullable))completionHandler;

- (void)loadObjects2WithCompletionHandler:
    (void (^)(NSArray<NSObject *> *_Nullable,
                               NSError *_Nullable))completionHandler;

- (void)useValue:(id)object
    withCompletionHandler:(void (^)(NSObject *_Nullable))completionHandler;

@end

NS_ASSUME_NONNULL_END

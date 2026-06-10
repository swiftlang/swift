#include <Foundation/Foundation.h>

#pragma clang assume_nonnull begin

typedef void (^CompletionHandler)(int status, NSInteger bytesTransferred);

@interface PFXObject : NSObject
- (BOOL)enqueueFailingRequestWithData:(nullable NSMutableData *)data
                                error:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler;
- (BOOL)enqueuePassingRequestWithData:(nullable NSMutableData *)data
                                error:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler;
- (BOOL)enqueueFailingRequestWithData:(nullable NSMutableData *)data
                    completionTimeout:(NSTimeInterval)completionTimeout
                                error:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler;
- (BOOL)enqueuePassingRequestWithData:(nullable NSMutableData *)data
                    completionTimeout:(NSTimeInterval)completionTimeout
                                error:(NSError *_Nullable *)error
                    completionHandler:
                        (nullable CompletionHandler)completionHandler;
@end

#pragma clang assume_nonnull end

#include <Foundation/Foundation.h>

#pragma clang assume_nonnull begin

@interface PFXObject : NSObject {
}
- (BOOL)failReturnWithError:(NSError *_Nullable *)error
          completionHandler:
              (void (^_Nonnull)(NSError *_Nullable error))completionHandler;
- (BOOL)failInvokeSyncWithError:(NSError *_Nullable *)error
              completionHandler:
                  (void (^_Nonnull)(NSError *_Nullable error))completionHandler;
- (BOOL)failInvokeAsyncWithError:(NSError *_Nullable *)error
               completionHandler:(void (^_Nonnull)(NSError *_Nullable error))
                                     completionHandler;
- (BOOL)succeedSyncWithError:(NSError *_Nullable *)error
           completionHandler:
               (void (^_Nonnull)(NSError *_Nullable error))completionHandler;
- (BOOL)succeedAsyncWithError:(NSError *_Nullable *)error
            completionHandler:
                (void (^_Nonnull)(NSError *_Nullable error))completionHandler;
@end

#pragma clang assume_nonnull end

@import Foundation;

@protocol CallerX
@required
- (BOOL)use:(NSInteger)x error:(void (^_Nonnull)(void))callback error:(NSError*_Nullable*_Nullable)error;
@end;
@protocol CallerY
@required
- (BOOL)use:(NSInteger)x error:(NSError*_Nullable*_Nullable)error error:(void (^_Nonnull)(void))callback;
@end;

@interface CallerBaseY
- (BOOL)use:(NSInteger)x error:(NSError*_Nullable*_Nullable)error error:(void (^_Nonnull)(void))callback;
@end


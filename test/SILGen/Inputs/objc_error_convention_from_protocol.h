@import Foundation;

@protocol Caller1
@required
- (BOOL)call:(void (^_Nonnull)(void))callback error:(NSError**)error;
@end;

@protocol CallerA
@required
- (BOOL)use:(NSInteger)x thenCall:(void (^_Nonnull)(void))callback error:(NSError**)error;
@end;

@protocol CallerB
@required
- (BOOL)use:(NSInteger)x error:(NSError**)error thenCall:(void (^_Nonnull)(void))callback;
@end;

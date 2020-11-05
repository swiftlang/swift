#ifndef Delegate_h
#define Delegate_h

@import Foundation;

@interface Request : NSObject

@end

@interface Delegate : NSObject

- (void)makeRequest1:(Request * _Nonnull)request completionHandler:(void (^ _Nullable)(void))handler;

- (void)makeRequest2:(NSObject * _Nonnull)request completionHandler:(void (^ _Nullable)(void))handler;

- (void)makeRequest3:(Request * _Nullable)request completionHandler:(void (^ _Nullable)(void))handler;

@end


#endif

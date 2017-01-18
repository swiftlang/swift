@import Foundation;

@interface NonnullMembers<T> : NSObject

- (T _Nonnull) method;

@property (readonly) T _Nonnull property;

- (T _Nonnull)objectForKeyedSubscript:(id _Nonnull)key;

@end

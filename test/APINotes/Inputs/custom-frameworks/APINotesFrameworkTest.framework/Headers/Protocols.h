#ifdef __OBJC__
#pragma clang assume_nonnull begin

@class ForwardClass; // used by API notes

@protocol ProtoWithVersionedUnavailableMember
- (nullable id)requirement;
@end

#pragma clang assume_nonnull end
#endif // __OBJC__

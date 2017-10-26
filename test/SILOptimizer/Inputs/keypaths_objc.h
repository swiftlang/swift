@import Foundation;

@interface ObjCFoo

@property(readonly) NSString *_Nonnull objcProp;

@end


@interface ObjCFoo (Extras)

@property(readonly) NSString *_Nonnull objcExtraProp;

@end

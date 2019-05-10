@import ObjectiveC;
@import Foundation;

@interface ObjCBridgeNonconforming
@property NSSet<NSDictionary<NSString *, id> *> * _Nonnull foo;
@end

@interface ObjCBridgeGeneric<Element> : NSObject
@property NSSet<Element> * _Nonnull foo;
@end

@interface ElementBase : NSObject
@end
@protocol ExtraElementProtocol
@end
@interface ElementConcrete : ElementBase <ExtraElementProtocol>
@end

@interface ObjCBridgeGenericConstrained<Element: ElementBase *> : NSObject
@property NSSet<Element> * _Nonnull foo;
@end

@interface ObjCBridgeGenericInsufficientlyConstrained<Element: id <NSObject>> : NSObject
@property NSSet<Element> * _Nonnull foo;
@end

@interface ObjCBridgeGenericConstrainedExtra<Element: NSObject <ExtraElementProtocol> *> : NSObject
@property NSSet<Element> * _Nonnull foo;
@end

@interface ObjCBridgeExistential : NSObject
@property NSSet<NSObject<ExtraElementProtocol> *> * _Nonnull foo;
@end

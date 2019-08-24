@import ObjectiveC;
@import Foundation;

@interface SKWidget : NSObject
- (void)someObjCMethod;
@end

@interface SKWidget(ObjCAPI)
- (void)someObjCExtensionMethod;
@property (readwrite,strong,nonnull) NSObject *anObject;
@end

@interface NSObject (SKWidget)
- (void)doSomethingWithWidget:(nonnull SKWidget *)widget;
@end

extern NSString * _Nonnull const SKWidgetErrorDomain;
typedef enum __attribute__((ns_error_domain(SKWidgetErrorDomain))) __attribute__((swift_name("SKWidget.Error"))) SKWidgetErrorCode : NSInteger {
  SKWidgetErrorNone = 0,
  SKWidgetErrorBoom = 1
} SKWidgetErrorCode;

@interface SKWidget(Erroneous)
- (SKWidgetErrorCode)getCurrentError;
@end

extern void someKitGlobalFunc(void);

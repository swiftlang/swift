#if __OBJC__

@import Foundation;

@protocol Widget
@end

@interface WidgetStore : NSObject

// A completion-handler method that also carries an availability attribute. The
// importer must still expose its async alternative so an async
// '@objc @implementation' member can match it.
- (void)fetchWidgetsWithCompletionHandler:(void (^ _Nonnull)(NSArray<id<Widget>> * _Nonnull widgets))completionHandler
    API_AVAILABLE(macos(10.15), ios(13.0), watchos(6.0), tvos(13.0));

@end

#endif

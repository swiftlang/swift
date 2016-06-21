@import Foundation;

@protocol NSApplicationDelegate
@end

struct NSPoint {
  float x, y;
};

@interface NSReferencePoint : NSObject
@property float x;
@property float y;
@end

int NSApplicationMain(int argc, const char *argv[]);


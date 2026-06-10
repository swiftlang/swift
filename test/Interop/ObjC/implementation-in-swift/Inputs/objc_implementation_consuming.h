#pragma once
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface ObjCClass : NSObject

- (void)take:(NSObject *)obj;
+ (void)runTests;

@end

// Helper to call -takeObject: from ObjC, ensuring we go through objc_msgSend.
static inline void callTakeObjectFromObjC(ObjCClass *self, NSObject *obj) {
  [self take:obj];
}

NS_ASSUME_NONNULL_END

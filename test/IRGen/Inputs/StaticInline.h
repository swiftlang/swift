#import <Foundation/Foundation.h>

static inline NSString *staticInlineFun() {
  return [[NSLocale currentLocale] localeIdentifier];
}

static inline __attribute__((ns_returns_retained))
NSURL *_Nullable test(NSFileManager *self_, NSURL *originalItemURL,
                      NSURL *newItemURL, NSString *_Nullable backupItemName,
                      NSFileManagerItemReplacementOptions options,
                      NSError **_Nullable error) {
  NSURL *result = nil;
  BOOL success = [self_ replaceItemAtURL:originalItemURL
                           withItemAtURL:newItemURL
                          backupItemName:backupItemName
                                 options:options
                        resultingItemURL:&result
                                   error:error];
  return success ? result : nil;
}

#include "rdar80704382.h"

#pragma clang assume_nonnull begin

@implementation PFXObject
- (instancetype)init {
  if (self = [super init]) {
  }
  return self;
}
+ (void)getIdentifierForUserVisibleFileAtURL:(NSURL *)url
                           completionHandler:
                               (void (^)(FileProviderItemIdentifier __nullable
                                             itemIdentifier,
                                         FileProviderDomainIdentifier __nullable
                                             domainIdentifier,
                                         NSError *__nullable error))
                                   completionHandler {
  completionHandler(@"item_id", @"file_id", NULL);
}
@end

#pragma clang assume_nonnull end

#include <Foundation/Foundation.h>

#pragma clang assume_nonnull begin

typedef NSString *FileProviderItemIdentifier NS_EXTENSIBLE_STRING_ENUM;
FileProviderItemIdentifier const
    FileProviderRootContainerItemIdentifier NS_SWIFT_NAME(FileProviderItemIdentifier.rootContainer);
FileProviderItemIdentifier const
    FileProviderWorkingSetContainerItemIdentifier NS_SWIFT_NAME(FileProviderItemIdentifier.workingSet);
FileProviderItemIdentifier const
    FileProviderTrashContainerItemIdentifier NS_SWIFT_NAME(FileProviderItemIdentifier.trashContainer);
typedef NSString *FileProviderDomainIdentifier NS_EXTENSIBLE_STRING_ENUM;

@interface PFXObject : NSObject
+ (void)getIdentifierForUserVisibleFileAtURL:(NSURL *)url
                           completionHandler:
                               (void (^)(FileProviderItemIdentifier __nullable
                                             itemIdentifier,
                                         FileProviderDomainIdentifier __nullable
                                             domainIdentifier,
                                         NSError *__nullable error))
                                   completionHandler;
@end

#pragma clang assume_nonnull end

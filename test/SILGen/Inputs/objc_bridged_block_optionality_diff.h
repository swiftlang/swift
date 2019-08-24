@import Foundation;

#pragma clang assume_nonnull begin
typedef void (^HandlerBlock)(NSString *(^message)(void));
#pragma clang assume_nonnull end

/// Default handler for logging.
extern _Null_unspecified HandlerBlock TheHandlerBlock;

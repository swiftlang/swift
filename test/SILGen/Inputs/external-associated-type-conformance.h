@import Foundation;

extern NSErrorDomain const BadErrorDomain;

typedef enum __attribute__((ns_error_domain(BadErrorDomain), swift_name("BadError")))
BadError: NSInteger {
  BadBig,
  BadSmall,
} BadError;



typedef char* NSString;

typedef NSString *NSValueTransformerName __attribute__((swift_wrapper(struct)));

extern "C" NSValueTransformerName const NSUnarchiveFromDataTransformerName
  __attribute__((availability(macos,introduced=10.3,deprecated=10.14,replacement="NSSecureUnarchiveFromDataTransformerName")))
  __attribute__((availability(ios,introduced=3.0,deprecated=12.0,replacement="NSSecureUnarchiveFromDataTransformerName")))
  __attribute__((availability(watchos,introduced=2.0,deprecated=5.0,replacement="NSSecureUnarchiveFromDataTransformerName")))
  __attribute__((availability(tvos,introduced=9.0,deprecated=12.0,replacement="NSSecureUnarchiveFromDataTransformerName")));
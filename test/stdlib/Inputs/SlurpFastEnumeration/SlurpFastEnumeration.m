#include <Foundation/Foundation.h>

void slurpFastEnumerationFromObjCImpl(NSDictionary *d,
                                      id<NSFastEnumeration> fe,
                                      NSMutableArray *keyValuePairs) {
  for (NSObject *key in fe) {
    [keyValuePairs addObject: [NSArray arrayWithObjects: key, d[key], nil]];
  }
}


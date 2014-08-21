#include <Foundation/Foundation.h>

void slurpFastEnumerationFromObjCImpl(id Dictionary,
                                      id<NSFastEnumeration> FE,
                                      NSMutableArray *KeyValuePairs) {
  NSDictionary *NSD = Dictionary;
  for (NSObject *Key in FE) {
    [KeyValuePairs addObject: Key];
    [KeyValuePairs addObject: NSD[Key]];
  }
}


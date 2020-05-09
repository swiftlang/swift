
#if __has_include(<Foundation/Foundation.h>)
#include <Foundation/Foundation.h>

void slurpFastEnumerationOfArrayFromObjCImpl(id Array, id<NSFastEnumeration> FE,
                                             NSMutableArray *Values) {
  NSArray *NSA = Array;
  for (NSObject *Value in FE) {
    [Values addObject: Value];
  }
}

void slurpFastEnumerationOfDictionaryFromObjCImpl(
    id Dictionary, id<NSFastEnumeration> FE, NSMutableArray *KeyValuePairs) {
  NSDictionary *NSD = Dictionary;
  for (NSObject *Key in FE) {
    [KeyValuePairs addObject: Key];
    [KeyValuePairs addObject: NSD[Key]];
  }
}
#endif


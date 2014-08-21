#include <Foundation/Foundation.h>

// Note: 'Dictionary' is declared 'id' instead of 'NSDictionary' to avoid
// automatic bridging to 'Swift.Dictionary' when this API is imported to Swift.
void slurpFastEnumerationFromObjCImpl(id Dictionary,
                                      id<NSFastEnumeration> FE,
                                      NSMutableArray *KeyValuePairs);

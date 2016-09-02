#include <Foundation/Foundation.h>

// Note: 'Array' is declared 'id' instead of 'NSArray' to avoid automatic
// bridging to 'Swift.Array' when this API is imported to Swift.
void slurpFastEnumerationOfArrayFromObjCImpl(id Array, id<NSFastEnumeration> FE,
                                             NSMutableArray *Values);

// Note: 'Dictionary' is declared 'id' instead of 'NSDictionary' to avoid
// automatic bridging to 'Swift.Dictionary' when this API is imported to Swift.
void slurpFastEnumerationOfDictionaryFromObjCImpl(
    id Dictionary, id<NSFastEnumeration> FE, NSMutableArray *KeyValuePairs);


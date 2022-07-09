#include <Foundation/NSString.h>

void useNSString(NSString  * _Nonnull s);
void useOptNSString(NSString  * _Nullable s);

NSString * _Nonnull returnNSString();
NSString * _Nullable returnOptNSString(BOOL some);
NSString * _Nonnull returnNullNSString();


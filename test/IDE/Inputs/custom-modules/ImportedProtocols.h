#ifndef IMPORTED_PROTOCOLS_H
#define IMPORTED_PROTOCOLS_H

@import Foundation;

@protocol ImportedProtocolBase;
@protocol ImportedProtocolBase <NSObject>
@end
typedef NSObject<ImportedProtocolBase> * ImportedProtocolBase_t;

@protocol IPSub;
@protocol IPSub <ImportedProtocolBase>
@end
typedef NSObject<IPSub> * IPSub_t;

typedef NSObject<IPSub> * Dummy;

void takesIPSub(IPSub_t in);

#endif // IMPORTED_PROTOCOLS_H

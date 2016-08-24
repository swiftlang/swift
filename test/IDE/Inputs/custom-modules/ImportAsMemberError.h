#ifndef IMPORT_AS_MEMBER_ERR_H
#define IMPORT_AS_MEMBER_ERR_H

struct __attribute__((swift_name("ErrorStruct"))) IAMStruct {
  double x, y, z;
};

@import Foundation;

@protocol ImportedProtocolBase;
@protocol ImportedProtocolBase <NSObject>
@end
typedef NSObject<ImportedProtocolBase> *ImportedProtocolBase_t;

// Non-prototype declaration
extern void IAMErrorStructHasPrototype(void)
    __attribute__((swift_name("ErrorStruct.hasPrototype()"))); // ok

#endif // IMPORT_AS_MEMBER_ERR_H

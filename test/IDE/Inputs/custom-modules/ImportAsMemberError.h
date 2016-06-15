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

@protocol ErrorProto;
@protocol ErrorProto <ImportedProtocolBase>
@end

typedef NSObject<ErrorProto> *ErrorProto_t;

// Instance and static member onto protocol
void mutateSomeStaticState(void)
    __attribute__((swift_name("ErrorProto.mutateSomeStaticState()"))); // ok
void mutateSomeInstanceState(ErrorProto_t self) __attribute__((
    swift_name("ErrorProto.mutateSomeInstanceState(self:)"))); // error

// Non-prototype declaration
extern void IAMErrorStructHasPrototype(void)
    __attribute__((swift_name("ErrorStruct.hasPrototype()"))); // ok

#endif // IMPORT_AS_MEMBER_ERR_H

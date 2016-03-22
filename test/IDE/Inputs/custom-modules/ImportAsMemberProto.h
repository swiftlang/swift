#ifndef IMPORT_AS_MEMBER_PROTO_H
#define IMPORT_AS_MEMBER_PROTO_H

@import Foundation;

@protocol ImportedProtocolBase;
@protocol ImportedProtocolBase <NSObject>
@end
typedef NSObject<ImportedProtocolBase> * ImportedProtocolBase_t;

@protocol IAMProto;
@protocol IAMProto <ImportedProtocolBase>
@end

// HACK HACK HACK: There is a bug in the importer where we can't begin with the type name...
typedef NSObject<IAMProto> * zIAMProto_t;
typedef NSObject<IAMProto> * Dummy;


void mutateSomeState(zIAMProto_t)
__attribute__((swift_name("IAMProto.mutateSomeState(self:)")));

void mutateSomeStateWithOtherProto(zIAMProto_t, zIAMProto_t other)
__attribute__((swift_name("IAMProto.mutateSomeState(self:otherProto:)")));

int getSomeValue(zIAMProto_t)
__attribute__((swift_name("getter:IAMProto.someValue(self:)")));
int setSomeValue(zIAMProto_t, int v)
__attribute__((swift_name("setter:IAMProto.someValue(self:_:)")));

#endif // IMPORT_AS_MEMBER_PROTO_H

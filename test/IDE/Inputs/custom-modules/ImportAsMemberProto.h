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

typedef NSObject<IAMProto> * IAMProto_t;

void mutateSomeState(IAMProto_t)
__attribute__((swift_name("IAMProto.mutateSomeState(self:)")));

void mutateSomeStateWithOtherProto(IAMProto_t, IAMProto_t other)
__attribute__((swift_name("IAMProto.mutateSomeState(self:otherProto:)")));

int getSomeValue(IAMProto_t)
__attribute__((swift_name("getter:IAMProto.someValue(self:)")));
int setSomeValue(IAMProto_t, int v)
__attribute__((swift_name("setter:IAMProto.someValue(self:_:)")));

#endif // IMPORT_AS_MEMBER_PROTO_H

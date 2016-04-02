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

void mutateSomeStateWithParameter(IAMProto_t, NSInteger)
__attribute__((swift_name("IAMProto.mutateSomeState(self:withParameter:)")));

void mutateSomeStateWithFirstParameter(NSInteger, IAMProto_t)
__attribute__((swift_name("IAMProto.mutateSomeState(withFirstParameter:self:)")));

int getSomeValue(IAMProto_t)
__attribute__((swift_name("getter:IAMProto.someValue(self:)")));
int setSomeValue(IAMProto_t, int v)
__attribute__((swift_name("setter:IAMProto.someValue(self:_:)")));

#endif // IMPORT_AS_MEMBER_PROTO_H

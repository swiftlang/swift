@import Base;
@import ObjectiveC;

BaseClass *getBaseClassObjC();
void useBaseClassObjC(BaseClass *);

@interface UserClass : NSObject <BaseProto>
@end

id <BaseProto> getBaseProtoObjC();
void useBaseProtoObjC(id <BaseProto>);


@interface BaseClass (ObjCExtensions)
- (void)categoryMethod;
@end


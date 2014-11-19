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

typedef OBJC_ENUM(unsigned char, BaseEnumObjC) {
  BaseEnumObjCZippity = BaseEnumZim,
  BaseEnumObjCDoo = BaseEnumZang,
  BaseEnumObjCDah = BaseEnumZung,
};

BaseEnum getBaseEnum();
void useBaseEnum(BaseEnum);

BaseEnumObjC getBaseEnumObjC();
void useBaseEnumObjC(BaseEnumObjC);


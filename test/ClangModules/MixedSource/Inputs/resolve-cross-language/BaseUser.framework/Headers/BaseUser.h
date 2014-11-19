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
- (BaseEnum)baseEnumMethod:(BaseEnum)be;
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

@protocol EnumProto
- (BaseEnum)getEnum;
@end

@interface AnotherClass (EnumProtoConformance) <EnumProto>
@end

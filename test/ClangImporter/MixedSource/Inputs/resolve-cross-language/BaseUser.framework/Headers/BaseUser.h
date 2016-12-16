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
- (RenamedEnum)renamedEnumMethod:(RenamedEnum)se;
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

// temporarily redefine OBJC_ENUM because ClangImporter cares about the macro name
#undef OBJC_ENUM
#define OBJC_ENUM(_type, _name, SWIFT_NAME) enum _name : _type _name __attribute__((swift_name(SWIFT_NAME))); enum __attribute__((swift_name(SWIFT_NAME))) _name : _type

typedef OBJC_ENUM(unsigned char, RenamedEnumObjC, "SwiftEnumObjC") {
  RenamedEnumObjCQuux = RenamedEnumQuux,
  RenamedEnumObjCCorge = RenamedEnumCorge,
  RenamedEnumObjCGrault = RenamedEnumGrault,
};

// put OBJC_ENUM back just in case
#undef OBJC_ENUM
#define OBJC_ENUM(_type, _name) enum _name : _type _name; enum _name : _type

RenamedEnum getRenamedEnum();
void useRenamedEnum(RenamedEnum);

RenamedEnumObjC getRenamedEnumObjC();
void useRenamedEnumObjC(RenamedEnumObjC);

@protocol EnumProto
- (BaseEnum)getEnum;
- (RenamedEnum)getSwiftEnum;
@end

@interface AnotherClass (EnumProtoConformance) <EnumProto>
@end

@protocol AnotherProto
@end
@protocol ExtendsTwoProtosOneOfWhichIsFromSwift <BaseProto, AnotherProto>
@end
@interface ExtendsTwoProtosImpl : NSObject <ExtendsTwoProtosOneOfWhichIsFromSwift>
@end

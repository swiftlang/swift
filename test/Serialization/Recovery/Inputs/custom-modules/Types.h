@interface Object
- (nonnull instancetype)init;
@end

@interface RenamedClass : Object
@end

@interface RenamedGenericClass<Element> : Object
@end

typedef int RenamedTypedef;
typedef int NewlyWrappedTypedef __attribute__((swift_wrapper(struct)));
typedef int RenamedWrappedTypedef __attribute__((swift_wrapper(struct)));

struct RenamedStruct {
  int value;
};

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type

typedef CF_ENUM(long, RenamedEnum) {
  RenamedEnumA
};

@protocol RenamedProtocol
@end

@interface RenamedClass (RenamedProtocol) <RenamedProtocol>
@end

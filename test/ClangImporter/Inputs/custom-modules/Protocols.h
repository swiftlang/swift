@protocol FooProto
@property int bar;
@end

@protocol AnotherProto
@end

Class <FooProto> _Nonnull processFooType(Class <FooProto> _Nonnull);
Class <FooProto, AnotherProto> _Nonnull processComboType(Class <FooProto, AnotherProto> _Nonnull);
Class <AnotherProto, FooProto> _Nonnull processComboType2(Class <AnotherProto, FooProto> _Nonnull);


@protocol SubProto <FooProto>
@end

@interface ProtocolTestingBase
@end

@interface SubProtoImpl: ProtocolTestingBase <SubProto>
@end

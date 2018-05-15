@import Foundation;

@interface GlobalToMember_Class_Container : NSObject
@end
@interface GlobalToMember_Class_Payload : NSObject
@end

@interface MemberToGlobal_Class_Container : NSObject
@end
@interface MemberToGlobal_Class_Payload: NSObject
@end

@interface MemberToMember_Class_Swift3 : NSObject
@end
@interface MemberToMember_Class_Swift4 : NSObject
@end
@interface MemberToMember_Class_Payload : NSObject
@end

@interface GlobalToMember_Typedef_Container : NSObject
@end
typedef Foo* GlobalToMember_Typedef_Payload;

@interface MemberToGlobal_Typedef_Container : NSObject
@end
typedef Foo* MemberToGlobal_Typedef_Payload;

@interface MemberToMember_Typedef_Swift3 : NSObject
@end
@interface MemberToMember_Typedef_Swift4 : NSObject
@end
typedef Foo* MemberToMember_Typedef_Payload;

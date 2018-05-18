@import Foundation;

// ===-------------------------------------------------------------------------
// class Payload
// ===-------------------------------------------------------------------------

// 3: Payload
// 4: Namespace.Payload
@interface GlobalToMember_Class_Container : NSObject
@end
@interface GlobalToMember_Class_Payload : NSObject
@end

// 3: Namespace.Payload
// 4: Payload
@interface MemberToGlobal_Class_Container : NSObject
@end
@interface MemberToGlobal_Class_Payload: NSObject
@end

// 3: Namespace_Swift3.PayloadFor3
// 4: Namespace_Swift4.PayloadFor4
@interface MemberToMember_Class_Swift3 : NSObject
@end
@interface MemberToMember_Class_Swift4 : NSObject
@end
@interface MemberToMember_Class_Payload : NSObject
@end

// 3: Namespace.PayloadFor3
// 4: Namespace.PayloadFor4
@interface MemberToMember_SameContainer_Class_Container : NSObject
@end
@interface MemberToMember_SameContainer_Class_Payload : NSObject
@end

// 3: Namespace_Swift3.Payload
// 4: Namespace_Swift4.Payload
@interface MemberToMember_SameName_Class_Swift3 : NSObject
@end
@interface MemberToMember_SameName_Class_Swift4 : NSObject
@end
@interface MemberToMember_SameName_Class_Payload : NSObject
@end

// ===-------------------------------------------------------------------------
// typealias Payload
// ===-------------------------------------------------------------------------

// 3: Payload
// 4: Namespace.Payload
@interface GlobalToMember_Typedef_Container : NSObject
@end
typedef Foo* GlobalToMember_Typedef_Payload;

// 3: Namespace.Payload
// 4: Payload
@interface MemberToGlobal_Typedef_Container : NSObject
@end
typedef Foo* MemberToGlobal_Typedef_Payload;

// 3: Namespace_Swift3.PayloadFor3
// 4: Namespace_Swift4.PayloadFor4
@interface MemberToMember_Typedef_Swift3 : NSObject
@end
@interface MemberToMember_Typedef_Swift4 : NSObject
@end
typedef Foo* MemberToMember_Typedef_Payload;

// 3: Namespace.PayloadFor3
// 4: Namespace.PayloadFor4
@interface MemberToMember_SameContainer_Typedef_Container : NSObject
@end
typedef Foo* MemberToMember_SameContainer_Typedef_Payload;

// 3: Namespace_Swift3.Payload
// 4: Namespace_Swift4.Payload
@interface MemberToMember_SameName_Typedef_Swift3 : NSObject
@end
@interface MemberToMember_SameName_Typedef_Swift4 : NSObject
@end
typedef Foo* MemberToMember_SameName_Typedef_Payload;

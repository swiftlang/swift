@class Public;
@class Internal;

enum InternalInner { a, b, c } __attribute__((swift_name("Public.Inner")));
enum PublicInner { d, e, f } __attribute__((swift_name("Internal.Inner")));
enum TopLevel { g, h, i };

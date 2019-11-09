@class Outer;
struct NestedInOuter {
  int a;
} __attribute((swift_name("Outer.Nested")));

@class OuterByObjCName_ObjC;
struct NestedInOuterByObjCName {
  int b;
} __attribute((swift_name("OuterByObjCName_ObjC.Nested")));

@class OuterBySwiftName_Swift;
struct NestedInOuterBySwiftName {
  int c;
} __attribute((swift_name("OuterBySwiftName_Swift.Nested")));

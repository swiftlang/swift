@import Foundation;

NSString * const MyErrorDomain;
typedef NS_ENUM(int, MyError) {
  MyErrorGood,
  MyErrorBad,
} __attribute__((ns_error_domain(MyErrorDomain)));

NSString * const MyRenamedErrorDomain;
typedef NS_ENUM(int, MyRenamedError) {
  MyRenamedErrorGood,
  MyRenamedErrorBad,
} __attribute__((ns_error_domain(MyRenamedErrorDomain))) __attribute__((swift_name("RenamedError")));


struct Wrapper {
  int unrelatedValue;
};

NSString * const MyMemberErrorDomain;
typedef NS_ENUM(int, MyMemberError) {
  MyMemberErrorA,
  MyMemberErrorB,
} __attribute__((ns_error_domain(MyMemberErrorDomain))) __attribute__((swift_name("Wrapper.MemberError")));

// Not actually an error enum, but it can still hang with us.
typedef NS_ENUM(int, MyMemberEnum) {
  MyMemberEnumA,
  MyMemberEnumB,
} __attribute__((swift_name("Wrapper.MemberEnum")));


typedef int WrapperByAttribute __attribute__((swift_wrapper(struct)));

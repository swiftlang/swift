@import Foundation;

#define NS_REFINED_FOR_SWIFT __attribute__((swift_private))

NS_REFINED_FOR_SWIFT
@protocol PrivProto
@end
  
@interface Foo : NSObject <PrivProto>
@property id privValue NS_REFINED_FOR_SWIFT;
@end

NS_REFINED_FOR_SWIFT
@interface PrivFooSub : Foo
@end

void privTest() NS_REFINED_FOR_SWIFT;

struct S0 {
  int privValue NS_REFINED_FOR_SWIFT;
};

struct PrivS1 {
  int value;
} NS_REFINED_FOR_SWIFT;

typedef struct {
  int value;
} PrivS2 NS_REFINED_FOR_SWIFT;

enum /* anonymous */ {
  PrivAnonymousA NS_REFINED_FOR_SWIFT
};

enum E0 {
  E0PrivA NS_REFINED_FOR_SWIFT
};

enum PrivE1 {
  PrivE1A
} NS_REFINED_FOR_SWIFT;

typedef enum {
  PrivE2A
} PrivE2 NS_REFINED_FOR_SWIFT;

typedef NS_ENUM(long, PrivNSEnum) {
  PrivNSEnumA
} NS_REFINED_FOR_SWIFT;

typedef NS_ENUM(long, NSEnum) {
  NSEnumPrivA NS_REFINED_FOR_SWIFT,
  NSEnumB
};

typedef NS_OPTIONS(long, PrivNSOptions) {
  PrivNSOptionsA = 1
} NS_REFINED_FOR_SWIFT;

typedef NS_OPTIONS(long, NSOptions) {
  NSOptionsPrivA NS_REFINED_FOR_SWIFT = 1,
  NSOptionsB = 2
};

typedef struct __attribute__((objc_bridge(id))) _PrivCFTypeRef *PrivCFTypeRef NS_REFINED_FOR_SWIFT;
typedef PrivCFTypeRef PrivCFSubRef NS_REFINED_FOR_SWIFT;
typedef int PrivInt NS_REFINED_FOR_SWIFT;

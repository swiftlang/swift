@import Foundation;

#define GET_UNTYPED_AND_OPAQUE(Name) \
  static NSArray * _Nonnull getUntypedNSArrayOf##Name##s() { \
    return getTypedNSArrayOf##Name##Wrappers(); \
  } \
  static id _Nonnull getOpaqueNSArrayOf##Name##s() { \
    return getTypedNSArrayOf##Name##Wrappers(); \
  }
#define CHECK_UNTYPED_AND_OPAQUE(Name) \
  static BOOL checkUntypedNSArrayOf##Name##s(NSArray * _Nonnull array) { \
    return checkTypedNSArrayOf##Name##Wrappers(array); \
  } \
  static BOOL checkOpaqueNSArrayOf##Name##s(id _Nonnull array) { \
    return checkTypedNSArrayOf##Name##Wrappers(array); \
  }

typedef NSString *StringWrapper NS_TYPED_EXTENSIBLE_ENUM;
static NSArray<StringWrapper> * _Nonnull getTypedNSArrayOfStringWrappers() {
  return @[@"abc", @"def"];
}
static BOOL
checkTypedNSArrayOfStringWrappers(NSArray<StringWrapper> * _Nonnull array) {
  return [array isEqual:getTypedNSArrayOfStringWrappers()];
}
GET_UNTYPED_AND_OPAQUE(String)
CHECK_UNTYPED_AND_OPAQUE(String)


typedef id <NSCopying, NSCoding> CopyingAndCodingWrapper
    NS_TYPED_EXTENSIBLE_ENUM;
static NSArray<CopyingAndCodingWrapper> * _Nonnull
getTypedNSArrayOfCopyingAndCodingWrappers() {
  return @[@"abc", @[]];
}
static BOOL checkTypedNSArrayOfCopyingAndCodingWrappers(
    NSArray<CopyingAndCodingWrapper> * _Nonnull array) {
  return [array isEqual:getTypedNSArrayOfCopyingAndCodingWrappers()];
}
GET_UNTYPED_AND_OPAQUE(CopyingAndCoding)
CHECK_UNTYPED_AND_OPAQUE(CopyingAndCoding)

typedef id <NSCopying> CopyingWrapper NS_TYPED_EXTENSIBLE_ENUM;
static NSArray<CopyingWrapper> * _Nonnull getTypedNSArrayOfCopyingWrappers() {
  return getTypedNSArrayOfCopyingAndCodingWrappers();
}
static BOOL
checkTypedNSArrayOfCopyingWrappers(NSArray<CopyingWrapper> * _Nonnull array) {
  return [array isEqual:getTypedNSArrayOfCopyingWrappers()];
}
GET_UNTYPED_AND_OPAQUE(Copying)
CHECK_UNTYPED_AND_OPAQUE(Copying)

typedef NSObject *ObjectWrapper NS_TYPED_EXTENSIBLE_ENUM;
static NSArray<ObjectWrapper> * _Nonnull getTypedNSArrayOfObjectWrappers() {
  return (NSArray<ObjectWrapper> *)getTypedNSArrayOfCopyingAndCodingWrappers();
}
static BOOL
checkTypedNSArrayOfObjectWrappers(NSArray<ObjectWrapper> * _Nonnull array) {
  return [array isEqual:getTypedNSArrayOfObjectWrappers()];
}
GET_UNTYPED_AND_OPAQUE(Object)
CHECK_UNTYPED_AND_OPAQUE(Object)

typedef NSError *ErrorWrapper NS_TYPED_EXTENSIBLE_ENUM;
static NSArray<ErrorWrapper> * _Nonnull getTypedNSArrayOfErrorWrappers() {
  return @[[NSError errorWithDomain:@"x" code:11 userInfo:nil],
           [NSError errorWithDomain:@"x" code:22 userInfo:nil]];
}
static BOOL
checkTypedNSArrayOfErrorWrappers(NSArray<ErrorWrapper> * _Nonnull array) {
  return [[array valueForKey:@"code"] isEqual:@[@11, @22]] &&
         NSNotFound == [array indexOfObjectPassingTest:^BOOL(ErrorWrapper error,
                                                             NSUInteger idx,
                                                             BOOL *stop) {
    return ![error isKindOfClass:[NSError class]];
  }];
}
GET_UNTYPED_AND_OPAQUE(Error)
CHECK_UNTYPED_AND_OPAQUE(Error)

#pragma clang assume_nonnull begin

@interface Base
#if NEW
- (void)testBaseNameChanges __attribute__((swift_name("testNewBaseName()")));
#else
- (void)testBaseNameChanges __attribute__((swift_name("testBaseNameChanges()")));
#endif

#if NEW
- (void)testArgumentNameChanges:(id)first next:(id)second  __attribute__((swift_name("testArgumentNameChanges(first:second:)")));
#else
- (void)testArgumentNameChanges:(id)first next:(id)second;
#endif

#if NEW
- (void)testNullabilityArgChanges:(nullable id)arg;
#else
- (void)testNullabilityArgChanges:(id)arg;
#endif

#if NEW
- (nullable id)testNullabilityReturnChanges;
#else
- (id)testNullabilityReturnChanges;
#endif
@end

#pragma clang assume_nonnull end

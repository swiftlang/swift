#ifndef SYSTEM_MODULE_WITH_WARNINGS_H
#define SYSTEM_MODULE_WITH_WARNINGS_H

// Test NS_SWIFT_NAME warning suppression (unresolvable_clang_decl)
typedef enum {
  TestValue1,
  TestValue2
} TestEnum __attribute__((swift_name("NonExistentType.TestEnum")));

#endif // SYSTEM_MODULE_WITH_WARNINGS_H

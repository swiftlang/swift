// RUN: %target-typecheck-verify-swift -I %S/Inputs/

typedef struct __attribute__((swift_attr("~Copyable"))) NonCopyable {
  float x, y;
} NonCopyable;

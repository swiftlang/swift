// REQUIRES: autolink-extract
// RUN: %clang -flto=thin -c -o - %s | %target-swift-autolink-extract -o - - 2>&1 | %FileCheck --allow-empty %s
// CHECK-NOT: The file was not recognized as a valid object file

int test() {
  return 1;
}

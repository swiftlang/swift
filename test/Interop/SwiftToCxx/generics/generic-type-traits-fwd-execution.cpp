// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/generic-type-traits-fwd.swift -module-name Generics -cxx-interoperability-mode=default -clang-header-expose-decls=has-expose-attr -typecheck -verify -emit-clang-header-path %t/generics.h
// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/generics.o
// RUN: %target-interop-build-swift %S/generic-type-traits-fwd.swift -o %t/generics -Xlinker %t/generics.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/generics
// RUN: %target-run %t/generics
// REQUIRES: executable_test

#include "generics.h"
#include <cassert>
#include <type_traits>
#include <utility>

template <class T>
void checkExistingMembers() {
  static_assert(
      std::is_same<decltype(std::declval<T &>().getItem()), swift::Int>::value,
      "preserve explicit getter");
  static_assert(
      std::is_same<decltype(std::declval<T &>().value()), swift::Int>::value,
      "preserve return-only overload");
  static_assert(
      std::is_same<decltype(std::declval<T &>().shared()), swift::Int>::value,
      "preserve instance method");
  auto value = T::init();
  assert(value.getItem() == 2);
  assert(value.value() == 4);
  assert(value.shared() == 6);
}

int main() {
  checkExistingMembers<Generics::GenericMemberCollision>();
  checkExistingMembers<Generics::GenericMemberCollisionReversed>();

  auto value = Generics::InheritedUser::init();
  static_assert(std::is_same<decltype(value.getItem()), swift::Int>::value,
                "preserve inherited getter");
  assert(value.getItem() == 8);
  assert(value.consume(12) == 22);
  assert(value[12] == 24);
  assert(value.read(value.novel()) == 11);
}

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/nested-classes-in-cxx.swift -enable-library-evolution -typecheck -module-name Classes -clang-header-expose-decls=all-public -emit-clang-header-path %t/classes.h

// RUN: %target-interop-build-clangxx -std=c++17 -c %s -I %t -o %t/swift-classes-execution.o

// RUN: %target-interop-build-swift %S/nested-classes-in-cxx.swift -enable-library-evolution -o %t/swift-classes-execution -Xlinker %t/swift-classes-execution.o -module-name Classes -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-classes-execution
// RUN: %target-run %t/swift-classes-execution

// REQUIRES: executable_test

#include "classes.h"
#include <cassert>

int main() {
  using namespace Classes;
  auto x = makeRecordConfig();
  RecordConfig::File::Gate y = x.getGate();
  assert(y.getProp() == 80);
  assert(y.computeValue() == 160);
  RecordConfig::AudioFormat z = x.getFile().getFormat();
  assert(z == RecordConfig::AudioFormat::ALAC);
  RecordConfig::File::Gate g = RecordConfig::File::Gate::init();
}

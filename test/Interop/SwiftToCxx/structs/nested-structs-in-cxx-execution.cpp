// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/nested-structs-in-cxx.swift -enable-library-evolution -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h

// RUN: %target-interop-build-clangxx -std=c++17 -c %s -I %t -o %t/swift-structs-execution.o

// RUN: %target-interop-build-swift %S/nested-structs-in-cxx.swift -enable-library-evolution -o %t/swift-structs-execution -Xlinker %t/swift-structs-execution.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-structs-execution
// RUN: %target-run %t/swift-structs-execution

// REQUIRES: executable_test

#include "structs.h"

int main() {
  using namespace Structs;
  auto x = makeRecordConfig();
  RecordConfig::File::Gate y = x.getGate();
  RecordConfig::AudioFormat z = x.getFile().getFormat();

  auto xx = makeAudioFileType();
  AudioFileType::SubType yy = xx.getCAF();
}

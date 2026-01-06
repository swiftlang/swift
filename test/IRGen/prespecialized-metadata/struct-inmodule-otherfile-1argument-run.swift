// RUN: %target-run-simple-swift(%S/Inputs/main.swift %S/Inputs/consume-logging-metadata-value.swift %S/Inputs/struct-nonfrozen-1argument.swift -Xfrontend -prespecialize-generic-metadata -target %module-target-future) | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios
// Executing on the simulator within __abort_with_payload with "No ABI plugin located for triple x86_64h-apple-ios -- shared libraries will not be registered!"
// UNSUPPORTED: CPU=x86_64 && OS=ios
// UNSUPPORTED: CPU=x86_64 && OS=tvos
// UNSUPPORTED: CPU=x86_64 && OS=watchos
// UNSUPPORTED: CPU=i386 && OS=watchos
// UNSUPPORTED: use_os_stdlib

func consume<First>(oneArgument: OneArgument<First>) {
  consume(oneArgument)
}

func doit() {
  // CHECK: [[METADATA_ADDRESS:[0-9a-f]+]] 3 @ [[@LINE+1]]
  consume(OneArgument(3))
  // CHECK: [[METADATA_ADDRESS]] 3 @ [[@LINE+1]]
  consume(oneArgument: OneArgument(3))
}

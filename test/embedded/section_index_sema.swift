// RUN: %target-typecheck-verify-swift -parse-as-library -enable-experimental-feature Embedded
// REQUIRES: swift_feature_Embedded
// REQUIRES: embedded_stdlib

// Embedded Swift emits no section index and has no runtime to read one, so a
// name the index could not describe is not worth a warning there. This name
// fails the ELF rule (not a C identifier) and the COFF rule (no "$" group).

@section(".init_array") @used var record: Int = 1

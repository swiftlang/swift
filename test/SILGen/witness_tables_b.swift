// RUN: %swift -parse -verify %s
// This module is used by the witness_tables.swift test, to test protocol
// conformance introduced by extensions across modules.

struct OtherModuleStruct {}
class OtherModuleClass {}


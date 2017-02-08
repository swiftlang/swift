// RUN: %target-typecheck-verify-swift

// This module is used by the sil_witness_tables.swift test to exercise
// referencing an external witness table for a conformance.

public protocol ExternalP {}

public struct ExternalConformer: ExternalP {}

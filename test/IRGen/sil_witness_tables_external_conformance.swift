// RUN: %swift -parse -verify %s

// This module is used by the sil_witness_tables.swift test to exercise
// referencing an external witness table for a conformance.

protocol ExternalP {}

struct ExternalConformer: ExternalP {}

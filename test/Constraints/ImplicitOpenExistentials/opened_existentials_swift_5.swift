// DEFINE: %{input}=%S/Inputs/opened_existentials.swift
//
// RUN: %target-swift-frontend -target %target-swift-5.7-abi-triple -dump-ast -verify \
// RUN:   %{input}                                                                    \
// RUN:   -language-mode 5                                                            \
// RUN: | %FileCheck                                                                  \
// RUN:   --check-prefixes=SWIFT-5-AND-6,SWIFT-5                                      \
// RUN:   %{input}

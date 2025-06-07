//===--- MockPlugin.h ---------------------------------------------*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_C_MOCK_PLUGIN_H
#define SWIFT_C_MOCK_PLUGIN_H

#ifdef __cplusplus
extern "C" {
#endif

int _mock_plugin_main(const char *);

#ifdef __cplusplus
}
#endif

/// Usage: MOCK_PLUGIN(JSON)
/// 'JSON' is a *bare* JSON value.
#define MOCK_PLUGIN(...)                                                       \
  int main() { return _mock_plugin_main(#__VA_ARGS__); }

#endif // SWIFT_C_MOCK_PLUGIN_H

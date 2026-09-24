// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/custom-modules %s -verify

// expected-warning@<unknown> * {{libc not found for }}

import TextualHeaders

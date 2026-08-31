// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules %s -typecheck -verify

// expected-warning@<unknown> * {{libc not found for }}

import ExternIntX

x += 1

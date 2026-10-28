// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules/ %s

// expected-warning@<unknown> * {{libc not found for }}

import PredefinedMacros

swift3ReadyToGo()

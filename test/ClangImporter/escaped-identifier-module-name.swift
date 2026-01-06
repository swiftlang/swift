// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify

import `Weird C Module`

_ = S().value
_ = `Weird C Module`.S().value

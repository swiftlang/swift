// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/custom-modules %s -verify
import TextualHeaders

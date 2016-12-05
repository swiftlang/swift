// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules/ %s

import PredefinedMacros

swift3ReadyToGo()

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules %s -parse -verify

import ExternIntX

x += 1

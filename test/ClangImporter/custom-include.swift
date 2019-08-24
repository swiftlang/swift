// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules %s -typecheck -verify

import ExternIntX

x += 1

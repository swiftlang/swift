// RUN: %target-swift-frontend %clang-importer-sdk -I %S/Inputs/custom-modules %s -parse -verify

import ExternIntX

x += 1

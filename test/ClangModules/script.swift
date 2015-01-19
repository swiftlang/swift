// RUN: %target-swift-frontend %clang-importer-sdk -parse -verify -I %S/Inputs/custom-modules %s

import script // Clang module

var _ : ScriptTy

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/frameworks %s -verify

import ImportAsMemberSubmodules

let _: IAMOuter.Inner?

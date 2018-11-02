// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -F %S/Inputs/frameworks -I %S/Inputs/custom-modules %s -verify

import ImportAsMember
import ImportAsMemberSubmodules

let _: IAMSOuter.Inner?
let _: IAMMultipleNested.Inner? // expected-error {{ambiguous type name 'Inner' in 'IAMMultipleNested'}}

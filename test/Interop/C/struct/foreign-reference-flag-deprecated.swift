// RUN: %target-build-swift %s -I %S/Inputs/ -Xfrontend -experimental-c-foreign-reference-types 2>&1 | %FileCheck %s

// CHECK: <unknown>:0: warning: flag '-experimental-c-foreign-reference-types' is deprecated

import ForeignReference

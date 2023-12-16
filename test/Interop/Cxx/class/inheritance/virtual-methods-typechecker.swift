// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import VirtualMethods

VirtualNonAbstractBase().nonAbstractMethod()

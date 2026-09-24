// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=swift-5.9 %s

import MockPOSIX

let _ = in_addr(s_addr: INADDR_ANY)

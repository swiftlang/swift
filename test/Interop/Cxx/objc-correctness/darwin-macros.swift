// RUN: %target-swift-frontend -cxx-interoperability-mode=default -typecheck -verify -I %S/Inputs %s

// REQUIRES: OS=macosx

import Darwin

let _ = COPYFILE_ALL

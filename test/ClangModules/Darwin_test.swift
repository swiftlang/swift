// RUN: %target-swift-frontend %s -verify

import Darwin

errno = 0
assert(errno == 0)

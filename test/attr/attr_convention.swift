// RUN: %target-parse-verify-swift

let f1: Int -> Int = { $0 }
let f2: @convention(swift) Int -> Int = { $0 }
let f3: @convention(block) Int -> Int = { $0 }
let f4: @convention(c) Int -> Int = { $0 }

let f5: @convention(INTERCAL) Int -> Int = { $0 } // expected-error{{convention 'INTERCAL' not supported}}

let f6: @convention(block) @objc_block Int -> Int = { $0 } // expected-error{{@convention attribute cannot be used with deprecated @objc_block attribute}}

let f7: @objc_block Int -> Int = { $0 } // expected-warning{{'@objc_block' attribute is deprecated; '@convention(block)' should be used instead}}{{10-20=convention(block)}}

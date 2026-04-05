// RUN: %target-typecheck-verify-swift \
// RUN:     -cxx-interoperability-mode=default -disable-availability-checking \
// RUN:     -I %S%{fs-sep}Inputs -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}refkit.hpp

import RefKit

let o = RefObject.createNull()
let _: Object? = o.getPtrUnretained()
let _: Object = o.getRefUnretained()

let n = RefNumberObj.create(42.0)
let _: NumberObj? = n.getPtrUnretained()
let _: NumberObj = n.getRefUnretained()

let f = RefBoxedFloat.create(42.0)
let _: BoxedFloat? = f.getPtrUnretained()
let _: BoxedFloat = f.getRefUnretained()

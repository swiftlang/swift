// RUN: %target-typecheck-verify-swift \
// RUN:   -I %S/Inputs -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking -disable-typo-correction -suppress-notes

import FrtSequence
import CxxStdlib

// --- std::vector<ImmortalNode*> iteration yields ImmortalNode? ---

let ipv = makeImmortalPtrVector()
let _ = ipv.begin() // expected-error {{has no member 'begin'}}
let _ = ipv.end()   // expected-error {{has no member 'end'}}
let _ = ipv.__beginUnsafe()
let _ = ipv.__endUnsafe()
for i in ipv {
  let _: ImmortalNode? = i
}

// Subscript on vector<FRT*> is read-write: operator[] returns FRT*& which
// is not FRT-stripped at the reference level, so the setter is preserved.
var mutableIpv = makeImmortalPtrVector()
let elem: ImmortalNode? = mutableIpv[0]
mutableIpv[0] = elem

// --- std::vector<SharedNode*> iteration yields SharedNode? ---

let spv = makeSharedPtrVector()
let _ = spv.begin() // expected-error {{has no member 'begin'}}
let _ = spv.end()   // expected-error {{has no member 'end'}}
let _ = spv.__beginUnsafe()
let _ = spv.__endUnsafe()
for i in spv {
  let _: SharedNode? = i
}

// --- std::vector<ImmortalNode2> iteration yields ImmortalNode2 ---

let ivv = makeImmortalValVector()
let _ = ivv.begin() // expected-error {{has no member 'begin'}}
let _ = ivv.end()   // expected-error {{has no member 'end'}}
let _ = ivv.__beginUnsafe()
let _ = ivv.__endUnsafe()
for i in ivv {
  let _: ImmortalNode2 = i
}

// --- RawPtrIterContainer: raw pointer begin()/end() over FRT ---

let rpc = RawPtrIterContainer()

// begin()/end() return const ImmortalNode*, which gets imported without the
// pointer as ImmortalNode? and cannot be iterated over.
for _ in rpc {} // expected-error {{for-in loop requires 'RawPtrIterContainer' to conform to 'Sequence'}}

// begin()/end() are still renamed to __beginUnsafe/__endUnsafe

let _ = rpc.begin() // expected-error {{has no member 'begin'}}
let _ = rpc.end()   // expected-error {{has no member 'end'}}
let _: ImmortalNode? = rpc.__beginUnsafe()
let _: ImmortalNode? = rpc.__endUnsafe()

// operator[] returns const ImmortalNode* which is a get-only subscript that
// returns ImmortalNode? (FRT pointer stripping + nullable pointer).
let node: ImmortalNode? = rpc[0]
rpc[1] = node // expected-error {{subscript is get-only}}

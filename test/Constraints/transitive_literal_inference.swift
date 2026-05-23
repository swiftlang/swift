// RUN: %target-typecheck-verify-swift -DSALVAGE -solver-disable-crash-on-valid-salvage
// RUN: not --crash %target-typecheck-verify-swift -DSALVAGE -solver-enable-crash-on-valid-salvage
// RUN: %target-typecheck-verify-swift -solver-enable-crash-on-valid-salvage

// All of the below should of course type check successfully.
// FIXME: Once everything below is passing, we can gyb it.

func f<T>(_: T, _: T) -> T {}

let b = false

// Not specifying a contextual type is broken sometimes
_ = f(nil, f(3, 3.0))
_ = f(nil, f(3.0, 3))
_ = f(3, f(3.0, nil))
_ = f(3, f(nil, 3.0))
_ = f(3.0, f(nil, 3))  // expected-error {{'nil' is not compatible with expected argument type 'Double'}}
_ = f(3.0, f(3, nil))  // expected-error {{'nil' is not compatible with expected argument type 'Double'}}

_ = f(f(nil, 3), 3.0)  // expected-error {{'nil' is not compatible with expected argument type 'Double'}}
_ = f(f(nil, 3.0), 3)
_ = f(f(3, 3.0), nil)
_ = f(f(3, nil), 3.0)  // expected-error {{'nil' is not compatible with expected argument type 'Double'}}
_ = f(f(3.0, nil), 3)
_ = f(f(3.0, 3), nil)

_ = (b ? nil : (b ? 3 : 3.0))
_ = (b ? nil : (b ? 3.0 : 3))
_ = (b ? 3 : (b ? 3.0 : nil))
_ = (b ? 3 : (b ? nil : 3.0))
_ = (b ? 3.0 : (b ? nil : 3))
_ = (b ? 3.0 : (b ? 3 : nil))

_ = (b ? (b ? nil : 3) : 3.0)
_ = (b ? (b ? nil : 3.0) : 3)
_ = (b ? (b ? 3 : 3.0) : nil)
_ = (b ? (b ? 3 : nil) : 3.0)
_ = (b ? (b ? 3.0 : nil) : 3)
_ = (b ? (b ? 3.0 : 3) : nil)

// Optional<_> contextual type works
let _: Optional = f(nil, f(3, 3.0))
let _: Optional = f(nil, f(3.0, 3))
let _: Optional = f(3, f(3.0, nil))
let _: Optional = f(3, f(nil, 3.0))
let _: Optional = f(3.0, f(nil, 3))
let _: Optional = f(3.0, f(3, nil))

let _: Optional = f(f(nil, 3), 3.0)
let _: Optional = f(f(nil, 3.0), 3)
let _: Optional = f(f(3, 3.0), nil)
let _: Optional = f(f(3, nil), 3.0)
let _: Optional = f(f(3.0, nil), 3)
let _: Optional = f(f(3.0, 3), nil)

let _: Optional = (b ? nil : (b ? 3 : 3.0))
let _: Optional = (b ? nil : (b ? 3.0 : 3))
let _: Optional = (b ? 3 : (b ? 3.0 : nil))
let _: Optional = (b ? 3 : (b ? nil : 3.0))
let _: Optional = (b ? 3.0 : (b ? nil : 3))
let _: Optional = (b ? 3.0 : (b ? 3 : nil))

let _: Optional = (b ? (b ? nil : 3) : 3.0)
let _: Optional = (b ? (b ? nil : 3.0) : 3)
let _: Optional = (b ? (b ? 3 : 3.0) : nil)
let _: Optional = (b ? (b ? 3 : nil) : 3.0)
let _: Optional = (b ? (b ? 3.0 : nil) : 3)
let _: Optional = (b ? (b ? 3.0 : 3) : nil)

// Optional<Double> contextual type works
let _: Double? = f(nil, f(3, 3.0))
let _: Double? = f(nil, f(3.0, 3))
let _: Double? = f(3, f(3.0, nil))
let _: Double? = f(3, f(nil, 3.0))
let _: Double? = f(3.0, f(nil, 3))
let _: Double? = f(3.0, f(3, nil))

let _: Double? = f(f(nil, 3), 3.0)
let _: Double? = f(f(nil, 3.0), 3)
let _: Double? = f(f(3, 3.0), nil)
let _: Double? = f(f(3, nil), 3.0)
let _: Double? = f(f(3.0, nil), 3)
let _: Double? = f(f(3.0, 3), nil)

let _: Double? = (b ? nil : (b ? 3 : 3.0))
let _: Double? = (b ? nil : (b ? 3.0 : 3))
let _: Double? = (b ? 3 : (b ? 3.0 : nil))
let _: Double? = (b ? 3 : (b ? nil : 3.0))
let _: Double? = (b ? 3.0 : (b ? nil : 3))
let _: Double? = (b ? 3.0 : (b ? 3 : nil))

let _: Double? = (b ? (b ? nil : 3) : 3.0)
let _: Double? = (b ? (b ? nil : 3.0) : 3)
let _: Double? = (b ? (b ? 3 : 3.0) : nil)
let _: Double? = (b ? (b ? 3 : nil) : 3.0)
let _: Double? = (b ? (b ? 3.0 : nil) : 3)
let _: Double? = (b ? (b ? 3.0 : 3) : nil)

// Optional<Float> contextual type is completely broken
let _: Float? = f(nil, f(3, 3.0))
let _: Float? = f(nil, f(3.0, 3))  // expected-error {{conflicting arguments to generic parameter 'T' ('Float?' vs. 'Float')}}
let _: Float? = f(3, f(3.0, nil))  // expected-error {{conflicting arguments to generic parameter 'T' ('Float?' vs. 'Double?')}}
let _: Float? = f(3, f(nil, 3.0))  // expected-error {{conflicting arguments to generic parameter 'T' ('Float?' vs. 'Double?')}}
let _: Float? = f(3.0, f(nil, 3))
let _: Float? = f(3.0, f(3, nil))

let _: Float? = f(f(nil, 3), 3.0)
let _: Float? = f(f(nil, 3.0), 3)  // expected-error {{conflicting arguments to generic parameter 'T' ('Float?' vs. 'Double?')}}
let _: Float? = f(f(3, 3.0), nil)
let _: Float? = f(f(3, nil), 3.0)
let _: Float? = f(f(3.0, nil), 3)  // expected-error {{conflicting arguments to generic parameter 'T' ('Float?' vs. 'Double?')}}
let _: Float? = f(f(3.0, 3), nil)  // expected-error {{conflicting arguments to generic parameter 'T' ('Float?' vs. 'Float')}}

let _: Float? = (b ? nil : (b ? 3 : 3.0))

// Everything in an #if SALVAGE block is solved in salvage
#if SALVAGE
let _: Float? = (b ? nil : (b ? 3.0 : 3))
#endif

let _: Float? = (b ? 3 : (b ? 3.0 : nil))
let _: Float? = (b ? 3 : (b ? nil : 3.0))

#if SALVAGE
let _: Float? = (b ? 3.0 : (b ? nil : 3))
let _: Float? = (b ? 3.0 : (b ? 3 : nil))
#endif

let _: Float? = (b ? (b ? nil : 3) : 3.0)

#if SALVAGE
let _: Float? = (b ? (b ? nil : 3.0) : 3)
#endif

let _: Float? = (b ? (b ? 3 : 3.0) : nil)
let _: Float? = (b ? (b ? 3 : nil) : 3.0)

#if SALVAGE
let _: Float? = (b ? (b ? 3.0 : nil) : 3)
let _: Float? = (b ? (b ? 3.0 : 3) : nil)
#endif

// TODO: Add more tests.

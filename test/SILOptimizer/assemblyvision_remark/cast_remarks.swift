// RUN: %target-swiftc_driver -O -Rpass-missed=sil-assembly-vision-remark-gen -Xfrontend -enable-copy-propagation=requested-passes-only -Xfrontend -enable-lexical-lifetimes=false -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil %s -o /dev/null -Xfrontend -verify

// REQUIRES: optimized_stdlib
// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: OS=macosx

///////////////////
// Generic Casts //
///////////////////

public func forcedCast<NS, T>(_ ns: NS) -> T {
  // Make sure the colon info is right so that the arrow is under the a.
  return ns as! T // expected-remark @:13 {{unconditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-3:33 {{of 'ns'}}
}

public func forcedCast2<NS, T>(_ ns: NS) -> T {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  let x = ns
  return x as! T  // expected-remark @:12 {{unconditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-7:34 {{of 'ns'}}
}

public func forcedCast3<NS, T>(_ ns: NS) -> T {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  var x = ns // expected-warning {{variable 'x' was never mutated}}
  return x as! T  // expected-remark @:12 {{unconditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-7:34 {{of 'ns'}}
}

public func forcedCast4<NS, T>(_ ns: NS, _ ns2: NS) -> T {
  // Make sure the colon info is right so that the arrow is under the a.
  var x = ns
  x = ns2
  return x as! T  // expected-remark @:12 {{unconditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-5:44 {{of 'ns2'}}
                  // expected-note @-4:7 {{of 'x'}}
}

public func condCast<NS, T>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  return ns as? T // expected-remark @:13 {{conditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-3:31 {{of 'ns'}}
}

public func condCast2<NS, T>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  let x = ns
  return x as? T  // expected-remark @:12 {{conditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-7:32 {{of 'ns'}}
}

public func condCast3<NS, T>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  var x = ns // expected-warning {{variable 'x' was never mutated}}
  return x as? T  // expected-remark @:12 {{conditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-7:32 {{of 'ns'}}
}

public func condCast4<NS, T>(_ ns: NS, _ ns2: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  var x = ns
  x = ns2
  return x as? T  // expected-remark @:12 {{conditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-5:42 {{of 'ns2'}}
                  // expected-note @-4:7 {{of 'x'}}
}

public func condCast5<NS, T>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we lose that x was assigned.
  if let x = ns as? T {  // expected-remark @:17 {{conditional runtime cast of value with type 'NS' to 'T'}}
    return x             // expected-note @-5:32 {{of 'ns'}}
  }
  return nil
}

public func condCast6<NS, T>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we lose that x was assigned.
  guard let x = ns as? T else {  // expected-remark @:20 {{conditional runtime cast of value with type 'NS' to 'T'}}
    return nil                   // expected-note @-5:32 {{of 'ns'}}
  }
  return x
}

//////////////////////////////////
// Any Object Constrained Casts //
//////////////////////////////////

public func forcedCast<NS: AnyObject, T: AnyObject>(_ ns: NS) -> T {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // TODO: We should also note the retain as being on 'ns'.
  return ns as! T // expected-remark @:13 {{unconditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-5:55 {{of 'ns'}}
                  // expected-remark @-2 {{retain of type 'NS'}}
}

public func forcedCast2<NS: AnyObject, T: AnyObject>(_ ns: NS) -> T {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow. We should also note the retain as being on 'ns'
  let x = ns
  return x as! T  // expected-remark @:12 {{unconditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-7:56 {{of 'ns'}}
                  // expected-remark @-2 {{retain of type 'NS'}}
}

public func forcedCast3<NS: AnyObject, T: AnyObject>(_ ns: NS) -> T {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  var x = ns // expected-warning {{variable 'x' was never mutated}}
  return x as! T  // expected-remark @:12 {{unconditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-7:56 {{of 'ns'}}
                  // expected-remark @-2 {{retain of type 'NS'}}
}

// Interestingly today, with AnyObject codegen, we do not lose the assignment to
// x and say the cast is on ns2!
public func forcedCast4<NS: AnyObject, T: AnyObject>(_ ns: NS, _ ns2: NS) -> T {
  // Make sure the colon info is right so that the arrow is under the a.
  var x = ns
  x = ns2
  return x as! T  // expected-remark @:12 {{unconditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-5:66 {{of 'ns2'}}
                  // expected-remark @-2 {{retain of type 'NS'}}
}

public func condCast<NS: AnyObject, T: AnyObject>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  return ns as? T // expected-remark @:13 {{conditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-3:53 {{of 'ns'}}
                  // expected-remark @-2 {{retain of type 'NS'}}
}

public func condCast2<NS: AnyObject, T: AnyObject>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  let x = ns
  return x as? T  // expected-remark @:12 {{conditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-7:54 {{of 'ns'}}
                  // expected-remark @-2 {{retain of type 'NS'}}
}

public func condCast3<NS: AnyObject, T: AnyObject>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  var x = ns // expected-warning {{variable 'x' was never mutated}}
  return x as? T  // expected-remark @:12 {{conditional runtime cast of value with type 'NS' to 'T'}}
                  // expected-note @-7:54 {{of 'ns'}}
                  // expected-remark @-2 {{retain of type 'NS'}}
}

public func condCast4<NS: AnyObject, T: AnyObject>(_ ns: NS, _ ns2: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  var x = ns
  x = ns2
  return x as? T // expected-remark @:12 {{conditional runtime cast of value with type 'NS' to 'T'}}
                 // expected-note @-5:64 {{of 'ns2'}}
                 // expected-remark @-2 {{retain of type 'NS'}}
}

public func condCast5<NS: AnyObject, T: AnyObject>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we lose that x was assigned.
  if let x = ns as? T {  // expected-remark @:17 {{conditional runtime cast of value with type 'NS' to 'T'}}
    return x             // expected-note @-5:54 {{of 'ns'}}
  }                      // expected-remark @-2 {{retain of type 'NS'}}
  return nil
}

public func condCast6<NS: AnyObject, T: AnyObject>(_ ns: NS) -> T? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we lose that x was assigned.
  guard let x = ns as? T else {  // expected-remark @:20 {{conditional runtime cast of value with type 'NS' to 'T'}}
    return nil                   // expected-note @-5:54 {{of 'ns'}}
  }                              // expected-remark @-2 {{retain of type 'NS'}}
  return x
}

/////////////////////////
// Existential Casting //
/////////////////////////

public protocol Existential1 {
}

public protocol Existential2 {
}

public func forcedCast(_ ns: Existential1) -> Existential2 {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // TODO: We should also note the retain as being on 'ns'.
  return ns as! Existential2 // expected-remark @:13 {{unconditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
                  // expected-note @-5:26 {{of 'ns'}}
}

public func forcedCast2(_ ns: Existential1) -> Existential2 {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow. We should also note the retain as being on 'ns'
  let x = ns
  return x as! Existential2  // expected-remark @:12 {{unconditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
                  // expected-note @-7:27 {{of 'ns'}}
}

public func forcedCast3(_ ns: Existential1) -> Existential2 {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  var x = ns // expected-warning {{variable 'x' was never mutated}}
  return x as! Existential2  // expected-remark @:12 {{unconditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
                  // expected-note @-7:27 {{of 'ns'}}
}

// TODO: We should be able to identify NS2 in this case with opaque values.
public func forcedCast4(_ ns: Existential1, _ ns2: Existential1) -> Existential2 {
  // Make sure the colon info is right so that the arrow is under the a.
  var x = ns
  x = ns2
  return x as! Existential2  // expected-remark @:12 {{unconditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
                             // expected-note @-5:47 {{of 'ns2'}}
                             // expected-note @-4:7 {{of 'x'}}
}

public func condCast(_ ns: Existential1) -> Existential2? {
  // Make sure the colon info is right so that the arrow is under the a.
  return ns as? Existential2 // expected-remark @:13 {{conditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
                  // expected-note @-3:24 {{of 'ns'}}
}

public func condCast2(_ ns: Existential1) -> Existential2? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  let x = ns
  return x as? Existential2  // expected-remark @:12 {{conditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
                  // expected-note @-7:25 {{of 'ns'}}
}

public func condCast3(_ ns: Existential1) -> Existential2? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we seem to completely eliminate 'x' here in the debug info. TODO:
  // Maybe we can recover this info somehow.
  var x = ns // expected-warning {{variable 'x' was never mutated}}
  return x as? Existential2  // expected-remark @:12 {{conditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
                  // expected-note @-7:25 {{of 'ns'}}
}

// TODO: We should be able to identify NS2 here!
public func condCast4(_ ns: Existential1, _ ns2: Existential1) -> Existential2? {
  // Make sure the colon info is right so that the arrow is under the a.
  var x = ns
  x = ns2
  return x as? Existential2 // expected-remark @:12 {{conditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
                            // expected-note @-5:45 {{of 'ns2'}}
                            // expected-note @-4:7 {{of 'x'}}
}

public func condCast5(_ ns: Existential1) -> Existential2? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we lose that x was assigned.
  if let x = ns as? Existential2 {  // expected-remark @:17 {{conditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
    return x                        // expected-note @-5:25 {{of 'ns'}}
  }
  return nil
}

public func condCast6(_ ns: Existential1) -> Existential2? {
  // Make sure the colon info is right so that the arrow is under the a.
  //
  // Today, we lose that x was assigned.
  guard let x = ns as? Existential2 else {  // expected-remark @:20 {{conditional runtime cast of value with type 'any Existential1' to 'any Existential2'}}
    return nil                   // expected-note @-5:25 {{of 'ns'}}
  }
  return x
}

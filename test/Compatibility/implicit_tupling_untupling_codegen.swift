// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test

// Even though we test that type-checking and exhaustiveness checking work fine
// in the presence of implicit tupling/untupling in exhaustive_switch.swift,
// make sure that the "patched" patterns do not lead to incorrect codegen.

enum Untupled {
  case upair(Int, Int)
}

let u_ex = Untupled.upair(1, 2)

func sr11212_content_untupled_pattern_tupled1(u: Untupled) -> (Int, Int) {
  switch u { case .upair((let x, let y)): return (x, y) }
}
print(sr11212_content_untupled_pattern_tupled1(u: u_ex))
// CHECK: (1, 2)

func sr11212_content_untupled_pattern_tupled2(u: Untupled) -> (Int, Int) {
  switch u { case .upair(let (x, y)): return (x, y) }
}
print(sr11212_content_untupled_pattern_tupled2(u: u_ex))
// CHECK: (1, 2)

func sr11212_content_untupled_pattern_tupled3(u: Untupled) -> (Int, Int) {
  switch u { case let .upair((x, y)): return (x, y) }
}
print(sr11212_content_untupled_pattern_tupled3(u: u_ex))
// CHECK: (1, 2)

func sr11212_content_untupled_pattern_untupled1(u: Untupled) -> (Int, Int) {
  switch u { case .upair(let x, let y): return (x, y) }
}
print(sr11212_content_untupled_pattern_untupled1(u: u_ex))
// CHECK: (1, 2)

func sr11212_content_untupled_pattern_untupled2(u: Untupled) -> (Int, Int) {
    switch u { case let .upair(x, y): return (x, y) }
}
print(sr11212_content_untupled_pattern_untupled2(u: u_ex))
// CHECK: (1, 2)

func sr11212_content_untupled_pattern_ambiguous1(u: Untupled) -> (Int, Int) {
  switch u { case .upair(let u_): return u_ }
}
print(sr11212_content_untupled_pattern_ambiguous1(u: u_ex))
// CHECK: (1, 2)

func sr11212_content_untupled_pattern_ambiguous2(u: Untupled) -> (Int, Int) {
  switch u { case let .upair(u_): return u_ }
}
print(sr11212_content_untupled_pattern_ambiguous2(u: u_ex))
// CHECK: (1, 2)

enum Tupled {
  case tpair((Int, Int))
}

let t_ex = Tupled.tpair((1, 2))

func sr11212_content_tupled_pattern_tupled1(t: Tupled) -> (Int, Int) {
  switch t { case .tpair((let x, let y)): return (x, y) }
}
print(sr11212_content_tupled_pattern_tupled1(t: t_ex))
// CHECK: (1, 2)

func sr11212_content_tupled_pattern_tupled2(t: Tupled) -> (Int, Int) {
  switch t { case .tpair(let (x, y)): return (x, y) }
}
print(sr11212_content_tupled_pattern_tupled2(t: t_ex))
// CHECK: (1, 2)

func sr11212_content_tupled_pattern_tupled3(t: Tupled) -> (Int, Int) {
  switch t { case let .tpair((x, y)): return (x, y) }
}
print(sr11212_content_tupled_pattern_tupled3(t: t_ex))
// CHECK: (1, 2)

func sr11212_content_tupled_pattern_untupled1(t: Tupled) -> (Int, Int) {
  switch t { case .tpair(let x, let y): return (x, y) }
}
print(sr11212_content_tupled_pattern_untupled1(t: t_ex))
// CHECK: (1, 2)

func sr11212_content_tupled_pattern_untupled2(t: Tupled) -> (Int, Int) {
  switch t { case let .tpair(x, y): return (x, y) }
}
print(sr11212_content_tupled_pattern_untupled2(t: t_ex))
// CHECK: (1, 2)

func sr11212_content_tupled_pattern_ambiguous1(t: Tupled) -> (Int, Int) {
  switch t { case .tpair(let t_): return t_ }
}
print(sr11212_content_tupled_pattern_ambiguous1(t: t_ex))
// CHECK: (1, 2)

func sr11212_content_tupled_pattern_ambiguous2(t: Tupled) -> (Int, Int) {
  switch t { case let .tpair(t_): return t_ }
}
print(sr11212_content_tupled_pattern_ambiguous2(t: t_ex))
// CHECK: (1, 2)

enum Box<T> {
  case box(T)
}

let b_ex : Box<(Int, Int)> = Box.box((1, 2))

func sr11212_content_generic_pattern_tupled1(b: Box<(Int, Int)>) -> (Int, Int) {
  switch b { case .box((let x, let y)): return (x, y) }
}
print(sr11212_content_generic_pattern_tupled1(b: b_ex))
// CHECK: (1, 2)

func sr11212_content_generic_pattern_tupled2(b: Box<(Int, Int)>) -> (Int, Int) {
  switch b { case .box(let (x, y)): return (x, y) }
}
print(sr11212_content_generic_pattern_tupled2(b: b_ex))
// CHECK: (1, 2)

func sr11212_content_generic_pattern_tupled3(b: Box<(Int, Int)>) -> (Int, Int) {
 switch b { case let .box((x, y)): return (x, y) }
}
print(sr11212_content_generic_pattern_tupled3(b: b_ex))
// CHECK: (1, 2)

func sr11212_content_generic_pattern_untupled1(b: Box<(Int, Int)>) -> (Int, Int) {
  switch b { case .box(let x, let y): return (x, y) }
}
print(sr11212_content_generic_pattern_untupled1(b: b_ex))
// CHECK: (1, 2)

func sr11212_content_generic_pattern_untupled2(b: Box<(Int, Int)>) -> (Int, Int) {
  switch b { case let .box(x, y): return (x, y) }
}
print(sr11212_content_generic_pattern_untupled2(b: b_ex))
// CHECK: (1, 2)

func sr11212_content_generic_pattern_ambiguous1(b: Box<(Int, Int)>) -> (Int, Int) {
  switch b { case .box(let b_): return b_ }
}
print(sr11212_content_generic_pattern_ambiguous1(b: b_ex))
// CHECK: (1, 2)

func sr11212_content_generic_pattern_ambiguous2(b: Box<(Int, Int)>) -> (Int, Int) {
  switch b { case let .box(b_): return b_ }
}
print(sr11212_content_generic_pattern_ambiguous2(b: b_ex))
// CHECK: (1, 2)

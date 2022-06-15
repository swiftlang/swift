// RUN: %target-swift-emit-silgen -disable-availability-checking -verify -wmo %s %S/Inputs/opaque_result_type_captured_wmo_2.swift
func foo(s: String?) {
  let x = PImpl()
    .burritoed()
    .wrapped(extra: 1)

  let butz = Butz(x)

  s.map { print("\($0) \(butz)") }
}

//===-- ADRuntime.swift ---------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Swift runtime support for automatic differentiation.
//
//===----------------------------------------------------------------------===//

@_versioned
class _ADTape<Element> {
  private var elements: [Element] = []

  @inline(never)
  @_semantics("autodiff.create_tape")
  @_silgen_name("_swift_autodiff_CreateTape")
  init() {}
}

extension _ADTape {
  var count: Int {
    @inline(never)
    @_semantics("autodiff.tape_element_count")
    @_silgen_name("_swift_autodiff_TapeElementCount")
    get {
      return elements.count
    }
  }

  @inline(never)
  @_semantics("autodiff.push_to_tape")
  @_silgen_name("_swift_autodiff_PushToTape")
  func push(_ value: Element) {
    elements.append(value)
  }

  @inline(never)
  @_semantics("autodiff.pop_from_tape")
  @_silgen_name("_swift_autodiff_PopFromTape")
  func pop() -> Element {
    guard let popped = elements.popLast() else {
      preconditionFailure("Cannot pop from an empty AD tape.")
    }
    return popped
  }
}

// This file should not have any syntax or type checker errors.
// RUN: %target-typecheck-verify-swift

// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=false > %t.printed.txt
// RUN: %FileCheck %s -strict-whitespace < %t.printed.txt

// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=true > %t.printed.txt
// RUN: %FileCheck %s -strict-whitespace < %t.printed.txt

struct FooStruct {
// CHECK-LABEL: {{^}}struct FooStruct {{{$}}

  var instanceVar: Int
// CHECK-NEXT: {{^}}  var instanceVar: Int{{$}}

  subscript(i: Int) -> Double {
    get {
      return Double(i)
    }
    set(v) {
      instanceVar = i
    }
  }
// CHECK-NEXT: {{^}}  subscript(i: Int) -> Double {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK:      {{^}}    set(v) {{{$}}
// CHECK:      {{^}}    }{{$}}
// CHECK:      {{^}}  }{{$}}

  subscript(i: Int, j: Int) -> Double {
    get {
      return Double(i + j)
    }
    set(v) {
      instanceVar = i + j
    }
  }
// CHECK: {{^}}  subscript(i: Int, j: Int) -> Double {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK:      {{^}}    set(v) {{{$}}
// CHECK:      {{^}}    }{{$}}
// CHECK:      {{^}}  }{{$}}
}

extension FooStruct {
// CHECK-LABEL: {{^}}extension FooStruct {{{$}}
  var extProp: Int {
    get {
      return 42
    }
    set(v) {}
  }
// CHECK-NEXT: {{^}}  var extProp: Int {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK:      {{^}}    set(v) {}{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
}

//===---
//===--- Variable declaration printing.
//===---

var topLevelVar1: Int {
  get {
    return 42
  }
}
// CHECK: {{^}}var topLevelVar1: Int {{{$}}
// CHECK-NEXT: {{^}}  get {{{$}}
// CHECK-NEXT: {{^}}    return {{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NEXT: {{^}}}{{$}}
// CHECK-NOT: topLevelVar1

var topLevelVar2: Int {
  get {
    return 22
  }
  set {
    if true {}
  }
}
// CHECK: {{^}}var topLevelVar2: Int {{{$}}
// CHECK-NEXT: {{^}}  get {{{$}}
// CHECK-NEXT: {{^}}    return {{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK: {{^}}  set {{{$}}
// CHECK-NEXT: {{^}}    if  {{{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NEXT: {{^}}}{{$}}
// CHECK-NOT: topLevelVar2

var topLevelVar3: Int {
  get {
    return 42
  }
  set(foo) {
    if true {}
  }
}
// CHECK: {{^}}var topLevelVar3: Int {{{$}}
// CHECK-NEXT: {{^}}  get {{{$}}
// CHECK-NEXT: {{^}}    return {{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK: {{^}}  set(foo) {{{$}}
// CHECK-NEXT: {{^}}    if  {{{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NEXT: {{^}}}{{$}}
// CHECK-NOT: topLevelVar3

class InClassVar1 {
// CHECK-LABEL: InClassVar1

  var instanceVar1: Int {
    get {
      return 12
    }
  }
// CHECK: {{^}}  var instanceVar1: Int {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NOT: instanceVar1

  var instanceVar2: Int {
    get {
      return 42
    }
    set {
      if true {}
    }
  }
// CHECK: {{^}}  var instanceVar2: Int {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK: {{^}}    set {{{$}}
// CHECK-NEXT: {{^}}      if  {{{$}}
// CHECK-NEXT: {{^}}      }{{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NOT: instanceVar2

  var instanceVar3: Int {
    get {
      return 42
    }
    set(foo) {
      if true {}
    }
  }
// CHECK: {{^}}  var instanceVar3: Int {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK: {{^}}    set(foo) {{{$}}
// CHECK-NEXT: {{^}}      if  {{{$}}
// CHECK-NEXT: {{^}}      }{{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NOT: instanceVar3
}

//===---
//===--- Subscript declaration printing.
//===---

class InClassSubscript1 {
// CHECK-LABEL: InClassSubscript1
  subscript(i: Int) -> Int {
    get {
      return 42
    }
    set {
      if true {}
    }
  }
// CHECK: {{^}}  subscript(i: Int) -> Int {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK: {{^}}    set {{{$}}
// CHECK-NEXT: {{^}}      if  {{{$}}
// CHECK-NEXT: {{^}}      }{{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NOT: subscript
}

class InClassSubscript2 {
// CHECK-LABEL: InClassSubscript2
  subscript(i: Int) -> Int {
    get {
      return 42
    }
    set(value) {
      if true {}
    }
  }
// CHECK: {{^}}  subscript(i: Int) -> Int {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK: {{^}}    set(value) {{{$}}
// CHECK-NEXT: {{^}}      if  {{{$}}
// CHECK-NEXT: {{^}}      }{{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NOT: subscript
}

class InClassSubscript3 {
// CHECK-LABEL: InClassSubscript3
  subscript(i: Int) -> Int {
    get {
      return 42
    }
    set(foo) {
      if true {}
    }
  }
// CHECK: {{^}}  subscript(i: Int) -> Int {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK: {{^}}    set(foo) {{{$}}
// CHECK-NEXT: {{^}}      if  {{{$}}
// CHECK-NEXT: {{^}}      }{{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NOT: subscript
}

class InClassSubscript4 {
// CHECK-LABEL: InClassSubscript4
  subscript<T>(i: T) -> T where T: Equatable {
    get {
      return i
    }
    set(foo) {
      if true {}
    }
  }
// CHECK: {{^}}  subscript<T>(i: T) -> T where T : Equatable {{{$}}
// CHECK-NEXT: {{^}}    get {{{$}}
// CHECK-NEXT: {{^}}      return {{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK: {{^}}    set(foo) {{{$}}
// CHECK-NEXT: {{^}}      if  {{{$}}
// CHECK-NEXT: {{^}}      }{{$}}
// CHECK-NEXT: {{^}}    }{{$}}
// CHECK-NEXT: {{^}}  }{{$}}
// CHECK-NOT: subscript
}



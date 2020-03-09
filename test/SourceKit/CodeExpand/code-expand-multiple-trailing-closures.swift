// RUN: %sourcekitd-test -req=expand-placeholder %s | %FileCheck %s

withMulti1Labeled(a: <#T##() -> ()#>, b: <#T##() -> ()#>)
// CHECK:      withMulti1Labeled {
// CHECK-NEXT: a: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: b: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: }

withMulti2Unlabled(<#T##() -> ()#>, _: <#T##() -> ()#>)
// CHECK:      withMulti2Unlabled {
// CHECK-NEXT: _: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: _: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: }

withMulti3SecondArg(a: <#T##__skip__##() -> ()#>, b: <#T##() -> ()#>)
// CHECK:      withMulti3SecondArg {
// CHECK-NEXT: a: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: b: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: }

withMulti4MiddleArg(a: <#T##() -> ()#>, b: <#T##__skip__##() -> ()#>, c: <#T##() -> ()#>)
// CHECK:      withMulti4MiddleArg {
// CHECK-NEXT: a: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: b: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: c: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: }

singleAlreadyExpand(a: { print("hi") }, b: <#T##() -> ()#>)
// CHECK:      singleAlreadyExpand(a: { print("hi") }) { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

nonTrailing1(a: <#T##() -> ()#>, b: { print("hi") })
// CHECK:      nonTrailing1(a: {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }, b: { print("hi") })

nonTrailingAndTrailing1(a: <#T##() -> ()#>, b: { print("hi") }, c: <#T##() -> ()#>)
// CHECK:      nonTrailingAndTrailing1(a: {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }, b: { print("hi") }) {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

singleNonClosure(a: <#T##Int#>, b: <#T##() -> ()#>)
// CHECK:      singleNonClosure(a: Int) { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

nonTrailing2(a: <#T##() -> ()#>, b: <#T##Int#>)
// CHECK:      nonTrailing2(a: {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }, b: Int)

nonTrailingAndTrailing2(a: <#T##() -> ()#>, b: <#T##Int#> c: <#T##() -> ()#>)
// CHECK:      nonTrailingAndTrailing2(a: {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }, b: Int) {
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }

withTypes1(a: <#T##(Bool, Int) -> ()#>, b: <#T##() -> Int#>)
// CHECK:      withTypes1 {
// CHECK-NEXT: a: { (<#Bool#>, <#Int#>) in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: b: { () -> Int in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: }

withTypesAndLabels1(a: <#T##(_ booly: Bool, inty: Int) -> ()#>, b: <#T##(solo: Xyz) -> ()#>)
// CHECK:      withTypesAndLabels1 {
// CHECK-NEXT: a: { (booly, inty) in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: b: { (solo) in
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: }

mixedFull1(arg1: <#T##Something#>, arg2: <#T##Other#>, callback1: <#T##() -> Void#>, callback2: <#T##() -> Void#>)
// CHECK:      mixedFull1(arg1: Something, arg2: Other) {
// CHECK-NEXT: callback1: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: callback2: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: }

mixedFull2(arg1: 1, arg2: "2"/*comment*/, callback1: <#T##() -> Void#>, callback2: <#T##() -> Void#>)
// CHECK:      mixedFull2(arg1: 1, arg2: "2") {
// CHECK-NEXT: callback1: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: callback2: { 
// CHECK-NEXT: <#code#>
// CHECK-NEXT: }
// CHECK-NEXT: }

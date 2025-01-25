// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test~partial.swiftmodule -module-name Test -primary-file %s
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/Test.swiftmodule %t/Test~partial.swiftmodule
// RUN: %target-swift-ide-test -print-module -module-to-print=Test -source-filename=x -I %t -prefer-type-repr=false -fully-qualified-types=true | %FileCheck %s

// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK-LABEL: func hasClosureDefaultArgWithComplexNestedPoundIfs(_ x: () -> Swift.Void = {
// CHECK-NOT: #if NOT_PROVIDED
// CHECK-NOT: print("should not exist")
// CHECK-NOT: #elseif !NOT_PROVIDED
// CHECK: let innerClosure = {
// CHECK-NOT: #if false
// CHECK-NOT: print("should also not exist")
// CHECK-NOT: #else
// CHECK: print("should exist")
// CHECK-NOT: #endif
// CHECK: }
// CHECK-NOT: #endif
// CHECK: })
public func hasClosureDefaultArgWithComplexNestedPoundIfs(_ x: () -> Void = {
  #if NOT_PROVIDED
    print("should not exist")
  #elseif !NOT_PROVIDED
    let innerClosure = {
      #if false
        print("should also not exist")
      #else
        print("should exist")
      #endif
    }
  #endif
}) {
}

// CHECK-LABEL: func hasClosureDefaultArgWithComplexPoundIf(_ x: () -> Swift.Void = {
// CHECK-NOT: #if NOT_PROVIDED
// CHECK-NOT: print("should not exist")
// CHECK-NOT: #else
// CHECK-NOT: #if NOT_PROVIDED
// CHECK-NOT: print("should also not exist")
// CHECK-NOT: #else
// CHECK: print("should exist"){{$}}
// CHECK-NOT: #if !second
// CHECK: print("should also exist"){{$}}
// CHECK-NOT: #endif
// CHECK-NEXT: })
public func hasClosureDefaultArgWithComplexPoundIf(_ x: () -> Void = {
  #if NOT_PROVIDED
    print("should not exist")
    #else
      #if NOT_PROVIDED
        print("should also not exist")
      #else
        print("should exist")
      #endif
    #endif

    #if !second
      print("should also exist")
    #endif
}) {
}

// CHECK-LABEL: func hasClosureDefaultArgWithMultilinePoundIfCondition(_ x: () -> Swift.Void = {
// CHECK-NOT: #if (
// CHECK-NOT:   !false && true
// CHECK-NOT: )
// CHECK: print("should appear")
// CHECK-NOT: #endif
// CHECK-NOT: #if (
// CHECK-NOT:   !true
// CHECK-NOT: )
// CHECK-NOT: print("should not appear")
// CHECK-NOT: #else
// CHECK: print("also should appear")
// CHECK-NOT: #endif
// CHECK-NEXT: })
public func hasClosureDefaultArgWithMultilinePoundIfCondition(_ x: () -> Void = {
  #if (
    !false && true
  )
  print("should appear")
  #endif

  #if (
    !true
  )
  print("should not appear")
  #else
  print("also should appear")
  #endif
}) {
}

// CHECK-LABEL: func hasClosureDefaultArgWithSinglePoundIf(_ x: () -> Swift.Void = {
// CHECK-NOT: #if true
// CHECK: print("true")
// CHECK-NOT: #else
// CHECK-NOT: print("false")
// CHECK-NOT: #endif
// CHECK-NEXT: })
public func hasClosureDefaultArgWithSinglePoundIf(_ x: () -> Void = {
  #if true
  print("true")
  #else
  print("false")
  #endif
}) {
}

// CHECK-LABEL: func hasComments
// CHECK: print(
// CHECK: "this should show up"
// CHECK-NOT: comment! don't mess up indentation!
// CHECK: {{^}}    """
// CHECK: {{^}}    """
// CHECK: #if compiler(>=5.3) {{$}}
// CHECK: print( "")
// CHECK: #endif
// CHECK: let x = 1
// CHECK-NEXT: let y = 2
// CHECK: let a = 3
// CHECK: let b = 2
// CHECK-NOT: #sourceLocation
// CHECK-NOT: #if
// CHECK-NOT: comment!
// CHECK: return true
@inlinable
public func hasComments(_ x: () -> Bool = {
  /* comment! */ // comment!
  #if NOT_PROVIDED
    // comment!
    return true
  #endif

  print(/* 
    comment! 
  */"this should show up")

  print(
    // comment! don't mess up indentation!
    """
    """)

  #if compiler(>=5.3) // comment!
  print(/*comment!*/"")
  #endif

  let x = 1/*
  */let y = 2

  let a = 3
  /* test */let b = 2

  #sourceLocation(file: "if-configs.swift", line: 200)

  #if !NOT_PROVIDED
    // comment!
    return/* comment! */true/* comment! */
  #endif
}) {
}

// CHECK-LABEL: func hasIfCompilerCheck
// CHECK:      #if compiler(>=5.3)
// CHECK-NEXT:   return true
// CHECK-NEXT: #else
// CHECK-NEXT:   return false
// CHECK-NEXT: #endif
@_alwaysEmitIntoClient
public func hasIfCompilerCheck(_ x: () -> Bool = {
#if compiler(>=5.3)
  return true
#else
  return false
#endif
  }) {
}

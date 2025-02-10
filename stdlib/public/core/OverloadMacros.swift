//===----------------------------------------------------------------------===//
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

#if $Macros && hasAttribute(attached)


/*

@_CompatibilityOverload(added: debugName, in: .swift_9999)
public init(
  debugName: String? = nil,
  priority: TaskPriority? = nil,
  @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async -> Success
)
*/
@freestanding(declaration)
internal macro _CompatibilityOverload(
  added: String,
  in:
) = Builtin.ErrorMacro

internal enum CompatOverload: String {
  case swift_5_1 = "@available(SwiftStdlib 5.1, *)"
  case swift_5_2 = "@available(SwiftStdlib 5.2, *)"
  case swift_5_3 = "@available(SwiftStdlib 5.3, *)"
  case swift_5_4 = "@available(SwiftStdlib 5.4, *)"
  case swift_5_5 = "@available(SwiftStdlib 5.5, *)"
  case swift_5_6 = "@available(SwiftStdlib 5.6, *)"
  case swift_5_7 = "@available(SwiftStdlib 5.7, *)"
  case swift_5_8 = "@available(SwiftStdlib 5.8, *)"
  case swift_5_9 = "@available(SwiftStdlib 5.9, *)"
  case swift_5_10 = "@available(SwiftStdlib 5.10, *)"
  case swift_6_0 = "@available(SwiftStdlib 6.0, *)"
  case swift_9999 = "@available(SwiftStdlib 9999, *)"
}

#endif // $Macros && hasAttribute(attached)

//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

#if SWIFT_OBSERVATION_MACROS
#if compiler(>=5.8) && hasAttribute(attached)
@available(SwiftStdlib 5.9, *)
@attached(member)
@attached(memberAttribute)
@attached(conformance)
public macro Observable() = 
  #externalMacro(module: "ObservationMacros", type: "ObservableMacro")

@available(SwiftStdlib 5.9, *)
@attached(accessor)
public macro ObservableProperty() = 
  #externalMacro(module: "ObservationMacros", type: "ObservablePropertyMacro")
#endif
#endif

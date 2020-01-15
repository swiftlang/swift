# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            './other.swiftdeps'
    fingerprint:     befb33f4269c9adc0644b060f467ef06
    sequenceNumber:  0
    defsIDependUpon: [ 5, 4, 2 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            './other.swiftdeps'
    fingerprint:     befb33f4269c9adc0644b060f467ef06
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            a
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            a
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            externalDepend
      aspect:          interface
      context:         ''
      name:            '/Users/ungar/s/fine2/build/Ninja-DebugAssert/swift-macosx-x86_64/lib/swift/macosx/Swift.swiftmodule/x86_64.swiftmodule'
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            externalDepend
      aspect:          interface
      context:         ''
      name:            '/Users/ungar/s/fine2/build/Ninja-DebugAssert/swift-macosx-x86_64/lib/swift/macosx/SwiftOnoneSupport.swiftmodule/x86_64.swiftmodule'
    sequenceNumber:  5
    defsIDependUpon: [  ]
    isProvides:      false
...

# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            other.swiftdeps
    fingerprint:     same
    sequenceNumber:  0
    defsIDependUpon: [ 8, 7, 2, 4, 6 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            other.swiftdeps
    fingerprint:     same
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            B
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            B
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            a
    sequenceNumber:  4
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            a
    sequenceNumber:  5
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            IntegerLiteralType
    sequenceNumber:  6
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            b
    sequenceNumber:  7
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            FloatLiteralType
    sequenceNumber:  8
    defsIDependUpon: [  ]
    isProvides:      false
...

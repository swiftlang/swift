# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            './other.swiftdeps'
    fingerprint:     0665c1c79514536cfd19ee3359008f19
    sequenceNumber:  0
    defsIDependUpon: [ 2, 4 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            './other.swiftdeps'
    fingerprint:     0665c1c79514536cfd19ee3359008f19
    sequenceNumber:  1
    defsIDependUpon: [ 5 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            F
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            F
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            AssignmentPrecedence
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            a
    sequenceNumber:  5
    defsIDependUpon: [  ]
    isProvides:      false
...

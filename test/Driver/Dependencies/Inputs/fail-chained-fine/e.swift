# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            e.swiftdeps
    fingerprint:     72e95f4a11b98227c1f6ad6ea7f6cdba
    sequenceNumber:  0
    defsIDependUpon: [ 2 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            e.swiftdeps
    fingerprint:     72e95f4a11b98227c1f6ad6ea7f6cdba
    sequenceNumber:  1
    defsIDependUpon: [ 4 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            e
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            e
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            bad
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
...

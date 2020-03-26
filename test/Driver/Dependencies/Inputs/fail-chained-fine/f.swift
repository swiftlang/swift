# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            f.swiftdeps
    fingerprint:     72e95f4a11b98227c1f6ad6ea7f6cdba
    sequenceNumber:  0
    defsIDependUpon: [ 2, 4 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            f.swiftdeps
    fingerprint:     72e95f4a11b98227c1f6ad6ea7f6cdba
    sequenceNumber:  1
    defsIDependUpon: [ ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            f
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            f
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            e
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
...

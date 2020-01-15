# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            d.swiftdeps
    fingerprint:     72e95f4a11b98227c1f6ad6ea7f6cdba
    sequenceNumber:  0
    defsIDependUpon: [ 2, 4 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            d.swiftdeps
    fingerprint:     72e95f4a11b98227c1f6ad6ea7f6cdba
    sequenceNumber:  1
    defsIDependUpon: [ ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            d
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            d
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            c
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
...

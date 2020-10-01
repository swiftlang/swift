# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            bad.swiftdeps
    fingerprint:     after
    sequenceNumber:  0
    defsIDependUpon: [ 2 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            bad.swiftdeps
    fingerprint:     after
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            bad
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            bad
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
...


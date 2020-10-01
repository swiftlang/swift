# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            './depends-on-bad.swiftdeps'
    fingerprint:     after
    sequenceNumber:  0
    defsIDependUpon: [ 2, 4 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            './depends-on-bad.swiftdeps'
    fingerprint:     after
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            DB
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            DB
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            bad
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
...

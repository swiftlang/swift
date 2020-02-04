# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            depends-on-a-ext.swiftdeps
    fingerprint:     eef421a8e034b3c72f85a3106b51620e
    sequenceNumber:  0
    defsIDependUpon: [ 6, 5, 7, 2, 4 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            depends-on-a-ext.swiftdeps
    fingerprint:     eef421a8e034b3c72f85a3106b51620e
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            V
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            V
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            a
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            nominal
      aspect:          interface
      context:         4main1aV
      name:            ''
    sequenceNumber:  5
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            member
      aspect:          interface
      context:         4main1aV
      name:            ext
    sequenceNumber:  6
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            member
      aspect:          interface
      context:         4main1aV
      name:            init
    sequenceNumber:  7
    defsIDependUpon: [  ]
    isProvides:      false
...

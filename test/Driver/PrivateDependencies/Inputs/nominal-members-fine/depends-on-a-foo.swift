# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            depends-on-a-foo.swiftdeps
    fingerprint:     341612d23b9b4ab590b3d75e35b5c6e0
    sequenceNumber:  0
    defsIDependUpon: [ 6, 5, 4, 7, 2 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            depends-on-a-foo.swiftdeps
    fingerprint:     341612d23b9b4ab590b3d75e35b5c6e0
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            Q
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            Q
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
      name:            foo
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

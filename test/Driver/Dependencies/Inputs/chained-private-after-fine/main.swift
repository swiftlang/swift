# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            main.swiftdeps
    fingerprint:     after
    sequenceNumber:  0
    defsIDependUpon: [ 2, 4 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            main.swiftdeps
    fingerprint:     after
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            nominal
      aspect:          interface
      context:         4main1bV
      name:            ''
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            nominal
      aspect:          implementation
      context:         4main1bV
      name:            ''
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
...

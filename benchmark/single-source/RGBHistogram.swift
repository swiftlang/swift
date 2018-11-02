//===--- RGBHistogram.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Performance benchmark for creating RGB histograms
// rdar://problem/18539486
//
// Description:
//     Create a sorted sparse RGB histogram from an array of 300 RGB values.
import Foundation
import TestsUtils

public let RGBHistogram = [
  BenchmarkInfo(name: "RGBHistogram", runFunction: run_RGBHistogram, tags: [.validation, .algorithm]),
  BenchmarkInfo(name: "RGBHistogramOfObjects", runFunction: run_RGBHistogramOfObjects, tags: [.validation, .algorithm]),
]

@inline(never)
public func run_RGBHistogram(_ N: Int) {
    var histogram = [(key: rrggbb_t, value: Int)]()
    for _ in 1...100*N {
        histogram = createSortedSparseRGBHistogram(samples)
        if !isCorrectHistogram(histogram) {
            break
        }
    }
    CheckResults(isCorrectHistogram(histogram))
}

typealias rrggbb_t = UInt32

let samples: [rrggbb_t] = [
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0x00FF0000, 0x00FF0000, 0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    0x00000000, 0x00555555, 0x00AAAAAA, 0x00FFFFFF, 0x00AAAAAA, 0x00FFFFFF,
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0x00FF0000, 0x00FF0000, 0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    0x00000000, 0x00555555, 0x00AAAAAA, 0x00FFFFFF, 0x00AAAAAA, 0x00FFFFFF,
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0x00FF0000, 0x00FF0000, 0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    0x00000000, 0x00555555, 0x00AAAAAA, 0x00FFFFFF, 0x00AAAAAA, 0x00FFFFFF,
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0x00FF0000, 0x00FF0000, 0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    0x00000000, 0x00555555, 0x00AAAAAA, 0x00FFFFFF, 0x00AAAAAA, 0x00FFFFFF,
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0x00FF0000, 0x00FF0000, 0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    0x00000000, 0x00555555, 0x00AAAAAA, 0x00FFFFFF, 0x00AAAAAA, 0x00FFFFFF,
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0x00FF0000, 0x00FF0000, 0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    0x00000000, 0x00555555, 0x00AAAAAA, 0x00FFFFFF, 0x00AAAAAA, 0x00FFFFFF,
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0x00FF0000, 0x00FF0000, 0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    0x00000000, 0x00555555, 0x00AAAAAA, 0x00FFFFFF, 0x00AAAAAA, 0x00FFFFFF,
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0x00FF0000, 0x00FF0000, 0x000000FF, 0x000000FF, 0x000000FF, 0x000000FF,
    0x00000000, 0x00555555, 0x00AAAAAA, 0x00FFFFFF, 0x00AAAAAA, 0x00FFFFFF,
    0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080, 0x00808080,
    0xCCD45FBC, 0xA56F39E4, 0x8C08DBA7, 0xDA4413D7, 0x43926C6B, 0x592975FE,
    0xC77E47ED, 0xB28F427D, 0x90D7C464, 0x805A003A, 0xAB79B390, 0x49D859B3,
    0x2419213A, 0x69E8C61D, 0xC4BE948F, 0x896CC6D0, 0xE4F3DFF1, 0x466B68FA,
    0xC8084E2A, 0x3FC1F2C4, 0x0E0D47F4, 0xB268BFE6, 0x9F990E6A, 0x7389F2F8,
    0x0720FD81, 0x65388005, 0xD8307612, 0xEC75B9B0, 0xB0C51360, 0x29647EB4,
    0x6E8B02E6, 0xEFE9F0F4, 0xFEF0EB89, 0x41BBD587, 0xCD19E510, 0x6A710BBD,
    0xFF146228, 0xFB34AD0C, 0x2AEB5588, 0x71993821, 0x9FC8CA5C, 0xF99E969B,
    0x8DF78241, 0x21ADFB7C, 0x4DE5E465, 0x0C171D2F, 0x2C08CECF, 0x3318440A,
    0xEC8F8D1C, 0x6CAFD68E, 0xCA35F571, 0x68A37E1A, 0x3047F87F, 0x50CC39DE,
    0x776CF5CB, 0x75DC4595, 0x77E32288, 0x14899C0D, 0x14835CF6, 0x0A732F76,
    0xA4B05790, 0x34CBED42, 0x5A6964CE, 0xEA4CA5F7, 0x3DECB0F1, 0x5015D419,
    0x84EBC299, 0xC656B381, 0xFA2840C5, 0x618D754E, 0x003B8D96, 0xCE91AA8E,
    0xBD9784DB, 0x9372E919, 0xC138BEA6, 0xF0B3E3AD, 0x4E4F60BF, 0xC1598ABE,
    0x930873DB, 0x0F029E3A, 0xBEFC0125, 0x10645D6D, 0x1FF93547, 0xA7069CB5,
    0xCF0B7E06, 0xE33EDC17, 0x8C5E1F48, 0x2FB345E1, 0x3B0070E0, 0x0421E568,
    0xB39A42A0, 0xB935DA8B, 0x281C30F0, 0xB2E48677, 0x277A9A45, 0x52AF9FC6,
    0xBBDF4048, 0xC668137A, 0xF39020D1, 0x71BEE810, 0x5F2B3825, 0x25C863FB,
    0x876144E8, 0x9B4108C3, 0xF735CB08, 0x8B77DEEC, 0x0185A067, 0xB964F42B,
    0xA2EC236B, 0x3C08646F, 0xB514C4BE, 0x37EE9689, 0xACF97317, 0x1EA4F7C6,
    0x453A6F13, 0x01C25E42, 0xA052BB3B, 0x71A699CB, 0xC728AE88, 0x128A656F,
    0x78F64E55, 0x045967E0, 0xC5DC4125, 0xDA39F6FE, 0x873785B9, 0xB6BB446A,
    0xF4F5093F, 0xAF05A4EC, 0xB5DB854B, 0x7ADA6A37, 0x9EA218E3, 0xCCCC9316,
    0x86A133F8, 0x8AF47795, 0xCBA235D4, 0xBB9101CC, 0xBCC8C8A3, 0x02BAC911,
    0x45C17A8C, 0x896C81FC, 0x4974FA22, 0xEA7CD629, 0x103ED364, 0x4C644503,
    0x607F4D9F, 0x9733E55E, 0xA360439D, 0x1DB568FD, 0xB7A5C3A1, 0xBE84492D
]

func isCorrectHistogram(_ histogram: [(key: rrggbb_t, value: Int)]) -> Bool {
    return histogram.count  == 157 &&
           histogram[0].0   == 0x00808080 && histogram[0].1   == 54 &&
           histogram[156].0 == 0x003B8D96 && histogram[156].1 == 1
}

func createSortedSparseRGBHistogram<S : Sequence>(
  _ samples: S
) -> [(key: rrggbb_t, value: Int)]
  where S.Iterator.Element == rrggbb_t
{
    var histogram = Dictionary<rrggbb_t, Int>()

    for sample in samples {
        let i = histogram.index(forKey: sample)
        histogram[sample] = ((i != nil) ? histogram[i!].1 : 0) + 1
    }

    return histogram.sorted() {
      if $0.1 == $1.1 {
        return $0.0 > $1.0
      } else {
        return $0.1 > $1.1
      }
    }
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  static func ==(lhs: Box, rhs: Box) -> Bool {
    return lhs.value == rhs.value
  }
}

func isCorrectHistogramOfObjects(_ histogram: [(key: Box<rrggbb_t>, value: Box<Int>)]) -> Bool {
    return histogram.count  == 157 &&
           histogram[0].0.value   == 0x00808080 && histogram[0].1.value   == 54 &&
           histogram[156].0.value == 0x003B8D96 && histogram[156].1.value == 1
}

func createSortedSparseRGBHistogramOfObjects<S : Sequence>(
  _ samples: S
) -> [(key: Box<rrggbb_t>, value: Box<Int>)]
  where S.Iterator.Element == rrggbb_t
{
    var histogram = Dictionary<Box<rrggbb_t>, Box<Int>>()

    for sample in samples {
        let boxedSample = Box(sample)
        let i = histogram.index(forKey: boxedSample)
        histogram[boxedSample] = Box(((i != nil) ? histogram[i!].1.value : 0) + 1)
    }

    return histogram.sorted() {
      if $0.1 == $1.1 {
        return $0.0.value > $1.0.value
      } else {
        return $0.1.value > $1.1.value
      }
    }
}

@inline(never)
public func run_RGBHistogramOfObjects(_ N: Int) {
    var histogram = [(key: Box<rrggbb_t>, value: Box<Int>)]()
    for _ in 1...100*N {
        histogram = createSortedSparseRGBHistogramOfObjects(samples)
        if !isCorrectHistogramOfObjects(histogram) {
            break
        }
    }
    CheckResults(isCorrectHistogramOfObjects(histogram))
}



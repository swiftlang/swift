
import Cocoa

let ImageWidth : CGFloat = 500
let ImageHeight : CGFloat = 500

enum ProblemSize {
  case Small
  case Normal
  case Large
}

let PSize = ProblemSize.Small

func drawJulia(context : CGContextRef, frame : CGRect) {
  var rRect : NSRect = NSRect(10, 10, 4, 4)
  var color = NSColor()

  // Set up color arrays
  var basicColorSet : Array<NSColor> = Array()
  //        basicColorSet = [NSColor.lightGrayColor(), NSColor.blueColor(), NSColor.greenColor(), NSColor.redColor(), NSColor.yellowColor(), NSColor.orangeColor(), NSColor.brownColor(), NSColor.darkGrayColor()]
  basicColorSet.append(NSColor.lightGrayColor())
  basicColorSet.append(NSColor.blueColor())
  basicColorSet.append(NSColor.greenColor())
  basicColorSet.append(NSColor.redColor())
  basicColorSet.append(NSColor.yellowColor())
  basicColorSet.append(NSColor.orangeColor())
  basicColorSet.append(NSColor.brownColor())
  basicColorSet.append(NSColor.darkGrayColor())

  var gradedColorSet : Array<NSColor> = Array()
  var aColor = NSColor.colorWithRed(0.5, green:0.5, blue:0.5, alpha:1.0)
  for c in 100..255 {
    gradedColorSet.append(NSColor.colorWithRed(Double(c)/255.0, green: Double(c)/255.0, blue: Double(c)/255.0, alpha:1.0))
  }

  var psychedelicColorSet : Array<NSColor> = Array()
  for c in 0..100 {
    psychedelicColorSet.append(NSColor.colorWithHue(Double(c)/100.0, saturation:1.0, brightness:1.0, alpha:1.0))
  }
  var colorSet : Array<NSColor> = Array()

  switch 2 {
    case 0:
    colorSet = basicColorSet
    case 1:
    colorSet = gradedColorSet
    case 2:
    colorSet = psychedelicColorSet
    default:
    colorSet = psychedelicColorSet
  }

  // Main Julia set algorithm

  var loopCountForPerf : Int = 0
  var maxc = 0
  switch PSize {
    case .Small:
    maxc = 100
    case .Normal:
    maxc = 1000
    case .Large:
    maxc = 10000
  }

  let at1 : Double = 0.15;
  let at2 : Double = 0.35;
  let rat : Double = 0.01*0.01;
  let xloc : Double = -1.5;
  let yloc : Double = -1.25;
  let yhigh : Double = 1.25;
  let mag : Double = 380.0/(yhigh - yloc)
  let cr : Double = 0.27334
  let ci : Double = 0.00712;

  var r : Double = 0.0
  var i : Double = 0.0
  var rh : Double = 0.0

  let methodStart : NSDate = NSDate()
  let xMax : Int = Int(frame.size.width)
  let yMax : Int = Int(frame.size.height)
  for x in 1..xMax {
    for y in 1..yMax {
      r = (Double(x)/mag) + xloc
      i = (Double(y)/mag) + yloc
      // cr = r ; ci = i

      // Initial color white
      color = NSColor.whiteColor()
      var countA=1;
      for iter in 1..maxc {
        rh = r*r - i*i
        i = 2*r*i
        r = rh
        r = r + cr
        i = i + ci
        if ((r*r + i*i) > 100) {
          break
        }
        if (((r-at1)*(r-at1) + (i-at2)*(i-at2)) <= rat) {
          break
        }
        countA++
        loopCountForPerf++
      }
      color = colorSet[countA % colorSet.count]
      if (countA >= maxc) {
        color = NSColor.blackColor()
      }
      // Draw the point
      rRect = CGRect(CGFloat(x), CGFloat(y), 4, 4);
      // rRect = NSRect(CGPoint(CGFloat(x),CGFloat(y)), CGSize(4.0, 4.0))
      assert(context != CGContextRef())
      CGContextSetFillColorWithColor(context, color.CGColor())
      CGContextFillRect(context, rRect)
    }
  }
  print("Total loop count: \(loopCountForPerf)")
  let methodFinish : NSDate = NSDate()
  var executionTime : NSTimeInterlet = methodFinish.timeIntervalSinceDate(anotherDate: methodStart)
  print("Execution time: \(executionTime)")
}

// Create the image bitmap context.
print("Initializing CGBitmapContext.\n")
var colorSpace =  NSColor.redColor().colorSpace.CGColorSpace
assert(colorSpace != CGColorSpaceRef())
let width : Swift.UInt = 500
let height : Swift.UInt = 500
var bitmapInfo = CGBitmapInfo(CGImageAlphaInfo.PremultipliedLast.rawValue)!
var data = malloc(width*4*height)
var context = CGBitmapContextCreate(data, width, height, 8, width*4, colorSpace, bitmapInfo)
assert(context != CGContextRef())

var rect : CGRect = CGRect(0, 0, ImageWidth, ImageHeight)
CGContextClearRect(context, rect)
CGContextSetRGBFillColor(context, 1, 1, 1, 1)
CGContextFillRect(context, rect)

// Draw the image.
print("Drawing Julia.\n")
drawJulia(context, rect)

let CFStringEncoding_ASCII : UInt32 = 0x0600

// Write the image to disk.
func writeImageToDisk(context : CGContextRef, outPath : NSString) -> Bool {
  var image = CGBitmapContextCreateImage(context)
  let encoding : NSStringEncoding = NSASCIIStringEncoding
  let path = CFStringCreateWithCString(CFAllocatorRef(), outPath.cStringUsingEncoding(encoding), CFStringEncoding_ASCII)
  assert(path != CFStringRef())
  var url = CFURLCreateWithString(CFAllocatorRef(), path, CFURLRef())
  assert(url != CFURLRef())
  var destination = CGImageDestinationCreateWithURL(url, kUTTypePNG, 1, CFDictionaryRef())
  assert(destination != CGImageDestinationRef())
  CGImageDestinationAddImage(destination, image, COpaquePointer())
  if !CGImageDestinationFinalize(destination) {
    print("Failed to write image")
    return (false)
  }
  return (true)
}

var outPath = NSURL.fileURLWithPath("out.png").absoluteString
writeImageToDisk(context, outPath! as NSString)

// Cleanup.
free(data)
CGContextRelease(context)
CGColorSpaceRelease(colorSpace)

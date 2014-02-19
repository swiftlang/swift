
#import <stdlib.h>

#import <Cocoa/Cocoa.h>
#import <CoreFoundation/CoreFoundation.h>

#define WIDTH 500
#define HEIGHT 500

void drawJulia(CGContextRef context, CGRect frame, int maxc) {

  NSRect rRect = NSMakeRect(10, 10, 4, 4);
  NSColor *color = [NSColor blueColor];
  //    [color set];
  //    [NSBezierPath fillRect:rRect];

  NSArray *basicColorSet = @[
    [NSColor lightGrayColor],
    [NSColor blueColor],
    [NSColor greenColor],
    [NSColor redColor],
    [NSColor yellowColor],
    [NSColor orangeColor],
    [NSColor brownColor],
    [NSColor darkGrayColor]
  ];
  NSMutableArray *gradedColorSet = [[NSMutableArray alloc] init];

  for (int c = 100; c<255; c++) {
    [gradedColorSet addObject:[NSColor colorWithRed:c/255.0 green:c/255.0 blue:c/255.0 alpha:1.0]];
  }

  NSMutableArray *psychedelicColorSet = [[NSMutableArray alloc] init];
  for (int c = 0; c<100; c++) {
    [psychedelicColorSet addObject:[NSColor colorWithHue:c/100.0 saturation:1.0 brightness:1.0 alpha:1.0]];
  }

  NSArray *colorSet;

  int type = 2;
  switch (type) {
  case 0:
    colorSet = basicColorSet;
    break;
  case 1:
    colorSet = gradedColorSet;
    break;
  case 2:
    colorSet = psychedelicColorSet;
    break;
  default:
    colorSet = gradedColorSet;
    break;
  }

  int loopCountForPerf = 0;
  double at1 = 0.15;
  double at2 = 0.35;
  double rat = 0.01*0.01;
  double xloc = -1.5;
  double yloc = -1.25;
  double yhigh = 1.25;
  double mag = 380.0/(yhigh - yloc);
  double cr=0.27334;
  double ci=0.00712;
  double r;
  double i;
  double rh;

  NSDate *methodStart = [NSDate date];
  double maxX = frame.size.width;
  double maxY = frame.size.height;

  for (int x=1; x<maxX; x++) {
    for (int y=1; y<maxY; y++) {
      r = (x/mag) + xloc;
      i = (y/mag) + yloc;
      // cr = r ; ci = i ;

      // Initial color white
      color = [NSColor whiteColor];
      int countA=1;
      for (countA=1; countA < maxc; countA++) {
        rh = r*r - i*i;
        i = 2*r*i;
        r = rh; // ???
        r = r + cr;
        i = i + ci;
        if ((r*r + i*i) > 100) {
          break;
        }
        if (((r-at1)*(r-at1) + (i-at2)*(i-at2)) <= rat) {
          break;
        }
        loopCountForPerf++;
      }
      color = colorSet[countA % [colorSet count]];
      if (countA >= maxc) {
        color = [NSColor blackColor];
      }
      // Draw the point
      CGContextSetFillColorWithColor(context, color.CGColor);
      CGContextFillRect(context, (CGRect){{x, y}, {4, 4}});
    }
  }
  NSLog(@"Total loop count %d", loopCountForPerf);
  NSDate *methodFinish = [NSDate date];
  NSTimeInterval executionTime = [methodFinish timeIntervalSinceDate:methodStart];
  NSLog(@"Execution time: %f", executionTime);
}

int main(int argc, char *argv[]) {

  if (argc != 2) {
    fprintf(stderr, "Invalid Argument!\n%s {small, normal, large}!\n", argv[0]);
    exit(-1);
  }

  int maxc;
  char *out_name;
  if (!strcmp(argv[1], "small")) {
    maxc = 100;
    out_name = "small.png";
  } else if (!strcmp(argv[1], "normal")) {
    maxc = 1000;
    out_name = "normal.png";
  } else if (!strcmp(argv[1], "large")) {
    maxc = 10000;
    out_name = "large.png";
  } else {
    fprintf(stderr, "Invalid Argument!\n%s {small, normal, large}!\n", argv[0]);
    exit(-1);
  }

  CGColorSpaceRef colorSpace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
  size_t width = WIDTH;
  size_t height = HEIGHT;
  size_t bytesPerRow = width * 4;
  assert(colorSpace != NULL);
  void *bitmapData = calloc(bytesPerRow*height, 1);
  CGContextRef context = CGBitmapContextCreate(bitmapData, WIDTH, HEIGHT, 8, bytesPerRow, colorSpace, kCGImageAlphaPremultipliedLast);
  assert(context != NULL);

  CGRect Rect = { .origin = { 0, 0 }, .size = { WIDTH, HEIGHT } };

  CGContextSetRGBFillColor(context, 0, 0, 0, 1);
  CGContextFillRect(context, Rect);

  drawJulia(context, Rect, maxc);

  CGImageRef image = CGBitmapContextCreateImage(context);
  NSURL *URL = [[NSURL fileURLWithPath:[NSString stringWithCString:out_name encoding:NSASCIIStringEncoding]] absoluteURL];  
  NSLog(@"%@", URL);
  assert(image);
  assert(URL);
  CGImageDestinationRef destination = CGImageDestinationCreateWithURL((__bridge CFURLRef)URL, kUTTypePNG, 1, NULL);
  assert(destination);
  CGImageDestinationAddImage(destination, image, NULL);
  if (!CGImageDestinationFinalize(destination))
    NSLog(@"Failed to write image!");

  CGContextRelease(context);
  CGColorSpaceRelease(colorSpace);
  free(bitmapData);

  return 0;
}

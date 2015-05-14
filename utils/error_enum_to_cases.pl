#!/usr/bin/perl -w
use English;

sub translateAvailability($) {
  my $version = shift;
  if ($version eq "NA") { return "unavailable"; }
  $version =~ /([0-9]+)_([0-9]+)/;
  return "introduced=$1.$2";
}

$prefixLength = 2;
my %minimumValues = ();
my %maximumValues = ();
my %rangeAvailability = ();
my $prev_had_availability = 0;
foreach $line (<STDIN>) {
  chomp $line;
  if ($line =~ /([A-Za-z_][A-Za-z_0-9]+).*=[^0-9]((0x)?[0-9]+)/) {
    my $fullname = $1;
    my $value = $2;
    my $has_availability = 0;
    
    my $availability = "";
    if ($line =~ /AVAILABLE\((([0-9]+_[0-9]+)|(NA))\s*,\s*(([0-9]+_[0-9]+)|(NA))\)/) {
      $has_availability = 1;
      $osx = $1;
      $ios = $4;
      $osx = translateAvailability($osx);
      $ios = translateAvailability($ios);
      $availability = "  \@available(OSX, $osx) \@available(iOS, $ios)\n";
    }

    # If the full name ends in "Minimum" or "Maximum", it's for a range. 
    my $rangeName = "";
    if ($fullname =~ /(Minimum|Maximum)$/) {
      $rangeName = substr $PREMATCH, $prefixLength;
      if ($MATCH eq "Minimum") {
        $minimumValues{$rangeName} = $value;
      } else {
        $maximumValues{$rangeName} = $value;
      }
      $rangeAvailability{$rangeName} = $availability;
    } else {
      if ($availability ne "" && $prev_had_availability == 0) {
        print("\n$availability");
      }
      $casename = substr $fullname, $prefixLength;
      print("  case $casename = $value\n");

      if ($availability ne "") {
        print("\n");
        $prev_had_availability = 1;
      } else {
        $prev_had_availability = 0;
      }
    }
  }
}

# Print properties for the ranges.
foreach $key (sort keys(%minimumValues)) {
  my $minimum = $minimumValues{$key};
  my $maximum = $maximumValues{$key};
  my $availability = $rangeAvailability{$key};
  print "\n";
  if ($availability ne "") {
    print $availability;
  }
  print("  public var is$key: Bool {\n");
  print("    return rawValue >= $minimum && rawValue <= $maximum;\n");
  print("  }\n");
}

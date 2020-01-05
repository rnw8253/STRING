#!/usr/bin/perl
#
#
#


$inp = $ARGV[0];


$range1[0][0] = -0.25;
$range1[0][1] = 0.4;
$range1[1][0] = 4.0;
$range1[1][1] = 5.0;
$range2[0][0] = 0.6;
$range2[0][1] = 1.4;
$range2[1][0] = 2.7;
$range2[1][1] = 3.5;


$nXvals = 100;
$nYvals = 100;


open(INP,"<$inp");

$count1=0;
$count2=0;
for ($x=0;$x<$nXvals;$x++) {
	for ($y=0;$y<$nYvals;$y++) {

		$line=<INP>;
		chomp $line;
		@array = split ' ', $line;

		$xVal = $array[0];
		$yVal = $array[1];
		$zVal = $array[2];

		if ($xVal < $range1[0][1] && $xVal > $range1[0][0] && $yVal < $range1[1][1] && $yVal > $range1[1][0]) {
			if ($count1 == 0 || $zVal < $min1) {
				$min1 = $zVal;
				$min1x = $x;
				$min1y = $y;
				$count1++;
			}
		}
		if ($xVal < $range2[0][1] && $xVal > $range2[0][0] && $yVal < $range2[1][1] && $yVal > $range2[1][0]) {
			if ($count2 == 0 || $zVal < $min2) {
				$min2 = $zVal;
				$min2x = $x;
				$min2y = $y;
				$count2++;
			}
		}

	}

}


close(INP);


printf "Min 1 has a value of %f at %d, %d\n", $min1, $min1x+1, $min1y+1;
printf "Min 2 has a value of %f at %d, %d\n", $min2, $min2x+1, $min2y+1;




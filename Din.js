// Skier code for a given mass.
function skierCodeMass(mass) {
  return mass;
}


// Skier code for a given height.
function skierCodeHeight(height) {
  switch (height) {
    case 0: return 7;
    case 1: return 8;
    case 2: return 9;
    case 3: return 10;
    case 4: return 11;
    case 5: return 12;
  }
}


// Skier code for a given mass and height.
function skierCode(mass, height) {
  return Math.min(skierCodeMass(mass), skierCodeHeight(height));
}


// Index for a given skier code.
function skierCodeIndex(skierCode) {
  return skierCode + 1;
}


// Adjust index based on age.
function adjustIndexForAge(age, index) {
  switch (age) {
    case 0: return index - 1;
    case 1: return index;
    case 2: return index - 1;
  }
}


// Adjust index based on skier type.
function adjustIndexForSkierType(skierType, index) {
  switch (skierType) {
    case 0: return index - 1;
    case 1: return index;
    case 2: return index + 1;
    case 3: return index + 2;
    case 4: return index + 3;
  }
}


// Limit skier type based on mass.  NOTE 2.
function limitSkierType(mass, skierType) {
  if (skierType == 0 && (mass == 0 || mass == 1))
    return 1;
  else
    return skierType;
}


// The DIN table.
const dinTable =
  // -
  [ [5, 18, [null, null, null, null, null, null, null, null]]
  // A
  , [8, 29, [0.75, 0.75, 0.75, null, null, null, null, null]]
  // B
  , [11, 40, [1.0, 0.75, 0.75, 0.75, null, null, null, null]]
  // C
  , [14, 52, [1.5, 1.25, 1.25, 1.0, null, null, null, null]]
  // D
  , [17, 64, [2.0, 1.75, 1.5, 1.5, 1.25, null, null, null]]
  // E
  , [20, 75, [2.5, 2.25, 2.0, 1.75, 1.5, 1.5, null, null]]
  // F
  , [23, 87, [3.0, 2.75, 2.5, 2.25, 2.0, 1.75, 1.75, null]]
  // G
  , [27, 102, [null, 3.5, 3.0, 2.75, 2.5, 2.25, 2.0, null]]
  // H
  , [31, 120, [null, null, 3.5, 3.0, 3.0, 2.75, 2.5, null]]
  // I
  , [37, 141, [null, null, 4.5, 4.0, 3.5, 3.5, 3.0, null]]
  // J
  , [43, 165, [null, null, 5.5, 5.0, 4.5, 4.0, 3.5, 3.0]]
  // K
  , [50, 194, [null, null, 6.5, 6.0, 5.5, 5.0, 4.5, 4.0]]
  // L
  , [58, 229, [null, null, 7.5, 7.0, 6.5, 6.0, 5.5, 5.0]]
  // M
  , [67, 271, [null, null, null, 8.5, 8.0, 7.0, 6.5, 6.0]]
  // N
  , [78, 320, [null, null, null, 10.0, 9.5, 8.5, 8.0, 7.5]]
  // O
  , [91, 380, [null, null, null, 11.5, 11.0, 10.0, 9.5, 9.0]]
  // -
  , [105, 452, [null, null, null, null, null, 12.0, 11.0, 10.5]]
  // -
  , [121, 520, [null, null, null, null, null, null, null, null]]
  // -
  , [137, 588, [null, null, null, null, null, null, null, null]]
  ];


/*
Calculate release torques and DIN given mass, height, skier type, age, and BSL.

Returns [twist, forwardLean, din], where twist and forwardLean are in Nm and
din is either a value or a null.

Arguments are encoded as integers:

  mass:
     0:  10 - 13 kg
     1:  14 - 17 kg
     2:  18 - 21 kg
     3:  22 - 25 kg
     4:  26 - 30 kg
     5:  31 - 35 kg
     6:  36 - 41 kg
     7:  42 - 48 kg
     8:  49 - 57 kg
     9:  58 - 66 kg
    10:  67 - 78 kg
    11:  79 - 94 kg
    12:  95+ kg

  height:
    0:  <= 148 cm
    1:  149 - 157 cm
    2:  158 - 166 cm
    3:  167 - 178 cm
    4:  179 - 194 cm
    5:  195+ cm

  skierType:
    0:  skier type 1 minus
    1:  skier type 1
    2:  skier type 2
    3:  skier type 3
    4:  skier type 3 plus

  age:
    0:  <= 9 years old
    1:  10 - 49 years old
    2:  50+ years old

  bsl:
    0:  <= 230 mm
    1:  231 - 250 mm
    2:  251 - 270 mm
    3:  271 - 290 mm
    4:  291 - 310 mm
    5:  311 - 330 mm
    6:  331 - 350 mm
    7:  351+ mm

*/
function calculateDin(mass, height, skierType, age, bsl) {

  // Get the index given the skier code.
  let index0 = skierCodeIndex(skierCode(mass, height));

  // Adjust the index for the skier type and limit skier type based on NOTE 2.
  let index1 = adjustIndexForSkierType((limitSkierType(mass, skierType)), index0);

  // Adjust the index for age.
  let index2 = adjustIndexForAge(age, index1);

  // Set the index based on NOTE 1.
  let index3 = mass == 0 ? index0 : index2;

  // Select row from table.
  let row = dinTable[index3];

  // Return [twist, forwardLean, and the DIN setting].
  return [row[0], row[1], row[2][bsl]];

}



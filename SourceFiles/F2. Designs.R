## Obtain transmatrix and modules (allowed routes) of each design

## ------ 1-2-3
## Specify modules with 47 items each
modules_123 <- matrix(0, 282, 6)
modules_123[1:47, 1] <- 1 
modules_123[48:94, 2] <- 1
modules_123[95:141, 3] <- 1
modules_123[142:188, 4] <- 1
modules_123[189:235, 5] <- 1
modules_123[236:282, 6] <- 1

## specify routes allowed 
trans_123 <- matrix(0,6,6)
trans_123[1, 2:3] <- 1
trans_123[2:3, 4:6] <- 1

## ------ 2-3
## Specify modules with 47 items each
modules_23 <- matrix(0, 235, 5)
modules_23 [1:47, 1] <- 1 
modules_23 [48:94, 2] <- 1
modules_23 [95:141, 3] <- 1
modules_23 [142:188, 4] <- 1
modules_23 [189:235, 5] <- 1

## specify routes allowed 
trans_23  <- matrix(0,5,5)
trans_23 [1:2, 3:5] <- 1

## ------ 3-3

## Specify modules with 47 items each
modules_33 <- matrix(0, 282, 6)
modules_33[1:47, 1] <- 1 
modules_33[48:94, 2] <- 1
modules_33[95:141, 3] <- 1
modules_33[142:188, 4] <- 1
modules_33[189:235, 5] <- 1
modules_33[236:282, 6] <- 1

## specify routes allowed 
trans_33 <- matrix(0,6,6)
trans_33[1:3, 4:6] <- 1

## ------ 4-3
## Specify modules with 47 items each
modules_43 <- matrix(0, 329, 7)
modules_43[1:47, 1] <- 1 
modules_43[48:94, 2] <- 1
modules_43[95:141, 3] <- 1
modules_43[142:188, 4] <- 1
modules_43[189:235, 5] <- 1
modules_43[236:282, 6] <- 1
modules_43[283:329, 7] <- 1

## specify routes allowed 
trans_43 <- matrix(0,7,7)
trans_43[1:4, 5:7] <- 1

## ------ 5-3

## Specify modules with 47 items each
modules_53 <- matrix(0, 376, 8)
modules_53 [1:47, 1] <- 1 
modules_53 [48:94, 2] <- 1
modules_53 [95:141, 3] <- 1
modules_53 [142:188, 4] <- 1
modules_53 [189:235, 5] <- 1
modules_53 [236:282, 6] <- 1
modules_53 [283:329, 7] <- 1
modules_53 [330:376, 8] <- 1

## specify routes allowed 
trans_53  <- matrix(0,8,8)
trans_53 [1:5, 6:8] <- 1

## ------ 8-3
## Specify modules with 47 items each
modules_83 <- matrix(0, 517, 11)
modules_83[1:47, 1] <- 1 
modules_83[48:94, 2] <- 1
modules_83[95:141, 3] <- 1
modules_83[142:188, 4] <- 1
modules_83[189:235, 5] <- 1
modules_83[236:282, 6] <- 1
modules_83[283:329, 7] <- 1
modules_83[330:376, 8] <- 1
modules_83[377:423, 9] <- 1
modules_83[424:470, 10] <- 1
modules_83[471:517, 11] <- 1

## specify routes allowed 
trans_83 <- matrix(0,11,11)
trans_83[1:8, 9:11] <- 1
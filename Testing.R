## This script is a loose collection of tests to try to find optimal paramters for
## the probability adjustment function

## Adjusted probabilities based on novelty, to be used in main function
decay.adj <- function(t = FALSE, result_table) {
  
  if(t == FALSE)
    return(result_table)
  
  # Probability we have left to work with
  prob.space <- 1 - result_table$Prob 
  
  # Optimal function structure after lots of testing
  adj <- (pi*25)^(-(log(result_table$Nov+t))/(pi*3))
  
  # Moving the probability towards 1 based on the novelty and previous prob
  result_table$Adj.Prob <- (prob.space * adj) + result_table$Prob
  
  result_table <- result_table[order(-result_table$Adj.Prob),]
  
  return(result_table)
}

## Checking accuracy for individual lines
test.pred <- function(string, tune) {
  
  input <- boff.cmp(string, end = TRUE)
  
  result <- boff.cmp(string,last = TRUE) %in% predict.ngram(tune = tune, input)$`Best Predictions`
  
  return(result)
}

## Checking accuracy for multiple lines, in parallel
library(parallel)

cluster <- makeCluster(detectCores())

clusterExport(cluster, varlist = c("boff.cmp","test.pred","predict.ngram",
                                   "adj.list","decay.adj"), envir = .GlobalEnv)

accuracy <- function(tune = FALSE, data, lines) {
  
  samp.data <- sample(data, lines)
  
  results <- unlist(parLapply(cluster, samp.data, test.pred, tune))#
  
  return(mean(results))
}

## Old testing numbers
accuracy(all_sent.test, length(all_sent.test)) #10 prune, 5 options 0.1933437
#5 prune, 5 options 0.2105889
#5 prune, 5 options, fixed 0.2729945

## Testing adjusted accuracy, looking for optimal tuning parameter
## Original function is e^(-x/t)
set.seed(131)
accuracy(tune = FALSE, all_sent.test, 1000) #Baseline accuracy 0.2791878
accuracy(tune = 5, all_sent.test, 1000) #0.2855392
accuracy(tune = 10, all_sent.test, 1000) #0.2786885
accuracy(tune = 50, all_sent.test, 1000) #0.3075916
accuracy(tune = 0.5, all_sent.test, 1000) #0.2484549
accuracy(tune = 100, all_sent.test, 1000) #0.2593516
accuracy(tune = 300, all_sent.test, 1000) #0.2962963
accuracy(tune = 500, all_sent.test, 1000) #0.2825553
accuracy(tune = 40, all_sent.test, 1000) #0.2692794
accuracy(tune = 60, all_sent.test, 1000) #0.2431373
accuracy(tune = 3, all_sent.test, 1000) #0.2804569
accuracy(tune = pi, all_sent.test, 1000) #0.2804569
accuracy(tune = 4, all_sent.test, 1000) #0.2855392


set.seed(101)
accuracy(tune = 50, all_sent.test, 1000) #0.2757306
accuracy(tune = 300, all_sent.test, 1000) #0.2378517
accuracy(tune = 5, all_sent.test, 1000) #0.2659975
accuracy(tune = 3, all_sent.test, 1000) #0.3082707
accuracy(tune = 2, all_sent.test, 1000) #0.2770013
accuracy(tune = 1, all_sent.test, 1000) #0.2782719
accuracy(tune = 0.5, all_sent.test, 1000)#0.2782719
accuracy(tune = 0.25, all_sent.test, 1000) #0.2429668
accuracy(tune = 4, all_sent.test, 1000) #0.2782719

## Function changes wildly
set.seed(101)

mini.test <- sample(all_sent.test, 2000)

mini.test.1 <- mini.test[1:1000]; mini.test.2 <- mini.test[1001:2000]; rm(mini.test)

accuracy(tune = 50, mini.test.1, 1000) #0.2480818
accuracy(tune = 50, mini.test.2, 1000) #0.2648515

accuracy(tune = 100, mini.test.1, 1000) #0.2429668
accuracy(tune = 100, mini.test.2, 1000) #0.2561881

## Base changed to 100
set.seed(114)

accuracy(tune = 2, all_sent.test, 10000) #0.2752225
accuracy(tune = 3, all_sent.test, 10000) #0.2752225
accuracy(tune = 4, all_sent.test, 10000) #0.2722103
accuracy(tune = 5, all_sent.test, 10000) #0.2776944
accuracy(tune = 6, all_sent.test, 10000) #0.2769694
accuracy(tune = 7, all_sent.test, 10000) #0.2683929
accuracy(tune = 8, all_sent.test, 10000) #0.2606076
accuracy(tune = 9, all_sent.test, 10000) #0.2693417
accuracy(tune = 10, all_sent.test, 10000) #0.2753153
accuracy(tune = 11, all_sent.test, 10000) #0.2710798

## Base changed to e
set.seed(114)

accuracy(tune = 2, all_sent.test, 10000) #0.2760998
accuracy(tune = 3, all_sent.test, 10000) #0.2722103
accuracy(tune = 4, all_sent.test, 10000) #0.2769442
accuracy(tune = 5, all_sent.test, 10000) #0.2773457
accuracy(tune = 6, all_sent.test, 10000) #0.2696377
accuracy(tune = 7, all_sent.test, 10000) #0.2623651
accuracy(tune = 8, all_sent.test, 10000) #0.2705956
accuracy(tune = 9, all_sent.test, 10000) #0.2760644
accuracy(tune = 10, all_sent.test, 10000) #0.2718349
accuracy(tune = 11, all_sent.test, 10000) #0.277375
accuracy(tune = 50, all_sent.test, 10000) #0.2672881

## Base changed to 500
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 2, all_sent.test, 10000) #0.2719575
accuracy(tune = 3, all_sent.test, 10000) #0.2775694
accuracy(tune = 4, all_sent.test, 10000) #0.2770948
accuracy(tune = 5, all_sent.test, 10000) #0.2685174
accuracy(tune = 6, all_sent.test, 10000) #0.2603565
accuracy(tune = 7, all_sent.test, 10000) #0.2697179
accuracy(tune = 8, all_sent.test, 10000) #0.2756898
accuracy(tune = 9, all_sent.test, 10000) #0.2705764
accuracy(tune = 10, all_sent.test, 10000) #0.27725
accuracy(tune = 11, all_sent.test, 10000) #0.264265
accuracy(tune = 50, all_sent.test, 10000) #0.274981

## Base changed to 1000
set.seed(114)

accuracy(tune = 2, all_sent.test, 10000) #0.2753478
accuracy(tune = 3, all_sent.test, 10000) #0.2720839
accuracy(tune = 4, all_sent.test, 10000) #0.2776944
accuracy(tune = 5, all_sent.test, 10000) #0.2772203
accuracy(tune = 6, all_sent.test, 10000) #0.2682684
accuracy(tune = 7, all_sent.test, 10000) #0.2603565
accuracy(tune = 8, all_sent.test, 10000) #0.2695925
accuracy(tune = 9, all_sent.test, 10000) #0.2756898
accuracy(tune = 10, all_sent.test, 10000) #0.2705764
accuracy(tune = 11, all_sent.test, 10000) #0.27725
accuracy(tune = 11, all_sent.test, 10000) #0.2700831
accuracy(tune = 15, all_sent.test, 10000) #0.276431
accuracy(tune = 21, all_sent.test, 10000) #0.2805
accuracy(tune = 33, all_sent.test, 10000) #0.2780427
accuracy(tune = 50, all_sent.test, 10000) #0.264391
accuracy(tune = 100, all_sent.test, 10000) #0.2759929
accuracy(tune = 500, all_sent.test, 10000) #0.2682069

## Base changed to 10000
set.seed(114)

accuracy(tune = 2, all_sent.test, 10000) #0.2676742
accuracy(tune = 3, all_sent.test, 10000) #0.2684091
accuracy(tune = 4, all_sent.test, 10000) #0.2792392
accuracy(tune = 5, all_sent.test, 10000) #0.27479
accuracy(tune = 6, all_sent.test, 10000) #0.2666583
accuracy(tune = 7, all_sent.test, 10000) #0.2707286
accuracy(tune = 8, all_sent.test, 10000) #0.275257
accuracy(tune = 9, all_sent.test, 10000) #0.277375
accuracy(tune = 10, all_sent.test, 10000) #0.2644846
accuracy(tune = 11, all_sent.test, 10000) #0.2639152
accuracy(tune = 11, all_sent.test, 10000) #0.2714932
accuracy(tune = 15, all_sent.test, 10000) #0.2704702
accuracy(tune = 21, all_sent.test, 10000) #0.2730346
accuracy(tune = 33, all_sent.test, 10000) #0.2765746

accuracy(tune = 45, all_sent.test, 10000) #0.2763504
accuracy(tune = 50, all_sent.test, 10000) #0.2722103
accuracy(tune = 55, all_sent.test, 10000) #

accuracy(tune = 100, all_sent.test, 10000) #0.270755
accuracy(tune = 500, all_sent.test, 10000) #0.2787978

## Changed to 5^(-exp(x)/t)
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 2, all_sent.test, 10000) #0.2752225
accuracy(tune = 3, all_sent.test, 10000) #0.2720839
accuracy(tune = 4, all_sent.test, 10000) #0.2773193
accuracy(tune = 5, all_sent.test, 10000) #0.2770948
accuracy(tune = 6, all_sent.test, 10000) #0.2687663
accuracy(tune = 7, all_sent.test, 10000) #0.260482
accuracy(tune = 8, all_sent.test, 10000) #0.2699687
accuracy(tune = 9, all_sent.test, 10000) #0.2758147
accuracy(tune = 10, all_sent.test, 10000) #0.2703247
accuracy(tune = 11, all_sent.test, 10000) #0.277
accuracy(tune = 12, all_sent.test, 10000) #0.2645169
accuracy(tune = 13, all_sent.test, 10000) #0.2728308
accuracy(tune = 14, all_sent.test, 10000) #0.2649422
accuracy(tune = 15, all_sent.test, 10000) #0.2699572
accuracy(tune = 20, all_sent.test, 10000) #0.2768118
accuracy(tune = 25, all_sent.test, 10000) #0.28
accuracy(tune = 50, all_sent.test, 10000) #0.2780427
accuracy(tune = 100, all_sent.test, 10000) #0.268049
accuracy(tune = 1000, all_sent.test, 10000) #0.2676552

## Changed to 100^(-(sqrt(x))/t)
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 2, all_sent.test, 10000) #0.2758491
accuracy(tune = 3, all_sent.test, 10000) #0.2719575
accuracy(tune = 4, all_sent.test, 10000) #0.2774444
accuracy(tune = 5, all_sent.test, 10000) #0.2787255
accuracy(tune = 6, all_sent.test, 10000) #0.2710071
accuracy(tune = 7, all_sent.test, 10000) #0.2624906
accuracy(tune = 8, all_sent.test, 10000) #0.2722257
accuracy(tune = 9, all_sent.test, 10000) #0.278187
accuracy(tune = 10, all_sent.test, 10000) #0.2734709
accuracy(tune = 11, all_sent.test, 10000) #0.27875
accuracy(tune = 12, all_sent.test, 10000) #0.2680438
accuracy(tune = 13, all_sent.test, 10000) #0.2785226
accuracy(tune = 14, all_sent.test, 10000) #0.2690859
accuracy(tune = 15, all_sent.test, 10000) #0.2748678
accuracy(tune = 20, all_sent.test, 10000) #0.2820155
accuracy(tune = 22, all_sent.test, 10000) #0.2844263
accuracy(tune = 25, all_sent.test, 10000) #0.28475
accuracy(tune = 30, all_sent.test, 10000) #0.2829384
accuracy(tune = 35, all_sent.test, 10000) #0.2766596
accuracy(tune = 50, all_sent.test, 10000) #0.2795483
accuracy(tune = 100, all_sent.test, 10000) #0.2665501
accuracy(tune = 1000, all_sent.test, 10000) #0.2557175

## Changed to 250^(-(sqrt(x))/t)
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 2, all_sent.test, 10000) #0.2757238
accuracy(tune = 3, all_sent.test, 10000) #0.2720839
accuracy(tune = 4, all_sent.test, 10000) #0.2778195
accuracy(tune = 5, all_sent.test, 10000) #0.2784747
accuracy(tune = 6, all_sent.test, 10000) #0.2706336
accuracy(tune = 7, all_sent.test, 10000) #0.2622395
accuracy(tune = 8, all_sent.test, 10000) #0.2703448
accuracy(tune = 9, all_sent.test, 10000) #0.2774379
accuracy(tune = 10, all_sent.test, 10000) #0.2739743
accuracy(tune = 11, all_sent.test, 10000) #0.2785
accuracy(tune = 12, all_sent.test, 10000) #0.2680438
accuracy(tune = 13, all_sent.test, 10000) #0.2785226
accuracy(tune = 14, all_sent.test, 10000) #0.2684581
accuracy(tune = 15, all_sent.test, 10000) #0.2736087
accuracy(tune = 20, all_sent.test, 10000) #0.2810001
accuracy(tune = 22, all_sent.test, 10000) #0.28525
accuracy(tune = 25, all_sent.test, 10000) #0.2814304
accuracy(tune = 30, all_sent.test, 10000) #0.2735448
accuracy(tune = 35, all_sent.test, 10000) #0.2740638
accuracy(tune = 50, all_sent.test, 10000) #0.2798567
accuracy(tune = 100, all_sent.test, 10000) #0.277548
accuracy(tune = 1000, all_sent.test, 10000) #0.2584073

## Changed to (exp(1)*100)^(-(x^(1/pi))/t)
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 2, all_sent.test, 10000) #0.2769771
accuracy(tune = 3, all_sent.test, 10000) #0.272463
accuracy(tune = 4, all_sent.test, 10000) #0.2780695
accuracy(tune = 5, all_sent.test, 10000) #0.2806071
accuracy(tune = 6, all_sent.test, 10000) #0.2725009
accuracy(tune = 7, all_sent.test, 10000) #0.2657545
accuracy(tune = 8, all_sent.test, 10000) #0.2762382
accuracy(tune = 9, all_sent.test, 10000) #0.2850543
accuracy(tune = 10, all_sent.test, 10000) #0.2810219
accuracy(tune = 11, all_sent.test, 10000) #0.28275
accuracy(tune = 12, all_sent.test, 10000) #0.2720746
accuracy(tune = 13, all_sent.test, 10000) #0.2816848
accuracy(tune = 14, all_sent.test, 10000) #0.2720994
accuracy(tune = 15, all_sent.test, 10000) #0.2753714
accuracy(tune = 20, all_sent.test, 10000) #0.2769387
accuracy(tune = 22, all_sent.test, 10000) #0.28475
accuracy(tune = 25, all_sent.test, 10000) #0.2780427
accuracy(tune = 30, all_sent.test, 10000) #0.272046
accuracy(tune = 35, all_sent.test, 10000) #0.2671526
accuracy(tune = 50, all_sent.test, 10000) #0.2736816
accuracy(tune = 100, all_sent.test, 10000) #0.2732857
accuracy(tune = 1000, all_sent.test, 10000) #0.2617827

set.seed(101)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2761053 <-- Baseline
accuracy(tune = 8, all_sent.test, 10000) #0.2762557
accuracy(tune = 9, all_sent.test, 20000) #0.2838124, 20k #0.2757713
accuracy(tune = 10, all_sent.test, 10000) #0.2771596

## Tesing it all for (exp(1)*100)^(-(x^(1/pi))/t)
accuracy(tune = 9, all_sent.test, length(all_sent.test)) #0.2802438
accuracy(tune = 10, all_sent.test, length(all_sent.test)) ##0.2803794##
accuracy(tune = 25, all_sent.test, length(all_sent.test)) #0.2755932
accuracy(tune = FALSE, all_sent.test, length(all_sent.test)) #0.2729945

## Changed to (e*1000)^(-(x^(1/2))/t)
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 2, all_sent.test, 10000) #0.2758491
accuracy(tune = 3, all_sent.test, 10000) #0.272463
accuracy(tune = 4, all_sent.test, 10000) #0.2781945
accuracy(tune = 5, all_sent.test, 10000) #0.277722
accuracy(tune = 6, all_sent.test, 10000) #0.2696377
accuracy(tune = 7, all_sent.test, 10000) #0.2611097
accuracy(tune = 8, all_sent.test, 10000) #0.2705956
accuracy(tune = 9, all_sent.test, 10000) #0.2768136
accuracy(tune = 10, all_sent.test, 10000) #0.2730934
accuracy(tune = 11, all_sent.test, 10000) #0.2785
accuracy(tune = 12, all_sent.test, 10000) #0.2657765
accuracy(tune = 13, all_sent.test, 10000) #0.2759929
accuracy(tune = 14, all_sent.test, 10000) #0.2682069
accuracy(tune = 15, all_sent.test, 10000) #0.2719718
accuracy(tune = 20, all_sent.test, 10000) #0.2789694
accuracy(tune = 22, all_sent.test, 10000) #0.28475
accuracy(tune = 25, all_sent.test, 10000) #0.2799247
accuracy(tune = 30, all_sent.test, 10000) #0.2745441
accuracy(tune = 35, all_sent.test, 10000) #0.2748178
accuracy(tune = 50, all_sent.test, 10000) #0.2831913
accuracy(tune = 100, all_sent.test, 10000) #0.2796791
accuracy(tune = 1000, all_sent.test, 10000) #0.2591574

## Changed to (exp(1)*100)^(-(log(x))/t)
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 2, all_sent.test, 10000) #0.2766011
accuracy(tune = 3, all_sent.test, 10000) #0.2717048
accuracy(tune = 4, all_sent.test, 10000) #0.2784446
accuracy(tune = 5, all_sent.test, 10000) #0.2801054
accuracy(tune = 6, all_sent.test, 10000) #0.2739948
accuracy(tune = 7, all_sent.test, 10000) #0.2676375
accuracy(tune = 8, all_sent.test, 10000) #0.2782445
accuracy(tune = 9, all_sent.test, 10000) #0.2866775
accuracy(tune = 10, all_sent.test, 10000) #0.2836647
accuracy(tune = 11, all_sent.test, 10000) #0.284875
accuracy(tune = 12, all_sent.test, 10000) #0.2725784
accuracy(tune = 13, all_sent.test, 10000) #0.2834556
accuracy(tune = 14, all_sent.test, 10000) #0.2752386
accuracy(tune = 15, all_sent.test, 10000) #0.2776379
accuracy(tune = 20, all_sent.test, 10000) #0.2783348
accuracy(tune = 22, all_sent.test, 10000) #0.285875
accuracy(tune = 25, all_sent.test, 10000) #0.2818068
accuracy(tune = 30, all_sent.test, 10000) #0.2749188
accuracy(tune = 35, all_sent.test, 10000) #0.2715506
accuracy(tune = 50, all_sent.test, 10000) #0.2775102
accuracy(tune = 100, all_sent.test, 10000) #0.2814341
accuracy(tune = 1000, all_sent.test, 10000) #0.2689086


## Changed to (100)^(-log(x+1)/t)
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 2, all_sent.test, 10000) #0.2769771
accuracy(tune = 3, all_sent.test, 10000) #0.272463
accuracy(tune = 4, all_sent.test, 10000) #0.2784446
accuracy(tune = 5, all_sent.test, 10000) #0.2821124
accuracy(tune = 6, all_sent.test, 10000) #0.2751152
accuracy(tune = 7, all_sent.test, 10000) #0.2720311
accuracy(tune = 8, all_sent.test, 10000) #0.2798746
accuracy(tune = 9, all_sent.test, 10000) #0.2880509
accuracy(tune = 10, all_sent.test, 10000) #0.2803927
accuracy(tune = 11, all_sent.test, 10000) #0.284375
accuracy(tune = 12, all_sent.test, 10000) #0.273838
accuracy(tune = 13, all_sent.test, 10000) #0.284088
accuracy(tune = 14, all_sent.test, 10000) #0.2734807
accuracy(tune = 15, all_sent.test, 10000) #0.2753714
accuracy(tune = 20, all_sent.test, 10000) #0.276431
accuracy(tune = 22, all_sent.test, 10000) #0.284625
accuracy(tune = 25, all_sent.test, 10000) #0.2799247
accuracy(tune = 30, all_sent.test, 10000) #0.2739196
accuracy(tune = 35, all_sent.test, 10000) #0.2711737
accuracy(tune = 50, all_sent.test, 10000) #0.2775102
accuracy(tune = 100, all_sent.test, 10000) #0.2816848
accuracy(tune = 1000, all_sent.test, 10000) #0.2689086

set.seed(1223)
accuracy(tune = 3*pi, all_sent.test, 10000) #0.2807789
accuracy(tune = 9, all_sent.test, 10000) #0.281344
accuracy(tune = pi*exp(1), all_sent.test, 10000) #0.292383
accuracy(tune = 8, all_sent.test, 10000) #

## Changed the t to the base, so (10*t)^(-log(x+1)/(pi*exp(1)))
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 1, all_sent.test, 10000) #0.2826169
accuracy(tune = 2, all_sent.test, 10000) #0.274485
accuracy(tune = 3, all_sent.test, 10000) #0.2854464
accuracy(tune = 4, all_sent.test, 10000) #0.2851229
accuracy(tune = 5, all_sent.test, 10000) #0.2736213
accuracy(tune = 6, all_sent.test, 10000) #0.2731609
accuracy(tune = 7, all_sent.test, 10000) #0.2805016
accuracy(tune = 8, all_sent.test, 10000) #0.2880509
accuracy(tune = 9, all_sent.test, 10000) #0.2826579
accuracy(tune = 10, all_sent.test, 10000) #0.284875
accuracy(tune = 11, all_sent.test, 10000) #0.2734601
accuracy(tune = 12, all_sent.test, 10000) #0.2828232
accuracy(tune = 13, all_sent.test, 10000) #0.2766198
accuracy(tune = 14, all_sent.test, 10000) #0.280408
accuracy(tune = 15, all_sent.test, 10000) #0.2841731
accuracy(tune = 20, all_sent.test, 10000) #0.289125
accuracy(tune = 22, all_sent.test, 10000) #0.2854454
accuracy(tune = 25, all_sent.test, 10000) #0.2785411
accuracy(tune = 30, all_sent.test, 10000) #0.2779593
accuracy(tune = 35, all_sent.test, 10000) #0.2886254
accuracy(tune = 50, all_sent.test, 10000) #0.2841921
accuracy(tune = 100, all_sent.test, 10000) #0.2739092
accuracy(tune = 1000, all_sent.test, 10000) #0.2762563

## Changed the t to the base, so (pi*t)^(-log(x+1)/(pi*exp(1))) ##
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 1, all_sent.test, 10000) #0.2687982
accuracy(tune = 2, all_sent.test, 10000) #0.2809452
accuracy(tune = 3, all_sent.test, 10000) #0.2809834
accuracy(tune = 4, all_sent.test, 10000) #0.2829905
accuracy(tune = 5, all_sent.test, 10000) #0.2701481
accuracy(tune = 6, all_sent.test, 10000) #0.2757367
accuracy(tune = 7, all_sent.test, 10000) #0.2871769
accuracy(tune = 8, all_sent.test, 10000) #0.2798893
accuracy(tune = 9, all_sent.test, 10000) #0.283875
accuracy(tune = 10, all_sent.test, 10000) #0.2737121
accuracy(tune = 11, all_sent.test, 10000) #0.2837086
accuracy(tune = 12, all_sent.test, 10000) #0.2762431
accuracy(tune = 13, all_sent.test, 10000) #0.2801561
accuracy(tune = 14, all_sent.test, 10000) #0.284427
accuracy(tune = 15, all_sent.test, 10000) #0.288875
accuracy(tune = 20, all_sent.test, 10000) #0.2865747
accuracy(tune = 22, all_sent.test, 10000) #0.2794154
accuracy(tune = 25, all_sent.test, 10000) #0.2785876
accuracy(tune = 30, all_sent.test, 10000) #0.2899839
accuracy(tune = 33, all_sent.test, 10000) #0.2855352
accuracy(tune = 35, all_sent.test, 10000) #0.2804101
accuracy(tune = 37, all_sent.test, 10000) #0.2820352
accuracy(tune = 40, all_sent.test, 10000) #0.286538
accuracy(tune = 46, all_sent.test, 10000) #0.286875
accuracy(tune = 50, all_sent.test, 10000) #0.2735139
accuracy(tune = 100, all_sent.test, 10000) #0.2730027
accuracy(tune = 1000, all_sent.test, 10000) #0.2750126

## Set.seed seems not to be consistent

set.seed(12321)
accuracy(tune = 10*pi, all_sent.test, 10000) #0.2830545

## Changed the t to the log additive, so (pi*10)^(-log(x+t)/(pi*exp(1)))
set.seed(114)

accuracy(tune = FALSE, all_sent.test, 10000) #0.2750971 <-- Baseline
accuracy(tune = 1, all_sent.test, 10000) #0.2760015
accuracy(tune = 2, all_sent.test, 10000) #0.2851963
accuracy(tune = 3, all_sent.test, 10000) #0.2853738
accuracy(tune = 4, all_sent.test, 10000) #0.2738703
accuracy(tune = 5, all_sent.test, 10000) #0.2731609
accuracy(tune = 6, all_sent.test, 10000) #0.2784953
accuracy(tune = 7, all_sent.test, 10000) #0.2884255
accuracy(tune = 8, all_sent.test, 10000) #0.2812736
accuracy(tune = 9, all_sent.test, 10000) #0.28375
accuracy(tune = 10, all_sent.test, 10000) #0.273838
accuracy(tune = 11, all_sent.test, 10000) #0.284847
accuracy(tune = 12, all_sent.test, 10000) #0.2756153
accuracy(tune = 13, all_sent.test, 10000) #0.2800302
accuracy(tune = 14, all_sent.test, 10000) #0.2849346
accuracy(tune = 15, all_sent.test, 10000) #0.29025
accuracy(tune = 20, all_sent.test, 10000) #0.2870765
accuracy(tune = 22, all_sent.test, 10000) #0.2806645
accuracy(tune = 25, all_sent.test, 10000) #0.2780849
accuracy(tune = 30, all_sent.test, 10000) #0.290725
accuracy(tune = 33, all_sent.test, 10000) #0.2907108
accuracy(tune = 35, all_sent.test, 10000) #0.2817852
accuracy(tune = 37, all_sent.test, 10000) #0.2839196
accuracy(tune = 40, all_sent.test, 10000) #0.2852845

## Changed function to only go up to x grams 
set.seed(114)

accuracy(tune = 15, all_sent.test, 10000) #0.2839 --> 5 grams
accuracy(tune = 15, all_sent.test, 10000) #0.2796 --> 4 grams
accuracy(tune = 15, all_sent.test, 10000) #0.2868 --> 6 grams

## 6-grams but backoff if match index is shorter than 3 words (2 & 1)
set.seed(114)

accuracy(tune = 15, all_sent.test, 10000) #0.2776

## Final test
accuracy(tune = FALSE, all_sent.test, length(all_sent.test)) #0.2729945
accuracy(tune = 15, all_sent.test, length(all_sent.test)) #

#######################################################################
## Re-doing tests, sticking to the same sample
set.seed(123)

small.test <- sample(all_sent.test, 10000)

# Baseline
accuracy(tune = FALSE, small.test, length(small.test)) #0.2703

# Finding base (pi*t)^(-(log(result_table$Nov+1))/(pi*exp(1)))
accuracy(tune = 1, small.test, length(small.test)) #0.275
accuracy(tune = 2, small.test, length(small.test)) #0.2772
accuracy(tune = 3, small.test, length(small.test)) #0.2786
accuracy(tune = 4, small.test, length(small.test)) #0.2802
accuracy(tune = 5, small.test, length(small.test)) #0.2811
accuracy(tune = 6, small.test, length(small.test)) #0.2814
accuracy(tune = 7, small.test, length(small.test)) #0.2827
accuracy(tune = 8, small.test, length(small.test)) #0.2824
accuracy(tune = 9, small.test, length(small.test)) #0.2824
accuracy(tune = 10, small.test, length(small.test)) #0.2821
accuracy(tune = 11, small.test, length(small.test)) #0.2825
accuracy(tune = 12, small.test, length(small.test)) #0.283
accuracy(tune = 13, small.test, length(small.test)) #0.2834
accuracy(tune = 14, small.test, length(small.test)) #0.2831
accuracy(tune = 15, small.test, length(small.test)) #0.2828
accuracy(tune = 20, small.test, length(small.test)) #0.2833
accuracy(tune = 25, small.test, length(small.test)) #0.2834
accuracy(tune = 35, small.test, length(small.test)) #0.2831
accuracy(tune = 50, small.test, length(small.test)) #0.2832

# Finding denom (pi*25)^(-(log(result_table$Nov+1))/(pi*t))
accuracy(tune = 1, small.test, length(small.test)) #0.2733
accuracy(tune = 2, small.test, length(small.test)) #0.281
accuracy(tune = 3, small.test, length(small.test)) #0.2829
accuracy(tune = 4, small.test, length(small.test)) #0.2816
accuracy(tune = 5, small.test, length(small.test)) #0.2789
accuracy(tune = 6, small.test, length(small.test)) #0.2775
accuracy(tune = 7, small.test, length(small.test)) #0.2766
accuracy(tune = 8, small.test, length(small.test)) #0.2762
accuracy(tune = 9, small.test, length(small.test)) #0.2754
accuracy(tune = 10, small.test, length(small.test)) #0.275
accuracy(tune = 11, small.test, length(small.test)) #0.2744
accuracy(tune = 50, small.test, length(small.test)) #0.2738

# Finding log add (pi*25)^(-(log(result_table$Nov+t))/(pi*3))
accuracy(tune = 1, small.test, length(small.test)) #0.2829
accuracy(tune = 2, small.test, length(small.test)) #0.2828
accuracy(tune = 3, small.test, length(small.test)) #0.2828
accuracy(tune = 4, small.test, length(small.test)) #0.2828
accuracy(tune = 5, small.test, length(small.test)) #0.2828
accuracy(tune = 10, small.test, length(small.test)) #0.2828
accuracy(tune = 15, small.test, length(small.test)) #0.2828
accuracy(tune = 20, small.test, length(small.test)) #0.2833
accuracy(tune = 30, small.test, length(small.test)) #0.2835
accuracy(tune = 40, small.test, length(small.test)) #0.2835
accuracy(tune = 50, small.test, length(small.test)) #0.2837
accuracy(tune = 250, small.test, length(small.test)) #0.2843
accuracy(tune = 1000, small.test, length(small.test)) #0.2848
accuracy(tune = 10000, small.test, length(small.test)) #0.2844

#############################################################
#Future additions:

## What if you focus on novelty counts of 10 and below.
## Adjusted these by individual percentages and play with these.
## Find all lines in the test set that have at least 5 matches,
## which all have novelty counts below 10. 

library(parallel)

cluster <- makeCluster(detectCores())

clusterExport(cluster, varlist = c("boff.cmp","predict.ngram", 
                                   "adj.list","decay.adj"), 
              envir = .GlobalEnv)

pred.tables <- parLapply(cluster, 
                         all_sent.test, 
                         predict.ngram, FALSE)

novel.index <- vector(mode = "logical", length = length(pred.tables))

for(i in 1:length(pred.tables)) {
  
  novel.index[i] <- any(pred.tables[[i]]$`Result Table`$Nov <= 10)
  
}

## Index that contains only lines which will be affected by the adjustment
adj.test <- all_sent.test[novel.index]

rm(pred.tables, novel.index)

## Proability adjustment


## Adjusting decay
decay.adj <- function(t = FALSE, result_table) {
  
  if(t == FALSE)
    return(result_table)
  
  # Probability we have left to work with
  prob.space <- 1 - result_table$Prob 
  
  
  
  # Moving the probability towards 1 based on the novelty and previous prob
  result_table$Adj.Prob <- (prob.space * adj) + result_table$Prob
  
  result_table <- result_table[order(-result_table$Adj.Prob),]
  
  return(result_table)
}
